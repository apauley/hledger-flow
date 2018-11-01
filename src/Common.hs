{-# LANGUAGE OverloadedStrings #-}

module Common
    ( docURL
    , lsDirs
    , onlyDirs
    , onlyFiles
    , validDirs
    , filterPaths
    , changeExtension
    , basenameLine
    , buildFilename
    , shellToList
    , takeLast
    , firstLine
    , firstExistingFile
    , toIncludeLines
    , groupValuesBy
    , groupPairs
    , pairBy
    , includeFilePath
    , includePreamble
    , toIncludeFiles
    , toIncludeLine
    , generatedIncludeText
    , groupAndWriteIncludeFiles
    , writeJournals
    , writeJournals'
    , writeMakeItSoJournal
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map

import Data.Function (on)
import qualified Data.List as List (sort, sortBy, groupBy)
import Data.Ord (comparing)

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\ll -> (fst . head $ ll, map snd ll)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

groupShellBy :: (Ord k, Ord v) => (v -> k) -> Shell v -> Shell (Map.Map k [v])
groupShellBy keyFun shellValues = do
  values <- shellToList shellValues
  return $ groupValuesBy keyFun values

docURL :: Line -> Text
docURL = format ("https://github.com/apauley/hledger-makeitso#"%l)

lsDirs :: FilePath -> Shell FilePath
lsDirs = validDirs . ls

onlyDirs :: Shell FilePath -> Shell FilePath
onlyDirs = filterPathsByFileStatus isDirectory

onlyFiles :: Shell FilePath -> Shell FilePath
onlyFiles = filterPathsByFileStatus isRegularFile

filterPathsByFileStatus :: (FileStatus -> Bool) -> Shell FilePath -> Shell FilePath
filterPathsByFileStatus filepred files = do
  files' <- shellToList files
  filtered <- filterPathsByFileStatus' filepred [] files'
  select filtered

filterPathsByFileStatus' :: (FileStatus -> Bool) -> [FilePath] -> [FilePath] -> Shell [FilePath]
filterPathsByFileStatus' _ acc [] = return acc
filterPathsByFileStatus' filepred acc (file:files) = do
  filestat <- stat file
  let filtered = if (filepred filestat) then file:acc else acc
  filterPathsByFileStatus' filepred filtered files

filterPaths :: (FilePath -> IO Bool) -> [FilePath] -> Shell [FilePath]
filterPaths = filterPaths' []

filterPaths' :: [FilePath] -> (FilePath -> IO Bool) -> [FilePath] -> Shell [FilePath]
filterPaths' acc _ [] = return acc
filterPaths' acc filepred (file:files) = do
  shouldInclude <- liftIO $ filepred file
  let filtered = if shouldInclude then file:acc else acc
  filterPaths' filtered filepred files

validDirs :: Shell FilePath -> Shell FilePath
validDirs = excludeWeirdPaths . onlyDirs

excludeWeirdPaths :: Shell FilePath -> Shell FilePath
excludeWeirdPaths = findtree (suffix $ noneOf "_")

firstExistingFile :: [FilePath] -> Shell (Maybe FilePath)
firstExistingFile files = do
  case files of
    []   -> return Nothing
    file:fs -> do
      exists <- testfile file
      if exists then return (Just file) else firstExistingFile fs

basenameLine :: FilePath -> Shell Line
basenameLine path = case (textToLine $ format fp $ basename path) of
  Nothing -> die $ format ("Unable to determine basename from path: "%fp%"\n") path
  Just bn -> return bn

buildFilename :: [Line] -> Text -> FilePath
buildFilename identifiers ext = fromText (T.intercalate "-" (map lineToText identifiers)) <.> ext

shellToList :: Shell a -> Shell [a]
shellToList files = fold files Fold.list

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

firstLine :: Text -> Line
firstLine = NonEmpty.head . textToLines

toIncludeLines :: Shell FilePath -> Shell Line
toIncludeLines paths = do
  journalFile <- paths
  return $ fromMaybe "" $ textToLine $ format ("!include "%fp) journalFile

includeFileName :: FilePath -> FilePath
includeFileName = (<.> "journal"). fromText . (format (fp%"-include")) . dirname

includeFilePath :: FilePath -> FilePath
includeFilePath p = (parent . parent) p </> includeFileName p

toIncludeFiles :: Shell (Map.Map FilePath [FilePath]) -> Shell (Map.Map FilePath Text)
toIncludeFiles fileMap = do
  m <- fileMap
  preMap <- preIncludes $ Map.keys m
  postMap <- postIncludes $ Map.keys m
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

preIncludes :: [FilePath] -> Shell (Map.Map FilePath [FilePath])
preIncludes = preIncludes' Map.empty

preIncludes' :: Map.Map FilePath [FilePath] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
preIncludes' acc [] = return acc
preIncludes' acc (file:files) = do
  pre <- preIncludesForFile file
  preIncludes' (Map.unionWith (++) acc pre) files

preIncludesForFile :: FilePath -> Shell (Map.Map FilePath [FilePath])
preIncludesForFile file = do
  let dirprefix = fst $ T.breakOn "-" $ format fp $ basename file
  let opening = fromText $ format (s%"-opening.journal") dirprefix :: FilePath
  let preFiles = map (directory file </>) [opening]
  filtered <- filterPaths testfile preFiles
  return $ Map.fromList [(file, filtered)]

postIncludes :: [FilePath] -> Shell (Map.Map FilePath [FilePath])
postIncludes = postIncludes' Map.empty

postIncludes' :: Map.Map FilePath [FilePath] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
postIncludes' acc [] = return acc
postIncludes' acc (file:files) = do
  post <- postIncludesForFile file
  postIncludes' (Map.unionWith (++) acc post) files

postIncludesForFile :: FilePath -> Shell (Map.Map FilePath [FilePath])
postIncludesForFile file = do
  let dirprefix = fst $ T.breakOn "-" $ format fp $ basename file
  let closing = fromText $ format (s%"-closing.journal") dirprefix :: FilePath
  let postFiles = map (directory file </>) [closing]
  filtered <- filterPaths testfile postFiles
  return $ Map.fromList [(file, filtered)]

toIncludeFiles' :: Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> Map.Map FilePath Text
toIncludeFiles' preMap postMap = Map.mapWithKey $ generatedIncludeText preMap postMap

addPreamble :: Map.Map FilePath Text -> Map.Map FilePath Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: FilePath -> FilePath -> Text
toIncludeLine base file = format ("!include "%fp) $ fromMaybe file $ stripPrefix (directory base) file

generatedIncludeText :: Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> FilePath -> [FilePath] -> Text
generatedIncludeText preMap postMap outputFile files = do
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let lns = map (toIncludeLine outputFile) $ preFiles ++ List.sort files ++ postFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: Text
includePreamble = "### Generated by hledger-makeitso - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: Shell FilePath -> Shell FilePath
groupAndWriteIncludeFiles = writeFiles . toIncludeFiles . groupShellBy includeFilePath

writeFiles :: Shell (Map.Map FilePath Text) -> Shell FilePath
writeFiles fileMap = do
  m <- fileMap
  writeFiles' m

writeFiles' :: Map.Map FilePath Text -> Shell FilePath
writeFiles' fileMap = do
  liftIO $ writeTextMap fileMap
  select $ Map.keys fileMap

writeTextMap :: Map.Map FilePath Text -> IO ()
writeTextMap = Map.foldlWithKey (\a k v -> a <> writeTextFile k v) (return ())

writeJournals :: FilePath -> Shell FilePath -> Shell ()
writeJournals = writeJournals' sort

writeJournals' :: (Shell FilePath -> Shell [FilePath]) -> FilePath -> Shell FilePath -> Shell ()
writeJournals' sortFun aggregateJournal journals = do
  let journalBaseDir = directory aggregateJournal
  liftIO $ writeTextFile aggregateJournal $ includePreamble <> "\n"
  journalFiles <- sortFun journals
  journalFile <- uniq $ select journalFiles
  let strippedJournal = fromMaybe journalFile $ stripPrefix journalBaseDir journalFile
  liftIO $ append aggregateJournal $ toIncludeLines $ return $ strippedJournal

writeMakeItSoJournal :: FilePath -> Shell FilePath -> Shell ()
writeMakeItSoJournal baseDir importedJournals = do
  let importAggregateJournal = baseDir </> "import-all.journal"
  writeJournals importAggregateJournal importedJournals
  let manualDir = baseDir </> "manual"
  let pre = manualDir </> "pre-import.journal"
  let post = manualDir </> "post-import.journal"
  mktree manualDir
  touch pre
  touch post
  let makeitsoJournal = baseDir </> "makeitso.journal"
  writeJournals' shellToList makeitsoJournal $ select [pre, importAggregateJournal, post]

changeExtension :: Text -> FilePath -> FilePath
changeExtension ext path = (dropExtension path) <.> ext
