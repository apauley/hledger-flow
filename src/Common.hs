{-# LANGUAGE OverloadedStrings #-}

module Common
    ( docURL
    , logErr
    , lsDirs
    , onlyFiles
    , filterPaths
    , changeExtension
    , basenameLine
    , buildFilename
    , shellToList
    , firstExistingFile
    , groupValuesBy
    , groupPairs
    , pairBy
    , includeFilePath
    , includePreamble
    , toIncludeFiles
    , toIncludeLine
    , groupAndWriteIncludeFiles
    , writeJournals
    , writeJournals'
    , writeMakeItSoJournal
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import qualified Data.Text as T
import Data.Maybe
import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map
import Data.Time.Clock

import Data.Function (on)
import qualified Data.List as List (nub, sort, sortBy, groupBy)
import Data.Ord (comparing)

logLines :: (Shell Line -> IO ()) -> Text -> Shell ()
logLines logfun msg = do
  t <- liftIO $ getCurrentTime
  liftIO $ logfun $ select $ textToLines $ format (s%" "%s) (repr t) msg

logErr :: Text -> Shell ()
logErr = logLines stderr

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\ll -> (fst . head $ ll, map snd ll)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

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

toIncludeLines :: Shell FilePath -> Shell Line
toIncludeLines paths = do
  journalFile <- paths
  return $ fromMaybe "" $ textToLine $ format ("!include "%fp) journalFile

includeFileName :: FilePath -> FilePath
includeFileName = (<.> "journal"). fromText . (format (fp%"-include")) . dirname

includeFilePath :: FilePath -> FilePath
includeFilePath p = (parent . parent) p </> includeFileName p

toIncludeFiles :: Map.Map FilePath [FilePath] -> Shell (Map.Map FilePath Text)
toIncludeFiles m = do
  preMap <- extraIncludes (Map.keys m) ["opening.journal"]
  postMap <- extraIncludes (Map.keys m) ["closing.journal"]
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

extraIncludes :: [FilePath] -> [Text] -> Shell (Map.Map FilePath [FilePath])
extraIncludes = extraIncludes' Map.empty

extraIncludes' :: Map.Map FilePath [FilePath] -> [FilePath] -> [Text] -> Shell (Map.Map FilePath [FilePath])
extraIncludes' acc [] _ = return acc
extraIncludes' acc (file:files) fileSuffixes = do
  extra <- extraIncludesForFile file fileSuffixes
  extraIncludes' (Map.unionWith (++) acc extra) files fileSuffixes

extraIncludesForFile :: FilePath -> [Text] -> Shell (Map.Map FilePath [FilePath])
extraIncludesForFile file fileSuffixes = do
  let dirprefix = fst $ T.breakOn "-" $ format fp $ basename file
  let fileNames = map (\suff -> fromText $ format (s%"-"%s) dirprefix suff) fileSuffixes
  let extraFiles = map (directory file </>) fileNames
  filtered <- filterPaths testfile extraFiles
  return $ Map.fromList [(file, filtered)]

toIncludeFiles' :: Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> Map.Map FilePath Text
toIncludeFiles' preMap postMap = Map.mapWithKey $ generatedIncludeText preMap postMap

addPreamble :: Map.Map FilePath Text -> Map.Map FilePath Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: FilePath -> FilePath -> Text
toIncludeLine base file = format ("!include "%fp) $ fromMaybe file $ stripPrefix (directory base) file

generatedIncludeText :: Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> FilePath -> [FilePath] -> Text
generatedIncludeText preMap postMap outputFile fs = do
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let files = List.nub . List.sort $ fs
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let lns = map (toIncludeLine outputFile) $ preFiles ++ files ++ postFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: Text
includePreamble = "### Generated by hledger-makeitso - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: [FilePath] -> Shell [FilePath]
groupAndWriteIncludeFiles = writeFileMap . groupValuesBy includeFilePath

writeFiles :: Shell (Map.Map FilePath Text) -> Shell [FilePath]
writeFiles fileMap = do
  m <- fileMap
  writeFiles' m

writeFiles' :: Map.Map FilePath Text -> Shell [FilePath]
writeFiles' fileMap = do
  liftIO $ writeTextMap fileMap
  return $ Map.keys fileMap

writeTextMap :: Map.Map FilePath Text -> IO ()
writeTextMap = Map.foldlWithKey (\a k v -> a <> writeTextFile k v) (return ())

writeFileMap :: Map.Map FilePath [FilePath] -> Shell [FilePath]
writeFileMap = writeFiles . toIncludeFiles

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

writeMakeItSoJournal :: FilePath -> [FilePath] -> Shell [FilePath]
writeMakeItSoJournal baseDir importedJournals = do
  let makeitsoJournal = baseDir </> "makeitso.journal"
  writeFileMap $ Map.singleton makeitsoJournal importedJournals

changeExtension :: Text -> FilePath -> FilePath
changeExtension ext path = (dropExtension path) <.> ext
