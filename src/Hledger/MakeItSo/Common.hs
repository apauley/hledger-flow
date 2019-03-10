{-# LANGUAGE OverloadedStrings #-}

module Hledger.MakeItSo.Common
    ( docURL
    , logVerbose
    , logVerboseTime
    , verboseTestFile
    , relativeToBase
    , lsDirs
    , onlyFiles
    , onlyDirs
    , filterPaths
    , changePathAndExtension
    , basenameLine
    , buildFilename
    , shellToList
    , firstExistingFile
    , groupValuesBy
    , groupIncludeFiles
    , extraIncludesForFile
    , groupPairs
    , pairBy
    , includeFilePath
    , includePreamble
    , toIncludeFiles
    , toIncludeLine
    , groupAndWriteIncludeFiles
    , writeIncludesUpTo
    , writeMakeItSoJournal
    , dirOrPwd
    , extractImportDirs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import qualified Data.Text as T
import Data.Maybe
import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map
import Data.Time.LocalTime

import Data.Function (on)
import qualified Data.List as List (nub, sort, sortBy, groupBy)
import Data.Ord (comparing)
import Hledger.MakeItSo.Data.Types

logLines :: (Shell Line -> IO ()) -> Text -> Shell ()
logLines logfun msg = do
  t <- liftIO $ getZonedTime
  liftIO $ logfun $ select $ textToLines $ format (s%"\thledger-makeitso "%s) (repr t) msg

logErr :: Text -> Shell ()
logErr = logLines stderr

logVerbose :: HMISOptions -> Text -> Shell ()
logVerbose opts msg = if (verbose opts) then logErr msg else return ()

logVerboseTime :: HMISOptions -> Text -> IO a -> IO (a, NominalDiffTime)
logVerboseTime opts msg action = do
  sh $ logVerbose opts $ format ("Begin: "%s) msg
  (result, diff) <- time action
  sh $ logVerbose opts $ format ("End:   "%s%" ("%s%")") msg $ repr diff
  return (result, diff)

verboseTestFile :: HMISOptions -> FilePath -> Shell Bool
verboseTestFile opts p = do
  fileExists <- testfile p
  let rel = relativeToBase opts p
  if fileExists
    then logVerbose opts $ format ("Found a "       %fp%" file at '"%fp%"'") (basename rel) rel
    else logVerbose opts $ format ("Did not find a "%fp%" file at '"%fp%"'") (basename rel) rel
  return fileExists

relativeToBase :: HMISOptions -> FilePath -> FilePath
relativeToBase opts p = fromMaybe p $ stripPrefix (directory $ baseDir opts) p

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\ll -> (fst . head $ ll, map snd ll)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

groupIncludeFiles :: [FilePath] -> Map.Map FilePath [FilePath]
groupIncludeFiles = groupValuesBy includeFilePath

docURL :: Line -> Text
docURL = format ("https://github.com/apauley/hledger-makeitso#"%l)

lsDirs :: FilePath -> Shell FilePath
lsDirs = onlyDirs . ls

onlyDirs :: Shell FilePath -> Shell FilePath
onlyDirs = excludeHiddenFiles . excludeWeirdPaths . filterPathsByFileStatus isDirectory

onlyFiles :: Shell FilePath -> Shell FilePath
onlyFiles = excludeHiddenFiles . filterPathsByFileStatus isRegularFile

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

excludeHiddenFiles :: Shell FilePath -> Shell FilePath
excludeHiddenFiles paths = do
  p <- paths
  case (match (prefix ".") $ format fp $ filename p) of
    [] -> select [p]
    _  -> select []

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

includeFileName :: FilePath -> FilePath
includeFileName = (<.> "journal"). fromText . (format (fp%"-include")) . dirname

includeFilePath :: FilePath -> FilePath
includeFilePath p = (parent . parent) p </> includeFileName p

toIncludeFiles :: Map.Map FilePath [FilePath] -> Shell (Map.Map FilePath Text)
toIncludeFiles m = do
  preMap  <- extraIncludes (Map.keys m) ["opening.journal",  "pre-import.journal"] ["_manual_/pre-import.journal"]
  postMap <- extraIncludes (Map.keys m) ["post-import.journal", "closing.journal"] ["_manual_/post-import.journal"]
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

extraIncludes :: [FilePath] -> [Text] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
extraIncludes = extraIncludes' Map.empty

extraIncludes' :: Map.Map FilePath [FilePath] -> [FilePath] -> [Text] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
extraIncludes' acc [] _ _ = return acc
extraIncludes' acc (file:files) extraSuffixes filesToTest = do
  extra <- extraIncludesForFile file extraSuffixes filesToTest
  extraIncludes' (Map.unionWith (++) acc extra) files extraSuffixes filesToTest

extraIncludesForFile :: FilePath -> [Text] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
extraIncludesForFile file extraSuffixes filesToTest = do
  let dirprefix = fromText $ fst $ T.breakOn "-" $ format fp $ basename file
  let fileNames = map (\suff -> fromText $ format (fp%"-"%s) dirprefix suff) extraSuffixes
  let suffixFiles = map (directory file </>) fileNames
  let suffixDirFiles = map (directory file </> dirprefix </>) filesToTest
  let extraFiles = suffixFiles ++ suffixDirFiles
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
groupAndWriteIncludeFiles = writeFileMap . groupIncludeFiles

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

writeIncludesUpTo :: FilePath -> [FilePath] -> Shell [FilePath]
writeIncludesUpTo _ [] = return []
writeIncludesUpTo stopAt paths = do
  let shouldStop = any (\dir -> dir == stopAt) $ map dirname paths
  if shouldStop
    then groupAndWriteIncludeFiles paths else
    do
      newPaths <- groupAndWriteIncludeFiles paths
      writeIncludesUpTo stopAt newPaths

writeMakeItSoJournal :: FilePath -> [FilePath] -> Shell [FilePath]
writeMakeItSoJournal bd importedJournals = do
  let makeitsoJournal = bd </> "makeitso.journal"
  writeFileMap $ Map.singleton makeitsoJournal importedJournals

changeExtension :: Text -> FilePath -> FilePath
changeExtension ext path = (dropExtension path) <.> ext

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir file = if (file == "1-in/" || file == "2-preprocessed/") then newOutputLocation else file

dirOrPwd :: Maybe FilePath -> IO FilePath
dirOrPwd maybeBaseDir = fmap (\p -> directory (p </> "temp")) (fromMaybe pwd $ fmap realpath maybeBaseDir)

importDirBreakdown ::  FilePath -> [FilePath]
importDirBreakdown = importDirBreakdown' []

importDirBreakdown' :: [FilePath] -> FilePath -> [FilePath]
importDirBreakdown' acc path = do
  let dir = directory path
  if (dirname dir == "import" || (dirname dir == ""))
    then dir:acc
    else importDirBreakdown' (dir:acc) $ parent dir

extractImportDirs :: FilePath -> Either Text ImportDirs
extractImportDirs inputFile = do
  case importDirBreakdown inputFile of
    [bd,owner,bank,account,filestate,year] -> Right $ ImportDirs bd owner bank account filestate year
    _ -> do
      Left $ format ("I couldn't find the right number of directories between \"import\" and the input file:\n"%fp
                      %"\n\nhledger-makitso expects to find input files in this structure:\n"%
                      "import/owner/bank/account/filestate/year/trxfile\n\n"%
                      "Have a look at the documentation for a detailed explanation:\n"%s) inputFile (docURL "input-files")
