{-# LANGUAGE OverloadedStrings #-}

module Hledger.MakeItSo.Common
    ( docURL
    , showCmdArgs
    , logVerbose
    , logVerboseTime
    , verboseTestFile
    , relativeToBase
    , relativeToBase'
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
    , allYearIncludeFiles
    , yearsIncludeMap
    , extraIncludesForFile
    , groupPairs
    , pairBy
    , includePreamble
    , toIncludeFiles
    , toIncludeLine
    , groupAndWriteIncludeFiles
    , writeIncludesUpTo
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
import Hledger.MakeItSo.Types
import qualified Hledger.MakeItSo.Import.Types as IT

showCmdArgs :: [Text] -> Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

escapeArg :: Text -> Text
escapeArg a = if (T.count " " a > 0) then "'" <> a <> "'" else a

logLines :: (Shell Line -> IO ()) -> Text -> Shell ()
logLines logfun msg = do
  t <- liftIO $ getZonedTime
  liftIO $ logfun $ select $ textToLines $ format (s%"\thledger-makeitso "%s) (repr t) msg

logErr :: Text -> Shell ()
logErr = logLines stderr

logVerbose :: HasVerbosity o => o -> Text -> Shell ()
logVerbose opts msg = if (verbose opts) then logErr msg else return ()

logVerboseTime :: HasVerbosity o => o -> Text -> IO a -> IO (a, NominalDiffTime)
logVerboseTime opts msg action = do
  sh $ logVerbose opts $ format ("Begin: "%s) msg
  (result, diff) <- time action
  sh $ logVerbose opts $ format ("End:   "%s%" ("%s%")") msg $ repr diff
  return (result, diff)

verboseTestFile :: (HasVerbosity o, HasBaseDir o) => o -> FilePath -> Shell Bool
verboseTestFile opts p = do
  fileExists <- testfile p
  let rel = relativeToBase opts p
  if fileExists
    then logVerbose opts $ format ("Found a "       %fp%" file at '"%fp%"'") (basename rel) rel
    else logVerbose opts $ format ("Did not find a "%fp%" file at '"%fp%"'") (basename rel) rel
  return fileExists

relativeToBase :: HasBaseDir o => o -> FilePath -> FilePath
relativeToBase opts = relativeToBase' (baseDir opts)

relativeToBase' :: FilePath -> FilePath -> FilePath
relativeToBase' bd p = fromMaybe p $ stripPrefix (forceTrailingSlash bd) p

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\ll -> (fst . head $ ll, map snd ll)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

initialIncludeFilePath :: FilePath -> FilePath
initialIncludeFilePath p = (parent . parent . parent) p </> includeFileName p

parentIncludeFilePath :: FilePath -> FilePath
parentIncludeFilePath p = (parent . parent) p </> (filename p)

allYearsPath :: FilePath -> FilePath
allYearsPath p = directory p </> "all-years.journal"

groupIncludeFiles :: [FilePath] -> (Map.Map FilePath [FilePath], Map.Map FilePath [FilePath])
groupIncludeFiles = allYearIncludeFiles . groupIncludeFilesPerYear

groupIncludeFilesPerYear :: [FilePath] -> Map.Map FilePath [FilePath]
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p:_) = if (dirname p == "import")
  then Map.singleton (((parent . parent) p) </> "makeitso.journal") ps
  else case extractImportDirs p of
    Right _ -> (groupValuesBy initialIncludeFilePath) ps
    Left  _ -> (groupValuesBy parentIncludeFilePath)  ps

allYearIncludeFiles :: Map.Map FilePath [FilePath] -> (Map.Map FilePath [FilePath], Map.Map FilePath [FilePath])
allYearIncludeFiles m = (m, yearsIncludeMap $ Map.keys m)

yearsIncludeMap :: [FilePath] -> Map.Map FilePath [FilePath]
yearsIncludeMap = groupValuesBy allYearsPath

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
includeFileName = (<.> "journal") . fromText . (format (fp%"-include")) . dirname

toIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> Map.Map FilePath [FilePath] -> Shell (Map.Map FilePath Text)
toIncludeFiles opts m = do
  preMap  <- extraIncludes opts (Map.keys m) ["opening.journal"] ["pre-import.journal"]
  postMap <- extraIncludes opts (Map.keys m) ["closing.journal"] ["post-import.journal"]
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

extraIncludes :: (HasBaseDir o, HasVerbosity o) => o -> [FilePath] -> [Text] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
extraIncludes opts = extraIncludes' opts Map.empty

extraIncludes' :: (HasBaseDir o, HasVerbosity o) => o -> Map.Map FilePath [FilePath] -> [FilePath] -> [Text] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
extraIncludes' _ acc [] _ _ = return acc
extraIncludes' opts acc (file:files) extraSuffixes manualFiles = do
  extra <- extraIncludesForFile opts file extraSuffixes manualFiles
  extraIncludes' opts (Map.unionWith (++) acc extra) files extraSuffixes manualFiles

extraIncludesForFile :: (HasVerbosity o, HasBaseDir o) => o -> FilePath -> [Text] -> [FilePath] -> Shell (Map.Map FilePath [FilePath])
extraIncludesForFile opts file extraSuffixes manualFiles = do
  let dirprefix = fromText $ fst $ T.breakOn "-" $ format fp $ basename file
  let fileNames = map (\suff -> fromText $ format (fp%"-"%s) dirprefix suff) extraSuffixes
  let suffixFiles = map (directory file </>) fileNames
  let suffixDirFiles = map (directory file </> "_manual_" </> dirprefix </>) manualFiles
  let extraFiles = suffixFiles ++ suffixDirFiles
  filtered <- filterPaths testfile extraFiles
  let logMsg = format ("Looking for possible extra include files for '"%fp%"' among these "%d%" options: "%s%". Found "%d%": "%s)
               (relativeToBase opts file) (length extraFiles) (repr $ relativeFilesAsText opts extraFiles)
               (length filtered) (repr $ relativeFilesAsText opts filtered)
  logVerbose opts logMsg
  return $ Map.fromList [(file, filtered)]

relativeFilesAsText :: HasBaseDir o => o -> [FilePath] -> [Text]
relativeFilesAsText opts ps = map ((format fp) . (relativeToBase opts)) ps

toIncludeFiles' :: Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> Map.Map FilePath Text
toIncludeFiles' preMap postMap = Map.mapWithKey $ generatedIncludeText preMap postMap

addPreamble :: Map.Map FilePath Text -> Map.Map FilePath Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: FilePath -> FilePath -> Text
toIncludeLine base file = format ("!include "%fp) $ relativeToBase' base file

generatedIncludeText :: Map.Map FilePath [FilePath] -> Map.Map FilePath [FilePath] -> FilePath -> [FilePath] -> Text
generatedIncludeText preMap postMap outputFile fs = do
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let files = List.nub . List.sort $ fs
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let lns = map (toIncludeLine $ directory outputFile) $ preFiles ++ files ++ postFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: Text
includePreamble = "### Generated by hledger-makeitso - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> [FilePath] -> Shell [FilePath]
groupAndWriteIncludeFiles opts = writeFileMap opts . groupIncludeFiles

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

writeFileMap :: (HasBaseDir o, HasVerbosity o) => o -> (Map.Map FilePath [FilePath], Map.Map FilePath [FilePath]) -> Shell [FilePath]
writeFileMap opts (m, allYears) = do
  _ <- writeFiles' $ (addPreamble . toIncludeFiles' Map.empty Map.empty) allYears
  writeFiles . (toIncludeFiles opts) $ m

writeIncludesUpTo :: (HasBaseDir o, HasVerbosity o) => o -> FilePath -> [FilePath] -> Shell [FilePath]
writeIncludesUpTo _ _ [] = return []
writeIncludesUpTo opts stopAt paths = do
  let shouldStop = any (\dir -> dir == stopAt) $ map dirname paths
  if shouldStop
    then groupAndWriteIncludeFiles opts paths else
    do
      newPaths <- groupAndWriteIncludeFiles opts paths
      writeIncludesUpTo opts stopAt newPaths

changeExtension :: Text -> FilePath -> FilePath
changeExtension ext path = (dropExtension path) <.> ext

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir file = if (file == "1-in/" || file == "2-preprocessed/") then newOutputLocation else file

dirOrPwd :: Maybe FilePath -> IO FilePath
dirOrPwd maybeBaseDir = fmap forceTrailingSlash (fromMaybe pwd $ fmap realpath maybeBaseDir)

forceTrailingSlash :: FilePath -> FilePath
forceTrailingSlash p = directory (p </> "temp")

importDirBreakdown ::  FilePath -> [FilePath]
importDirBreakdown = importDirBreakdown' []

importDirBreakdown' :: [FilePath] -> FilePath -> [FilePath]
importDirBreakdown' acc path = do
  let dir = directory path
  if (dirname dir == "import" || (dirname dir == ""))
    then dir:acc
    else importDirBreakdown' (dir:acc) $ parent dir

extractImportDirs :: FilePath -> Either Text IT.ImportDirs
extractImportDirs inputFile = do
  case importDirBreakdown inputFile of
    [bd,owner,bank,account,filestate,year] -> Right $ IT.ImportDirs bd owner bank account filestate year
    _ -> do
      Left $ format ("I couldn't find the right number of directories between \"import\" and the input file:\n"%fp
                      %"\n\nhledger-makitso expects to find input files in this structure:\n"%
                      "import/owner/bank/account/filestate/year/trxfile\n\n"%
                      "Have a look at the documentation for a detailed explanation:\n"%s) inputFile (docURL "input-files")
