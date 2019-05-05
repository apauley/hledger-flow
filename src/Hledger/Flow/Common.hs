{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Common where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GHC.IO.Handle.FD as H

import Data.Maybe
import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map
import Data.Time.LocalTime

import Data.Function (on)
import qualified Data.List as List (nub, null, sort, sortBy, groupBy)
import Data.Ord (comparing)
import Hledger.Flow.Types
import qualified Hledger.Flow.Import.Types as IT
import Control.Concurrent.STM

import qualified Data.List.NonEmpty as NE
import Paths_hledger_flow (version)
import qualified Data.Version as Version (showVersion)

type InputFileBundle = Map.Map FilePath [FilePath]

versionInfo :: NE.NonEmpty Line
versionInfo = textToLines $ T.pack ("hledger-flow " ++ Version.showVersion version)

hledgerPathFromOption :: Maybe FilePath -> IO FilePath
hledgerPathFromOption pathOption = do
  case pathOption of
    Just h  -> do
      isOnDisk <- testfile h
      if isOnDisk then return h else do
        let msg = format ("Unable to find hledger at "%fp) h
        errExit' 1 (T.hPutStrLn H.stderr) msg h
    Nothing -> do
      maybeH <- which "hledger"
      case maybeH of
        Just h  -> return h
        Nothing -> do
          let msg = "Unable to find hledger in your path.\n"
                <> "You need to either install hledger, or add it to your PATH, or provide the path to an hledger executable.\n\n"
                <> "There are a number of installation options on the hledger website: https://hledger.org/download.html"
          errExit' 1 (T.hPutStrLn H.stderr) msg "/"

hledgerVersionFromPath :: FilePath -> IO Text
hledgerVersionFromPath hlp = fmap (T.strip . linesToText) (single $ shellToList $ inproc (format fp hlp) ["--version"] empty)

hledgerInfoFromPath :: Maybe FilePath -> IO HledgerInfo
hledgerInfoFromPath pathOption = do
  hlp <- hledgerPathFromOption pathOption
  hlv <- hledgerVersionFromPath hlp
  return $ HledgerInfo hlp hlv

showCmdArgs :: [Text] -> Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

escapeArg :: Text -> Text
escapeArg a = if (T.count " " a > 0) then "'" <> a <> "'" else a

dummyLogger :: TChan LogMessage -> Text -> IO ()
dummyLogger _ _ = return ()

channelOut :: TChan LogMessage -> Text -> IO ()
channelOut ch txt = atomically $ writeTChan ch $ StdOut txt

channelOutLn :: TChan LogMessage -> Text -> IO ()
channelOutLn ch txt = channelOut ch (txt <> "\n")

channelErr :: TChan LogMessage -> Text -> IO ()
channelErr ch txt = atomically $ writeTChan ch $ StdErr txt

channelErrLn :: TChan LogMessage -> Text -> IO ()
channelErrLn ch txt = channelErr ch (txt <> "\n")

errExit :: Int -> TChan LogMessage -> Text -> a -> IO a
errExit exitStatus ch = errExit' exitStatus (channelErrLn ch)

errExit' :: Int -> (Text -> IO ()) -> Text -> a -> IO a
errExit' exitStatus logFun errorMessage dummyReturnValue = do
  logFun errorMessage
  sleep 0.1
  _ <- exit $ ExitFailure exitStatus
  return dummyReturnValue

timestampPrefix :: Text -> IO Text
timestampPrefix txt = do
  t <- getZonedTime
  return $ format (s%"\thledger-flow "%s) (repr t) txt

logToChannel :: TChan LogMessage -> Text -> IO ()
logToChannel ch msg = do
  ts <- timestampPrefix msg
  channelErrLn ch ts

consoleChannelLoop :: TChan LogMessage -> IO ()
consoleChannelLoop ch = do
  logMsg <- atomically $ readTChan ch
  case logMsg of
    StdOut msg -> do
      T.hPutStr H.stdout msg
      consoleChannelLoop ch
    StdErr msg -> do
      T.hPutStr H.stderr msg
      consoleChannelLoop ch
    Terminate  -> return ()

terminateChannelLoop :: TChan LogMessage -> IO ()
terminateChannelLoop ch = atomically $ writeTChan ch Terminate

logVerbose :: HasVerbosity o => o -> TChan LogMessage -> Text -> IO ()
logVerbose opts ch msg = if (verbose opts) then logToChannel ch msg else return ()

descriptiveOutput :: Text -> Text -> Text
descriptiveOutput outputLabel outTxt = do
  if not (T.null outTxt)
    then format (s%":\n"%s%"\n") outputLabel outTxt
    else ""

logTimedAction :: HasVerbosity o => o -> TChan LogMessage -> Text -> [Text]
  -> (TChan LogMessage -> Text -> IO ()) -> (TChan LogMessage -> Text -> IO ())
  -> IO FullOutput
  -> IO FullTimedOutput
logTimedAction opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger action = do
  logVerbose opts ch $ format ("Begin: "%s) cmdLabel
  if (List.null extraCmdLabels) then return () else logVerbose opts ch $ T.intercalate "\n" extraCmdLabels
  timed@((ec, stdOut, stdErr), diff) <- time action
  stdoutLogger ch stdOut
  stderrLogger ch stdErr
  logVerbose opts ch $ format ("End:   "%s%" "%s%" ("%s%")") cmdLabel (repr ec) (repr diff)
  return timed

timeAndExitOnErr :: (HasSequential o, HasVerbosity o) => o -> TChan LogMessage -> Text
  -> (TChan LogMessage -> Text -> IO ()) -> (TChan LogMessage -> Text -> IO ())
  -> ProcFun -> ProcInput
  -> IO FullTimedOutput
timeAndExitOnErr opts ch cmdLabel = timeAndExitOnErr' opts ch cmdLabel []

timeAndExitOnErr' :: (HasSequential o, HasVerbosity o) => o -> TChan LogMessage -> Text -> [Text]
  -> (TChan LogMessage -> Text -> IO ()) -> (TChan LogMessage -> Text -> IO ())
  -> ProcFun -> ProcInput
  -> IO FullTimedOutput
timeAndExitOnErr' opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger procFun (cmd, args, stdInput) = do
  let action = procFun cmd args stdInput
  timed@((ec, stdOut, stdErr), _) <- logTimedAction opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger action
  case ec of
    ExitFailure i -> do
      let cmdText = format (s%" "%s) cmd $ showCmdArgs args
      let msgOut = descriptiveOutput "Standard output" stdOut
      let msgErr = descriptiveOutput "Error output" stdErr

      let exitMsg = format ("\n=== Begin Error: "%s%" ===\nExternal command:\n"%s%"\nExit code "%d%"\n"
                            %s%s%"=== End Error: "%s%" ===\n") cmdLabel cmdText i msgOut msgErr cmdLabel
      errExit i ch exitMsg timed
    ExitSuccess -> return timed

procWithEmptyOutput :: ProcFun
procWithEmptyOutput cmd args stdinput = do
  ec <- proc cmd args stdinput
  return (ec, T.empty, T.empty)

parAwareProc :: HasSequential o => o -> ProcFun
parAwareProc opts = if (sequential opts) then procWithEmptyOutput else procStrictWithErr

inprocWithErrFun :: (Text -> IO ()) -> ProcInput -> Shell Line
inprocWithErrFun errFun (cmd, args, standardInput) = do
  result <- inprocWithErr cmd args standardInput
  case result of
    Right ln -> return ln
    Left  ln -> do
      (liftIO . errFun . lineToText) ln
      empty

verboseTestFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> FilePath -> IO Bool
verboseTestFile opts ch p = do
  fileExists <- testfile p
  let rel = relativeToBase opts p
  if fileExists
    then logVerbose opts ch $ format ("Found a "       %fp%" file at '"%fp%"'") (basename rel) rel
    else logVerbose opts ch $ format ("Did not find a "%fp%" file at '"%fp%"'") (basename rel) rel
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
allYearsPath = allYearsPath' directory

allYearsPath' :: (FilePath -> FilePath) -> FilePath -> FilePath
allYearsPath' dir p = dir p </> "all-years.journal"

groupIncludeFiles :: [FilePath] -> (InputFileBundle, InputFileBundle)
groupIncludeFiles = allYearIncludeFiles . groupIncludeFilesPerYear

groupIncludeFilesPerYear :: [FilePath] -> InputFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p:_) = case extractImportDirs p of
    Right _ -> (groupValuesBy initialIncludeFilePath) ps
    Left  _ -> (groupValuesBy parentIncludeFilePath)  ps

allYearIncludeFiles :: InputFileBundle -> (InputFileBundle, InputFileBundle)
allYearIncludeFiles m = (m, yearsIncludeMap $ Map.keys m)

yearsIncludeMap :: [FilePath] -> InputFileBundle
yearsIncludeMap = groupValuesBy allYearsPath

docURL :: Line -> Text
docURL = format ("https://github.com/apauley/hledger-flow#"%l)

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

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
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

toIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> InputFileBundle -> Shell (Map.Map FilePath Text)
toIncludeFiles opts ch m = do
  preMap  <- extraIncludes opts ch (Map.keys m) ["opening.journal"] ["pre-import.journal"]
  postMap <- extraIncludes opts ch (Map.keys m) ["closing.journal"] ["post-import.journal"]
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

extraIncludes :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [FilePath] -> [Text] -> [FilePath] -> Shell (InputFileBundle)
extraIncludes opts ch = extraIncludes' opts ch Map.empty

extraIncludes' :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> InputFileBundle -> [FilePath] -> [Text] -> [FilePath] -> Shell (InputFileBundle)
extraIncludes' _ _ acc [] _ _ = return acc
extraIncludes' opts ch acc (file:files) extraSuffixes manualFiles = do
  extra <- extraIncludesForFile opts ch file extraSuffixes manualFiles
  extraIncludes' opts ch (Map.unionWith (++) acc extra) files extraSuffixes manualFiles

extraIncludesForFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> FilePath -> [Text] -> [FilePath] -> Shell (InputFileBundle)
extraIncludesForFile opts ch file extraSuffixes manualFiles = do
  let dirprefix = fromText $ fst $ T.breakOn "-" $ format fp $ basename file
  let fileNames = map (\suff -> fromText $ format (fp%"-"%s) dirprefix suff) extraSuffixes
  let suffixFiles = map (directory file </>) fileNames
  let suffixDirFiles = map (directory file </> "_manual_" </> dirprefix </>) manualFiles
  let extraFiles = suffixFiles ++ suffixDirFiles
  filtered <- filterPaths testfile extraFiles
  let logMsg = format ("Looking for possible extra include files for '"%fp%"' among these "%d%" options: "%s%". Found "%d%": "%s)
               (relativeToBase opts file) (length extraFiles) (repr $ relativeFilesAsText opts extraFiles)
               (length filtered) (repr $ relativeFilesAsText opts filtered)
  liftIO $ logVerbose opts ch logMsg
  return $ Map.fromList [(file, filtered)]

relativeFilesAsText :: HasBaseDir o => o -> [FilePath] -> [Text]
relativeFilesAsText opts ps = map ((format fp) . (relativeToBase opts)) ps

toIncludeFiles' :: InputFileBundle -> InputFileBundle -> InputFileBundle -> Map.Map FilePath Text
toIncludeFiles' preMap postMap = Map.mapWithKey $ generatedIncludeText preMap postMap

addPreamble :: Map.Map FilePath Text -> Map.Map FilePath Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: FilePath -> FilePath -> Text
toIncludeLine base file = format ("!include "%fp) $ relativeToBase' base file

generatedIncludeText :: InputFileBundle -> InputFileBundle -> FilePath -> [FilePath] -> Text
generatedIncludeText preMap postMap outputFile fs = do
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let files = List.nub . List.sort $ fs
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let lns = map (toIncludeLine $ directory outputFile) $ preFiles ++ files ++ postFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: Text
includePreamble = "### Generated by hledger-flow - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [FilePath] -> Shell [FilePath]
groupAndWriteIncludeFiles opts ch = writeFileMap opts ch . groupIncludeFiles

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

writeFileMap :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> (InputFileBundle, InputFileBundle) -> Shell [FilePath]
writeFileMap opts ch (m, allYears) = do
  _ <- writeFiles' $ (addPreamble . toIncludeFiles' Map.empty Map.empty) allYears
  writeFiles . (toIncludeFiles opts ch) $ m

writeIncludesUpTo :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> FilePath -> [FilePath] -> Shell [FilePath]
writeIncludesUpTo _ _ _ [] = return []
writeIncludesUpTo opts ch stopAt paths = do
  let shouldStop = any (\dir -> dir == stopAt) $ map dirname paths
  if shouldStop
    then do
      let allTop = groupValuesBy (allYearsPath' (parent . parent)) paths
      writeFileMap opts ch (Map.empty, allTop)
    else do
      newPaths <- groupAndWriteIncludeFiles opts ch paths
      writeIncludesUpTo opts ch stopAt newPaths

changeExtension :: Text -> FilePath -> FilePath
changeExtension ext path = (dropExtension path) <.> ext

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir file = if (file == "1-in/" || file == "2-preprocessed/") then newOutputLocation else file

errorMessageBaseDir :: FilePath -> Text
errorMessageBaseDir startDir = format ("\nUnable to find an hledger-flow import directory at '"%fp
                                       %"' (or in any of its parent directories).\n\n"
                                       %"Have a look at the documentation for more information:\n"%s%"\n")
                               startDir (docURL "getting-started")

determineBaseDir :: Maybe FilePath -> IO FilePath
determineBaseDir (Just suppliedDir) = determineBaseDir' suppliedDir
determineBaseDir Nothing = pwd >>= determineBaseDir'

determineBaseDir' :: FilePath -> IO FilePath
determineBaseDir' startDir = do
  ebd <- determineBaseDir'' startDir startDir
  case ebd of
    Right bd -> return bd
    Left  t  -> die t

determineBaseDir'' :: FilePath -> FilePath -> IO (Either Text FilePath)
determineBaseDir'' startDir currentDir = do
  foundBaseDir <- testdir $ currentDir </> "import"
  if foundBaseDir
    then return $ Right $ forceTrailingSlash currentDir
    else
    do
      let doneSearching = (currentDir `elem` ["/", "./"])
      if doneSearching
        then return $ Left $ errorMessageBaseDir startDir
        else determineBaseDir'' startDir $ parent currentDir

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
                      %"\n\nhledger-flow expects to find input files in this structure:\n"%
                      "import/owner/bank/account/filestate/year/trxfile\n\n"%
                      "Have a look at the documentation for a detailed explanation:\n"%s) inputFile (docURL "input-files")
