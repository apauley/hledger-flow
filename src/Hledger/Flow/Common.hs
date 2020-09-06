{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Common where

import qualified Turtle
import Turtle ((%), (</>), (<.>))

import Prelude hiding (putStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified GHC.IO.Handle.FD as H

import Data.Char (isDigit)
import Data.Maybe
import Data.Either

import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map

import Data.Function (on)
import qualified Data.List as List (nub, null, sort, sortBy, groupBy)
import Data.Ord (comparing)

import Hledger.Flow.Types
import qualified Hledger.Flow.Import.Types as IT

import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.BaseDir (turtleBaseDir, relativeToBase, relativeToBase')
import Hledger.Flow.DocHelpers (docURL)

import Control.Concurrent.STM

import qualified Data.List.NonEmpty as NE
import Paths_hledger_flow (version)
import qualified Data.Version as Version (showVersion)
import qualified System.Info as Sys

type InputFileBundle = Map.Map TurtlePath [TurtlePath]

versionInfo :: NE.NonEmpty Turtle.Line
versionInfo = Turtle.textToLines versionInfo'

versionInfo' :: T.Text
versionInfo' = T.pack ("hledger-flow " ++ Version.showVersion version ++ " " ++
                       os systemInfo ++ " " ++ arch systemInfo ++ " " ++
                       compilerName systemInfo ++ " " ++
                       Version.showVersion (compilerVersion systemInfo))

systemInfo :: SystemInfo
systemInfo = SystemInfo { os = Sys.os
                        , arch = Sys.arch
                        , compilerName = Sys.compilerName
                        , compilerVersion = Sys.compilerVersion
                        }

hledgerPathFromOption :: Maybe TurtlePath -> IO TurtlePath
hledgerPathFromOption pathOption = do
  case pathOption of
    Just h  -> do
      isOnDisk <- Turtle.testfile h
      if isOnDisk then return h else do
        let msg = Turtle.format ("Unable to find hledger at "%Turtle.fp) h
        errExit' 1 (T.hPutStrLn H.stderr) msg h
    Nothing -> do
      maybeH <- Turtle.which "hledger"
      case maybeH of
        Just h  -> return h
        Nothing -> do
          let msg = "Unable to find hledger in your path.\n"
                <> "You need to either install hledger, or add it to your PATH, or provide the path to an hledger executable.\n\n"
                <> "There are a number of installation options on the hledger website: https://hledger.org/download.html"
          errExit' 1 (T.hPutStrLn H.stderr) msg "/"

hledgerVersionFromPath :: TurtlePath -> IO T.Text
hledgerVersionFromPath hlp = fmap (T.strip . Turtle.linesToText) (Turtle.single $ shellToList $ Turtle.inproc (Turtle.format Turtle.fp hlp) ["--version"] Turtle.empty)

hledgerInfoFromPath :: Maybe TurtlePath -> IO HledgerInfo
hledgerInfoFromPath pathOption = do
  hlp <- hledgerPathFromOption pathOption
  hlv <- hledgerVersionFromPath hlp
  return $ HledgerInfo hlp hlv

showCmdArgs :: [T.Text] -> T.Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

escapeArg :: T.Text -> T.Text
escapeArg a = if T.count " " a > 0 then "'" <> a <> "'" else a

errExit :: Int -> TChan LogMessage -> T.Text -> a -> IO a
errExit exitStatus ch = errExit' exitStatus (channelErrLn ch)

errExit' :: Int -> (T.Text -> IO ()) -> T.Text -> a -> IO a
errExit' exitStatus logFun errorMessage dummyReturnValue = do
  logFun errorMessage
  Turtle.sleep 0.1
  _ <- Turtle.exit $ Turtle.ExitFailure exitStatus
  return dummyReturnValue

descriptiveOutput :: T.Text -> T.Text -> T.Text
descriptiveOutput outputLabel outTxt = do
  if not (T.null outTxt)
    then Turtle.format (Turtle.s%":\n"%Turtle.s%"\n") outputLabel outTxt
    else ""

logTimedAction :: HasVerbosity o => o -> TChan LogMessage -> T.Text -> [T.Text]
  -> (TChan LogMessage -> T.Text -> IO ()) -> (TChan LogMessage -> T.Text -> IO ())
  -> IO FullOutput
  -> IO FullTimedOutput
logTimedAction opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger action = do
  logVerbose opts ch $ Turtle.format ("Begin: "%Turtle.s) cmdLabel
  if (List.null extraCmdLabels) then return () else logVerbose opts ch $ T.intercalate "\n" extraCmdLabels
  timed@((ec, stdOut, stdErr), diff) <- Turtle.time action
  stdoutLogger ch stdOut
  stderrLogger ch stdErr
  logVerbose opts ch $ Turtle.format ("End:   "%Turtle.s%" "%Turtle.s%" ("%Turtle.s%")") cmdLabel (Turtle.repr ec) (Turtle.repr diff)
  return timed

timeAndExitOnErr :: (HasSequential o, HasVerbosity o) => o -> TChan LogMessage -> T.Text
  -> (TChan LogMessage -> T.Text -> IO ()) -> (TChan LogMessage -> T.Text -> IO ())
  -> ProcFun -> ProcInput
  -> IO FullTimedOutput
timeAndExitOnErr opts ch cmdLabel = timeAndExitOnErr' opts ch cmdLabel []

timeAndExitOnErr' :: (HasSequential o, HasVerbosity o) => o -> TChan LogMessage -> T.Text -> [T.Text]
  -> (TChan LogMessage -> T.Text -> IO ()) -> (TChan LogMessage -> T.Text -> IO ())
  -> ProcFun -> ProcInput
  -> IO FullTimedOutput
timeAndExitOnErr' opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger procFun (cmd, args, stdInput) = do
  let action = procFun cmd args stdInput
  timed@((ec, stdOut, stdErr), _) <- logTimedAction opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger action
  case ec of
    Turtle.ExitFailure i -> do
      let cmdText = Turtle.format (Turtle.s%" "%Turtle.s) cmd $ showCmdArgs args
      let msgOut = descriptiveOutput "Standard output" stdOut
      let msgErr = descriptiveOutput "Error output" stdErr

      let exitMsg = Turtle.format ("\n=== Begin Error: "%Turtle.s%" ===\nExternal command:\n"%Turtle.s%"\nExit code "%Turtle.d%"\n"
                            %Turtle.s%Turtle.s%"=== End Error: "%Turtle.s%" ===\n") cmdLabel cmdText i msgOut msgErr cmdLabel
      errExit i ch exitMsg timed
    Turtle.ExitSuccess -> return timed

procWithEmptyOutput :: ProcFun
procWithEmptyOutput cmd args stdinput = do
  ec <- Turtle.proc cmd args stdinput
  return (ec, T.empty, T.empty)

parAwareProc :: HasSequential o => o -> ProcFun
parAwareProc opts = if (sequential opts) then procWithEmptyOutput else Turtle.procStrictWithErr

parAwareActions :: HasSequential o => o -> [IO a] -> IO [a]
parAwareActions opts = parAwareFun opts
  where
    parAwareFun op = if (sequential op) then sequence else Turtle.single . shellToList . Turtle.parallel

inprocWithErrFun :: (T.Text -> IO ()) -> ProcInput -> Turtle.Shell Turtle.Line
inprocWithErrFun errFun (cmd, args, standardInput) = do
  result <- Turtle.inprocWithErr cmd args standardInput
  case result of
    Right ln -> return ln
    Left  ln -> do
      (Turtle.liftIO . errFun . Turtle.lineToText) ln
      Turtle.empty

verboseTestFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> TurtlePath -> IO Bool
verboseTestFile opts ch p = do
  fileExists <- Turtle.testfile p
  let rel = relativeToBase opts p
  if fileExists
    then logVerbose opts ch $ Turtle.format ("Found '"%Turtle.fp%"'") rel
    else logVerbose opts ch $ Turtle.format ("Looked for but did not find '"%Turtle.fp%"'") rel
  return fileExists

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\ll -> (fst . head $ ll, map snd ll)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

initialIncludeFilePath :: TurtlePath -> TurtlePath
initialIncludeFilePath p = (Turtle.parent . Turtle.parent . Turtle.parent) p </> includeFileName p

parentIncludeFilePath :: TurtlePath -> TurtlePath
parentIncludeFilePath p = (Turtle.parent . Turtle.parent) p </> (Turtle.filename p)

allYearsPath :: TurtlePath -> TurtlePath
allYearsPath = allYearsPath' Turtle.directory

allYearsPath' :: (TurtlePath -> TurtlePath) -> TurtlePath -> TurtlePath
allYearsPath' dir p = dir p </> "all-years.journal"

allYearsFileName :: TurtlePath
allYearsFileName = "all-years" <.> "journal"

groupIncludeFiles :: [TurtlePath] -> (InputFileBundle, InputFileBundle)
groupIncludeFiles = allYearIncludeFiles . groupIncludeFilesPerYear

groupIncludeFilesPerYear :: [TurtlePath] -> InputFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p:_) = case extractImportDirs p of
    Right _ -> (groupValuesBy initialIncludeFilePath) ps
    Left  _ -> (groupValuesBy parentIncludeFilePath)  ps

allYearIncludeFiles :: InputFileBundle -> (InputFileBundle, InputFileBundle)
allYearIncludeFiles m = (m, yearsIncludeMap $ Map.keys m)

yearsIncludeMap :: [TurtlePath] -> InputFileBundle
yearsIncludeMap = groupValuesBy allYearsPath

lsDirs :: TurtlePath -> Turtle.Shell TurtlePath
lsDirs = onlyDirs . Turtle.ls

onlyDirs :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
onlyDirs = excludeHiddenFiles . excludeWeirdPaths . filterPathsByFileStatus Turtle.isDirectory

onlyFiles :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
onlyFiles = excludeHiddenFiles . filterPathsByFileStatus Turtle.isRegularFile

filterPathsByFileStatus :: (Turtle.FileStatus -> Bool) -> Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
filterPathsByFileStatus filepred files = do
  files' <- shellToList files
  filtered <- filterPathsByFileStatus' filepred [] files'
  Turtle.select filtered

filterPathsByFileStatus' :: (Turtle.FileStatus -> Bool) -> [TurtlePath] -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPathsByFileStatus' _ acc [] = return acc
filterPathsByFileStatus' filepred acc (file:files) = do
  filestat <- Turtle.stat file
  let filtered = if (filepred filestat) then file:acc else acc
  filterPathsByFileStatus' filepred filtered files

filterPaths :: (TurtlePath -> IO Bool) -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPaths = filterPaths' []

filterPaths' :: [TurtlePath] -> (TurtlePath -> IO Bool) -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPaths' acc _ [] = return acc
filterPaths' acc filepred (file:files) = do
  shouldInclude <- Turtle.liftIO $ filepred file
  let filtered = if shouldInclude then file:acc else acc
  filterPaths' filtered filepred files

excludeHiddenFiles :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
excludeHiddenFiles paths = do
  p <- paths
  case (Turtle.match (Turtle.prefix ".") $ Turtle.format Turtle.fp $ Turtle.filename p) of
    [] -> Turtle.select [p]
    _  -> Turtle.select []

excludeWeirdPaths :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
excludeWeirdPaths = Turtle.findtree (Turtle.suffix $ Turtle.noneOf "_")

firstExistingFile :: [TurtlePath] -> IO (Maybe TurtlePath)
firstExistingFile files = do
  case files of
    []   -> return Nothing
    file:fs -> do
      exists <- Turtle.testfile file
      if exists then return (Just file) else firstExistingFile fs

basenameLine :: TurtlePath -> Turtle.Shell Turtle.Line
basenameLine path = case (Turtle.textToLine $ Turtle.format Turtle.fp $ Turtle.basename path) of
  Nothing -> Turtle.die $ Turtle.format ("Unable to determine basename from path: "%Turtle.fp%"\n") path
  Just bn -> return bn

buildFilename :: [Turtle.Line] -> T.Text -> TurtlePath
buildFilename identifiers ext = Turtle.fromText (T.intercalate "-" (map Turtle.lineToText identifiers)) <.> ext

shellToList :: Turtle.Shell a -> Turtle.Shell [a]
shellToList files = Turtle.fold files Fold.list

includeFileName :: TurtlePath -> TurtlePath
includeFileName = (<.> "journal") . Turtle.fromText . (Turtle.format (Turtle.fp%"-include")) . Turtle.dirname

toIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> InputFileBundle -> IO (Map.Map TurtlePath T.Text)
toIncludeFiles opts ch m = do
  preMap  <- extraIncludes opts ch (Map.keys m) ["opening.journal"] ["pre-import.journal"] []
  postMap <- extraIncludes opts ch (Map.keys m) ["closing.journal"] ["post-import.journal"] ["prices.journal"]
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

extraIncludes :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [TurtlePath] -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO InputFileBundle
extraIncludes opts ch = extraIncludes' opts ch Map.empty

extraIncludes' :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> InputFileBundle -> [TurtlePath] -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO InputFileBundle
extraIncludes' _ _ acc [] _ _ _ = return acc
extraIncludes' opts ch acc (file:files) extraSuffixes manualFiles prices = do
  extra <- extraIncludesForFile opts ch file extraSuffixes manualFiles prices
  extraIncludes' opts ch (Map.unionWith (++) acc extra) files extraSuffixes manualFiles prices

extraIncludesForFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> TurtlePath -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO InputFileBundle
extraIncludesForFile opts ch file extraSuffixes manualFiles prices = do
  let dirprefix = Turtle.fromText $ fst $ T.breakOn "-" $ Turtle.format Turtle.fp $ Turtle.basename file
  let fileNames = map (\suff -> Turtle.fromText $ Turtle.format (Turtle.fp%"-"%Turtle.s) dirprefix suff) extraSuffixes
  let suffixFiles = map (Turtle.directory file </>) fileNames
  let suffixDirFiles = map (Turtle.directory file </> "_manual_" </> dirprefix </>) manualFiles
  let priceFiles = map (Turtle.directory file </> ".." </> "prices" </> dirprefix </>) prices
  let extraFiles = suffixFiles ++ suffixDirFiles ++ priceFiles
  filtered <- Turtle.single $ filterPaths Turtle.testfile extraFiles
  let logMsg = Turtle.format ("Looking for possible extra include files for '"%Turtle.fp%"' among these "%Turtle.d%" options: "%Turtle.s%". Found "%Turtle.d%": "%Turtle.s)
               (relativeToBase opts file) (length extraFiles) (Turtle.repr $ relativeFilesAsText opts extraFiles)
               (length filtered) (Turtle.repr $ relativeFilesAsText opts filtered)
  logVerbose opts ch logMsg
  return $ Map.fromList [(file, filtered)]

relativeFilesAsText :: HasBaseDir o => o -> [TurtlePath] -> [T.Text]
relativeFilesAsText opts ps = map ((Turtle.format Turtle.fp) . (relativeToBase opts)) ps

toIncludeFiles' :: InputFileBundle -> InputFileBundle -> InputFileBundle -> Map.Map TurtlePath T.Text
toIncludeFiles' preMap postMap = Map.mapWithKey $ generatedIncludeText preMap postMap

addPreamble :: Map.Map TurtlePath T.Text -> Map.Map TurtlePath T.Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: TurtlePath -> TurtlePath -> T.Text
toIncludeLine base file = Turtle.format ("!include "%Turtle.fp) $ relativeToBase' base file

generatedIncludeText :: InputFileBundle -> InputFileBundle -> TurtlePath -> [TurtlePath] -> T.Text
generatedIncludeText preMap postMap outputFile fs = do
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let files = List.nub . List.sort $ fs
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let lns = map (toIncludeLine $ Turtle.directory outputFile) $ preFiles ++ files ++ postFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: T.Text
includePreamble = "### Generated by hledger-flow - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [TurtlePath] -> IO [TurtlePath]
groupAndWriteIncludeFiles opts ch = writeFileMap opts ch . groupIncludeFiles

writeFiles :: IO (Map.Map TurtlePath T.Text) -> IO [TurtlePath]
writeFiles fileMap = do
  m <- fileMap
  writeFiles' m

writeFiles' :: Map.Map TurtlePath T.Text -> IO [TurtlePath]
writeFiles' fileMap = do
  writeTextMap fileMap
  return $ Map.keys fileMap

writeTextMap :: Map.Map TurtlePath T.Text -> IO ()
writeTextMap = Map.foldlWithKey (\a k v -> a <> Turtle.writeTextFile k v) (return ())

writeFileMap :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> (InputFileBundle, InputFileBundle) -> IO [TurtlePath]
writeFileMap opts ch (m, allYears) = do
  _ <- writeFiles' $ (addPreamble . toIncludeFiles' Map.empty Map.empty) allYears
  writeFiles . (toIncludeFiles opts ch) $ m

writeIncludesUpTo :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtlePath -> [TurtlePath] -> IO [TurtlePath]
writeIncludesUpTo _ _ _ [] = return []
writeIncludesUpTo opts ch stopAt journalFiles = do
  let shouldStop = any (\dir -> dir == stopAt) $ map Turtle.parent journalFiles
  if shouldStop
    then return journalFiles
    else do
      newJournalFiles <- groupAndWriteIncludeFiles opts ch journalFiles
      writeIncludesUpTo opts ch stopAt newJournalFiles

writeToplevelAllYearsInclude :: (HasBaseDir o, HasVerbosity o) => o -> IO [TurtlePath]
writeToplevelAllYearsInclude opts = do
  let allTop = Map.singleton (turtleBaseDir opts </> allYearsFileName) ["import" </> allYearsFileName]
  writeFiles' $ (addPreamble . toIncludeFiles' Map.empty Map.empty) allTop

changeExtension :: T.Text -> TurtlePath -> TurtlePath
changeExtension ext path = (Turtle.dropExtension path) <.> ext

changePathAndExtension :: TurtlePath -> T.Text -> TurtlePath -> TurtlePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeOutputPath :: TurtlePath -> TurtlePath -> TurtlePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ Turtle.splitDirectories srcFile
  where changeSrcDir file = if (file == "1-in/" || file == "2-preprocessed/") then newOutputLocation else file

importDirBreakdown ::  TurtlePath -> [TurtlePath]
importDirBreakdown = importDirBreakdown' []

importDirBreakdown' :: [TurtlePath] -> TurtlePath -> [TurtlePath]
importDirBreakdown' acc path = do
  let dir = Turtle.directory path
  if (Turtle.dirname dir == "import" || (Turtle.dirname dir == ""))
    then dir:acc
    else importDirBreakdown' (dir:acc) $ Turtle.parent dir

extractImportDirs :: TurtlePath -> Either T.Text IT.ImportDirs
extractImportDirs inputFile = do
  case importDirBreakdown inputFile of
    [bd,owner,bank,account,filestate,year] -> Right $ IT.ImportDirs bd owner bank account filestate year
    _ -> do
      Left $ Turtle.format ("I couldn't find the right number of directories between \"import\" and the input file:\n"%Turtle.fp
                      %"\n\nhledger-flow expects to find input files in this structure:\n"%
                      "import/owner/bank/account/filestate/year/trxfile\n\n"%
                      "Have a look at the documentation for a detailed explanation:\n"%Turtle.s) inputFile (docURL "input-files")

listOwners :: HasBaseDir o => o -> Turtle.Shell TurtlePath
listOwners opts = fmap Turtle.basename $ lsDirs $ (turtleBaseDir opts) </> "import"

intPath :: Integer -> TurtlePath
intPath = Turtle.fromText . (Turtle.format Turtle.d)

includeYears :: TChan LogMessage -> TurtlePath -> IO [Integer]
includeYears ch includeFile = do
  txt <- Turtle.readTextFile includeFile
  case includeYears' txt of
    Left  msg   -> do
      channelErrLn ch msg
      return []
    Right years -> return years

includeYears' :: T.Text -> Either T.Text [Integer]
includeYears' txt = case partitionEithers (includeYears'' txt) of
  (errors, []) -> do
    let msg = Turtle.format ("Unable to extract years from the following text:\n"%Turtle.s%"\nErrors:\n"%Turtle.s) txt (T.intercalate "\n" $ map T.pack errors)
    Left msg
  (_, years) -> Right years

includeYears'' :: T.Text -> [Either String Integer]
includeYears'' txt = map extractDigits (T.lines txt)

extractDigits :: T.Text -> Either String Integer
extractDigits txt = fmap fst $ (T.decimal . (T.filter isDigit)) txt
