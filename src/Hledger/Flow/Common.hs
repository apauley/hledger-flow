{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Common where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.Async (concurrently, wait, withAsync)
import Control.Concurrent.STM
import qualified Control.Foldl as Fold
import Data.Char (isDigit)
import Data.Either
import Data.Function (on)
import qualified Data.List as List (groupBy, isPrefixOf, isSuffixOf, null, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.Marshal.Utils (withMany)
import qualified GHC.IO.Handle.FD as H
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, RelDir, TurtlePath, fromTurtleAbsFile)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types
import Path (reldir, relfile, toFilePath)
import qualified Path
import qualified Path.IO as Path
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose)
import qualified System.Process as Process
import System.IO.Error (tryIOError)
import System.FilePath ((<.>))
import qualified Turtle
import Prelude hiding (putStrLn, readFile, writeFile)

hledgerPathFromOption :: Maybe TurtlePath -> IO AbsFile
hledgerPathFromOption pathOption = do
  case pathOption of
    Just h -> do
      hlAbs <- fromTurtleAbsFile h
      isOnDisk <- Path.doesFileExist hlAbs
      if isOnDisk
        then return hlAbs
        else do
          let msg = "Unable to find hledger at " <> T.pack h
          errExit' 1 (T.hPutStrLn H.stderr) msg hlAbs
    Nothing -> do
      maybeH <- Path.findExecutable [relfile|hledger|]
      case maybeH of
        Just h -> return h
        Nothing -> do
          let msg =
                "Unable to find hledger in your path.\n"
                  <> "You need to either install hledger, or add it to your PATH, or provide the path to an hledger executable.\n\n"
                  <> "There are a number of installation options on the hledger website: https://hledger.org/download.html"
          ioError (userError msg)

hledgerVersionFromPath :: AbsFile -> IO T.Text
hledgerVersionFromPath hlp = do
  let command = (Process.proc (toFilePath hlp) ["--version"])
        { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe }
  Process.withCreateProcess command $ \input output _ handle -> do
    mapM_ hClose input
    Just stdoutPipe <- return output
    version <- T.hGetContents stdoutPipe
    status <- Process.waitForProcess handle
    case status of
      ExitSuccess -> return (T.strip version)
      ExitFailure _ -> exitWith status

hledgerInfoFromPath :: Maybe TurtlePath -> IO HledgerInfo
hledgerInfoFromPath pathOption = do
  hlp <- hledgerPathFromOption pathOption
  hlv <- hledgerVersionFromPath hlp
  return $ HledgerInfo hlp hlv

hledgerConfArgs :: RT.RuntimeOptions -> [T.Text]
hledgerConfArgs opts =
  case RT.hledgerConf opts of
    Nothing -> []
    Just conf -> ["--conf", T.pack conf]

showCmdArgs :: [T.Text] -> T.Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

escapeArg :: T.Text -> T.Text
escapeArg a = if T.count " " a > 0 then "'" <> a <> "'" else a

errExit :: Int -> TChan LogMessage -> T.Text -> a -> IO a
errExit exitStatus ch = errExit' exitStatus (channelErrLn ch)

errExit' :: Int -> (T.Text -> IO ()) -> T.Text -> a -> IO a
errExit' exitStatus logFun errorMessage _ = do
  logFun errorMessage
  threadDelay 100000
  exitWith (ExitFailure exitStatus)

descriptiveOutput :: T.Text -> T.Text -> T.Text
descriptiveOutput outputLabel outTxt = do
  if not (T.null outTxt)
    then outputLabel <> ":\n" <> outTxt <> "\n"
    else ""

logTimedAction ::
  (HasVerbosity o) =>
  o ->
  TChan LogMessage ->
  T.Text ->
  [T.Text] ->
  T.Text ->
  [T.Text] ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  IO FullOutput ->
  IO FullTimedOutput
logTimedAction opts ch cmdLabel extraCmdLabels cmd args stdoutLogger stderrLogger action = do
  logVerbose opts ch $ "Begin: " <> cmdLabel
  logVerbose opts ch $ cmd <> " " <> T.intercalate " " args
  if (List.null extraCmdLabels) then return () else logVerbose opts ch $ T.intercalate "\n" extraCmdLabels
  timed@((ec, stdOut, stdErr), diff) <- timeAction action
  stdoutLogger ch stdOut
  stderrLogger ch stdErr
  logVerbose opts ch $ "End:   " <> cmdLabel <> " " <> T.pack (show ec) <> " (" <> T.pack (show diff) <> ")"
  return timed

-- Drain both output pipes concurrently so a full stderr pipe cannot stall stdout.
runProcessCaptured :: ProcInput -> IO FullOutput
runProcessCaptured (cmd, args, input) = do
  let process = (Process.proc (T.unpack cmd) (map T.unpack args))
        { Process.std_in = maybe Process.CreatePipe Process.UseHandle input,
          Process.std_out = Process.CreatePipe,
          Process.std_err = Process.CreatePipe
        }
  Process.withCreateProcess process $ \stdinPipe output errors handle -> do
    mapM_ hClose stdinPipe
    Just stdoutPipe <- return output
    Just stderrPipe <- return errors
    (stdoutText, stderrText) <- concurrently (T.hGetContents stdoutPipe) (T.hGetContents stderrPipe)
    status <- Process.waitForProcess handle
    return (status, stdoutText, stderrText)

timeAndExitOnErr ::
  (HasVerbosity o) =>
  o ->
  TChan LogMessage ->
  T.Text ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  ProcInput ->
  IO FullTimedOutput
timeAndExitOnErr opts ch cmdLabel = timeAndExitOnErr' opts ch cmdLabel []

timeAndExitOnErr' ::
  (HasVerbosity o) =>
  o ->
  TChan LogMessage ->
  T.Text ->
  [T.Text] ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  ProcInput ->
  IO FullTimedOutput
timeAndExitOnErr' opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger input@(cmd, args, _) = do
  let action = runProcessCaptured input
  timed@((ec, stdOut, stdErr), _) <- logTimedAction opts ch cmdLabel extraCmdLabels cmd args stdoutLogger stderrLogger action
  case ec of
    ExitFailure i -> do
      let cmdText = cmd <> " " <> showCmdArgs args
      let msgOut = descriptiveOutput "Standard output" stdOut
      let msgErr = descriptiveOutput "Error output" stdErr

      let exitMsg =
            "\n=== Begin Error: " <> cmdLabel <> " ===\nExternal command:\n"
              <> cmdText <> "\nExit code " <> T.pack (show i) <> "\n" <> msgOut <> msgErr
              <> "=== End Error: " <> cmdLabel <> " ===\n"
      errExit i ch exitMsg timed
    ExitSuccess -> return timed

parAwareActions :: (HasSequential o, HasBatchSize o) => o -> [IO a] -> IO [a]
parAwareActions opts = if (sequential opts) then sequence else parBatchedActions (batchSize opts) []

parBatchedActions :: Int -> [a] -> [IO a] -> IO [a]
parBatchedActions batch done todo
  | batch < 1 = ioError (userError "--batch-size must be positive")
  | otherwise = (done ++) <$> runBatches todo
  where
    runBatches [] = return []
    runBatches actions = do
      let (current, remaining) = splitAt batch actions
      -- Start the whole batch, then collect results and exceptions in input order.
      results <- withMany withAsync current (mapM wait)
      (results ++) <$> runBatches remaining

verboseTestFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> AbsFile -> IO Bool
verboseTestFile opts ch p = do
  fileExists <- Path.doesFileExist p
  let rel = T.pack (relativeToBase opts (toFilePath p))
  if fileExists
    then logVerbose opts ch ("Found '" <> rel <> "'")
    else logVerbose opts ch ("Looked for but did not find '" <> rel <> "'")
  return fileExists

needsRegeneration :: AbsFile -> AbsFile -> IO Bool
needsRegeneration src target = do
  targetExists <- Path.doesFileExist target
  if targetExists
    then do
      -- Either file can disappear after the existence check.
      newer <- tryIOError $ (>) <$> modificationSecond src <*> modificationSecond target
      return (either (const True) id newer)
    else return True
  where
    -- Preserve the whole-second comparison used by --new-files-only.
    modificationSecond :: AbsFile -> IO Integer
    modificationSecond file = floor . utcTimeToPOSIXSeconds <$> Path.getModificationTime file

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' =
  mapMaybe
    ( \ll -> case ll of
        [] -> Nothing
        ((k, _) : _) -> Just (k, map snd ll)
    )
    . List.groupBy ((==) `on` fst)
    . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

allYearsFileName :: TurtlePath
allYearsFileName = "all-years" <.> "journal"

directivesFile :: (HasBaseDir o) => o -> AbsFile
directivesFile opts = baseDir opts Path.</> [relfile|directives.journal|]

onlyFiles :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
onlyFiles = excludeHiddenFiles . filterPathsByFileStatus Turtle.isRegularFile

filterPathsByFileStatus :: (Turtle.FileStatus -> Bool) -> Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
filterPathsByFileStatus filepred files = do
  files' <- shellToList files
  filtered <- filterPathsByFileStatus' filepred [] files'
  Turtle.select filtered

filterPathsByFileStatus' :: (Turtle.FileStatus -> Bool) -> [TurtlePath] -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPathsByFileStatus' _ acc [] = return acc
filterPathsByFileStatus' filepred acc (file : files) = do
  filestat <- Turtle.stat file
  let filtered = if (filepred filestat) then file : acc else acc
  filterPathsByFileStatus' filepred filtered files

filterPaths :: MonadIO m => (TurtlePath -> IO Bool) -> [TurtlePath] -> m [TurtlePath]
filterPaths = filterPaths' []

filterPaths' :: MonadIO m => [TurtlePath] -> (TurtlePath -> IO Bool) -> [TurtlePath] -> m [TurtlePath]
filterPaths' acc predicate files = do
  found <- liftIO $ filterM predicate files
  return (reverse found ++ acc)

excludeHiddenFiles :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
excludeHiddenFiles paths = do
  p <- paths
  if "." `List.isPrefixOf` FilePath.takeFileName p then Turtle.empty else return p

firstExistingFile :: [AbsFile] -> IO (Maybe AbsFile)
firstExistingFile [] = return Nothing
firstExistingFile (file : rest) = do
  exists <- Path.doesFileExist file
  if exists then return (Just file) else firstExistingFile rest

basenameLine :: TurtlePath -> Turtle.Shell Turtle.Line
basenameLine path = case (Turtle.textToLine $ T.pack $ Turtle.basename path) of
  Nothing -> liftIO $ ioError (userError ("Unable to determine basename from path: " ++ path ++ "\n"))
  Just bn -> return bn

buildFilename :: [Turtle.Line] -> T.Text -> TurtlePath
buildFilename identifiers ext = T.unpack (T.intercalate "-" (map Turtle.lineToText identifiers)) <.> (T.unpack ext)

shellToList :: Turtle.Shell a -> Turtle.Shell [a]
shellToList files = Turtle.fold files Fold.list

writeFiles :: IO (Map.Map TurtlePath T.Text) -> IO [TurtlePath]
writeFiles fileMap = do
  m <- fileMap
  writeFiles' m

writeFiles' :: Map.Map TurtlePath T.Text -> IO [TurtlePath]
writeFiles' fileMap = do
  writeTextMap fileMap
  return $ Map.keys fileMap

writeTextMap :: Map.Map TurtlePath T.Text -> IO ()
writeTextMap files = forM_ (Map.toList files) $ \(path, contents) -> do
  current <- tryIOError (T.readFile path)
  when (either (const True) (/= contents) current) $ T.writeFile path contents

changeExtension :: T.Text -> TurtlePath -> TurtlePath
changeExtension ext path = (FilePath.dropExtension path) <.> (T.unpack ext)

changePathAndExtension :: TurtlePath -> T.Text -> TurtlePath -> TurtlePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeOutputPath :: TurtlePath -> TurtlePath -> TurtlePath
changeOutputPath newOutputLocation = concatMap changeSrcDir . FilePath.splitPath
  where
    changeSrcDir "." = "./"
    changeSrcDir ".." = "../"
    changeSrcDir part
      | FilePath.dropTrailingPathSeparator part `elem` ["1-in", "2-preprocessed"] = newOutputLocation
      | otherwise = part

listOwners :: HasBaseDir o => o -> IO [RelDir]
listOwners opts = do
  let root = baseDir opts Path.</> [reldir|import|]
  perms <- Directory.getPermissions (toFilePath root)
  if Directory.readable perms
    then do
      (dirs, _) <- Path.listDirRel root
      return (reverse (filter visible dirs))
    else return []
  where
    visible dir =
      let name = FilePath.dropTrailingPathSeparator (toFilePath dir)
       in not ("." `List.isPrefixOf` name || "_" `List.isSuffixOf` name)

intPath :: Integer -> TurtlePath
intPath = show

includeYears :: TChan LogMessage -> AbsFile -> IO [Integer]
includeYears ch includeFile = do
  txt <- T.readFile (toFilePath includeFile)
  case includeYears' txt of
    Left msg -> do
      channelErrLn ch msg
      return []
    Right years -> return years

includeYears' :: T.Text -> Either T.Text [Integer]
includeYears' txt = case partitionEithers (includeYears'' txt) of
  (errors, []) -> do
    let msg = "Unable to extract years from the following text:\n" <> txt <> "\nErrors:\n" <> T.intercalate "\n" (map T.pack errors)
    Left msg
  (_, years) -> Right years

includeYears'' :: T.Text -> [Either String Integer]
includeYears'' txt = map extractDigits (T.lines txt)

extractDigits :: T.Text -> Either String Integer
extractDigits txt = fmap fst $ (T.decimal . (T.filter isDigit)) txt
