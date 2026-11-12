{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Common where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.STM
import Data.Char (isAlphaNum, isAscii, isDigit)
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
import qualified GHC.IO.Handle.FD as H
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, RelDir, fromTurtleAbsFile, literalGlobPath)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types
import Path (reldir, relfile, toFilePath)
import qualified Path
import qualified Path.IO as Path
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose)
import qualified System.Info as Sys
import qualified System.Process as Process
import System.IO.Error (tryIOError)
import System.FilePath ((<.>))
import Prelude hiding (putStrLn, readFile, writeFile)

hledgerPathFromOption :: Maybe FilePath -> IO AbsFile
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
      maybeH <- Directory.findExecutable "hledger"
      case maybeH of
        -- Relative PATH entries can produce relative executable paths.
        Just h -> Path.resolveFile' h
        Nothing -> do
          let msg =
                "Unable to find hledger in your path.\n"
                  <> "You need to either install hledger, or add it to your PATH, or provide the path to an hledger executable.\n\n"
                  <> "There are a number of installation options on the hledger website: https://hledger.org/download.html"
          ioError (userError msg)

-- | The hledger to use, or Nothing when it can't be found or run. Never exits.
findHledger :: Maybe FilePath -> IO (Maybe HledgerInfo)
findHledger pathOption = do
  found <- case pathOption of
    Just h -> do
      exists <- Directory.doesFileExist h
      if exists then Just <$> Path.resolveFile' h else return Nothing
    Nothing -> Directory.findExecutable "hledger" >>= traverse Path.resolveFile'
  case found of
    Nothing -> return Nothing
    Just hlp -> do
      version <- try (hledgerVersionFromPath hlp) :: IO (Either SomeException T.Text)
      return (either (const Nothing) (Just . HledgerInfo hlp) version)

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

hledgerInfoFromPath :: Maybe FilePath -> IO HledgerInfo
hledgerInfoFromPath pathOption = do
  hlp <- hledgerPathFromOption pathOption
  hlv <- hledgerVersionFromPath hlp
  return $ HledgerInfo hlp hlv

-- | @"hledger 1.50.2, mac-aarch64"@ → @"1.50.2"@.
hledgerVersionNumber :: T.Text -> T.Text
hledgerVersionNumber txt = case dropWhile (== "hledger") (T.words txt) of
  v : _ -> T.dropWhileEnd (== ',') v
  [] -> ""

-- | hledger started expanding globs in --file arguments in 1.52.
hledgerFileArgument :: HledgerInfo -> FilePath -> T.Text
hledgerFileArgument info path
  | Right (major, rest) <- T.decimal (hledgerVersionNumber (hlVersion info)),
    Just suffix <- T.stripPrefix "." rest,
    Right (minor, _) <- T.decimal suffix,
    (major, minor) >= ((1, 52) :: (Int, Int)) = T.pack (literalGlobPath path)
  | otherwise = T.pack path

hledgerConfArgs :: RT.RuntimeOptions -> [T.Text]
hledgerConfArgs opts =
  case RT.hledgerConf opts of
    Nothing -> []
    Just conf -> ["--conf", T.pack conf]

showCmdArgs :: [T.Text] -> T.Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

escapeArg :: T.Text -> T.Text
escapeArg a
  | not (T.null a) && T.all safe a = a
  | otherwise = posixLiteral a
  where
    safe c = isAscii c && (isAlphaNum c || c `elem` ("-_./:=+,@%" :: String))

-- | The shell used for copyable commands on this platform.
data Shell = Posix | PowerShell deriving (Eq, Show)

nativeShell :: Shell
nativeShell = if Sys.os == "mingw32" then PowerShell else Posix

shellArgument :: Shell -> T.Text -> T.Text
shellArgument Posix arg = escapeArg arg
shellArgument PowerShell arg
  | not (T.null arg) && T.all simple arg = arg
  | otherwise = powerShellLiteral arg
  where
    simple c = isAscii c && (isAlphaNum c || c `elem` ("-_./" :: String))

posixLiteral :: T.Text -> T.Text
posixLiteral arg = "'" <> T.replace "'" "'\"'\"'" arg <> "'"

powerShellLiteral :: T.Text -> T.Text
powerShellLiteral arg = "'" <> T.concatMap escape arg <> "'"
  where
    -- PowerShell also recognises typographic single quotes as delimiters.
    escape c | c == '\'' || (c >= '\x2018' && c <= '\x201b') = T.replicate 2 (T.singleton c)
             | otherwise = T.singleton c

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
        { -- With all three streams redirected, process otherwise starts a
          -- Windows child without our console, losing its UTF-8 code page.
          Process.create_group = Sys.os == "mingw32",
          Process.std_in = maybe Process.CreatePipe Process.UseHandle input,
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
timeAndExitOnErr opts ch cmdLabel = timeAndExitOnErr' opts ch cmdLabel [] []

timeAndExitOnErr' ::
  (HasVerbosity o) =>
  o ->
  TChan LogMessage ->
  T.Text ->
  [T.Text] ->
  [T.Text] ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  (TChan LogMessage -> T.Text -> IO ()) ->
  ProcInput ->
  IO FullTimedOutput
timeAndExitOnErr' opts ch cmdLabel extraCmdLabels hints stdoutLogger stderrLogger input@(cmd, args, _) = do
  let action = runProcessCaptured input
  -- On failure the error output goes into the (trimmed) message, not straight to the console.
  timed@((ec, stdOut, stdErr), _) <- logTimedAction opts ch cmdLabel extraCmdLabels cmd args stdoutLogger (\_ _ -> return ()) action
  case ec of
    ExitFailure i -> do
      let cmdText = cmd <> " " <> showCmdArgs args
      let msgOut = descriptiveOutput "Standard output" (trimToolOutput stdOut)
      let msgErr = descriptiveOutput "Error output" (trimToolOutput stdErr)
      let exitMsg =
            "Failed " <> cmdLabel <> " (exit code " <> T.pack (show i) <> ").\n"
              <> "Command: " <> cmdText <> "\n" <> msgOut <> msgErr
              <> T.unlines hints
      errExit i ch exitMsg timed
    ExitSuccess -> do
      stderrLogger ch stdErr
      return timed

-- | Keep a tool's output readable: at most 20 lines of at most 300 characters.
-- hledger can echo a whole statement on one line when parsing fails.
trimToolOutput :: T.Text -> T.Text
trimToolOutput txt
  | length ls <= maxLines && all ((<= maxWidth) . T.length) ls = txt
  | otherwise = T.intercalate "\n" (map cut (take maxLines ls) ++ dropped) <> (if "\n" `T.isSuffixOf` txt then "\n" else "")
  where
    ls = T.lines txt
    maxLines = 20
    maxWidth = 300
    cut l = if T.length l > maxWidth then T.take maxWidth l <> "…" else l
    dropped = case length ls - maxLines of
      n | n > 0 -> ["… (" <> T.pack (show n) <> " more lines)"]
      _ -> []

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
      -- Keep result order, but cancel the batch as soon as any action fails.
      results <- mapConcurrently id current
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

allYearsFileName :: FilePath
allYearsFileName = "all-years" <.> "journal"

-- Keep the configured spelling for diagnostics and expand a leading ~ for IO.
configuredLedgerFile :: IO (Maybe (FilePath, FilePath))
configuredLedgerFile = do
  value <- lookupEnv "LEDGER_FILE"
  case value of
    Just path | not (null path) -> do
      resolved <- case path of
        "~" -> Directory.getHomeDirectory
        '~' : sep : rest | FilePath.isPathSeparator sep -> (FilePath.</> rest) <$> Directory.getHomeDirectory
        _ -> return path
      return (Just (path, resolved))
    _ -> return Nothing

directivesFile :: (HasBaseDir o) => o -> AbsFile
directivesFile opts = baseDir opts Path.</> [relfile|directives.journal|]

filterPaths :: MonadIO m => (FilePath -> IO Bool) -> [FilePath] -> m [FilePath]
filterPaths = filterPaths' []

filterPaths' :: MonadIO m => [FilePath] -> (FilePath -> IO Bool) -> [FilePath] -> m [FilePath]
filterPaths' acc predicate files = do
  found <- liftIO $ filterM predicate files
  return (reverse found ++ acc)

firstExistingFile :: [AbsFile] -> IO (Maybe AbsFile)
firstExistingFile [] = return Nothing
firstExistingFile (file : rest) = do
  exists <- Path.doesFileExist file
  if exists then return (Just file) else firstExistingFile rest

writeFiles :: IO (Map.Map FilePath T.Text) -> IO [FilePath]
writeFiles fileMap = do
  m <- fileMap
  writeFiles' m

writeFiles' :: Map.Map FilePath T.Text -> IO [FilePath]
writeFiles' fileMap = do
  writeTextMap fileMap
  return $ Map.keys fileMap

writeTextMap :: Map.Map FilePath T.Text -> IO ()
writeTextMap files = forM_ (Map.toList files) $ \(path, contents) -> do
  changed <- textFileNeedsUpdate path contents
  when changed $ T.writeFile path contents

textFileNeedsUpdate :: FilePath -> T.Text -> IO Bool
textFileNeedsUpdate path contents = either (const True) (/= contents) <$> tryIOError (T.readFile path)

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

intPath :: Integer -> FilePath
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
