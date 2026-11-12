{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Common where

import Control.Monad (filterM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.STM
import Data.Char (isAlphaNum, isAscii, isDigit)
import Data.Either
import qualified Data.List as List (isPrefixOf, isSuffixOf, nub, null)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified GHC.IO.Handle.FD as H
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, RelDir, fromTurtleAbsFile, literalGlobPath, withStagedOutput)
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
          errExit' 1 (T.hPutStrLn H.stderr) msg
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
      result <- tryIOError (probeHledgerVersion hlp)
      return $ case result of
        Right (ExitSuccess, version) -> Just (HledgerInfo hlp version)
        _ -> Nothing

hledgerVersionFromPath :: AbsFile -> IO T.Text
hledgerVersionFromPath hlp = do
  (status, version) <- probeHledgerVersion hlp
  case status of
    ExitSuccess -> return version
    ExitFailure _ -> exitWith status

probeHledgerVersion :: AbsFile -> IO (ExitCode, T.Text)
probeHledgerVersion hlp = do
  let command = (Process.proc (toFilePath hlp) ["--version"])
        { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe }
  Process.withCreateProcess command $ \input output _ handle -> do
    mapM_ hClose input
    Just stdoutPipe <- return output
    version <- T.hGetContents stdoutPipe
    status <- Process.waitForProcess handle
    return (status, T.strip version)

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

-- | Pass only a given config; hledger finds the default one itself.
hledgerConfArgs :: RT.RuntimeOptions -> [T.Text]
hledgerConfArgs opts =
  case RT.hledgerConf opts of
    Just (GivenConf conf) -> ["--conf", T.pack conf]
    _ -> []

-- | The config file hledger will use: the given one, or the first that hledger 1.40+ finds
-- by itself, like possibleConfFiles in hledger's Hledger.Cli.Conf: hledger.conf in the current
-- directory or a parent, ~/.hledger.conf, or hledger.conf in the XDG config directory.
findHledgerConf :: Maybe FilePath -> IO (Maybe HledgerConf)
findHledgerConf (Just given) = return (Just (GivenConf given))
findHledgerConf Nothing = do
  found <- tryIOError $ do
    here <- Directory.getCurrentDirectory
    home <- Directory.getHomeDirectory
    xdg <- Directory.getXdgDirectory Directory.XdgConfig "hledger"
    let file d = d FilePath.</> if d == home then ".hledger.conf" else "hledger.conf"
    filterM Directory.doesFileExist (map file (List.nub (upToRoot here ++ [home, xdg])))
  return $ case found of
    Right (f : _) -> Just (FoundConf f)
    _ -> Nothing
  where
    upToRoot d = let up = FilePath.takeDirectory d in d : if up == d then [] else upToRoot up

showCmdArgs :: [T.Text] -> T.Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

-- | The home folder as HOME spells it and as resolved, so paths from either spelling match,
-- such as macOS's /private/var for /var.
homeDirs :: IO [FilePath]
homeDirs = either (const []) id <$> tryIOError (do
  home <- Directory.getHomeDirectory
  real <- Directory.canonicalizePath home
  return (List.nub [home, real]))

-- | The path's components below the home folder; @Just []@ for the home folder itself.
belowHome :: [FilePath] -> FilePath -> Maybe [FilePath]
belowHome homes path = case [drop (length h) p | h <- map FilePath.splitDirectories homes, under h] of
  rest : _ -> Just rest
  [] -> Nothing
  where
    p = FilePath.splitDirectories path
    under h = not (null h) && length h <= length p && and (zipWith FilePath.equalFilePath h p)

-- | A path for people to read, with the home folder as @~@.
tildePath :: [FilePath] -> FilePath -> FilePath
tildePath homes path = maybe path (FilePath.joinPath . ("~" :)) (belowHome homes path)

-- | A command line for people to read, with paths under the home folder as @~@.
showCmdArgsAt :: [FilePath] -> [T.Text] -> T.Text
showCmdArgsAt homes = T.unwords . map arg
  where
    arg a = case belowHome homes (T.unpack a) of
      Just [] -> "~"
      Just rest -> "~" <> T.singleton FilePath.pathSeparator <> escapeArg (T.pack (FilePath.joinPath rest))
      Nothing -> escapeArg a

-- | A path under the home folder to paste into a command, as @"$HOME/…"@: the shell expands
-- that, unlike a quoted @~@. 'Nothing' outside the home folder, or when the rest of the path
-- has a character that is special inside double quotes; callers then paste the absolute path.
homeShellPath :: Shell -> [FilePath] -> FilePath -> Maybe T.Text
homeShellPath shell homes path = case belowHome homes path of
  Just rest | not (any (any special) rest) -> Just ("\"" <> T.pack (FilePath.joinPath ("$HOME" : rest)) <> "\"")
  _ -> Nothing
  where
    special c = case shell of
      Posix -> c `elem` ("$`\"\\!" :: String)
      PowerShell -> c `elem` ("$`\"" :: String) || (c >= '\x201c' && c <= '\x201e')

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

errExit :: Int -> TChan LogMessage -> T.Text -> IO a
errExit exitStatus ch = errExit' exitStatus (channelErrLn ch)

errExit' :: Int -> (T.Text -> IO ()) -> T.Text -> IO a
errExit' exitStatus logFun errorMessage = do
  logFun errorMessage
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
  homes <- homeDirs
  logVerbose opts ch $ showCmdArgsAt homes (cmd : args)
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
      homes <- homeDirs
      let cmdText = showCmdArgsAt homes (cmd : args)
      let msgOut = descriptiveOutput "Standard output" (trimToolOutput stdOut)
      let msgErr = descriptiveOutput "Error output" (trimToolOutput stdErr)
      let exitMsg =
            "Failed " <> cmdLabel <> " (exit code " <> T.pack (show i) <> ").\n"
              <> "Command: " <> cmdText <> "\n" <> msgOut <> msgErr
              <> T.unlines hints
      errExit i ch exitMsg
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
  -- A missing or unreadable timestamp also requires regeneration.
  newer <- tryIOError $ (>) <$> Path.getModificationTime src <*> Path.getModificationTime target
  return (either (const True) id newer)

-- | Group by key, preserving input order within each group.
groupValuesBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy key values =
  Map.map reverse (Map.fromListWith (++) [(key v, [v]) | v <- values])

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
  when changed $ writeGeneratedText path contents

writeGeneratedText :: FilePath -> T.Text -> IO ()
writeGeneratedText path contents = do
  -- Replace the file behind an existing symlink and keep its permissions.
  target <- Directory.canonicalizePath path >>= Path.parseAbsFile
  withStagedOutput target $ \staged -> do
    let tmp = toFilePath staged
    exists <- Path.doesFileExist target
    when exists $ do
      T.writeFile tmp ""
      Directory.copyPermissions (toFilePath target) tmp
    T.writeFile tmp contents

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
