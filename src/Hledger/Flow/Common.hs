{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Common where

import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.Stream (mapConcurrentlyBounded)
import Control.Concurrent.STM
import Data.Char (isAlphaNum, isAscii, isDigit)
import Data.Either
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime)
import Text.Printf (printf)
import qualified Data.List as List (take, isPrefixOf, isSuffixOf, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified GHC.IO.Handle.FD as H
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, literalGlobPath, withStagedOutput)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types
import Path (reldir, relfile, stripProperPrefix, toFilePath)
import qualified Path
import qualified Path.IO as Path
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose)
import qualified System.Info as Sys
import qualified System.Process as Process
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.FilePath ((<.>))
import Prelude hiding (putStrLn, readFile, writeFile)

hledgerPathFromOption :: Maybe FilePath -> IO AbsFile
hledgerPathFromOption pathOption = do
  case pathOption of
    Just h -> do
      hlAbs <- Path.parseAbsFile h
      exists <- Path.doesFileExist hlAbs
      let failWith = errExit' 1 (T.hPutStrLn H.stderr)
      unless exists $ failWith ("Unable to find hledger at " <> T.pack h)
      -- Windows has no POSIX execute bit.
      unless (Sys.os == "mingw32") $ do
        permissions <- Directory.getPermissions h
        unless (Directory.executable permissions) $
          failWith ("The hledger file is not executable: " <> T.pack h)
      return hlAbs
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
    ExitFailure code -> errExit' code (T.hPutStrLn H.stderr) $
      "Failed to run " <> showCmdArgs [T.pack (toFilePath hlp), "--version"]
        <> " (exit code " <> T.pack (show code) <> ")."

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

-- Share a successful probe among parallel conversions in this invocation.
deferredHledgerVersion :: AbsFile -> IO (IO T.Text)
deferredHledgerVersion path = do
  cached <- newMVar Nothing
  return $ modifyMVar cached $ \previous -> do
    version <- maybe (hledgerVersionFromPath path) return previous
    return (Just version, version)

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
findHledgerConf (Just given) = pure (Just (GivenConf given))
findHledgerConf Nothing = do
  result <- tryIOError (hledgerConfCandidates >>= filterM Directory.doesFileExist)
  pure $ case result of
    Right (f : _) -> Just (FoundConf f)
    _ -> Nothing

-- | Fresh lookup for callers that need to distinguish absence from failed discovery.
lookupHledgerConf :: Maybe FilePath -> IO (Maybe HledgerConf)
lookupHledgerConf (Just given) = return (Just (GivenConf given))
lookupHledgerConf Nothing = fmap FoundConf <$> (hledgerConfCandidates >>= firstConfirmedFile)

hledgerConfCandidates :: IO [FilePath]
hledgerConfCandidates = do
  here <- Directory.getCurrentDirectory
  home <- Directory.getHomeDirectory
  xdg <- Directory.getXdgDirectory Directory.XdgConfig "hledger"
  let file d = d FilePath.</> if d == home then ".hledger.conf" else "hledger.conf"
  pure (map file (List.nub (List.take 100 (upToRoot here) ++ [home, xdg])))
  where
    upToRoot d = let up = FilePath.takeDirectory d in d : if up == d then [] else upToRoot up

-- doesFileExist also returns False on permission errors. Verify a negative result
-- before advancing to the next candidate so an uncertain selection cannot be cached.
firstConfirmedFile :: [FilePath] -> IO (Maybe FilePath)
firstConfirmedFile [] = pure Nothing
firstConfirmedFile (file : rest) = do
  exists <- Directory.doesFileExist file
  if exists then pure (Just file) else do
    status <- tryIOError (Directory.getPermissions file)
    case status of
      Left e | isDoesNotExistError e -> firstConfirmedFile rest
             | otherwise -> ioError e
      Right _ -> do
        dir <- Directory.doesDirectoryExist file
        if dir then firstConfirmedFile rest else ioError (userError ("Could not determine the file type of " ++ file))

showCmdArgs :: [T.Text] -> T.Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

-- | The home folder as HOME spells it and as resolved, so paths from either spelling match,
-- such as macOS's /private/var for /var.
homeDirs :: IO [FilePath]
homeDirs = either (const []) id <$> tryIOError (do
  home <- Directory.getHomeDirectory
  -- PowerShell expands $HOME from USERPROFILE. The Windows profile API can
  -- still return the original profile after USERPROFILE has been overridden.
  shellHome <- if nativeShell == PowerShell then lookupEnv "USERPROFILE" else pure (Just home)
  case shellHome of
    Just value | FilePath.equalFilePath value home -> do
      real <- Directory.canonicalizePath home
      pure (List.nub [home, real])
    _ -> pure [])

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

-- | Move to a file path, with the same collision rules in either shell.
moveFileCommand :: Shell -> [FilePath] -> AbsDir -> BaseDir -> RelFile -> RelFile -> T.Text
moveFileCommand shell homes cwd base source target = case shell of
  Posix ->
    makeDirectory Posix (path dir) <> " && [ ! -e " <> dest <> " ] && [ ! -L " <> dest
      <> " ] && mv -n " <> path src <> " " <> dest
  PowerShell ->
    makeDirectory PowerShell (literal dir) <> " -ErrorAction Stop | Out-Null; "
      <> "[System.IO.File]::Move(" <> nativePath src <> ", " <> nativePath file <> ")"
  where
    atRoot = cwd == base
    fromHere rel = if atRoot then toFilePath rel else toFilePath (base Path.</> rel)
    src = fromHere source
    file = fromHere target
    dir = FilePath.takeDirectory file
    literal = powerShellLiteral . T.pack
    -- .NET resolves paths against the process directory, not PowerShell's location.
    nativePath name
      | atRoot = "(Join-Path $PWD.ProviderPath " <> literal name <> ")"
      | otherwise = literal name
    path name = fromMaybe (shellArgument shell (T.pack name)) (homeShellPath shell homes name)
    dest = path file

createDirectoryCommand :: Shell -> [FilePath] -> AbsDir -> AbsDir -> T.Text
createDirectoryCommand shell homes cwd dir = makeDirectory shell argument
  where
    absolute = FilePath.dropTrailingPathSeparator (toFilePath dir)
    relative = FilePath.dropTrailingPathSeparator . toFilePath <$> stripProperPrefix cwd dir
    argument = case relative of
      Just path -> shellArgument shell (T.pack path)
      Nothing -> fromMaybe (shellArgument shell (T.pack absolute)) (homeShellPath shell homes absolute)

makeDirectory :: Shell -> T.Text -> T.Text
makeDirectory Posix argument = "mkdir -p " <> argument
makeDirectory PowerShell argument = "New-Item -ItemType Directory -Force -Path " <> argument

-- | An external program hledger-flow runs, described for the people reading its output.
data External = External
  { -- | Who printed the output and who failed: @hledger@, or a hook script's path.
    extName :: T.Text,
    -- | What it runs for, after "to": @convert import/…/a.csv with …@.
    extPurpose :: T.Text,
    -- | The command line to show, as one or more commands piped together.
    extCommand :: [[T.Text]],
    -- | What to check when it fails.
    extHints :: [T.Text]
  }

-- | A command line to paste into this platform's shell from the given directory: paths below it
-- start with @./@, and other paths under the home folder with @~@ or @$HOME@.
pasteCommand :: Shell -> FilePath -> [FilePath] -> [[T.Text]] -> T.Text
pasteCommand shell base homes = T.intercalate " | " . map command
  where
    command argv = case shell of
      Posix -> T.unwords (map posixArg argv)
      PowerShell -> case map powerShellArg argv of
        -- PowerShell reads a quoted first word as a string, not a command to run.
        first : rest | "'" `T.isPrefixOf` first || "\"" `T.isPrefixOf` first -> T.unwords ("&" : first : rest)
        ws -> T.unwords ws
    belowBase a = case belowHome [base] (T.unpack a) of
      Just [] -> Just "."
      Just rest -> Just (T.pack ('.' : FilePath.pathSeparator : FilePath.joinPath rest))
      Nothing -> Nothing
    posixArg a = maybe (showCmdArgsAt homes [a]) escapeArg (belowBase a)
    powerShellArg a = case belowBase a of
      Just rel -> shellArgument PowerShell rel
      Nothing -> fromMaybe (shellArgument PowerShell a) (homeShellPath PowerShell homes (T.unpack a))

-- hledger discovers config files from its working directory, and scripts can read
-- relative paths. Preserve that directory when replaying a command at the ledger root.
externalCommand :: HasBaseDir o => o -> External -> IO T.Text
externalCommand opts ext = do
  homes <- homeDirs
  cwd <- Directory.getCurrentDirectory
  let command = pasteCommand nativeShell cwd homes (extCommand ext)
      location = fromMaybe (shellArgument nativeShell (T.pack cwd)) (homeShellPath nativeShell homes cwd)
  pure $ if FilePath.equalFilePath cwd (toFilePath (baseDir opts))
    then command
    else case nativeShell of
      Posix -> "(cd " <> location <> " && " <> command <> ")"
      PowerShell -> "& { Push-Location -LiteralPath " <> location <> " -ErrorAction Stop; try { "
                    <> command <> " } finally { Pop-Location } }"

-- | In verbose mode, say which program is about to run and show its command line.
announceExternal :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> External -> IO ()
announceExternal opts ch ext = when (verbose opts) $ do
  command <- externalCommand opts ext
  color <- useColor H.stderr
  logToChannel ch $
    "Running " <> extName ext <> " to " <> extPurpose ext <> ":\n"
      <> "  " <> dim color "$" <> " " <> cyan color command

-- | Run an external program, show what it printed, and stop with its exit code when it fails.
-- Returns its standard output, which goes to the console only when 'showStdout' is set.
runExternal :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> External -> Bool -> ProcInput -> IO T.Text
runExternal opts ch ext showStdout input = do
  announceExternal opts ch ext
  (result@(_, out, _), duration) <- timeAction (runProcessCaptured input)
  checkExternal opts ch ext showStdout duration result
  return out

-- | Show what an external program printed. When it failed, stop with its exit code and
-- say plainly that the error came from that program.
checkExternal :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> External -> Bool -> NominalDiffTime -> FullOutput -> IO ()
checkExternal opts ch ext showStdout duration (status, out, err) = do
  outColor <- useColor H.stdout
  errColor <- useColor H.stderr
  case status of
    ExitSuccess -> do
      logVerbose opts ch (green errColor (extName ext <> " finished") <> " in " <> showDuration duration)
      when showStdout $ passOn (channelOut ch) outColor "Output" out
      passOn (channelErr ch) errColor "Messages" err
    ExitFailure code -> do
      when showStdout $ passOn (channelOut ch) outColor "Output" out
      commandLines <- if verbose opts then pure [] else do
        command <- externalCommand opts ext
        pure ["Command:", "  " <> command]
      let section label txt = outputSection errColor (verbose opts) (label <> " from " <> extName ext) txt
      errExit code ch $ T.intercalate "\n" $
        [ red errColor (bold errColor (extName ext <> " failed with exit code " <> T.pack (show code)))
            <> " while trying to " <> extPurpose ext <> "."
        ]
          -- Verbose output showed the command just before it ran.
          ++ commandLines
          ++ (if showStdout then [] else section "Output" out)
          ++ section "Error output" err
          ++ extHints ext
  where
    -- In verbose mode, mark the output as the program's; otherwise pass it on as it is.
    passOn write color label txt
      | T.null txt = return ()
      | verbose opts = case outputSection color True (label <> " from " <> extName ext) txt of
          heading : lns -> logToChannel ch heading >> write (T.unlines lns)
          [] -> return ()
      | otherwise = write txt

-- | Another program's output under a label, each line behind a bar. Without 'complete',
-- long output is trimmed.
outputSection :: Bool -> Bool -> T.Text -> T.Text -> [T.Text]
outputSection color complete label txt
  | T.null (T.strip txt) = []
  | otherwise = (label <> ":") : map bar (T.lines (if complete then txt else trimToolOutput txt))
  where
    -- Keep the program's own colours from spilling into the lines that follow.
    bar l = dim color "  │" <> " " <> l <> (if color && "\ESC" `T.isInfixOf` l then resetCode else "")

showDuration :: NominalDiffTime -> T.Text
showDuration d
  | d < 1 = T.pack (show (round (d * 1000) :: Integer)) <> " ms"
  | otherwise = T.pack (printf "%.1f s" (realToFrac d :: Double))

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
      n | n > 0 -> ["… (" <> T.pack (show n) <> " more lines; run with --verbose to see them all)"]
      _ -> []

parAwareActions :: (HasSequential o, HasBatchSize o) => o -> [IO a] -> IO [a]
parAwareActions opts = if (sequential opts) then sequence else parBatchedActions (batchSize opts) []

parBatchedActions :: Int -> [a] -> [IO a] -> IO [a]
parBatchedActions batch done todo
  | batch < 1 = ioError (userError "--batch-size must be positive")
  | null (drop batch todo) = (done ++) <$> mapConcurrently id todo
  | otherwise = (done ++) <$> mapConcurrentlyBounded batch id todo

-- | Whether the file exists, saying in verbose mode what was looked for and where.
verboseTestFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> T.Text -> AbsFile -> IO Bool
verboseTestFile opts ch what p = do
  fileExists <- Path.doesFileExist p
  color <- useColor H.stderr
  let rel = T.pack (relativeToBase opts (toFilePath p))
  logVerbose opts ch $
    if fileExists
      then green color ("Found " <> what) <> ": " <> rel
      else "No " <> what <> " at " <> rel
  return fileExists

-- | The first of these files that exists, saying in verbose mode which ones were looked for.
findFirstFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> T.Text -> [AbsFile] -> IO (Maybe AbsFile)
findFirstFile _ _ _ [] = return Nothing
findFirstFile opts ch what (file : rest) = do
  exists <- verboseTestFile opts ch what file
  if exists then return (Just file) else findFirstFile opts ch what rest

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
  withStagedOutput target (writeStagedText target contents)

writeStagedText :: AbsFile -> T.Text -> AbsFile -> IO ()
writeStagedText target contents staged = do
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

includePreamble :: T.Text
includePreamble = "### Generated by hledger-flow - DO NOT EDIT ###\n"

includeYears' :: T.Text -> Either T.Text [Integer]
includeYears' txt
  | T.stripEnd txt == T.stripEnd includePreamble = Right []
  | otherwise = case partitionEithers (includeYears'' txt) of
      (errors, []) -> do
        let msg = "Unable to extract years from the following text:\n" <> txt <> "\nErrors:\n" <> T.intercalate "\n" (map T.pack errors)
        Left msg
      (_, years) -> Right years

includeYears'' :: T.Text -> [Either String Integer]
includeYears'' txt = map extractDigits (T.lines txt)

extractDigits :: T.Text -> Either String Integer
extractDigits txt = fmap fst $ (T.decimal . (T.filter isDigit)) txt
