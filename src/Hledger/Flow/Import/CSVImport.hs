{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.CSVImport
  ( importCSVs,
  )
where

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.Environment (importSettings, renderSettings)
import Hledger.Flow.BaseDir (effectiveRunDir, relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.Import.ImportHelpers
import Hledger.Flow.Import.IncludeFiles (writeIncludesFromInventory, writeToplevelAllYearsInclude)
import Hledger.Flow.Import.Inventory
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, isGlobMetacharacter)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import qualified Path
import qualified Path.IO as PathIO
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import Control.Concurrent.Async (concurrently)
import qualified Data.Text.IO as T
import System.IO (hClose, stderr)
import qualified System.Info as Sys
import qualified System.Process as Process
import System.FilePath (dropTrailingPathSeparator, takeExtension, (</>))
import qualified Data.List as List
import Hledger.Flow.Status (plural)
import Hledger.Flow.Status.Gather (accountName)
import Prelude hiding (putStrLn, take, writeFile)

type FileWasGenerated = Bool

importCSVs :: RuntimeOptions -> IO ()
importCSVs opts = withConsoleLogging $ \ch -> do
  homes <- homeDirs
  when (showOptions opts) $ do
    info <- hledgerInfo opts
    channelOutLn ch (T.unlines (renderSettings id (importSettings homes info opts)))
  logVerbose opts ch $ "Base directory: " <> T.pack (tildePath homes (dropTrailingPathSeparator (Path.toFilePath (baseDir opts))))
  (journals, diff) <- timeAction $ importCSVs' opts ch
  let importRoot = baseDir opts Path.</> [Path.reldir|import|]
      effectiveDir = effectiveRunDir (baseDir opts) (importRunDir opts)
      scope = T.pack (dropTrailingPathSeparator (relativeToBase opts (Path.toFilePath effectiveDir)))
      accounts = List.nub (map (accountName importRoot . fst) journals)
  when (verbose opts) $ channelErr ch "\n"
  channelOutLn ch (importSummary scope (length journals) (length (filter snd journals)) accounts)
  logVerbose opts ch ("Import finished in " <> showDuration diff)

-- | One line saying what import did.
importSummary :: T.Text -> Int -> Int -> [T.Text] -> T.Text
importSummary scope total generated accounts
  | total == 0 = "No statements to import in " <> scope <> "."
  | generated == total = "Imported " <> plural total "statement" <> " for " <> accountsText <> "."
  | otherwise =
      "Imported " <> T.pack (show generated) <> " of " <> plural total "statement" <> " for " <> accountsText
        <> " (" <> T.pack (show (total - generated)) <> " already up to date)."
  where
    accountsText
      | length accounts <= 3 = T.intercalate ", " accounts
      | otherwise = plural (length accounts) "account"

importCSVs' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [(AbsFile, FileWasGenerated)]
importCSVs' opts ch = do
  let effectiveDir = effectiveRunDir (baseDir opts) (importRunDir opts)
      importRoot = baseDir opts Path.</> [Path.reldir|import|]
      selected = inImportScope effectiveDir (fromMaybe 0 (importStartYear opts))
  let startYearMsg = maybe "" (\y -> " from " <> T.pack (show y) <> " onwards") (importStartYear opts)
  let relative = T.pack . relativeToBase opts . Path.toFilePath
  logVerbose opts ch $ "Looking for statements" <> startYearMsg <> " in " <> relative effectiveDir
    <> " (import/<owner>/<bank>/<account>/1-in/<year>/)"
  (inventory, diff) <- timeAction $ findImportFiles importRoot
  let inputFiles = filter selected (stageFiles importRoot [Path.reldir|1-in|] inventory)
      fileCount = length inputFiles
  collisions <- conflictingOutputs inputFiles
  unless (null collisions) $
    errExit 1 ch ("Multiple input files map to the same output:\n" <>
      T.intercalate "\n" [relative output <> " <- " <> T.intercalate ", " (map relative sources)
                         | (output, sources) <- collisions])
  logVerbose opts ch $ "Found " <> plural fileCount "statement" <> " in " <> showDuration diff
  -- Glob escaping depends on the hledger version. Check it before any worker
  -- writes output; conversions reuse this result. directives.journal and
  -- cached CSVs share these paths' base and statement names.
  when (any (any isGlobMetacharacter . Path.toFilePath) inputFiles) $ void (hledgerVersion opts)
  importedJournals <- parAwareActions opts (map (extractAndImport opts ch importRoot) inputFiles)

  -- Running a preprocess hook also forces conversion. When every journal was
  -- skipped, neither a hook nor a conversion could have changed the inventory.
  let cached = if any snd importedJournals then Nothing else Just inventory
  ((refreshed, orphans, journalsOnDisk), journalFindTime) <- timeAction $ reconciliationInventory importRoot cached
  forM_ (filter selected orphans) $ \file -> do
    target <- moveToTrash file
    channelOutLn ch $ "Moved orphaned output " <> relative file <> " to " <> relative target
  withLogBlock opts True ch "Include files" $ \_ -> do
    (_, writeIncludeTime1) <- timeAction $ writeIncludesFromInventory opts ch importRoot refreshed journalsOnDisk
    (_, writeIncludeTime2) <- timeAction $ writeToplevelAllYearsInclude opts
    let includeGenTime = journalFindTime + writeIncludeTime1 + writeIncludeTime2
    logVerbose opts ch $ "Updated include files for " <> plural (length journalsOnDisk) "generated journal" <> " in " <> showDuration includeGenTime
  return importedJournals

extractAndImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsDir -> AbsFile -> IO (AbsFile, FileWasGenerated)
extractAndImport opts parent root inputFile =
  withLogBlock opts (sequential opts) parent ("Statement " <> T.pack (relativeToBase opts (Path.toFilePath inputFile))) $ \ch ->
  case extractImportDirs root inputFile of
    Right importDirs -> importCSV opts ch importDirs inputFile
    Left errorMessage -> do
      errExit 1 ch errorMessage

importCSV :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> IO (AbsFile, FileWasGenerated)
importCSV opts ch importDirs srcFile = do
  csvOut <- generatedOutput [Path.reldir|2-preprocessed|] ".csv" srcFile
  (csvFile, preprocessHappened) <- preprocessIfNeeded opts ch importDirs srcFile csvOut
  journalOut <- generatedOutput [Path.reldir|3-journal|] ".journal" csvFile
  shouldImport <-
    if onlyNewFiles opts && not preprocessHappened
      then needsRegeneration csvFile journalOut
      else return True
  PathIO.createDirIfMissing True (Path.parent journalOut)
  if shouldImport
    then do
      hook <- findHook opts ch "construct script" (accountDir importDirs Path.</> [Path.relfile|construct|])
      case hook of
        Just script -> withStagedOutput journalOut (customConstruct opts ch script importDirs csvFile journalOut)
        Nothing -> hledgerImport opts ch importDirs csvFile journalOut
      logVerbose opts ch ("Wrote " <> T.pack (relativeToBase opts (Path.toFilePath journalOut)))
    else logNewFileSkip opts ch "import" journalOut
  return (journalOut, shouldImport)

preprocessIfNeeded :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO (AbsFile, Bool)
preprocessIfNeeded opts ch dirs src csvOut = do
  hook <- findHook opts ch "preprocess script" (accountDir dirs Path.</> [Path.relfile|preprocess|])
  case hook of
    Nothing -> do
      cached <- verboseTestFile opts ch "preprocessed CSV from an earlier run" csvOut
      if cached
        then do
          stale <- needsRegeneration src csvOut
          when stale $ ioError $ userError $
            relativeToBase opts (Path.toFilePath src) ++ " is newer than its cached CSV "
              ++ relativeToBase opts (Path.toFilePath csvOut) ++ ". Restore the preprocess hook to regenerate it,"
              ++ " or move the cached CSV aside to use the source directly. Then run hledger-flow import."
          return (csvOut, False)
        else return (src, False)
    Just script -> do
      regenerate <- if onlyNewFiles opts then needsRegeneration src csvOut else return True
      if regenerate
        then do
          withStagedOutput csvOut (preprocess opts ch script dirs src csvOut)
          logVerbose opts ch ("Wrote " <> T.pack (relativeToBase opts (Path.toFilePath csvOut)))
        else logNewFileSkip opts ch "preprocess" csvOut
      return (csvOut, regenerate)

findHook :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO (Maybe AbsFile)
findHook opts ch what base = do
  alternatives <- if Sys.os == "mingw32"
    then mapM (`Path.addExtension` base) [".ps1", ".exe"]
    else return []
  findFirstFile opts ch what (base : alternatives)

hookCommand :: AbsFile -> [T.Text] -> IO (T.Text, [T.Text])
hookCommand script args
  | Sys.os == "mingw32" && takeExtension path == ".ps1" = do
      root <- getEnv "SystemRoot"
      let shell = root </> "System32" </> "WindowsPowerShell" </> "v1.0" </> "powershell.exe"
      -- Windows PowerShell -File treats construct's literal "-" as an option.
      -- Invoke the script with literal strings and propagate its exit status.
      let command = "[Console]::OutputEncoding = New-Object Text.UTF8Encoding($false); & "
            <> T.unwords (map powerShellLiteral (T.pack path : args))
            <> "; if (!$?) { if ($LASTEXITCODE) { exit $LASTEXITCODE }; exit 1 }"
      return (T.pack shell, ["-NoProfile", "-NonInteractive", "-OutputFormat", "Text", "-Command", command])
  | otherwise = return (T.pack path, args)
  where
    path = Path.toFilePath script

logNewFileSkip :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO ()
logNewFileSkip opts ch logIdentifier absTarget =
  Control.Monad.when (onlyNewFiles opts) $ do
    let relativeTarget = relativeToBase opts (Path.toFilePath absTarget)
    logVerbose opts ch $
      "Skipping " <> logIdentifier <> ": " <> T.pack relativeTarget <> " is at least as new as its source"

-- | Run the preprocess script, writing to the staged file. The command shown names the final
-- output instead, so a pasted command writes where hledger-flow does.
preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> AbsFile -> IO ()
preprocess opts ch script dirs src csvOut staged = do
  let args out = map (T.pack . Path.toFilePath) [src, out] ++ hookDirArgs dirs
      relSrc = relativeToBase opts (Path.toFilePath src)
      external = External (relativeHook opts script) ("preprocess " <> T.pack relSrc)
        [T.pack (Path.toFilePath script) : args csvOut] []
  (cmd, hookArgs) <- hookCommand script (args staged)
  void $ runExternal opts ch external True (cmd, hookArgs, Nothing)

relativeHook :: RuntimeOptions -> AbsFile -> T.Text
relativeHook opts = T.pack . relativeToBase opts . Path.toFilePath

hledgerImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
hledgerImport opts ch importDirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  candidates <- rulesFileCandidates csvFile importDirs
  maybeRulesFile <- findFirstFile opts ch "rules file" candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = relativeToBase opts (Path.toFilePath rf)
      let hledger = T.pack (Path.toFilePath (hledgerExecutable opts))
      let directives = directivesFile opts
      directivesExist <- PathIO.doesFileExist directives
      let paths = csvSrc : [Path.toFilePath directives | directivesExist]
      fileArgument <- if any (any isGlobMetacharacter) paths
        then hledgerFileArgument <$> hledgerInfo opts
        else return T.pack
      let directivesArgs = if directivesExist then ["--file", fileArgument (Path.toFilePath directives)] else []
      let csvArgs = ["--file", fileArgument csvSrc, "--rules-file", T.pack (Path.toFilePath rf)]
      let args = hledgerConfArgs opts ++ ["print", "--explicit"] ++ directivesArgs ++ csvArgs

      let hints = ["Check the skip, fields and separator lines in " <> T.pack relRules <> ".", "Docs: " <> docURL RulesFiles]
          external = External "hledger" ("convert " <> T.pack relCSV <> " with the rules in " <> T.pack relRules) [hledger : args] hints
      stdOut <- runExternal opts ch external False (hledger, args, Nothing)
      writeGeneratedText (Path.toFilePath journalOut) stdOut
    Nothing ->
      do
        let candidatesTxt = T.intercalate "\n" $ map (T.pack . relativeToBase opts . Path.toFilePath) candidates
        let msg =
              "I couldn't find an hledger rules file while trying to import\n" <> T.pack relCSV
                <> "\n\nI will happily use the first rules file I can find from any one of these "
                <> T.pack (show (length candidates)) <> " files:\n" <> candidatesTxt
                <> "\n\nHere is a bit of documentation about rules files that you may find helpful:\n"
                <> docURL RulesFiles
        errExit 1 ch msg

hookDirArgs :: ImportDirs b -> [T.Text]
hookDirArgs dirs = map (T.concat . importDirLines) [bankDir dirs, accountDir dirs, ownerDir dirs]

-- | Pipe the construct script's output through hledger into the staged journal. The command
-- shown names the final journal instead, so a pasted command writes where hledger-flow does.
customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> AbsFile -> IO ()
customConstruct opts ch constructScript dirs csvFile journalOut staged = do
  let csvSrc = Path.toFilePath csvFile
      relScript = relativeHook opts constructScript
      relSrc = T.pack (relativeToBase opts csvSrc)
      constructArgs = [T.pack csvSrc, "-"] ++ hookDirArgs dirs
      hledger = T.pack (Path.toFilePath (hledgerExecutable opts))
      hledgerArgs out = hledgerConfArgs opts ++ ["print", "--ignore-assertions", "--file", "-", "--output-file", T.pack (Path.toFilePath out)]
      shown = [T.pack (Path.toFilePath constructScript) : constructArgs, hledger : hledgerArgs journalOut]
      constructExt = External relScript ("construct a journal from " <> relSrc) shown []
      hledgerExt = External "hledger" ("read the journal " <> relScript <> " constructed from " <> relSrc) shown []
  (cmd, hookArgs) <- hookCommand constructScript constructArgs
  let hook = (Process.proc (T.unpack cmd) (map T.unpack hookArgs))
        { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
  announceExternal opts ch constructExt
  Process.withCreateProcess hook $ \input output errors handle -> do
    Just stdinPipe <- return input
    Just stdoutPipe <- return output
    Just stderrPipe <- return errors
    hClose stdinPipe
    ((converted@(hledgerStatus, _, _), duration), hookErr) <- concurrently
      (timeAction (runProcessCaptured (hledger, hledgerArgs staged, Just stdoutPipe)))
      (T.hGetContents stderrPipe)
    status <- Process.waitForProcess handle
    color <- useColor stderr
    -- When hledger stops reading early, the script fails with a broken pipe, so hledger's error
    -- comes first, along with anything the script said.
    let scriptNote = case status of
          ExitFailure code -> [relScript <> " also failed, with exit code " <> T.pack (show code) <> "."]
          ExitSuccess -> []
        scriptOutput = outputSection color (verbose opts) ("Error output from " <> relScript) hookErr
    when (hledgerStatus /= ExitSuccess) $
      checkExternal opts ch hledgerExt {extHints = scriptNote ++ scriptOutput} True duration converted
    checkExternal opts ch constructExt True duration (status, "", hookErr)
    checkExternal opts ch hledgerExt True duration converted
