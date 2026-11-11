{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.CSVImport
  ( importCSVs,
  )
where

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (effectiveRunDir, relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.ImportHelpers
import Hledger.Flow.Import.ImportHelpersTurtle (writeIncludesFromInventory, writeToplevelAllYearsInclude)
import Hledger.Flow.Import.Inventory
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import qualified Path
import qualified Path.IO as PathIO
import System.Environment (getEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose)
import qualified System.Info as Sys
import qualified System.Process as Process
import System.FilePath (dropTrailingPathSeparator, takeExtension, (</>))
import qualified Data.List as List
import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Status (nextStep, plural)
import Hledger.Flow.Status.Gather (accountName, gatherStatus)
import Hledger.Flow.Status.Render (renderNext, useColor)
import System.IO (stdout)
import Prelude hiding (putStrLn, take, writeFile)

type FileWasGenerated = Bool

importCSVs :: RuntimeOptions -> IO ()
importCSVs opts = withConsoleLogging $ \ch -> do
  when (showOptions opts) (channelOutLn ch (T.pack (show opts)))
  logVerbose opts ch "Starting import"
  (journals, diff) <- timeAction $ importCSVs' opts ch
  let importRoot = baseDir opts Path.</> [Path.reldir|import|]
      effectiveDir = effectiveRunDir (baseDir opts) (importRunDir opts)
      scope = T.pack (dropTrailingPathSeparator (relativeToBase opts (Path.toFilePath effectiveDir)))
      accounts = List.nub (map (accountName importRoot . fst) journals)
  channelOutLn ch (importSummary scope (length journals) (length (filter snd journals)) accounts)
  logVerbose opts ch ("Import took " <> T.pack (show diff))
  status <- gatherStatus (Just (hledgerInfo opts)) (baseDir opts)
  year <- currentYear
  color <- useColor stdout
  channelOut ch (renderNext color (nextStep year (Sys.os == "mingw32") status))

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
  let startYearMsg = maybe " " (\y -> " (for the year " <> T.pack (show y) <> " and onwards) ") (importStartYear opts)
  let dirText = T.pack (Path.toFilePath effectiveDir)
  logVerbose opts ch $ "Collecting input files" <> startYearMsg <> "from " <> dirText
  (inventory, diff) <- timeAction $ findImportFiles importRoot
  let inputFiles = filter selected (stageFiles importRoot [Path.reldir|1-in|] inventory)
      fileCount = length inputFiles
  collisions <- conflictingOutputs inputFiles
  let relative = T.pack . relativeToBase opts . Path.toFilePath
  unless (null collisions) $
    errExit 1 ch ("Multiple input files map to the same output:\n" <>
      T.intercalate "\n" [relative output <> " <- " <> T.intercalate ", " (map relative sources)
                         | (output, sources) <- collisions]) ()
  logVerbose opts ch $ "Found " <> T.pack (show fileCount) <> " input files" <> startYearMsg <> "in " <> T.pack (show diff) <> ". Proceeding with import..."
  importedJournals <- parAwareActions opts (map (extractAndImport opts ch) inputFiles)

  -- Hooks may create files, so reconcile against a fresh inventory only after
  -- every selected conversion succeeds. A cached CSV is never a source.
  (refreshed, journalFindTime) <- timeAction $ findImportFiles importRoot
  expected <- expectedOutputs (stageFiles importRoot [Path.reldir|1-in|] refreshed)
  let stageWithExtension stage ext = filter ((== ext) . takeExtension . Path.toFilePath) (stageFiles importRoot stage refreshed)
      journals = stageWithExtension [Path.reldir|3-journal|] ".journal"
      generated = stageWithExtension [Path.reldir|2-preprocessed|] ".csv" ++ journals
  classified <- forM generated $ \file -> do
    key <- canonicalFile file
    return (file, Map.member key expected)
  let orphans = [file | (file, False) <- classified, selected file]
      journalsOnDisk = [file | (file, True) <- classified, takeExtension (Path.toFilePath file) == ".journal"]
  forM_ orphans $ \file -> do
    target <- moveToTrash file
    channelOutLn ch $ "Moved orphaned output " <> relative file <> " to " <> relative target
  (_, writeIncludeTime1) <- timeAction $ writeIncludesFromInventory opts ch importRoot refreshed journalsOnDisk
  (_, writeIncludeTime2) <- timeAction $ writeToplevelAllYearsInclude opts
  let includeGenTime = journalFindTime + writeIncludeTime1 + writeIncludeTime2
  logVerbose opts ch $ "Wrote include files for " <> T.pack (show (length journalsOnDisk)) <> " generated journals in " <> T.pack (show includeGenTime)
  return importedJournals

extractAndImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> IO (AbsFile, FileWasGenerated)
extractAndImport opts ch inputFile = do
  case extractImportDirs inputFile of
    Right importDirs -> importCSV opts ch importDirs inputFile
    Left errorMessage -> do
      errExit 1 ch errorMessage (inputFile, False)

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
      hook <- findHook opts ch (accountDir importDirs Path.</> [Path.relfile|construct|])
      case hook of
        Just script -> withStagedOutput journalOut (customConstruct opts ch script importDirs csvFile)
        Nothing -> hledgerImport opts ch importDirs csvFile journalOut
    else logNewFileSkip opts ch "import" journalOut
  return (journalOut, shouldImport)

preprocessIfNeeded :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO (AbsFile, Bool)
preprocessIfNeeded opts ch dirs src csvOut = do
  hook <- findHook opts ch (accountDir dirs Path.</> [Path.relfile|preprocess|])
  case hook of
    Nothing -> do
      cached <- verboseTestFile opts ch csvOut
      return (if cached then csvOut else src, False)
    Just script -> do
      regenerate <- if onlyNewFiles opts then needsRegeneration src csvOut else return True
      if regenerate
        then withStagedOutput csvOut (preprocess opts ch script dirs src)
        else logNewFileSkip opts ch "preprocess" csvOut
      return (csvOut, regenerate)

findHook :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> IO (Maybe AbsFile)
findHook opts ch base = do
  alternatives <- if Sys.os == "mingw32"
    then mapM (`Path.addExtension` base) [".ps1", ".exe"]
    else return []
  first (base : alternatives)
  where
    first [] = return Nothing
    first (file : rest) = do
      exists <- verboseTestFile opts ch file
      if exists then return (Just file) else first rest

hookCommand :: AbsFile -> [T.Text] -> IO (T.Text, [T.Text])
hookCommand script args
  | Sys.os == "mingw32" && takeExtension path == ".ps1" = do
      root <- getEnv "SystemRoot"
      let shell = root </> "System32" </> "WindowsPowerShell" </> "v1.0" </> "powershell.exe"
      -- Windows PowerShell -File treats construct's literal "-" as an option.
      -- Invoke the script with literal strings and propagate its exit status.
      let command = "[Console]::OutputEncoding = New-Object Text.UTF8Encoding($false); & "
            <> T.unwords (map literal (T.pack path : args))
            <> "; if (!$?) { if ($LASTEXITCODE) { exit $LASTEXITCODE }; exit 1 }"
      return (T.pack shell, ["-NoProfile", "-NonInteractive", "-OutputFormat", "Text", "-Command", command])
  | otherwise = return (T.pack path, args)
  where
    path = Path.toFilePath script
    -- PowerShell also recognises typographic single quotes as delimiters.
    literal s = "'" <> T.concatMap escape s <> "'"
    escape c | c == '\'' || (c >= '\x2018' && c <= '\x201b') = T.replicate 2 (T.singleton c)
             | otherwise = T.singleton c

logNewFileSkip :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO ()
logNewFileSkip opts ch logIdentifier absTarget =
  Control.Monad.when (onlyNewFiles opts) $ do
    let relativeTarget = relativeToBase opts (Path.toFilePath absTarget)
    logVerbose opts ch $
      "Skipping " <> logIdentifier <> " - output file is at least as new as source: '" <> T.pack relativeTarget <> "'"

preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
preprocess opts ch script dirs src csvOut = do
  let args = map (T.pack . Path.toFilePath) [src, csvOut] ++ hookDirArgs dirs
  let relScript = relativeToBase opts (Path.toFilePath script)
  let relSrc = relativeToBase opts (Path.toFilePath src)
  let cmdLabel = "executing '" <> T.pack relScript <> "' on '" <> T.pack relSrc <> "'"
  (cmd, hookArgs) <- hookCommand script args
  _ <- timeAndExitOnErr opts ch cmdLabel channelOut channelErr (cmd, hookArgs, Nothing)
  return ()

hledgerImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
hledgerImport opts ch importDirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  candidates <- rulesFileCandidates csvFile importDirs
  maybeRulesFile <- firstExistingFile candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = relativeToBase opts (Path.toFilePath rf)
      let hledger = T.pack (Path.toFilePath (FlowTypes.hlPath (hledgerInfo opts)))
      let directives = directivesFile opts
      directivesExist <- PathIO.doesFileExist directives
      let directivesArgs = if directivesExist then ["--file", T.pack (Path.toFilePath directives)] else []
      let csvArgs = ["--file", T.pack csvSrc, "--rules-file", T.pack (Path.toFilePath rf)]
      let args = hledgerConfArgs opts ++ ["print", "--explicit"] ++ directivesArgs ++ csvArgs

      let cmdLabel = "importing '" <> T.pack relCSV <> "' using rules file '" <> T.pack relRules <> "'"
      let hints = ["Check the skip, fields and separator lines in " <> T.pack relRules <> ".", "Docs: " <> docURL "rules-files"]
      ((_, stdOut, _), _) <- timeAndExitOnErr' opts ch cmdLabel [] hints dummyLogger channelErr (hledger, args, Nothing)
      T.writeFile (Path.toFilePath journalOut) stdOut
    Nothing ->
      do
        let candidatesTxt = T.intercalate "\n" $ map (T.pack . relativeToBase opts . Path.toFilePath) candidates
        let msg =
              "I couldn't find an hledger rules file while trying to import\n" <> T.pack relCSV
                <> "\n\nI will happily use the first rules file I can find from any one of these "
                <> T.pack (show (length candidates)) <> " files:\n" <> candidatesTxt
                <> "\n\nHere is a bit of documentation about rules files that you may find helpful:\n"
                <> docURL "rules-files"
        errExit 1 ch msg ()

hookDirArgs :: ImportDirs b -> [T.Text]
hookDirArgs dirs = map (T.concat . importDirLines) [bankDir dirs, accountDir dirs, ownerDir dirs]

customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
customConstruct opts ch constructScript dirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  let relScript = relativeToBase opts (Path.toFilePath constructScript)
  let constructArgs = [T.pack csvSrc, "-"] ++ hookDirArgs dirs
  let constructCmdText = "Running: " <> T.pack relScript <> " " <> showCmdArgs constructArgs
  (cmd, hookArgs) <- hookCommand constructScript constructArgs
  let hook = (Process.proc (T.unpack cmd) (map T.unpack hookArgs))
        { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe }
  let hledger = T.pack (Path.toFilePath (FlowTypes.hlPath (hledgerInfo opts)))
  let args =
        hledgerConfArgs opts
          ++ [ "print",
               "--ignore-assertions",
               "--file",
               "-",
               "--output-file",
               T.pack (Path.toFilePath journalOut)
             ]

  let relSrc = relativeToBase opts csvSrc
  let cmdLabel = "executing '" <> T.pack relScript <> "' on '" <> T.pack relSrc <> "'"
  Process.withCreateProcess hook $ \input output _ handle -> do
    Just stdinPipe <- return input
    Just stdoutPipe <- return output
    hClose stdinPipe
    _ <- timeAndExitOnErr' opts ch cmdLabel [constructCmdText] [] channelOut channelErr
      (hledger, args, Just stdoutPipe)
    hClose stdoutPipe
    status <- Process.waitForProcess handle
    case status of
      ExitSuccess -> return ()
      ExitFailure _ -> exitWith status
