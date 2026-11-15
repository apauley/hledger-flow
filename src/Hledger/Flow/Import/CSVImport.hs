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
import Hledger.Flow.Import.Incremental
import Hledger.Flow.Import.Conversion
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
import System.FilePath (dropTrailingPathSeparator)
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
  let run tracking = parAwareActions opts (map (extractAndImport tracking opts ch importRoot) inputFiles)
  outcomes <- case importPolicy opts of
    RunScripts -> withIncrementalRun opts ch fileCount (run . Just)
    SkipUnchangedScripts -> withIncrementalRun opts ch fileCount (run . Just)
    _ -> run Nothing
  let importedJournals = [(journal, generated) | (journal, generated, _) <- outcomes]
      cached = if or [refresh | (_, _, refresh) <- outcomes] then Nothing else Just inventory
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

extractAndImport :: Maybe IncrementalRun -> RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsDir -> AbsFile -> IO (AbsFile, Bool, Bool)
extractAndImport tracking opts parent root inputFile =
  withLogBlock opts (sequential opts) parent ("Statement " <> T.pack (relativeToBase opts (Path.toFilePath inputFile))) $ \ch ->
  case extractImportDirs root inputFile of
    Right importDirs -> case tracking of
      Just run -> importStatement run opts ch importDirs inputFile
      Nothing -> do
        (output, generated) <- importCSV opts ch importDirs inputFile
        pure (output, generated, generated)
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
      effective <- savedInput opts ch src csvOut
      return (effective, False)
    Just script -> do
      regenerate <- if onlyNewFiles opts then needsRegeneration src csvOut else return True
      if regenerate
        then do
          withStagedOutput csvOut (preprocess opts ch script dirs src csvOut)
          logVerbose opts ch ("Wrote " <> T.pack (relativeToBase opts (Path.toFilePath csvOut)))
        else logNewFileSkip opts ch "preprocess" csvOut
      return (csvOut, regenerate)
