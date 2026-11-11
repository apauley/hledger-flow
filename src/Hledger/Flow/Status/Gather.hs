{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Collect a ledger's status from disk. Read-only and silent.
module Hledger.Flow.Status.Gather
  ( classifyDir,
    gatherStatus,
    accountName,
    cachedStatement,
    hookExists,
  )
where

import Control.Monad (forM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Hledger.Flow.BaseDir (findBaseDir, relativeToBase')
import Hledger.Flow.Common (configuredLedgerFile, firstExistingFile)
import Hledger.Flow.Import.ImportHelpers (defaultRulesCandidates, extractImportDirs, rulesFileCandidates)
import Hledger.Flow.Import.ImportHelpersTurtle (includesNeedUpdate)
import Hledger.Flow.Import.Inventory (classifyGeneratedOutputs, findImportFiles, generatedOutput, isManualJournal, managedIncludeFiles, stageFiles)
import Hledger.Flow.Import.Types (ImportDirs (..))
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelFile)
import Hledger.Flow.Status
import Hledger.Flow.Types (BaseDir, HledgerInfo)
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Info as Sys
import System.IO.Error (tryIOError)

-- | Is this folder inside a ledger, empty, or something else?
classifyDir :: AbsDir -> IO Situation
classifyDir dir = do
  found <- findBaseDir dir
  case found of
    Just base -> return (InLedger base)
    Nothing -> do
      (dirs, files) <- PathIO.listDirRel dir
      let names = map (FilePath.dropTrailingPathSeparator . toFilePath) dirs ++ map toFilePath files
      return (if all ("." `List.isPrefixOf`) names then EmptyDir else Elsewhere)

gatherStatus :: Maybe HledgerInfo -> BaseDir -> IO LedgerStatus
gatherStatus hledger base = do
  let root = base </> [reldir|import|]
  files <- findImportFiles root
  let statements = stageFiles root [reldir|1-in|] files
      groups = Map.toAscList (Map.fromListWith (flip (++)) [(accountName root s, [s]) | s <- statements])
  accounts <- forM groups (uncurry (accountStatus base))
  configured <- configuredLedgerFile
  ledgerFile <- forM configured $ \(value, path) -> LedgerFile value <$> Directory.doesFileExist path
  allYears <- PathIO.doesFileExist (base </> [relfile|all-years.journal|])
  (orphans, journals) <- classifyGeneratedOutputs root files statements
  -- A newly initialised, empty ledger has no indexes to reconcile yet.
  reconciliation <- if allYears && null orphans
    then includesNeedUpdate base root files journals
    else return (not (null orphans) || not (null (managedIncludeFiles root files)))
  return
    LedgerStatus
      { lsBaseDir = base,
        lsHledger = hledger,
        lsLedgerFile = ledgerFile,
        lsAccounts = accounts,
        lsMisplaced = List.sortOn mpPath (misplacedFiles root files),
        lsManualJournals = length (filter (isManualJournal root) files),
        lsAllYearsExists = allYears,
        lsNeedsReconciliation = reconciliation
      }

accountName :: AbsDir -> AbsFile -> T.Text
accountName root file = case stripProperPrefix root file of
  Just rel -> T.intercalate "/" (map T.pack (take 3 (FilePath.splitDirectories (toFilePath rel))))
  Nothing -> T.pack (toFilePath file)

accountStatus :: BaseDir -> T.Text -> [AbsFile] -> IO AccountStatus
accountStatus base name statements = do
  dirs <- case statements of
    s : _ -> either (ioError . userError . T.unpack) return (extractImportDirs s)
    [] -> ioError (userError ("No statements for " ++ T.unpack name))
  let relative = relativeToBase' (toFilePath base) . toFilePath
  defaults <- defaultRulesCandidates dirs
  rulesFile <- firstExistingFile defaults
  preprocess <- hookFile (accountDir dirs) [relfile|preprocess|]
  construct <- hookFile (accountDir dirs) [relfile|construct|]
  directives <- firstExistingFile [base </> [relfile|directives.journal|]]
  results <- forM statements $ \s -> do
    cached <- cachedStatement s
    rules <- rulesFileCandidates (fromMaybe s cached) dirs >>= firstExistingFile
    let conversionInputs = case construct of
          Just script -> [Just script]
          Nothing -> [rules, directives]
        inputs = s : catMaybes (cached : preprocess : conversionInputs)
    journal <- generatedOutput [reldir|3-journal|] ".journal" s
    state <- journalState inputs journal
    return (rules, state)
  let (resolved, journals) = unzip results
  return
    AccountStatus
      { asAccount = name,
        asStatements = length statements,
        asYears = List.nub (List.sort (map yearOf statements)),
        asRulesFile = relative <$> rulesFile,
        asRulesCandidates = map relative defaults,
        asWithoutRules = length (filter isNothing resolved),
        asPreprocess = isJust preprocess,
        asConstruct = isJust construct,
        asJournalsMissing = length (filter (== Missing) journals),
        asJournalsOutdated = length (filter (== Outdated) journals)
      }
  where
    yearOf s = read (FilePath.dropTrailingPathSeparator (toFilePath (dirname (parent s))))

data JournalState = Current | Outdated | Missing deriving (Eq)

journalState :: [AbsFile] -> AbsFile -> IO JournalState
journalState inputs journal = do
  exists <- PathIO.doesFileExist journal
  if exists
    then do
      -- Keep filesystem precision so a rule edited just after import is noticed.
      newer <- tryIOError $ do
        written <- PathIO.getModificationTime journal
        any (> written) <$> mapM PathIO.getModificationTime inputs
      return (if either (const True) id newer then Outdated else Current)
    else return Missing

-- | Import reads this statement's cached CSV when present, even without a hook.
cachedStatement :: AbsFile -> IO (Maybe AbsFile)
cachedStatement statement = do
  csv <- generatedOutput [reldir|2-preprocessed|] ".csv" statement
  firstExistingFile [csv]

-- | Same hook names import looks for.
hookExists :: AbsDir -> RelFile -> IO Bool
hookExists dir hook = isJust <$> hookFile dir hook

hookFile :: AbsDir -> RelFile -> IO (Maybe AbsFile)
hookFile dir hook = do
  let base = dir </> hook
  alternatives <-
    if Sys.os == "mingw32"
      then mapM (`addExtension` base) [".ps1", ".exe"]
      else return []
  firstExistingFile (base : alternatives)
