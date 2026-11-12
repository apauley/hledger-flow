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

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
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
  sources <- forM statements $ \s -> do
    cached <- cachedStatement s
    rules <- rulesFileCandidates (fromMaybe s cached) dirs >>= firstExistingFile
    return (s, cached, rules)
  let resolved = [rules | (_, _, rules) <- sources]
  -- Statements in an account commonly share one rules file.
  ruleTimes <- case construct of
    Just _ -> return Map.empty
    Nothing -> traverse (tryIOError . latestRuleChange) $
      Map.fromSet id (Set.fromList (catMaybes resolved))
  journals <- forM sources $ \(s, cached, rules) -> do
    let inputs = s : catMaybes [cached, preprocess, construct <|> directives]
    journal <- generatedOutput [reldir|3-journal|] ".journal" s
    journalState inputs (rules >>= (`Map.lookup` ruleTimes)) journal
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

-- hledger interprets each include path relative to the file containing it.
latestRuleChange :: AbsFile -> IO UTCTime
latestRuleChange = visit Set.empty . toFilePath
  where
    visit ancestors file = do
      key <- Directory.canonicalizePath file
      when (Set.member key ancestors) $ ioError (userError ("cyclic rules include: " ++ file))
      modified <- Directory.getModificationTime file
      contents <- T.readFile file
      let children = [FilePath.takeDirectory file FilePath.</> T.unpack (T.strip path)
                     | line <- T.lines (T.dropWhile (== '\xFEFF') contents)
                     , Just path <- [T.stripPrefix "include " line]]
      times <- mapM (visit (Set.insert key ancestors)) children
      return (maximum (modified : times))

journalState :: [AbsFile] -> Maybe (Either IOError UTCTime) -> AbsFile -> IO JournalState
journalState inputs rulesTime journal = do
  exists <- PathIO.doesFileExist journal
  if exists
    then do
      -- Keep filesystem precision so a rule edited just after import is noticed.
      newer <- tryIOError $ do
        written <- PathIO.getModificationTime journal
        times <- mapM PathIO.getModificationTime inputs
        return (any (> written) times || maybe False (either (const True) (> written)) rulesTime)
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
