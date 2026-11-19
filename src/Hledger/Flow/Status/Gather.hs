{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Collect a ledger's status from disk. Read-only and silent.
module Hledger.Flow.Status.Gather
  ( classifyDir,
    gatherStatus,
    gatherStatusWithConfig,
    accountName,
    cachedStatement,
    hookExists,
    mainRulesCandidates,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, when)
import Data.Char (toLower)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
import Hledger.Flow.BaseDir (findBaseDir, relativeToBase')
import Hledger.Flow.Common (firstExistingFile, groupValuesBy, hledgerFileArgument, runProcessCaptured)
import Hledger.Flow.HledgerConfig (findConfigConflicts)
import Hledger.Flow.Import.ImportHelpers (defaultRulesCandidates, extractImportDirs, rulesFileCandidates)
import Hledger.Flow.Import.IncludeFiles (includesNeedUpdate)
import Hledger.Flow.Import.Inventory (classifyGeneratedOutputs, findImportFiles, generatedOutput, isManualJournal, managedIncludeFiles, stageFiles)
import Hledger.Flow.Import.Types (ImportDirs (..))
import Hledger.Flow.Init.Example (directivesAreComments, journalIsCommentsOnly, sampleAccountDir, untouchedExample)
import Hledger.Flow.LedgerSelection
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelFile, pathOccupied)
import Hledger.Flow.ReportReview (loadReportReview, reviewItems)
import Hledger.Flow.StatementScan (StatementYear (..), inferStatementYear, isDelimitedFile, readDelimitedStatement)
import Hledger.Flow.Status
import Hledger.Flow.Types (BaseDir, HledgerConf, HledgerInfo (..))
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import System.Exit (ExitCode (..))
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
gatherStatus hledger = gatherStatusWithConfig hledger Nothing

gatherStatusWithConfig :: Maybe HledgerInfo -> Maybe HledgerConf -> BaseDir -> IO LedgerStatus
gatherStatusWithConfig hledger conf base = do
  let root = base </> [reldir|import|]
  files <- findImportFiles root
  let statements = stageFiles root [reldir|1-in|] files
      manualJournals = filter (isManualJournal root) files
      groups = Map.toAscList (groupValuesBy (accountName root) statements)
  accounts <- forM groups (uncurry (accountStatus base))
  conflicts <- maybe (pure []) (\hl -> findConfigConflicts hl conf) hledger
  selection <- selectLedger hledger base
  let defaultJournal = base </> [relfile|all-years.journal|]
  allYears <- PathIO.doesFileExist defaultJournal
  let selected = aggregateJournal selection
  (orphans, journals) <- classifyGeneratedOutputs root files statements
  -- A newly initialised, empty ledger has no indexes to reconcile yet.
  reconciliation <- if (allYears || isJust selected) && null orphans
    then includesNeedUpdate base root files journals allYears
    else return (not (null orphans) || not (null (managedIncludeFiles root files)))
  needsPrices <- case (hledger, selected) of
    (Just info, Just journal) | not reconciliation -> journalNeedsPrices info journal
    _ -> return False
  review <- maybe (pure Nothing) (\journal -> loadReportReview base journal conf) selected
  examplePresent <- either (const True) id <$> tryIOError (pathOccupied (sampleAccountDir base))
  example <- case review of
    Just current | null (reviewItems current) -> untouchedExample base statements
    _ -> return False
  manualWork <- case manualJournals of
    [] -> pure False
    -- A wrapper can contribute journal entries of its own.
    _ | not (null statements) || maybe False (/= defaultJournal) selected -> pure True
    _ -> either (const True) not <$> tryIOError (onlyJournalComments base manualJournals)
  misplaced <- traverse (inspectMisplaced base) (List.sortOn mpPath (misplacedFiles root files))
  return
    LedgerStatus
      { lsBaseDir = base,
        lsHledger = hledger,
        lsLedgerFile = selectedLedgerFile selection,
        lsAggregateJournal = selected,
        lsHledgerConf = conf,
        lsConfigConflicts = conflicts,
        lsAccounts = accounts,
        lsMisplaced = misplaced,
        lsManualJournals = length manualJournals,
        lsHasManualWork = manualWork,
        lsAllYearsExists = allYears,
        lsNeedsReconciliation = reconciliation,
        lsNeedsPrices = needsPrices,
        lsReview = review,
        lsExampleAccountPresent = examplePresent,
        lsUntouchedExample = example
      }

-- The starter manual journals are comments. Directives or unreadable
-- journals still need the ordinary import/report guidance.
onlyJournalComments :: BaseDir -> [AbsFile] -> IO Bool
onlyJournalComments base [] = directivesAreComments base
onlyJournalComments base (file : rest) = do
  commentOnly <- journalIsCommentsOnly file
  if commentOnly then onlyJournalComments base rest else return False

inspectMisplaced :: BaseDir -> Misplaced -> IO Misplaced
inspectMisplaced base item = do
  evidence <- case mpYear item of
    UnknownYear | isDelimitedFile (mpPath item), Just path <- parseRelFile (mpPath item) -> do
      scanned <- tryIOError (readDelimitedStatement Nothing (base </> path))
      return $ case scanned of
        Right (Right statement) -> inferStatementYear statement
        _ -> UnknownYear
    known -> return known
  let suggestion = case evidence of
        SingleYear year -> FilePath.joinPath
          [if part == "<year>" then show year else part | part <- FilePath.splitDirectories (mpSuggestion item)]
        _ -> mpSuggestion item
      enriched = item {mpYear = evidence, mpSuggestion = suggestion}
  occupied <- case placementDestination enriched of
    Nothing -> return False
    Just path -> either (const True) id <$> tryIOError (pathOccupied (base </> path))
  return enriched {mpDestinationExists = occupied}

-- Ask hledger to resolve includes and parse the journal. If it cannot read
-- the journal, status can still report what it found in the import tree.
journalNeedsPrices :: HledgerInfo -> AbsFile -> IO Bool
journalNeedsPrices info journal = do
  commodities <- query ["commodities", "--used"]
  case T.lines <$> commodities of
    Just (_ : _ : _) -> (== Just "") . fmap T.strip <$> query ["prices"]
    _ -> return False
  where
    query args = do
      let executable = T.pack (toFilePath (hlPath info))
          fileArgs = ["--no-conf", "--file", hledgerFileArgument info (toFilePath journal)]
      result <- tryIOError (runProcessCaptured (executable, args ++ fileArgs, Nothing))
      return $ case result of
        Right (ExitSuccess, output, _) -> Just output
        _ -> Nothing

accountName :: AbsDir -> AbsFile -> T.Text
accountName root file = case stripProperPrefix root file of
  Just rel -> T.intercalate "/" (map T.pack (take 3 (FilePath.splitDirectories (toFilePath rel))))
  Nothing -> T.pack (toFilePath file)

accountStatus :: BaseDir -> T.Text -> [AbsFile] -> IO AccountStatus
accountStatus base name statements = do
  dirs <- case statements of
    s : _ -> either (ioError . userError . T.unpack) return (extractImportDirs (base </> [reldir|import|]) s)
    [] -> ioError (userError ("No statements for " ++ T.unpack name))
  let relative = relativeToBase' (toFilePath base) . toFilePath
  defaults <- defaultRulesCandidates dirs
  rulesFile <- firstExistingFile defaults
  preprocess <- hookFile (accountDir dirs) [relfile|preprocess|]
  construct <- hookFile (accountDir dirs) [relfile|construct|]
  directives <- firstExistingFile [base </> [relfile|directives.journal|]]
  sources <- forM statements $ \s -> do
    cached <- cachedStatement s
    candidates <- rulesFileCandidates (fromMaybe s cached) dirs
    rules <- firstExistingFile candidates
    return (s, cached, rules, candidates)
  let resolved = [rules | (_, _, rules, _) <- sources]
      withoutRules = length (filter isNothing resolved)
  (otherRules, targetOccupied) <- if withoutRules > 0 && isNothing construct
    then do
      other <- tryIOError (mainRulesCandidates (accountDir dirs) (concat [xs | (_, _, _, xs) <- sources]))
      occupied <- case defaults of
        target : _ -> either (const True) id <$> tryIOError (pathOccupied target)
        _ -> return False
      return (either (const []) id other, occupied)
    else return ([], False)
  -- Statements in an account commonly share one rules file.
  ruleTimes <- case construct of
    Just _ -> return Map.empty
    Nothing -> traverse (tryIOError . latestRuleChange) $
      Map.fromSet id (Set.fromList (catMaybes resolved))
  journals <- forM sources $ \(s, cached, rules, _) -> do
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
        asWithoutRules = withoutRules,
        asOtherRules = map relative otherRules,
        asRulesTargetOccupied = targetOccupied,
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
      children <- ruleIncludes file
      times <- mapM (visit (Set.insert key ancestors)) children
      return (maximum (modified : times))

ruleIncludes :: FilePath -> IO [FilePath]
ruleIncludes file = do
  contents <- T.readFile file
  return [FilePath.takeDirectory file FilePath.</> T.unpack (T.strip path)
         | line <- T.lines (T.dropWhile (== '\xFEFF') contents),
           Just path <- [T.stripPrefix "include " line]]

mainRulesCandidates :: AbsDir -> [AbsFile] -> IO [AbsFile]
mainRulesCandidates dir recognized = do
  (_, files) <- PathIO.listDir dir
  let local = List.sort [file | file <- files, map toLower (FilePath.takeExtension (toFilePath file)) == ".rules"]
      canonical = Directory.canonicalizePath . toFilePath
  recognizedTargets <- Set.fromList <$> traverse canonical recognized
  children <- concat <$> traverse (ruleIncludes . toFilePath) local
  childTargets <- Set.fromList <$> traverse Directory.canonicalizePath children
  pairs <- traverse (\file -> (,) file <$> canonical file) local
  return [file | (file, target) <- pairs, Set.notMember target (Set.union recognizedTargets childTargets)]

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
