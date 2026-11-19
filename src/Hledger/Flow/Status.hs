{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | What a ledger contains, and the one thing to do next.
module Hledger.Flow.Status
  ( Situation (..),
    LedgerFile (..),
    AccountStatus (..),
    Misplaced (..),
    LedgerStatus (..),
    NextStep (..),
    statementExtensions,
    suggestPlacement,
    misplacedFiles,
    placementDestination,
    misplacedMoveCommand,
    nextStep,
    firstStatementStep,
    flowCommand,
    ledgerFileNote,
    balanceHint,
    withConfigCheck,
    hledgerVersionNumber,
    plural,
  )
where

import Data.Char (isDigit, toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hledger.Flow.Common (Shell (..), createDirectoryCommand, hledgerFileArgument, hledgerVersionNumber, homeShellPath, moveFileCommand, posixLiteral, powerShellLiteral, shellArgument)
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.HledgerConfig (ConfigConflict, ConfigUse (..), firstConfigConflict, renderConfigConflict)
import Hledger.Flow.Import.Inventory (stageFiles)
import Hledger.Flow.LedgerSelection (Coverage (..), LedgerFile (..), ledgerFileProblem)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelFile)
import Hledger.Flow.ReportReview
import Hledger.Flow.StatementScan (StatementYear (..))
import Hledger.Flow.Types (BaseDir, HledgerConf (..), HledgerInfo)
import Path (parseRelDir, parseRelFile, reldir, relfile, stripProperPrefix, toFilePath, (</>))
import qualified System.FilePath as FilePath

data Situation = InLedger BaseDir | EmptyDir | Elsewhere deriving (Eq, Show)

data AccountStatus = AccountStatus
  { asAccount :: T.Text,
    asStatements :: Int,
    asYears :: [Integer],
    asRulesFile :: Maybe FilePath,
    asRulesCandidates :: [FilePath],
    asWithoutRules :: Int,
    asOtherRules :: [FilePath],
    asRulesTargetOccupied :: Bool,
    asPreprocess :: Bool,
    asConstruct :: Bool,
    asJournalsMissing :: Int,
    asJournalsOutdated :: Int
  }
  deriving (Eq, Show)

-- | Both paths are relative to the base directory and start with @import@.
data Misplaced = Misplaced
  { mpPath :: FilePath,
    mpSuggestion :: FilePath,
    mpYear :: StatementYear,
    mpDestinationExists :: Bool
  }
  deriving (Eq, Show)

data LedgerStatus = LedgerStatus
  { lsBaseDir :: BaseDir,
    lsHledger :: Maybe HledgerInfo,
    lsLedgerFile :: Maybe LedgerFile,
    lsAggregateJournal :: Maybe AbsFile,
    lsHledgerConf :: Maybe HledgerConf,
    lsConfigConflicts :: [ConfigConflict],
    lsAccounts :: [AccountStatus],
    lsMisplaced :: [Misplaced],
    lsManualJournals :: Int,
    lsHasManualWork :: Bool,
    lsAllYearsExists :: Bool,
    lsNeedsReconciliation :: Bool,
    lsNeedsPrices :: Bool,
    lsReview :: Maybe Review,
    lsExampleAccountPresent :: Bool,
    lsUntouchedExample :: Bool
  }
  deriving (Show)

data NextStep = NextStep
  { nsSummary :: T.Text,
    nsCommand :: Maybe T.Text,
    nsDetail :: [T.Text],
    nsDocs :: Maybe T.Text
  }
  deriving (Eq, Show)

statementExtensions :: [String]
statementExtensions = ["csv", "tsv", "ssv", "ofx", "qfx", "qif", "xls", "xlsx"]

isYear :: String -> Bool
isYear s = length s == 4 && all isDigit s

-- | Where a file found at these path components (below @import/@) belongs.
suggestPlacement :: [FilePath] -> FilePath
suggestPlacement parts = FilePath.joinPath (["import"] ++ identity ++ ["1-in", year, file])
  where
    dirs = if null parts then [] else init parts
    file = if null parts then "" else last parts
    year = case filter isYear dirs of
      y : _ -> y
      [] -> "<year>"
    stages = ["1-in", "2-preprocessed", "3-journal"]
    named = take 3 (takeWhile (\d -> d `notElem` stages && not (isYear d)) dirs)
    identity = named ++ drop (length named) ["<owner>", "<bank>", "<account>"]

-- | Statement-like files under the import root that import will never read.
misplacedFiles :: AbsDir -> [AbsFile] -> [Misplaced]
misplacedFiles root files = mapMaybe misplaced (filter candidate files)
  where
    staged = Set.fromList (stageFiles root [reldir|1-in|] files ++ cachedCSVs)
    cachedCSVs = filter ((== ".csv") . FilePath.takeExtension . toFilePath) (stageFiles root [reldir|2-preprocessed|] files)
    candidate f = extension f `elem` statementExtensions && Set.notMember f staged
    extension = map toLower . drop 1 . FilePath.takeExtension . toFilePath
    misplaced f = do
      rel <- stripProperPrefix root f
      let parts = FilePath.splitDirectories (toFilePath rel)
      let evidence = case filter isYear (take (length parts - 1) parts) of
            first : _ -> SingleYear (read first)
            [] -> UnknownYear
      return (Misplaced (FilePath.joinPath ("import" : parts)) (suggestPlacement parts) evidence False)

placementDestination :: Misplaced -> Maybe RelFile
placementDestination item = case mpYear item of
  SingleYear _ | null (missingParts item) -> parseRelFile (mpSuggestion item)
  _ -> Nothing

missingParts :: Misplaced -> [T.Text]
missingParts item =
  [T.pack part | part <- ["owner", "bank", "account"],
    ("<" ++ part ++ ">") `elem` FilePath.splitDirectories (FilePath.takeDirectory (mpSuggestion item))]

misplacedMoveCommand :: Shell -> [FilePath] -> AbsDir -> BaseDir -> Misplaced -> Maybe T.Text
misplacedMoveCommand _ _ _ _ item | mpDestinationExists item = Nothing
misplacedMoveCommand shell homes cwd base item = do
  source <- parseRelFile (mpPath item)
  target <- placementDestination item
  return (moveFileCommand shell homes cwd base source target)

placementDetails :: Shell -> [FilePath] -> AbsDir -> BaseDir -> Misplaced -> [T.Text]
placementDetails shell homes cwd base item =
  [T.pack (mpPath item) <> " → " <> T.pack (mpSuggestion item)]
    ++ ["Choose the " <> T.intercalate ", " parts <> "." | parts@(_ : _) <- [missingParts item]]
    ++ case mpYear item of
      SpanningYears years ->
        ["This statement contains transactions from " <> T.intercalate ", " (map (T.pack . show) years)
          <> ". Split it into one file per year."]
      UnknownYear -> ["Couldn't tell the year from the file. Use the year of its transactions."]
      SingleYear _ | mpDestinationExists item ->
        ["The destination already exists. Compare the two paths above before moving the statement."]
      SingleYear _ -> ["Move it with: " <> cmd | Just cmd <- [misplacedMoveCommand shell homes cwd base item]]

plural :: Int -> T.Text -> T.Text
plural 1 noun = "1 " <> noun
plural n noun = T.pack (show n) <> " " <> noun <> "s"

firstStatementStep :: Integer -> Shell -> [FilePath] -> AbsDir -> BaseDir -> Bool -> NextStep
firstStatementStep year shell homes cwd base examplePresent =
  NextStep
    "Put your first statement in import/<owner>/<bank>/<account>/1-in/<year>/"
    Nothing
    ( [ "owner: whose money it is, for example " <> ownerName <> ", a partner or a business",
        "bank: the institution, for example mybank",
        "account: one account at that bank, for example checking"
      ]
        ++ ["Change the names, then run: " <> createDirectoryCommand shell homes cwd
              (base </> [reldir|import|] </> owner </> [reldir|mybank/checking/1-in|] </> folder)
            | Just folder <- [parseRelDir (show year)]]
    )
    (Just (docURL InputFiles))
  where
    owner = if examplePresent then [reldir|personal|] else [reldir|me|]
    ownerName = T.pack (FilePath.dropTrailingPathSeparator (toFilePath owner))

-- | The single most important next step. The first matching rule wins.
-- Paths under one of @homes@ show as @~@, and as @"$HOME/…"@ in commands.
nextStep :: Integer -> Shell -> [FilePath] -> AbsDir -> LedgerStatus -> NextStep
nextStep year shell homes cwd s
  | Nothing <- lsHledger s =
      NextStep "Install hledger, or pass its location with --hledger-path" Nothing [] (Just "https://hledger.org/install.html")
  | statements == 0 && null (lsMisplaced s) && not (lsHasManualWork s) && not (lsNeedsReconciliation s) =
      firstStatement
  | not (null (lsMisplaced s)) =
      NextStep
        ("Move " <> plural (length (lsMisplaced s)) "file" <> " to where hledger-flow looks")
        Nothing
        (concatMap (placementDetails shell homes cwd (lsBaseDir s)) (lsMisplaced s))
        (Just (docURL (if any spansYears (lsMisplaced s) then StatementYearFolders else InputFiles)))
  | a : _ <- filter needsRules (lsAccounts s),
    source : other <- asOtherRules a =
      case other of
        [] | not (asRulesTargetOccupied a), Just commandLine <- renameRules a source ->
          NextStep ("Rename " <> T.pack source <> " to " <> T.pack (relativeCandidate a))
            (Just commandLine) ["Files it includes can keep their names."] (Just (docURL RulesFiles))
        _ ->
          NextStep ("Choose the rules to save as " <> T.pack (relativeCandidate a)) Nothing
            (map T.pack (source : other) ++
              ["The target already exists. Check it before making changes." | asRulesTargetOccupied a] ++
              ["Rename the rules for this account. Files it includes can keep their names."])
            (Just (docURL RulesFiles))
  | a : _ <- filter needsRules (lsAccounts s) =
      NextStep
        ("Create " <> T.pack (relativeCandidate a) <> " (rules for " <> asAccount a <> ")")
        (Just (commandArgs "rules" [asAccount a]))
        ["It creates that file from the statement\'s columns, for you to edit."]
        (Just (docURL RulesFiles))
  | toImport > 0 =
      withConfigCheck ImportConfig s $
        NextStep ("Import " <> plural toImport "new or changed statement") (Just importCommand) [] Nothing
  | Nothing <- lsAggregateJournal s, not (lsAllYearsExists s), not missingConfigured =
      withConfigCheck ImportConfig s $
        NextStep "Run import to create all-years.journal" (Just importCommand) [] Nothing
  | lsNeedsReconciliation s =
      withConfigCheck ImportConfig s $
        NextStep "Run import to reconcile generated files and journal includes" (Just importCommand) [] Nothing
  | lsNeedsPrices s =
      withConfigCheck ImportConfig s $
        NextStep
          "Add market prices for your commodities"
          Nothing
          ["Save them in prices/<year>/prices.journal, then run " <> importCommand <> "."]
          (Just (docURL PriceFiles))
  | Just lf <- lsLedgerFile s, not (lfExists lf) =
      NextStep
        "Point LEDGER_FILE at this ledger so hledger finds it"
        (Just (setLedgerFileCommand shell homes s))
        [ledgerFileProblem lf]
        (Just (docURL SetLedgerFile))
  | otherwise = case lsReview s of
      Nothing -> withConfigCheck ReportConfig s $
        NextStep "Generate reports, or explore with hledger" (Just (command "report"))
          ["Or try: " <> hint | Nothing <- [ledgerFileNote shell homes s], Just hint <- [balanceHint shell homes s]] Nothing
      Just review
        | lsUntouchedExample s, null (reviewItems review) -> firstStatement
            { nsDetail = nsDetail firstStatement ++
              ["The example is safe to delete: remove import/me and reports/me. Then run "
                <> importCommand <> " and " <> command "report" <> "."]
            }
        | otherwise -> reviewStep importCommand firstStatement review
  where
    commandArgs = flowCommand shell homes cwd (lsBaseDir s) (lsHledgerConf s)
    command subcommand = commandArgs subcommand []
    spansYears item = case mpYear item of SpanningYears _ -> True; _ -> False
    missingConfigured = maybe False (not . lfExists) (lsLedgerFile s)
    firstStatement = firstStatementStep year shell homes cwd (lsBaseDir s) (lsExampleAccountPresent s)
    importCommand = command "import"
    statements = sum (map asStatements (lsAccounts s))
    toImport = sum [asJournalsMissing a + asJournalsOutdated a | a <- lsAccounts s]
    needsRules a = asWithoutRules a > 0 && not (asConstruct a)
    relativeCandidate a = fromMaybe "a rules file" (case asRulesCandidates a of
      target : _ -> Just target
      [] -> Nothing)
    renameRules a source = do
      old <- parseRelFile source
      new <- parseRelFile (relativeCandidate a)
      return (moveFileCommand shell homes cwd (lsBaseDir s) old new)

reviewStep :: T.Text -> NextStep -> Review -> NextStep
reviewStep importCommand firstStatement review
  | not (null unknowns) =
      NextStep (maybe "Review unknown transactions" (\n -> "Categorise " <> plural n "unknown transaction")
        (reviewCount (reviewUnknowns review))) Nothing
        (map (\path -> "Review " <> T.pack path <> ".") (take 1 unknowns) ++
          ["Add if rules to the relevant account's rules file, then run: " <> importCommand] ++
          ["Or start with transfers: " <> T.pack path | path : _ <- [transfers]])
        (Just (docURL ReviewUnknownTransactions))
  | not (null transfers) =
      NextStep ("Check transfers in " <> plural (length years) "year") Nothing
        (["Review " <> T.pack path <> "." | path : _ <- [transfers]] ++
          ["A transfer may need review when the matching statement hasn't arrived yet."])
        (Just (docURL ReviewTransfers))
  | otherwise = firstStatement {nsSummary = "Everything is imported and reported"}
  where
    unknowns = reviewFiles (reviewUnknowns review)
    transfers = reviewFiles (reviewTransfers review)
    years = nub [itemYear item | item <- reviewItems review, itemKind item == TransferBalances]

flowCommand :: Shell -> [FilePath] -> AbsDir -> BaseDir -> Maybe HledgerConf -> T.Text -> [T.Text] -> T.Text
flowCommand shell homes cwd base conf subcommand arguments =
  T.unwords (["hledger-flow"] ++ config ++ [subcommand] ++ map (shellArgument shell) arguments ++ directory)
  where
    config = case conf of
      Just (GivenConf file) | subcommand `elem` ["import", "report", "rules"] ->
        ["--hledger-conf", shellPath shell homes file]
      _ -> []
    directory = [shellPath shell homes (FilePath.dropTrailingPathSeparator (toFilePath base)) | cwd /= base]

shellPath :: Shell -> [FilePath] -> FilePath -> T.Text
shellPath shell homes file =
  fromMaybe (shellArgument shell (T.pack file)) (homeShellPath shell homes file)

setLedgerFileCommand :: Shell -> [FilePath] -> LedgerStatus -> T.Text
setLedgerFileCommand shell homes s = case shell of
  Posix -> "export LEDGER_FILE=" <> setting posixLiteral
  PowerShell -> "$env:LEDGER_FILE = " <> setting powerShellLiteral
  where
    path = toFilePath (lsBaseDir s </> [relfile|all-years.journal|])
    setting literal = fromMaybe (literal (T.pack path)) (homeShellPath shell homes path)

balanceHint :: Shell -> [FilePath] -> LedgerStatus -> Maybe T.Text
balanceHint shell homes s = do
  case firstConfigConflict BalanceConfig (lsConfigConflicts s) of
    Just _ -> Nothing
    Nothing -> Just ()
  info <- lsHledger s
  journal <- lsAggregateJournal s
  let config = case lsHledgerConf s of
        Just (GivenConf configPath) -> ["--conf", shellPath shell homes configPath]
        _ -> []
      path = T.unpack (hledgerFileArgument info (toFilePath journal))
  return (T.unwords (["hledger"] ++ config ++ ["-f", shellPath shell homes path, "balance"]))

ledgerFileNote :: Shell -> [FilePath] -> LedgerStatus -> Maybe T.Text
ledgerFileNote shell homes s = do
  _ <- lsAggregateJournal s
  message <- case lsLedgerFile s of
    Nothing -> Just "LEDGER_FILE isn't set, so hledger won't find this ledger by default."
    Just file | lfExists file && lfCoverage file /= CoversLedger -> Just (ledgerFileProblem file)
    _ -> Nothing
  return (T.intercalate "\n" ([message, "hledger-flow report uses all-years.journal.", "For hledger, run: " <> setLedgerFileCommand shell homes s]
      ++ ["Or try: " <> hint | Just hint <- [balanceHint shell homes s]]))

withConfigCheck :: ConfigUse -> LedgerStatus -> NextStep -> NextStep
withConfigCheck use status next = case firstConfigConflict use (lsConfigConflicts status) of
  Just problem -> NextStep "Fix hledger.conf before continuing" Nothing
    (T.lines (renderConfigConflict problem)) (Just (docURL StatusCommand))
  Nothing -> next
