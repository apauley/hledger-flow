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
    nextStep,
    flowCommand,
    ledgerFileNote,
    balanceHint,
    withConfigCheck,
    hledgerVersionNumber,
    plural,
  )
where

import Data.Char (isDigit, toLower)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hledger.Flow.Common (Shell (..), hledgerFileArgument, hledgerVersionNumber, homeShellPath, posixLiteral, powerShellLiteral, shellArgument, tildePath)
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.HledgerConfig (ConfigConflict, ConfigUse (..), firstConfigConflict, renderConfigConflict)
import Hledger.Flow.Import.Inventory (stageFiles)
import Hledger.Flow.LedgerSelection (Coverage (..), LedgerFile (..), ledgerFileProblem)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile)
import Hledger.Flow.Types (BaseDir, HledgerConf (..), HledgerInfo)
import Path (reldir, relfile, stripProperPrefix, toFilePath, (</>))
import qualified System.FilePath as FilePath

data Situation = InLedger BaseDir | EmptyDir | Elsewhere deriving (Eq, Show)

data AccountStatus = AccountStatus
  { asAccount :: T.Text,
    asStatements :: Int,
    asYears :: [Integer],
    asRulesFile :: Maybe FilePath,
    asRulesCandidates :: [FilePath],
    asWithoutRules :: Int,
    asPreprocess :: Bool,
    asConstruct :: Bool,
    asJournalsMissing :: Int,
    asJournalsOutdated :: Int
  }
  deriving (Eq, Show)

-- | Both paths are relative to the base directory and start with @import@.
data Misplaced = Misplaced {mpPath :: FilePath, mpSuggestion :: FilePath} deriving (Eq, Show)

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
    lsAllYearsExists :: Bool,
    lsNeedsReconciliation :: Bool,
    lsNeedsPrices :: Bool
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
      return (Misplaced (FilePath.joinPath ("import" : parts)) (suggestPlacement parts))

plural :: Int -> T.Text -> T.Text
plural 1 noun = "1 " <> noun
plural n noun = T.pack (show n) <> " " <> noun <> "s"

-- | The single most important next step. The first matching rule wins.
-- Paths under one of @homes@ show as @~@, and as @"$HOME/…"@ in commands.
nextStep :: Integer -> Shell -> [FilePath] -> AbsDir -> LedgerStatus -> NextStep
nextStep year shell homes cwd s
  | Nothing <- lsHledger s =
      NextStep "Install hledger, or pass its location with --hledger-path" Nothing [] (Just "https://hledger.org/install.html")
  | statements == 0 && null (lsMisplaced s) && lsManualJournals s == 0 && not (lsNeedsReconciliation s) =
      NextStep
        "Put your first statement in import/<owner>/<bank>/<account>/1-in/<year>/"
        Nothing
        [ "For example: import/me/mybank/checking/1-in/" <> T.pack (show year) <> "/statement.csv",
          "Then check with: " <> command "status"
        ]
        (Just (docURL InputFiles))
  | not (null (lsMisplaced s)) =
      NextStep
        ("Move " <> plural (length (lsMisplaced s)) "file" <> " to where hledger-flow looks")
        Nothing
        [T.pack (mpPath m) <> " → " <> T.pack (mpSuggestion m) | m <- lsMisplaced s]
        (Just (docURL InputFiles))
  | a : _ <- filter needsRules (lsAccounts s) =
      NextStep
        ("Create " <> T.pack (tildePath homes (firstCandidate a)) <> " (rules for " <> asAccount a <> ")")
        (Just (commandArgs "status" ["--rules-template", asAccount a]))
        ["Save its output as that file, then edit it."]
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
  | otherwise =
      withConfigCheck ReportConfig s $
        NextStep "Generate reports, or explore with hledger" (Just (command "report"))
          ["Or try: " <> hint | Nothing <- [ledgerFileNote shell homes s], Just hint <- [balanceHint shell homes s]] Nothing
  where
    commandArgs = flowCommand shell homes cwd (lsBaseDir s) (lsHledgerConf s)
    command subcommand = commandArgs subcommand []
    missingConfigured = maybe False (not . lfExists) (lsLedgerFile s)
    importCommand = command "import"
    statements = sum (map asStatements (lsAccounts s))
    toImport = sum [asJournalsMissing a + asJournalsOutdated a | a <- lsAccounts s]
    needsRules a = asWithoutRules a > 0 && not (asConstruct a)
    firstCandidate a = case asRulesCandidates a of
      c : _ | cwd /= lsBaseDir s -> toFilePath (lsBaseDir s) FilePath.</> c
            | otherwise -> c
      [] -> "a rules file"
flowCommand :: Shell -> [FilePath] -> AbsDir -> BaseDir -> Maybe HledgerConf -> T.Text -> [T.Text] -> T.Text
flowCommand shell homes cwd base conf subcommand arguments =
  T.unwords (["hledger-flow"] ++ config ++ [subcommand] ++ map (shellArgument shell) arguments ++ directory)
  where
    config = case conf of
      Just (GivenConf file) | subcommand `elem` ["import", "report"] ->
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
