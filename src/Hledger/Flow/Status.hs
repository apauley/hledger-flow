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
    hledgerVersionNumber,
    plural,
  )
where

import Data.Char (isDigit, toLower)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hledger.Flow.Common (Shell (..), posixLiteral, powerShellLiteral, shellArgument)
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.Inventory (stageFiles)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile)
import Hledger.Flow.Types (BaseDir, HledgerInfo)
import Path (reldir, relfile, stripProperPrefix, toFilePath, (</>))
import qualified System.FilePath as FilePath

data Situation = InLedger BaseDir | EmptyDir | Elsewhere deriving (Eq, Show)

data LedgerFile = LedgerFile {lfValue :: FilePath, lfExists :: Bool} deriving (Eq, Show)

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
    lsAccounts :: [AccountStatus],
    lsMisplaced :: [Misplaced],
    lsManualJournals :: Int,
    lsAllYearsExists :: Bool,
    lsNeedsReconciliation :: Bool
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
nextStep :: Integer -> Shell -> AbsDir -> LedgerStatus -> NextStep
nextStep year shell cwd s
  | Nothing <- lsHledger s =
      NextStep "Install hledger, or pass its location with --hledger-path" Nothing [] (Just "https://hledger.org/install.html")
  | statements == 0 && null (lsMisplaced s) && lsManualJournals s == 0 && not (lsNeedsReconciliation s) =
      NextStep
        "Put your first statement in import/<owner>/<bank>/<account>/1-in/<year>/"
        Nothing
        [ "For example: import/me/mybank/checking/1-in/" <> T.pack (show year) <> "/statement.csv",
          "Then check with: " <> command "status"
        ]
        (Just (docURL "input-files"))
  | not (null (lsMisplaced s)) =
      NextStep
        ("Move " <> plural (length (lsMisplaced s)) "file" <> " to where hledger-flow looks")
        Nothing
        [T.pack (mpPath m) <> " → " <> T.pack (mpSuggestion m) | m <- lsMisplaced s]
        (Just (docURL "input-files"))
  | a : _ <- filter needsRules (lsAccounts s) =
      NextStep
        ("Create " <> T.pack (firstCandidate a) <> " (rules for " <> asAccount a <> ")")
        (Just (command "status" <> " --rules-template " <> shellArgument shell (asAccount a)))
        ["Save its output as that file, then edit it."]
        (Just (docURL "rules-files"))
  | toImport > 0 =
      NextStep ("Import " <> plural toImport "new or changed statement") (Just importCommand) [] Nothing
  | not (lsAllYearsExists s) =
      NextStep "Run import to create all-years.journal" (Just importCommand) [] Nothing
  | lsNeedsReconciliation s =
      NextStep "Run import to reconcile generated files and journal includes" (Just importCommand) [] Nothing
  | maybe True (not . lfExists) (lsLedgerFile s) =
      NextStep
        "Point LEDGER_FILE at this ledger so hledger finds it"
        (Just exportLine)
        [ "LEDGER_FILE is set to " <> T.pack (lfValue lf) <> ", which doesn't exist."
          | Just lf <- [lsLedgerFile s]
        ]
        (Just "https://github.com/apauley/hledger-flow/blob/master/QUICKSTART.md#import-your-first-statement")
  | otherwise =
      NextStep "Generate reports, or explore with hledger" (Just (command "report")) ["Or try: hledger balance"] Nothing
  where
    command subcommand = "hledger-flow " <> subcommand <> baseArgument
    baseArgument
      | cwd == lsBaseDir s = ""
      | otherwise = " " <> shellArgument shell (T.pack (FilePath.dropTrailingPathSeparator (toFilePath (lsBaseDir s))))
    importCommand = command "import"
    statements = sum (map asStatements (lsAccounts s))
    toImport = sum [asJournalsMissing a + asJournalsOutdated a | a <- lsAccounts s]
    needsRules a = asWithoutRules a > 0 && not (asConstruct a)
    firstCandidate a = case asRulesCandidates a of
      c : _ | cwd /= lsBaseDir s -> toFilePath (lsBaseDir s) FilePath.</> c
            | otherwise -> c
      [] -> "a rules file"
    allYears = T.pack (toFilePath (lsBaseDir s </> [relfile|all-years.journal|]))
    exportLine = case shell of
      Posix -> "export LEDGER_FILE=" <> posixLiteral allYears
      PowerShell -> "$env:LEDGER_FILE = " <> powerShellLiteral allYears

-- | @"hledger 1.50.2, mac-aarch64"@ → @"1.50.2"@.
hledgerVersionNumber :: T.Text -> T.Text
hledgerVersionNumber txt = case dropWhile (== "hledger") (T.words txt) of
  v : _ -> T.dropWhileEnd (== ',') v
  [] -> ""
