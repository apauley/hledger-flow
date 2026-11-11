{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Status.Unit (tests, blank, acct) where

import qualified Data.Text as T
import Hledger.Flow.Status
import Path
import qualified System.FilePath as FilePath
import Test.HUnit
import TestHelpers (defaultHlInfo, testRoot)

blank :: LedgerStatus
blank =
  LedgerStatus
    { lsBaseDir = testRoot </> [reldir|fin|],
      lsHledger = Just defaultHlInfo,
      lsLedgerFile = Just (LedgerFile "/x" True),
      lsAccounts = [],
      lsMisplaced = [],
      lsManualJournals = 0
    }

acct :: AccountStatus
acct =
  AccountStatus
    { asAccount = "me/mybank/checking",
      asStatements = 1,
      asYears = [2026],
      asRulesFile = Just "import/me/mybank/checking/mybank-checking.rules",
      asRulesCandidates = ["import/me/mybank/checking/mybank-checking.rules", "import/mybank.rules"],
      asWithoutRules = 0,
      asPreprocess = False,
      asConstruct = False,
      asJournalsMissing = 0,
      asJournalsOutdated = 0
    }

native :: FilePath -> FilePath
native = FilePath.joinPath . FilePath.splitDirectories

testSuggest :: Test
testSuggest =
  TestList
    [ suggestPlacement ["me", "mybank", "jan.csv"] ~?= native "import/me/mybank/<account>/1-in/<year>/jan.csv",
      suggestPlacement ["jan.csv"] ~?= native "import/<owner>/<bank>/<account>/1-in/<year>/jan.csv",
      suggestPlacement ["me", "mybank", "checking", "2026", "jan.csv"] ~?= native "import/me/mybank/checking/1-in/2026/jan.csv",
      suggestPlacement ["me", "mybank", "checking", "1-in", "jan.csv"] ~?= native "import/me/mybank/checking/1-in/<year>/jan.csv"
    ]

testMisplacedUpperCase :: Test
testMisplacedUpperCase = TestCase $ do
  let root = testRoot </> [reldir|fin/import|]
      files =
        [ root </> [relfile|me/mybank/JAN.CSV|],
          root </> [relfile|me/mybank/checking/1-in/2026/feb.csv|],
          root </> [relfile|me/mybank/notes.md|]
        ]
  map mpPath (misplacedFiles root files) @?= [native "import/me/mybank/JAN.CSV"]

summaryOf :: LedgerStatus -> T.Text
summaryOf = nsSummary . nextStep 2026 False

commandOf :: LedgerStatus -> Maybe T.Text
commandOf = nsCommand . nextStep 2026 False

allYears :: T.Text
allYears = T.pack (toFilePath (lsBaseDir blank </> [relfile|all-years.journal|]))

testRules :: Test
testRules =
  TestList
    [ "1 hledger missing" ~: summaryOf blank {lsHledger = Nothing} ~?= "Install hledger, or pass its location with --hledger-path",
      "2 empty ledger" ~: summaryOf blank ~?= "Put your first statement in import/<owner>/<bank>/<account>/1-in/<year>/",
      "2 example uses current year" ~: nsDetail (nextStep 2026 False blank) ~?= ["For example: import/me/mybank/checking/1-in/2026/statement.csv", "Then check with: hledger-flow status"],
      "2 skipped for manual-only ledger" ~: summaryOf blank {lsManualJournals = 1} ~?= "Generate reports, or explore with hledger",
      "3 misplaced" ~: summaryOf blank {lsMisplaced = [Misplaced "import/a.csv" "x"]} ~?= "Move 1 file to where hledger-flow looks",
      "3 misplaced plural" ~: summaryOf blank {lsMisplaced = [Misplaced "import/a.csv" "x", Misplaced "import/b.csv" "y"]} ~?= "Move 2 files to where hledger-flow looks",
      "3 detail" ~: nsDetail (nextStep 2026 False blank {lsMisplaced = [Misplaced "import/a.csv" "x"]}) ~?= ["import/a.csv → x"],
      "4 rules" ~: commandOf blank {lsAccounts = [acct {asWithoutRules = 1, asRulesFile = Nothing}]} ~?= Just "hledger-flow status --rules-template me/mybank/checking",
      "4 summary" ~: summaryOf blank {lsAccounts = [acct {asWithoutRules = 1, asRulesFile = Nothing}]} ~?= "Create import/me/mybank/checking/mybank-checking.rules (rules for me/mybank/checking)",
      "4 skipped with construct" ~: commandOf blank {lsAccounts = [acct {asWithoutRules = 1, asConstruct = True}]} ~?= Just "hledger-flow report",
      "5 import" ~: summaryOf blank {lsAccounts = [acct {asJournalsMissing = 1, asJournalsOutdated = 1}]} ~?= "Import 2 new or changed statements",
      "5 import singular" ~: summaryOf blank {lsAccounts = [acct {asJournalsMissing = 1}]} ~?= "Import 1 new or changed statement",
      "6 posix" ~: commandOf blank {lsAccounts = [acct], lsLedgerFile = Nothing} ~?= Just ("export LEDGER_FILE=\"" <> allYears <> "\""),
      "6 windows" ~: nsCommand (nextStep 2026 True blank {lsAccounts = [acct], lsLedgerFile = Nothing}) ~?= Just ("$env:LEDGER_FILE = \"" <> allYears <> "\""),
      "6 set but missing" ~: nsDetail (nextStep 2026 False blank {lsAccounts = [acct], lsLedgerFile = Just (LedgerFile "/nope.journal" False)}) ~?= ["LEDGER_FILE is set to /nope.journal, which doesn't exist."],
      "7 done" ~: commandOf blank {lsAccounts = [acct]} ~?= Just "hledger-flow report"
    ]

testVersion :: Test
testVersion =
  TestList
    [ hledgerVersionNumber "hledger 1.50.2, mac-aarch64" ~?= "1.50.2",
      hledgerVersionNumber "1.50" ~?= "1.50"
    ]

tests :: Test
tests = TestList [testSuggest, testMisplacedUpperCase, testRules, testVersion]
