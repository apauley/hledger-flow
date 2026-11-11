{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Status.Integration (tests) where

import Data.Maybe (isNothing)
import Data.Time.Clock (addUTCTime)
import Hledger.Flow.Common (findHledger)
import Hledger.Flow.PathHelpers (AbsFile)
import Hledger.Flow.Status
import Hledger.Flow.Status.Gather (classifyDir, gatherStatus)
import Path
import Path.IO
import System.Environment (setEnv, unsetEnv)
import System.FilePath (joinPath)
import Test.HUnit hiding (Path)
import TestHelpers (defaultHlInfo, withTestDir)

touch :: AbsFile -> IO ()
touch file = do
  createDirIfMissing True (parent file)
  writeFile (toFilePath file) "Date,Desc,Amount\n"

testClassify :: Test
testClassify = TestCase $ withTestDir $ \tmp -> do
  let ledger = tmp </> [reldir|ledger|]
      empty = tmp </> [reldir|empty|]
      other = tmp </> [reldir|other|]
  mapM_ (createDirIfMissing True) [ledger </> [reldir|import/me|], empty, other]
  writeFile (toFilePath (empty </> [relfile|.DS_Store|])) ""
  writeFile (toFilePath (other </> [relfile|notes.txt|])) ""
  (@?= InLedger ledger) =<< classifyDir (ledger </> [reldir|import/me|])
  (@?= EmptyDir) =<< classifyDir empty
  (@?= Elsewhere) =<< classifyDir other

testGather :: Test
testGather = TestCase $ withTestDir $ \base -> do
  let imp = base </> [reldir|import|]
      cardCsv = imp </> [relfile|other/bank/card/1-in/2026/c.csv|]
      cardJournal = imp </> [relfile|other/bank/card/3-journal/2026/c.journal|]
  mapM_ touch
    [ imp </> [relfile|me/mybank/checking/1-in/2025/a.csv|],
      imp </> [relfile|me/mybank/checking/1-in/2026/b.csv|],
      cardCsv,
      imp </> [relfile|bank.rules|],
      cardJournal,
      imp </> [relfile|me/mybank/jan.csv|],
      imp </> [relfile|me/_manual_/2026/pre-import.journal|]
    ]
  now <- getModificationTime cardCsv
  setModificationTime cardJournal (addUTCTime (-3600) now)
  unsetEnv "LEDGER_FILE"
  s <- gatherStatus (Just defaultHlInfo) base
  map asAccount (lsAccounts s) @?= ["me/mybank/checking", "other/bank/card"]
  case lsAccounts s of
    [checking, card] -> do
      (asStatements checking, asYears checking, asRulesFile checking, asWithoutRules checking, asJournalsMissing checking)
        @?= (2, [2025, 2026], Nothing, 2, 2)
      asRulesCandidates checking @?= [joinPath ["import", "me", "mybank", "checking", "mybank-checking.rules"], joinPath ["import", "mybank.rules"]]
      (asRulesFile card, asWithoutRules card, asJournalsMissing card, asJournalsOutdated card)
        @?= (Just (joinPath ["import", "bank.rules"]), 0, 0, 1)
    _ -> assertFailure "expected two accounts"
  map mpPath (lsMisplaced s) @?= [joinPath ["import", "me", "mybank", "jan.csv"]]
  lsManualJournals s @?= 1
  lsLedgerFile s @?= Nothing
  setEnv "LEDGER_FILE" (toFilePath (base </> [relfile|missing.journal|]))
  s2 <- gatherStatus Nothing base
  unsetEnv "LEDGER_FILE"
  fmap lfExists (lsLedgerFile s2) @?= Just False

testFindHledgerMissing :: Test
testFindHledgerMissing = TestCase $ do
  r <- findHledger (Just "/nonexistent/hledger")
  assertBool "Nothing for a missing path" (isNothing r)

tests :: Test
tests = TestList [testClassify, testGather, testFindHledgerMissing]
