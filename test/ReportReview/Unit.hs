{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ReportReview.Unit (tests) where

import qualified Data.Text as T
import Hledger.Flow.InputSnapshot (inputsUnchanged, snapshotInputs)
import Hledger.Flow.ReportReview
import Path
import qualified Path.IO as PathIO
import Test.HUnit hiding (Path)
import TestHelpers (withTestDir)

tests :: Test
tests = "report review" ~: TestList
  [ countUnknownTransactions "" ~?= Just 0,
    countUnknownTransactions "2016-01-02\n    expenses:unknown  2\n" ~?= Just 1,
    countUnknownTransactions (T.unlines ["2016-01-01 Shop", "    expenses:unknown  5", "    assets:bank"]) ~?= Just 1,
    countUnknownTransactions (T.unlines ["2016-01-01 Shop", "    expenses:unknown  5", "2017-01-01 Again", "    expenses:unknown  7"]) ~?= Just 2,
    countUnknownTransactions "\"txnidx\",\"date\",\"account\"\n\"1\",\"2016-01-01\",\"assets:bank\"" ~?= Nothing,
    reviewHasData UnknownTransactions "\"txnidx\",\"date\",\"account\"\n" ~?= False,
    countTransferAccounts (T.unlines ["Balance changes in 2016:", " || 2016Q1 2016Q2", "==++==", "assets:transfer || 5 -5"]) ~?= Just 1,
    countTransferAccounts (T.unlines ["Balance changes in 2016:", "│ ║ 2016Q1 │", "╞═╬═══╡", "│ assets:transfer ║ 5 │", "└─╨───┘"]) ~?= Just 1,
    countTransferAccounts (T.unlines ["Balance changes in 2016:", " || 2016Q1", "==++=="]) ~?= Just 0,
    reviewHasData TransferBalances "\"account\",\"2016Q1\"\n\"assets:transfer\",\"5\"\n" ~?= True,
    reviewHasData TransferBalances "\"account\",\"2016Q1\"\n" ~?= False,
    "a timestamp equal to the marker is stale" ~: (withTestDir $ \base -> do
      let file = base </> [relfile|input.journal|]
      writeFile (toFilePath file) "; journal\n"
      recorded <- snapshotInputs [file]
      marker <- PathIO.getModificationTime file
      inputsUnchanged recorded marker >>= (@?= False))
  ]
