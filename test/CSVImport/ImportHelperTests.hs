{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CSVImport.ImportHelperTests where

import qualified Data.Text as T
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.Import.ImportHelpers (extractImportDirs, groupIncludesUpTo)
import Hledger.Flow.Import.Types (ImportDirs (..), InputFileBundle)
import Path
import Test.HUnit hiding (Path)
import TestHelpers

testExtractImportDirs :: Test
testExtractImportDirs = TestList
  [ TestCase $ check [reldir|.|],
    TestCase $ check (testRoot </> [reldir|finance|])
  ]
  where
    check :: Path b Dir -> Assertion
    check base = do
      let root = base </> [reldir|import|]
          owner = root </> [reldir|john|]
          bank = owner </> [reldir|mybank|]
          account = bank </> [reldir|checking|]
          state = account </> [reldir|1-in|]
          year = state </> [reldir|2019|]
          statement = year </> [relfile|2019-01-01.csv|]
      assertEqual "statement directories"
        (Right (ImportDirs root owner bank account state year)) (extractImportDirs statement)
      checkMalformed (account </> [relfile|2019-01-01.csv|])
      checkMalformed (year </> [relfile|extra/2019-01-01.csv|])
      checkMalformed (base </> [relfile|outside/john/mybank/checking/1-in/2019/statement.csv|])

    checkMalformed file =
      case extractImportDirs file of
        Right _ -> assertFailure "expected malformed import layout to be rejected"
        Left err -> assertEqual "malformed layout diagnostic"
          ("I couldn't find the right number of directories between \"import\" and the input file:\n"
            <> T.pack (toFilePath file)
            <> "\n\nhledger-flow expects to find input files in this structure:\n"
            <> "import/owner/bank/account/filestate/year/trxfile\n\n"
            <> "Have a look at the documentation for a detailed explanation:\n"
            <> docURL InputFiles) err

testGroupIncludesUpTo :: Test
testGroupIncludesUpTo = TestCase $ do
  let first = [relfile|import/jane/bank/savings/3-journal/2019/first.journal|]
      second = [relfile|import/jane/bank/savings/3-journal/2020/second.journal|]
      other = [relfile|import/john/bank/checking/3-journal/2019/other.journal|]
      expected =
        [ ([relfile|import/jane/bank/savings/2019-include.journal|], [first]),
          ([relfile|import/jane/bank/savings/2020-include.journal|], [second]),
          ([relfile|import/jane/bank/2019-include.journal|], [[relfile|import/jane/bank/savings/2019-include.journal|]]),
          ([relfile|import/jane/bank/2020-include.journal|], [[relfile|import/jane/bank/savings/2020-include.journal|]]),
          ([relfile|import/jane/2019-include.journal|], [[relfile|import/jane/bank/2019-include.journal|]]),
          ([relfile|import/jane/2020-include.journal|], [[relfile|import/jane/bank/2020-include.journal|]])
        ] :: InputFileBundle
  assertEqual "group both years up to the owner, excluding siblings" expected
    (groupIncludesUpTo [reldir|import/jane|] [second, other, first])
  assertEqual "empty input has no includes" [] (groupIncludesUpTo [reldir|import|] [])

tests :: Test
tests = TestList [testExtractImportDirs, testGroupIncludesUpTo]
