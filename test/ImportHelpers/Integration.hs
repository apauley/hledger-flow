{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ImportHelpers.Integration (tests) where

import qualified Data.List as List
import Data.Either (isLeft)
import qualified Data.Text.IO as T
import Hledger.Flow.Import.ImportHelpers (withStagedOutput)
import Hledger.Flow.Import.Inventory
import TestHelpers (withTestDir, touchAll)
import Hledger.Flow.PathHelpers (AbsFile)
import Path
import Path.IO
import qualified System.FilePath
import System.IO.Error (tryIOError)
import Test.HUnit

touchFile :: AbsFile -> IO ()
touchFile f = do
  createDirIfMissing True (parent f)
  T.writeFile (toFilePath f) "x"

testInventory :: Test
testInventory = TestLabel "active inventory discovery and scope" $ TestCase $ withTestDir $ \base -> do
  let root = base </> [reldir|import|]
      account = root </> [reldir|owner/bank/checking|]
      inputs = [ "owner/bank/checking/1-in/2018/old.csv",
                 "owner/bank/checking/1-in/2019/march.bank.txt",
                 "owner/bank/checking/1-in/2020/new.csv",
                 "2-preprocessed/3-journal/1-in/1-in/2019/reserved.csv" ]
      others = [ "owner/bank/checking/2-preprocessed/2019/march.bank.csv",
                 "owner/bank/checking/3-journal/2019/march.bank.journal",
                 "owner/_manual_/2019/post-import.journal",
                 "owner/bank/checking/1-in/20a0/bad.csv",
                 "owner/bank/checking/1-in/20190/bad.csv",
                 "owner/bank/checking/1-in/2019/nested/bad.csv" ]
      hidden = [ "owner/bank/checking/1-in/2019/.hidden.csv",
                 ".hidden/bank/account/1-in/2019/bad.csv",
                 "owner/bank/checking/.trash/1-in/2019/bad.csv",
                 "owner/bank/checking/1-in/__pycache__/2019/bad.csv" ]
      absolute = (toFilePath root System.FilePath.</>)
  touchAll (map absolute (inputs ++ others ++ hidden))
  inventory <- findImportFiles root
  expected <- mapM (parseAbsFile . absolute) (inputs ++ others)
  assertEqual "exclude hidden files, hidden directories and Python cache" (List.sort expected) inventory
  sources <- mapM (parseAbsFile . absolute) inputs
  assertEqual "only direct children of a valid input year" (List.sort sources)
    (stageFiles root [reldir|1-in|] inventory)
  selected <- mapM (parseAbsFile . absolute) (take 2 (drop 1 inputs))
  assertEqual "account scope and inclusive start year" selected
    (filter (inImportScope account 2019) (stageFiles root [reldir|1-in|] inventory))
  let source = account </> [relfile|1-in/2019/march.bank.txt|]
  generatedOutput [reldir|2-preprocessed|] ".csv" source >>= assertEqual "keep earlier filename extensions"
    (account </> [relfile|2-preprocessed/2019/march.bank.csv|])
  generatedOutput [reldir|3-journal|] ".journal" source >>= assertEqual "journal destination"
    (account </> [relfile|3-journal/2019/march.bank.journal|])
  assertEqual "discover generated journals separately"
    [account </> [relfile|3-journal/2019/march.bank.journal|]]
    (stageFiles root [reldir|3-journal|] inventory)

testStagedOutput :: Test
testStagedOutput = TestCase $ withSystemTempDir "hlflow-output" $ \dir -> do
  let target = dir </> [relfile|march.bank.csv|]
      failAfterWriting staged = do
        assertEqual "hook output keeps its filename" (filename target) (filename staged)
        assertBool "hook output has a temporary parent" (parent staged /= dir)
        T.writeFile (toFilePath staged) "partial"
        ioError (userError "failed hook")
      checkFailure = do
        result <- tryIOError (withStagedOutput target failAfterWriting)
        assertBool "propagate hook failure" (isLeft result)
        (dirs, _) <- listDir dir
        assertEqual "clean up staging directories" [] dirs
  checkFailure
  doesFileExist target >>= assertEqual "failed first run publishes no file" False
  T.writeFile (toFilePath target) "previous"
  checkFailure
  T.readFile (toFilePath target) >>= assertEqual "preserve previous output" "previous"
  withStagedOutput target $ \staged -> T.writeFile (toFilePath staged) "complete"
  T.readFile (toFilePath target) >>= assertEqual "publish successful retry" "complete"

tests :: Test
tests = TestList [testInventory, testStagedOutput, testTrashLock]

testTrashLock :: Test
testTrashLock = TestCase $ withSystemTempDir "hlflow-trash" $ \root -> do
  let source = root </> [relfile|import/owner/bank/account/3-journal/2020/march.journal|]
      lock = root </> [reldir|import/owner/bank/account/.trash/.move-lock|]
      target = root </> [relfile|import/owner/bank/account/.trash/2020/3-journal/march.journal|]
  touchFile source
  createDirIfMissing True lock
  result <- tryIOError (moveToTrash source)
  assertBool "A concurrent trash move must not race for a destination" (isLeft result)
  T.readFile (toFilePath source) >>= assertEqual "Busy trash leaves source intact" "x"
  doesFileExist target >>= assertBool "Busy trash writes no destination" . not
  removeDir lock
  actual <- moveToTrash source
  assertEqual "Retry after the other move completes succeeds" target actual
