{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.Integration (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically, newTChanIO, tryReadTChan)
import qualified Data.List as List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime(..), addUTCTime, secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Hledger.Flow.Common
import Hledger.Flow.PathHelpers (AbsDir, TurtlePath)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types (LogMessage (StdErr))
import qualified Path
import qualified Path.IO as PathIO
import System.Directory (doesFileExist)
import System.Timeout (timeout)
import Test.HUnit
import TestHelpers (defaultOpts)
import TestHelpersTurtle
import Turtle

testHiddenFiles :: Test
testHiddenFiles =
  TestCase
    ( sh
        ( do
            let tmpbase = "." </> "test" </> "tmp"
            mktree tmpbase
            tmpdir <- using (mktempdir tmpbase "hlflowtest")
            let tmpJournals = map (tmpdir </>) journalFiles :: [TurtlePath]
            let tmpExtras = map (tmpdir </>) extraFiles :: [TurtlePath]
            let tmpHidden = map (tmpdir </>) hiddenFiles :: [TurtlePath]
            let onDisk = List.sort $ tmpJournals ++ tmpExtras ++ tmpHidden
            touchAll onDisk
            filtered <- fmap List.sort $ shellToList $ onlyFiles $ select onDisk
            let expected = List.sort $ tmpExtras ++ tmpJournals
            liftIO $ assertEqual "Hidden files should be excluded" expected filtered
        )
    )

testFilterPaths :: Test
testFilterPaths =
  TestCase
    ( sh
        ( do
            let tmpbase = "." </> "test" </> "tmp"
            mktree tmpbase
            tmpdir <- using (mktempdir tmpbase "hlflowtest")
            let tmpJournals = map (tmpdir </>) journalFiles :: [TurtlePath]
            let tmpExtras = map (tmpdir </>) extraFiles :: [TurtlePath]
            let tmpHidden = map (tmpdir </>) hiddenFiles :: [TurtlePath]
            let onDisk = List.sort $ tmpJournals ++ tmpExtras ++ tmpHidden
            touchAll onDisk

            let nonExistant = map (tmpdir </>) ["where", "is", "my", "mind"]
            let toFilter = nonExistant ++ onDisk
            filtered <- liftIO $ filterPaths doesFileExist toFilter
            liftIO $ assertEqual "Keep existing include files in reverse candidate order" (reverse onDisk) filtered
        )
    )

testFirstExistingFile :: Test
testFirstExistingFile = TestCase $ withTestDir $ \base -> do
  let missing = base Path.</> [Path.relfile|missing.rules|]
      first = base Path.</> [Path.relfile|first.rules|]
      second = base Path.</> [Path.relfile|second.rules|]
  T.writeFile (Path.toFilePath first) ""
  T.writeFile (Path.toFilePath second) ""
  found <- firstExistingFile [missing, first, second]
  assertEqual "first existing file in candidate order" (Just first) found
  absent <- firstExistingFile [missing]
  assertEqual "no existing files" Nothing absent

withTestDir :: (AbsDir -> IO ()) -> IO ()
withTestDir action = do
  cwd <- PathIO.getCurrentDir
  let tmpbase = cwd Path.</> [Path.reldir|test/tmp|]
  PathIO.createDirIfMissing True tmpbase
  PathIO.withTempDir tmpbase "hlflowtest" action

testVerboseTestFile :: Test
testVerboseTestFile = TestCase $ withTestDir $ \base -> do
  let opts = (defaultOpts base) {RT.verbose = True}
      file = base Path.</> [Path.relfile|statement.csv|]
  ch <- newTChanIO
  let check expected message = do
        exists <- verboseTestFile opts ch file
        assertEqual "file existence" expected exists
        entry <- atomically (tryReadTChan ch)
        case entry of
          Just (StdErr txt) -> do
            let (timestamp, body) = T.breakOn "\t" txt
            assertBool "verbose log starts with a timestamp" (not (T.null timestamp))
            assertEqual "verbose log uses the relative path" ("\thledger-flow " <> message) body
          _ -> assertFailure "expected a verbose message on stderr"
  check False "Looked for but did not find 'statement.csv'\n"
  T.writeFile (Path.toFilePath file) ""
  check True "Found 'statement.csv'\n"
  _ <- verboseTestFile (defaultOpts base) ch file
  quietEntry <- atomically (tryReadTChan ch)
  case quietEntry of
    Nothing -> return ()
    Just msg -> assertFailure ("unexpected log with verbosity off: " ++ show msg)

testNeedsRegeneration :: Test
testNeedsRegeneration = TestCase $ withTestDir $ \base -> do
  let source = base Path.</> [Path.relfile|source.txt|]
      target = base Path.</> [Path.relfile|target.txt|]
      older = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
      newer = addUTCTime 1 older
      check message expected = needsRegeneration source target >>= assertEqual message expected

  T.writeFile (Path.toFilePath source) ""
  check "missing target" True

  T.writeFile (Path.toFilePath target) ""
  PathIO.setModificationTime target older
  PathIO.setModificationTime source newer
  check "newer source" True

  PathIO.setModificationTime source older
  PathIO.setModificationTime target newer
  check "newer target" False

  PathIO.setModificationTime target older
  check "equal timestamps" False
  PathIO.setModificationTime source (addUTCTime 0.75 older)
  check "subsecond differences alone do not trigger regeneration" False

  PathIO.removeFile source
  check "source timestamp no longer available" True

testBatchOrder :: Test
testBatchOrder = TestCase $ do
  secondStarted <- newEmptyMVar
  firstFinished <- newEmptyMVar
  let first = takeMVar secondStarted >> putMVar firstFinished () >> return (1 :: Int)
      second = putMVar secondStarted () >> return 2
      third = do
        finished <- tryTakeMVar firstFinished
        assertEqual "finish the current batch before starting the next" (Just ()) finished
        return 3
  result <- timeout 10000000 (parBatchedActions 2 [0] [first, second, third])
  assertEqual "preserve input order across parallel batches" (Just [0, 1, 2, 3]) result

tests :: Test
tests = TestList [testHiddenFiles, testFilterPaths, testFirstExistingFile, testVerboseTestFile, testNeedsRegeneration, testBatchOrder]
