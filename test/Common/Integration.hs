{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.Integration (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically, newTChanIO, tryReadTChan)
import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import qualified Data.List as List (intercalate, isInfixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime(..), addUTCTime, secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Hledger.Flow.Common
import qualified Hledger.Flow.PathHelpers as Paths
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types (LogMessage (StdErr))
import qualified Path
import qualified Path.IO as PathIO
import System.Directory (doesFileExist)
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Error (tryIOError)
import qualified System.Info as Sys
import System.Timeout (timeout)
import Test.HUnit
import TestHelpers (defaultOpts, withTestDir)
import TestHelpersTurtle
import System.FilePath ((</>), normalise, searchPathSeparator)

-- HUnit runs these cases sequentially; restore process-wide PATH even on failure.
withSearchPath :: [FilePath] -> IO a -> IO a
withSearchPath dirs action = bracket (lookupEnv "PATH") restore $ \_ -> do
  setEnv "PATH" (List.intercalate [searchPathSeparator] dirs)
  action
  where
    restore = maybe (unsetEnv "PATH") (setEnv "PATH")

testHledgerLookup :: Test
testHledgerLookup = TestLabel "hledger executable lookup" $ TestCase $ withTestDir $ \base -> do
  let root = Path.toFilePath base
      name = "hledger" ++ Directory.exeExtension
      first = root </> "first bin" </> name
      second = root </> "second" </> name
      check dirs expected = withSearchPath dirs $ do
        result <- tryIOError (hledgerPathFromOption Nothing)
        case result of
          Left err -> assertFailure ("lookup in " ++ show dirs ++ ": " ++ show err)
          Right actual -> do
            expectedPath <- Directory.canonicalizePath expected
            assertEqual ("lookup in " ++ show dirs) expectedPath (Path.toFilePath actual)
  forM_ (["first bin", "second", "work", "empty"] :: [FilePath]) $ \dir ->
    Directory.createDirectory (root </> dir)
  forM_ ([first, second] :: [FilePath]) $ \file -> do
    writeFile file ""
    perms <- Directory.getPermissions file
    Directory.setPermissions file (Directory.setOwnerExecutable True perms)
  Directory.withCurrentDirectory root $ do
    check ["first bin"] first
    check [root </> "first bin"] first
    check ["first bin", root </> "second"] first
    check ["second", "first bin"] second
    Directory.withCurrentDirectory (root </> "first bin") $ check ["."] first
    Directory.withCurrentDirectory (root </> "work") $ check [".." </> "first bin"] first
    withSearchPath ["second"] $ do
      actual <- hledgerPathFromOption (Just first)
      assertEqual "explicit path overrides PATH" first (Path.toFilePath actual)
    withSearchPath ["empty"] $ do
      result <- tryIOError (hledgerPathFromOption Nothing)
      case result of
        Left err -> assertBool "missing executable diagnostic"
          ("Unable to find hledger" `List.isInfixOf` show err)
        Right actual -> assertFailure ("unexpected executable: " ++ show actual)
    -- Windows symlinks require privileges; .exe lookup above runs on every OS.
    unless (Sys.os == "mingw32") $ do
      Directory.createFileLink ("first bin" </> name) (root </> name)
      check ["."] first

testHiddenFiles :: Test
testHiddenFiles = TestCase $ withTestDir $ \base -> do
  let paths = map (Path.toFilePath base </>) (journalFiles ++ extraFiles ++ hiddenFiles)
  touchAll paths
  found <- Paths.findFilesIn (const True) [] base
  let expected = map (Path.toFilePath base </>) (journalFiles ++ extraFiles)
  assertEqual "Hidden files should be excluded"
    (List.sort (map normalise expected)) (List.sort (map Path.toFilePath found))

testFilterPaths :: Test
testFilterPaths = TestCase $ withTestDir $ \base -> do
  let onDisk = map (Path.toFilePath base </>) (journalFiles ++ extraFiles ++ hiddenFiles)
      missing = map (Path.toFilePath base </>) ["where", "is", "my", "mind"]
  touchAll onDisk
  filtered <- filterPaths doesFileExist (missing ++ onDisk)
  assertEqual "Keep existing include files in reverse candidate order" (reverse onDisk) filtered

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
tests = TestList [testHledgerLookup, testHiddenFiles, testFilterPaths, testFirstExistingFile, testVerboseTestFile, testNeedsRegeneration, testBatchOrder]
