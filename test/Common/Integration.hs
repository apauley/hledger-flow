{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Integration (tests) where

import qualified Data.List as List (sort)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Hledger.Flow.Common
import Hledger.Flow.PathHelpers (TurtlePath)
import qualified System.Directory as Dir
import Test.HUnit
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
            filtered <- single $ filterPaths testfile toFilter
            let actual = List.sort filtered
            liftIO $ assertEqual "The filtered paths should exclude files not actually on disk" onDisk actual
        )
    )

testFirstExistingFile :: Test
testFirstExistingFile =
  TestCase
    ( sh
        ( do
            let tmpbase = "." </> "test" </> "tmp"
            mktree tmpbase
            tmpdir <- using (mktempdir tmpbase "hlflowtest")
            let missing = tmpdir </> "does-not-exist.txt"
            let existing1 = tmpdir </> "first.txt"
            let existing2 = tmpdir </> "second.txt"
            touchAll [existing1, existing2]

            found1 <- liftIO $ firstExistingFile [missing, existing1, existing2]
            liftIO $ assertEqual "Should return the first existing file in the list" (Just existing1) found1

            found2 <- liftIO $ firstExistingFile [missing]
            liftIO $ assertEqual "Should return Nothing if no files exist" Nothing found2
        )
    )

-- Helper to set mtime on a Turtle path
setMtime :: TurtlePath -> UTCTime -> IO ()
setMtime path time = Dir.setModificationTime path time

-- Fixed timestamps for deterministic tests
olderTime :: UTCTime
olderTime = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)

newerTime :: UTCTime
newerTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

testNeedsRegeneration :: Test
testNeedsRegeneration =
  TestCase
    ( sh
        ( do
            let tmpbase = "." </> "test" </> "tmp"
            mktree tmpbase
            tmpdir <- using (mktempdir tmpbase "hlflowtest")
            let source = tmpdir </> "source.txt"
            let target = tmpdir </> "target.txt"

            -- Test case 1: target doesn't exist -> returns True
            touch source
            result1 <- liftIO $ needsRegeneration source target
            liftIO $ assertEqual "Should return True when target doesn't exist" True result1

            -- Test case 2: target exists, source is newer -> returns True
            touch target
            liftIO $ setMtime target olderTime
            liftIO $ setMtime source newerTime
            result2 <- liftIO $ needsRegeneration source target
            liftIO $ assertEqual "Should return True when source is newer than target" True result2

            -- Test case 3: target exists, target is newer -> returns False
            liftIO $ setMtime source olderTime
            liftIO $ setMtime target newerTime
            result3 <- liftIO $ needsRegeneration source target
            liftIO $ assertEqual "Should return False when target is newer than source" False result3

            -- Test case 4: equal mtimes -> returns False (boundary condition)
            liftIO $ setMtime source olderTime
            liftIO $ setMtime target olderTime
            result4 <- liftIO $ needsRegeneration source target
            liftIO $ assertEqual "Should return False when mtimes are equal" False result4
        )
    )

tests :: Test
tests = TestList [testHiddenFiles, testFilterPaths, testFirstExistingFile, testNeedsRegeneration]
