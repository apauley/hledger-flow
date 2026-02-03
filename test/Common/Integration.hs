{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Integration (tests) where

import qualified Data.List as List (sort)
import Hledger.Flow.Common
import Hledger.Flow.PathHelpers (TurtlePath)
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

tests :: Test
tests = TestList [testHiddenFiles, testFilterPaths, testFirstExistingFile]
