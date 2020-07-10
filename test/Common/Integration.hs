{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Common.Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.List as List (sort)

import TestHelpers
import Hledger.Flow.Common

testHiddenFiles :: Test
testHiddenFiles = TestCase (
  sh (
      do
        let tmpbase = "." </> "test" </> "tmp"
        mktree tmpbase
        tmpdir <- using (mktempdir tmpbase "hlflowtest")
        let tmpJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let tmpExtras = map (tmpdir </>) extraFiles :: [FilePath]
        let tmpHidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        let onDisk = List.sort $ tmpJournals ++ tmpExtras ++ tmpHidden
        touchAll onDisk
        filtered <- (fmap List.sort) $ shellToList $ onlyFiles $ select onDisk
        let expected = List.sort $ tmpExtras ++ tmpJournals
        liftIO $ assertEqual "Hidden files should be excluded" expected filtered
     )
  )

testFilterPaths :: Test
testFilterPaths = TestCase (
  sh (
      do
        let tmpbase = "." </> "test" </> "tmp"
        mktree tmpbase
        tmpdir <- using (mktempdir tmpbase "hlflowtest")
        let tmpJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let tmpExtras = map (tmpdir </>) extraFiles :: [FilePath]
        let tmpHidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        let onDisk = List.sort $ tmpJournals ++ tmpExtras ++ tmpHidden
        touchAll onDisk

        let nonExistant = map (tmpdir </>) ["where", "is", "my", "mind"]
        let toFilter = nonExistant ++ onDisk
        filtered <- single $ filterPaths testfile toFilter
        let actual = List.sort filtered
        liftIO $ assertEqual "The filtered paths should exclude files not actually on disk" onDisk actual
     )
  )

tests :: Test
tests = TestList [testHiddenFiles, testFilterPaths]
