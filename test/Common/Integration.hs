{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Common.Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Data.List as List (sort)

import TestHelpers
import Hledger.Flow.Import.Types
import Hledger.Flow.Common
import Control.Concurrent.STM

testHiddenFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "hlflow")
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

testDirOrPwd = TestCase (
  sh (
      do
        currentDir <- fmap (\p -> directory (p </> "t")) pwd
        tmpdir <- using (mktempdir "." "hlflow")
        let fooDir = collapse $ currentDir </> tmpdir </> "foo/"
        let barDir = collapse $ currentDir </> tmpdir </> "bar/"
        mkdir fooDir
        mkdir barDir
        d1 <- liftIO $ dirOrPwd Nothing
        liftIO $ assertEqual "dirOrPwd returns pwd as a fallback" currentDir d1
        liftIO $ assertEqual "dirOrPwd assumes the fallback is a directory" (directory d1) d1
        d2 <- liftIO $ dirOrPwd $ Just $ tmpdir </> "foo"
        liftIO $ assertEqual "dirOrPwd returns the supplied dir - no trailing slash supplied" fooDir d2
        liftIO $ assertEqual "dirOrPwd assumes the supplied dir is a directory - no trailing slash supplied" (directory d2) d2
        d3 <- liftIO $ dirOrPwd $ Just $ tmpdir </> "bar/"
        liftIO $ assertEqual "dirOrPwd returns the supplied dir - trailing slash supplied" barDir d3
        liftIO $ assertEqual "dirOrPwd assumes the supplied dir is a directory - trailing slash supplied" (directory d3) d3
     )
  )

testFilterPaths = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "hlflow")
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


tests = TestList [testDirOrPwd, testHiddenFiles, testFilterPaths]
