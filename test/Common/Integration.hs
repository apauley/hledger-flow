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

testDirOrPwd :: Test
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

testDetermineBaseDir :: Test
testDetermineBaseDir = TestCase (
  sh (
      do
        error1 <- liftIO $ determineBaseDir'' "/path/to/dir" "/path/to/dir"
        liftIO $ assertEqual "determineBaseDir produces an error message when given a non-existant dir" (Left $ errorMessageBaseDir "/path/to/dir") error1
        tmpdir <- using (mktempdir "." "hlflow")

        let unrelatedDir = collapse $ tmpdir </> "unrelated"
        mkdir unrelatedDir

        bdUnrelated <- liftIO $ determineBaseDir'' unrelatedDir unrelatedDir
        liftIO $ assertEqual "determineBaseDir produces an error message when it cannot find a baseDir" (Left $ errorMessageBaseDir unrelatedDir) bdUnrelated

        currentDir <- pwd
        let baseDir = forceTrailingSlash $ collapse $ currentDir </> tmpdir </> "bd1"

        let importDir = baseDir </> "import"
        let ownerDir = importDir </> "john"
        let bankDir = ownerDir </> "mybank"
        let accDir = bankDir </> "myacc"
        let inDir = accDir </> "1-in"
        let yearDir = inDir </> "2019"
        mktree yearDir

        let reportDir = baseDir </> "report"
        mkdir reportDir

        cd baseDir
        bd <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - in the base dir" baseDir bd

        cd reportDir
        bdReport <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - report dir" baseDir bdReport

        cd yearDir
        bdYear <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - year dir" baseDir bdYear

        cd inDir
        bdIn <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - input dir" baseDir bdIn

        cd accDir
        bdAcc <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - account dir" baseDir bdAcc

        cd bankDir
        bdBank <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - bank dir" baseDir bdBank

        cd ownerDir
        bdOwner <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - owner dir" baseDir bdOwner

        cd importDir
        bdImport <- liftIO $ determineBaseDir Nothing
        liftIO $ assertEqual "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - import dir" baseDir bdImport
     )
  )

testFilterPaths :: Test
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

tests :: Test
tests = TestList [testDirOrPwd, testDetermineBaseDir, testHiddenFiles, testFilterPaths]
