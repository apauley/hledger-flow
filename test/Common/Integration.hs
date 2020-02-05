{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Common.Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.List as List (sort)
import qualified Data.Text as T

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

assertSubDirsForDetermineBaseDir :: FilePath -> [FilePath] -> IO ()
assertSubDirsForDetermineBaseDir expectedBaseDir importDirs = do
  _ <- sequence $ map (assertDetermineBaseDir expectedBaseDir) importDirs
  return ()

assertDetermineBaseDir :: FilePath -> FilePath -> IO ()
assertDetermineBaseDir expectedBaseDir subDir = do
  initialPwd <- pwd
  bd1 <- determineBaseDir $ Just subDir
  cd subDir
  bd2 <- determineBaseDir Nothing
  bd3 <- determineBaseDir $ Just "."
  cd initialPwd
  let msg = format ("determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - "%fp) subDir
  _ <- sequence $ map (assertEqual (T.unpack msg) expectedBaseDir) [bd1, bd2, bd3]
  return ()

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

        let baseDir = "bd1"
        let importDir = baseDir </> "import"
        let ownerDir = importDir </> "john"
        let bankDir = ownerDir </> "mybank"
        let accDir = bankDir </> "myacc"
        let inDir = accDir </> "1-in"
        let yearDir = inDir </> "2019"
        let subDirs = [yearDir, inDir, accDir, bankDir, ownerDir, importDir, baseDir]

        mktree $ tmpdir </> yearDir

        let subDirsRelativeToTop = map (tmpdir </>) subDirs

        currentDir <- pwd
        let absoluteTempDir = forceTrailingSlash $ collapse $ currentDir </> tmpdir
        let absoluteSubDirs = map (absoluteTempDir </>) subDirs

        let absoluteBaseDir = forceTrailingSlash $ absoluteTempDir </> baseDir
        liftIO $ assertSubDirsForDetermineBaseDir absoluteBaseDir absoluteSubDirs
        liftIO $ assertSubDirsForDetermineBaseDir absoluteBaseDir subDirsRelativeToTop

        cd absoluteTempDir
        liftIO $ assertSubDirsForDetermineBaseDir absoluteBaseDir subDirs
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
tests = TestList [testDetermineBaseDir, testHiddenFiles, testFilterPaths]
