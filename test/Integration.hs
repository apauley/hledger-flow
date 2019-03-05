{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Data.List as List (sort)

import Common

inputFiles = ["import/john/bogartbank/checking/1-in/2018/2018-04-30.csv",
              "import/john/bogartbank/savings/1-in/2018/d2f1.csv",
              "import/john/bogartbank/checking/1-in/2018/2018-03-30.csv",
              "import/john/bogartbank/savings/1-in/2018/d2f2.csv",
              "import/john/bogartbank/checking/1-in/2018/2018-05-30.csv"] :: [FilePath]

journalFiles = map (changePathAndExtension "3-journal" "journal") inputFiles
extraFiles = ["import/john/bogartbank/savings/3-journal/2018-opening.journal"]
hiddenFiles = [".hiddenfile", "checking/.DS_Store", "import/john/bogartbank/savings/1-in/.anotherhiddenfile", "import/john/bogartbank/checking/1-in/2018/.hidden"]

touchAll :: [FilePath] -> Shell ()
touchAll = foldl (\acc file -> acc <> superTouch file) (return ())

superTouch :: FilePath -> Shell ()
superTouch file = do
  mktree $ directory file
  touch file

testHiddenFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
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
        tmpdir <- using (mktempdir "." "makeitso")
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
        tmpdir <- using (mktempdir "." "makeitso")
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

testWriteIncludeFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
        let importedJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let extras = map (tmpdir </>) extraFiles :: [FilePath]
        let hidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        touchAll $ importedJournals ++ extras ++ hidden

        let j1 = tmpdir </> "import/john/bogartbank/checking/3-journal/2018-include.journal"
        let j2 = tmpdir </> "import/john/bogartbank/savings/3-journal/2018-include.journal"
        let expectedIncludes = [j1, j2]

        reportedAsWritten <- single $ groupAndWriteIncludeFiles importedJournals
        liftIO $ assertEqual "groupAndWriteIncludeFiles should return which files it wrote" expectedIncludes reportedAsWritten

        let expectedOnDisk = List.sort $ reportedAsWritten ++ extras ++ importedJournals
        allFilesOnDisk <- single $ sort $ onlyFiles $ lstree tmpdir
        liftIO $ assertEqual "The actual files on disk should match what groupAndWriteIncludeFiles reported" expectedOnDisk allFilesOnDisk

        let expectedJ1Contents = includePreamble <> "\n"
              <> "!include 2018/2018-03-30.journal\n"
              <> "!include 2018/2018-04-30.journal\n"
              <> "!include 2018/2018-05-30.journal\n"
        actualJ1Contents <- liftIO $ readTextFile j1
        liftIO $ assertEqual "J1: The include file contents should be the journal files" expectedJ1Contents actualJ1Contents

        let expectedJ2Contents = includePreamble <> "\n"
              <> "!include 2018-opening.journal\n"
              <> "!include 2018/d2f1.journal\n"
              <> "!include 2018/d2f2.journal\n"
        actualJ2Contents <- liftIO $ readTextFile j2
        liftIO $ assertEqual "J2: The include file contents should be the journal files" expectedJ2Contents actualJ2Contents
     )
  )

tests = TestList [testDirOrPwd, testHiddenFiles, testFilterPaths, testWriteIncludeFiles]
