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

inputFiles = ["dir1/2018-04-30.csv",
              "dir2/d2f1.csv",
              "dir1/2018-03-30.csv",
              "dir2/d2f2.csv",
              "dir1/2018-05-30.csv"] :: [FilePath]

journalFiles = map (changeExtension "journal") inputFiles
extraFiles = ["dir2-opening.journal"]

touchAll :: [FilePath] -> Shell ()
touchAll = foldl (\acc file -> acc <> superTouch file) (return ())

superTouch :: FilePath -> Shell ()
superTouch file = do
  mktree $ directory file
  touch file

testFilterPaths = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
        let tmpJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let tmpExtras = map (tmpdir </>) extraFiles :: [FilePath]
        let onDisk = List.sort $ tmpJournals ++ tmpExtras
        touchAll onDisk
        let importedJournals = select tmpJournals :: Shell FilePath

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
        let tmpJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let tmpExtras = map (tmpdir </>) extraFiles :: [FilePath]
        touchAll $ tmpJournals ++ tmpExtras
        let importedJournals = select tmpJournals :: Shell FilePath

        let j1 = tmpdir </> "dir1-include.journal"
        let j2 = tmpdir </> "dir2-include.journal"
        let expectedIncludes = [j1, j2]

        reportedAsWritten <- single $ shellToList $ groupAndWriteIncludeFiles importedJournals
        liftIO $ assertEqual "groupAndWriteIncludeFiles should return which files it wrote" expectedIncludes reportedAsWritten

        let expectedOnDisk = expectedIncludes ++ tmpExtras
        includeFilesOnDisk <- single $ sort $ onlyFiles $ ls tmpdir
        liftIO $ assertEqual "The actual files on disk should match what groupAndWriteIncludeFiles reported" expectedOnDisk includeFilesOnDisk

        let expectedJ1Contents = includePreamble <> "\n"
              <> "!include dir1/2018-03-30.journal\n"
              <> "!include dir1/2018-04-30.journal\n"
              <> "!include dir1/2018-05-30.journal\n"
        actualJ1Contents <- liftIO $ readTextFile j1
        liftIO $ assertEqual "J1: The include file contents should be the journal files" expectedJ1Contents actualJ1Contents

        let expectedJ2Contents = includePreamble <> "\n"
              <> "!include dir2-opening.journal\n"
              <> "!include dir2/d2f1.journal\n"
              <> "!include dir2/d2f2.journal\n"
        actualJ2Contents <- liftIO $ readTextFile j2
        liftIO $ assertEqual "J2: The include file contents should be the journal files" expectedJ2Contents actualJ2Contents
     )
  )

tests = TestList [testFilterPaths, testWriteIncludeFiles]
