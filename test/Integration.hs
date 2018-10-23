{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold
import qualified Data.Text as T

import Common

files = ["dir1/2018-04-30.journal", "dir2/d2f1.journal", "dir1/2018-03-30.journal", "dir2/d2f2.journal", "dir1/2018-05-30.journal"] :: [FilePath]

makeDirs :: [FilePath] -> IO ()
makeDirs = foldl makeDirForFile (return ())

makeDirForFile :: IO () -> FilePath -> IO ()
makeDirForFile acc p = acc <> (mktree $ directory p)

testWriteIncludeFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
        let tmpfiles = map (tmpdir </>) files :: [FilePath]
        liftIO $ makeDirs tmpfiles

        let j1 = tmpdir </> "dir1-include.journal"
        let j2 = tmpdir </> "dir2-include.journal"
        let expected = [j1, j2]

        reportedAsWritten <- single $ shellToList $ groupAndWriteIncludeFiles' tmpfiles
        liftIO $ assertEqual "groupAndWriteIncludeFiles should return which files it wrote" expected reportedAsWritten

        includeFilesOnDisk <- single $ sort $ onlyFiles $ ls tmpdir
        liftIO $ assertEqual "The actual files on disk should match what groupAndWriteIncludeFiles reported" expected includeFilesOnDisk

        let expectedJ1Contents = includePreamble <> "\n"
              <> "!include dir1/2018-03-30.journal\n"
              <> "!include dir1/2018-04-30.journal\n"
              <> "!include dir1/2018-05-30.journal\n"
        actualJ1Contents <- liftIO $ readTextFile j1
        liftIO $ assertEqual "J1: The include file contents should be the journal files" expectedJ1Contents actualJ1Contents

        let expectedJ2Contents = includePreamble <> "\n!include dir2/d2f1.journal\n" <> "!include dir2/d2f2.journal\n"
        actualJ2Contents <- liftIO $ readTextFile j2
        liftIO $ assertEqual "J2: The include file contents should be the journal files" expectedJ2Contents actualJ2Contents
     )
  )

tests = TestList [testWriteIncludeFiles]
