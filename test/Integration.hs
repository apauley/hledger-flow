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

files = ["dir1/d1f1.journal", "dir1/d1f2.journal", "dir2/d2f1.journal", "dir2/d2f2.journal"] :: [FilePath]

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
        let expected = [tmpdir </> "dir1.journal", tmpdir </> "dir2.journal"]
        reportedAsWritten <- single $ shellToList $ writeIncludeFiles tmpfiles
        liftIO $ assertEqual "writeIncludeFiles should return which files it wrote" expected reportedAsWritten

        includeFilesOnDisk <- single $ sort $ onlyFiles $ ls tmpdir
        liftIO $ assertEqual "The actual files on disk should match what writeIncludeFiles reported" expected includeFilesOnDisk
     )
  )

tests = TestList [testWriteIncludeFiles]
