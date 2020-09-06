{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (findInputFiles, findJournalFiles) where

import Path
import Data.Char (isDigit)

import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, findFilesIn)

findInputFiles :: AbsDir -> IO [AbsFile]
findInputFiles = do
  let excludeDirs = [[Path.reldir|2-preprocessed|], [Path.reldir|3-journal|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [Path.reldir|1-in|]) excludeDirs

findJournalFiles :: AbsDir -> IO [AbsFile]
findJournalFiles = do
  let excludeDirs = [[Path.reldir|1-in|], [Path.reldir|2-preprocessed|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [Path.reldir|3-journal|]) excludeDirs

-- | Include only files directly underneath parentDir/yearDir, e.g. 1-in/2020/* or 3-journal/2020/*
includeYearFilesForParent :: RelDir -> AbsDir -> Bool
includeYearFilesForParent parentDir d = (dirname . parent) d == parentDir
  && length shortDirName == 4
  && all isDigit shortDirName
    where shortDirName = dirToStringNoSlash d

dirToStringNoSlash :: AbsDir -> String
dirToStringNoSlash = init . Path.toFilePath . Path.dirname

commonExcludeDirs :: [RelDir]
commonExcludeDirs = [[Path.reldir|_manual_|], [Path.reldir|__pycache__|]]