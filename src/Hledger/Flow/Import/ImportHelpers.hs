{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (findInputFiles, findJournalFiles) where

import Path
import Data.Char (isDigit)

import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, findFilesIn)

findInputFiles :: Integer -> AbsDir -> IO [AbsFile]
findInputFiles startYear = do
  let excludeDirs = [[Path.reldir|2-preprocessed|], [Path.reldir|3-journal|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [Path.reldir|1-in|] startYear) excludeDirs

findJournalFiles :: AbsDir -> IO [AbsFile]
findJournalFiles = do
  let excludeDirs = [[Path.reldir|1-in|], [Path.reldir|2-preprocessed|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [Path.reldir|3-journal|] 0) excludeDirs

-- | Include only files directly underneath parentDir/yearDir, e.g. 1-in/2020/* or 3-journal/2020/*
includeYearFilesForParent :: RelDir -> Integer -> AbsDir -> Bool
includeYearFilesForParent parentDir startYear d = (dirname . parent) d == parentDir
  && length shortDirName == 4
  && all isDigit shortDirName
  && read shortDirName >= startYear
    where shortDirName = dirToStringNoSlash d

dirToStringNoSlash :: AbsDir -> String
dirToStringNoSlash = init . Path.toFilePath . Path.dirname

commonExcludeDirs :: [RelDir]
commonExcludeDirs = [[Path.reldir|_manual_|], [Path.reldir|__pycache__|]]
