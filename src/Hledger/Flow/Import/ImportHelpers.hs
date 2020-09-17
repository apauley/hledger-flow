{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (findInputFiles, findJournalFiles, groupIncludesUpTo, includeFileName) where

import Path
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.FilePath (dropTrailingPathSeparator)

import Hledger.Flow.Common (groupValuesBy)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, findFilesIn, pathSize)
import Hledger.Flow.Import.Types (InputFileBundle)

import qualified Data.Map.Strict as Map

findInputFiles :: Integer -> AbsDir -> IO [AbsFile]
findInputFiles startYear = do
  let excludeDirs = [[reldir|2-preprocessed|], [reldir|3-journal|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [reldir|1-in|] startYear) excludeDirs

findJournalFiles :: AbsDir -> IO [AbsFile]
findJournalFiles = do
  let excludeDirs = [[reldir|1-in|], [reldir|2-preprocessed|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [reldir|3-journal|] 0) excludeDirs

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
commonExcludeDirs = [[reldir|_manual_|], [reldir|__pycache__|]]

groupIncludesUpTo :: RelDir -> [RelFile] -> InputFileBundle
groupIncludesUpTo = groupIncludesUpTo' Map.empty

groupIncludesUpTo' :: InputFileBundle -> RelDir -> [RelFile] -> InputFileBundle
groupIncludesUpTo' acc _ [] = acc
groupIncludesUpTo' acc stopAt journals = do
  let dirs = map parent journals :: [RelDir]
  let shouldStop = stopAt `elem` dirs
  if shouldStop then acc else do
    let grouped = groupIncludeFilesPerYear journals
    groupIncludesUpTo' (acc <> grouped) stopAt (Map.keys grouped)

groupIncludeFilesPerYear :: [RelFile] -> InputFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p:_) = if pathSize (parent p) == 6
  then groupValuesBy initialIncludeFilePath ps
  else groupValuesBy parentIncludeFilePath ps

initialIncludeFilePath :: RelFile -> RelFile
initialIncludeFilePath p = (parent . parent . parent) p </> includeFileName p

parentIncludeFilePath :: RelFile -> RelFile
parentIncludeFilePath p = (parent . parent) p </> filename p

includeFileName :: RelFile -> RelFile
includeFileName f = do
  let includeFile = (dropTrailingPathSeparator . toFilePath . dirname . parent) f ++ "-include.journal"
  fromMaybe [relfile|unknown-include.journal|] $ parseRelFile includeFile
