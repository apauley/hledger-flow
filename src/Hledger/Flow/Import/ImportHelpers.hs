{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (extractImportDirs, findInputFiles, findJournalFiles, groupIncludesUpTo, includeFileName) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hledger.Flow.Common (groupValuesBy)
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.Types (ImportDirs (..), InputFileBundle)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, findFilesIn, pathSize)
import Path
import System.FilePath (dropTrailingPathSeparator)

extractImportDirs :: Path b File -> Either T.Text (ImportDirs b)
extractImportDirs inputFile = walk (parent inputFile) []
  where
    walk dir children
      | dirname dir == [reldir|import|] = case children of
          [owner, bank, account, state, year] -> Right (ImportDirs dir owner bank account state year)
          _ -> malformed
      | parent dir == dir = malformed
      | otherwise = walk (parent dir) (dir : children)

    malformed = Left $
      "I couldn't find the right number of directories between \"import\" and the input file:\n"
      <> T.pack (toFilePath inputFile)
      <> "\n\nhledger-flow expects to find input files in this structure:\n"
      <> "import/owner/bank/account/filestate/year/trxfile\n\n"
      <> "Have a look at the documentation for a detailed explanation:\n"
      <> docURL "input-files"

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
includeYearFilesForParent parentDir startYear d =
  (dirname . parent) d == parentDir
    && length shortDirName == 4
    && all isDigit shortDirName
    && read shortDirName >= startYear
  where
    shortDirName = dirToStringNoSlash d

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
  if shouldStop
    then acc
    else do
      let grouped = groupIncludeFilesPerYear journals
      groupIncludesUpTo' (acc <> grouped) stopAt (Map.keys grouped)

groupIncludeFilesPerYear :: [RelFile] -> InputFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p : _) =
  if pathSize (parent p) == 6
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
