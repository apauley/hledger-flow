{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (withStagedOutput, extractImportDirs, findInputFiles, findJournalFiles, groupIncludesUpTo, includeFileName) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hledger.Flow.Common (groupValuesBy)
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.Types (ImportDirs (..), InputFileBundle)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, findFilesInWith, pathSize)
import Path
import qualified Path.IO as PathIO
import System.FilePath (dropTrailingPathSeparator)

-- Keep hook output on the destination filesystem and publish it only on success.
withStagedOutput :: AbsFile -> (AbsFile -> IO ()) -> IO ()
withStagedOutput target writeOutput = do
  let dir = parent target
  PathIO.createDirIfMissing True dir
  PathIO.withTempDir dir ".hledger-flow-" $ \tmp -> do
    let staged = tmp </> filename target
    writeOutput staged
    PathIO.renameFile staged target

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
  let excludeDirs = excludeStageDirs [[reldir|2-preprocessed|], [reldir|3-journal|]]
  findFilesInWith (includeYearFilesForParent [reldir|1-in|] startYear) excludeDirs

findJournalFiles :: AbsDir -> IO [AbsFile]
findJournalFiles = do
  let excludeDirs = excludeStageDirs [[reldir|1-in|], [reldir|2-preprocessed|]]
  findFilesInWith (includeYearFilesForParent [reldir|3-journal|] 0) excludeDirs

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

-- Stage names are reserved only directly below an account.
excludeStageDirs :: [RelDir] -> AbsDir -> [RelDir]
excludeStageDirs stages dir
  | dirname (parent (parent (parent dir))) == [reldir|import|] = stages ++ common
  | otherwise = common
  where
    common = [[reldir|_manual_|], [reldir|__pycache__|]]

groupIncludesUpTo :: RelDir -> [RelFile] -> InputFileBundle
groupIncludesUpTo stopAt = collect Map.empty
  where
    collect acc files =
      let belowStop f = isProperPrefixOf stopAt f && parent f /= stopAt
          groups = Map.filterWithKey (\f _ -> isProperPrefixOf stopAt f) $
            groupValuesBy includePath (filter belowStop files)
       in if Map.null groups
            then acc
            else collect (Map.unionWith (++) acc groups) (Map.keys groups)

    includePath file
      | pathSize (parent file) == 6 = initialIncludeFilePath file
      | otherwise = parentIncludeFilePath file

initialIncludeFilePath :: RelFile -> RelFile
initialIncludeFilePath p = (parent . parent . parent) p </> includeFileName p

parentIncludeFilePath :: RelFile -> RelFile
parentIncludeFilePath p = (parent . parent) p </> filename p

includeFileName :: RelFile -> RelFile
includeFileName f = do
  let includeFile = (dropTrailingPathSeparator . toFilePath . dirname . parent) f ++ "-include.journal"
  fromMaybe [relfile|unknown-include.journal|] $ parseRelFile includeFile
