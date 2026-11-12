{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (withStagedOutput, extractImportDirs, groupIncludesUpTo, includeFileName, rulesFileCandidates, defaultRulesCandidates, importDirLines) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hledger.Flow.Common (groupValuesBy)
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.Import.Types (ImportDirs (..), InputFileBundle)
import Hledger.Flow.PathHelpers (AbsFile, RelDir, RelFile, pathSize, withStagedOutput)
import Path
import Control.Monad.Catch (MonadThrow)
import System.FilePath (dropTrailingPathSeparator)
import qualified System.FilePath as FilePath

extractImportDirs :: Path b Dir -> Path b File -> Either T.Text (ImportDirs b)
extractImportDirs root inputFile = walk (parent inputFile) []
  where
    walk dir children
      | dir == root = case children of
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
      <> docURL InputFiles

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

-- | Rules files for a statement in lookup order: statement-specific overrides,
-- then the account's default rules files.
rulesFileCandidates :: AbsFile -> ImportDirs Abs -> IO [AbsFile]
rulesFileCandidates csv dirs = do
  overrides <- if "rfo" `T.isPrefixOf` suffix && not (T.any (== '_') suffix)
    then do
      name <- rulesName [suffix]
      return [dir </> name | dir <- [accountDir dirs, bankDir dirs, importDir dirs]]
    else return []
  (overrides ++) <$> defaultRulesCandidates dirs
  where
    statementName = T.pack (toFilePath (filename csv))
    -- The statement prefix may contain dots, and the override may have several extensions.
    suffix = T.takeWhile (/= '.') $ case T.breakOnEnd "_rfo" statementName of
      ("", _) -> statementName
      (_, rest) -> "rfo" <> rest

-- | The account-level and bank-level rules files, in lookup order.
defaultRulesCandidates :: ImportDirs Abs -> IO [AbsFile]
defaultRulesCandidates dirs = do
  let bank = importDirLines (bankDir dirs)
      account = importDirLines (accountDir dirs)
  accountRule <- rulesName (bank ++ account)
  bankRule <- rulesName bank
  return [accountDir dirs </> accountRule, importDir dirs </> bankRule]

rulesName :: MonadThrow m => [T.Text] -> m RelFile
rulesName parts = parseRelFile (T.unpack (T.intercalate "-" parts) FilePath.<.> "rules")

importDirLines :: Path b Dir -> [T.Text]
importDirLines = T.splitOn "\n" . T.pack . dropTrailingPathSeparator . toFilePath . dirname
