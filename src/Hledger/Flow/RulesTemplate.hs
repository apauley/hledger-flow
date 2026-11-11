{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A starter hledger rules file built from a statement's header row.
module Hledger.Flow.RulesTemplate
  ( splitHeader,
    looksLikeHeader,
    templateFields,
    rulesTemplate,
    rulesTemplateFor,
  )
where

import Data.Char (isAlphaNum, isDigit, toLower)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hledger.Flow.BaseDir (relativeToBase')
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.Inventory (findImportFiles, stageFiles)
import Hledger.Flow.Status.Gather (accountName, cachedStatement)
import Hledger.Flow.Types (BaseDir)
import Path
import qualified System.FilePath as FilePath
import System.IO (IOMode (ReadMode), hIsEOF, hSetEncoding, mkTextEncoding, withFile)
import qualified Data.Text.IO as T

-- | The separator and the column names of a header line. Strips a UTF-8 BOM,
-- surrounding quotes and spaces.
-- ponytail: a quoted name containing the separator is split; use a CSV parser if that bites.
splitHeader :: T.Text -> (Char, [T.Text])
splitHeader line = (sep, map clean (T.splitOn (T.singleton sep) l))
  where
    l = T.dropWhileEnd (== '\r') (T.dropWhile (== '\xFEFF') line)
    count c = T.count (T.singleton c) l
    sep = List.foldl' (\best c -> if count c > count best then c else best) ',' [';', '\t']
    clean = T.strip . unquote . T.strip
    unquote t
      | T.length t >= 2 && T.head t == '"' && T.last t == '"' = T.init (T.tail t)
      | otherwise = t

-- | False when any column looks like a date or an amount.
looksLikeHeader :: [T.Text] -> Bool
looksLikeHeader = not . any dataish
  where
    dataish f = not (T.null f) && T.all (`elem` ("0123456789-/.:,+$€£ " :: String)) f

-- | hledger field names for the columns: obvious date, description and amount
-- columns are named for hledger; the rest keep a normalised version of their name.
templateFields :: [T.Text] -> [T.Text]
templateFields names = dedupe (zipWith pick [0 ..] normalised)
  where
    normalised = zipWith normalise [1 :: Int ..] names
    normalise i n =
      let t = T.intercalate "_" (filter (not . T.null) (T.splitOn "_" (T.map (\c -> if isAlphaNum c then toLower c else '_') n)))
       in if T.null t then "field" <> T.pack (show i) else t
    indexed = zip [0 :: Int ..] normalised
    dateIx = fst <$> List.find (("date" `T.isInfixOf`) . snd) indexed
    descIx = fst <$> List.find (\(i, n) -> Just i /= dateIx && any (`T.isInfixOf` n) ["desc", "payee", "narrat", "detail"]) indexed
    amountIx = case [i | (i, n) <- indexed, "amount" `T.isInfixOf` n, Just i /= dateIx, Just i /= descIx] of
      [i] -> Just i
      _ -> fst <$> List.find ((== "amount") . snd) indexed
    pick i n
      | Just i == dateIx = "date"
      | Just i == descIx = "description"
      | Just i == amountIx = "amount"
      | isSpecialField n = "bank_" <> n
      | otherwise = n
    dedupe = go []
      where
        go _ [] = []
        go seen (n : rest) =
          let k = length (filter (== n) seen)
           in (if k == 0 then n else n <> "_" <> T.pack (show (k + 1))) : go (n : seen) rest

-- | Names hledger gives a meaning of its own, such as balance (assertions) or status.
isSpecialField :: T.Text -> Bool
isSpecialField n =
  n `elem` ["status", "code", "comment", "balance", "currency", "date2", "account", "amount", "date", "description"]
    || (T.dropWhileEnd isDigit n `elem` ["account", "amount", "balance", "comment", "currency"] && T.any isDigit n)

notCsv :: FilePath -> T.Text
notCsv path =
  T.pack path <> " isn't a CSV file, so there's no header row to build rules from. "
    <> "A preprocess script can convert it to CSV first: "
    <> docURL "the-preprocess-script"

-- | The starter rules file for a statement, from its contents.
rulesTemplate :: T.Text -> T.Text -> FilePath -> T.Text -> Either T.Text T.Text
rulesTemplate bank account path contents
  | map toLower (FilePath.takeExtension path) `notElem` [".csv", ".tsv", ".ssv"] = Left (notCsv path)
  | T.null (T.strip line) || not (looksLikeHeader names) =
      Left ("The first line of " <> T.pack path <> " looks like data, not column names. Write the fields line yourself: " <> docURL "rules-files")
  | otherwise =
      Right . T.unlines $
        [ "# Starter rules for " <> bank <> "/" <> account <> ", made by hledger-flow from the header row of",
          "# " <> T.pack path <> ". Check every line, then run: hledger-flow import",
          "# hledger's CSV rules reference: https://hledger.org/hledger.html#csv",
          "",
          "skip 1"
        ]
          ++ ["separator " <> (if sep == '\t' then "TAB" else T.singleton sep) | sep /= ',']
          ++ [ "fields " <> T.intercalate ", " fields,
               ""
             ]
          ++ [ "# Columns prefixed with bank_ would otherwise mean something to hledger.\n"
                 <> "# For example, rename bank_balance to balance to check balance assertions.\n"
               | any (\f -> maybe False isSpecialField (T.stripPrefix "bank_" f)) fields
             ]
          ++ [
               "# Make the date format match your statement, e.g. %d/%m/%Y",
               "# date-format %Y-%m-%d",
               "",
               "# Separate debit and credit columns? Name them amount-out and amount-in",
               "# in the fields line instead of amount.",
               "",
               "account1 assets:" <> bank <> ":" <> account,
               "account2 expenses:unknown"
             ]
  where
    line = T.takeWhile (/= '\n') contents
    (sep, names) = splitHeader line
    fields = templateFields names

-- | The starter rules file for an account (@owner/bank/account@), from its newest statement.
rulesTemplateFor :: BaseDir -> T.Text -> IO (Either T.Text T.Text)
rulesTemplateFor base requested = do
  let account = T.dropWhileEnd (== '/') (maybe requested id (T.stripPrefix "import/" requested))
      root = base </> [reldir|import|]
  files <- findImportFiles root
  let inputs = stageFiles root [reldir|1-in|] files
      statements = filter ((== account) . accountName root) inputs
      known = List.nub (map (accountName root) inputs)
  case reverse statements of
    [] ->
      return . Left $
        "No statements found for " <> account <> ". Accounts with statements: "
          <> (if null known then "none" else T.intercalate ", " known) <> "."
    newest : _ -> do
      source <- fromMaybe newest <$> cachedStatement newest
      let path = relativeToBase' (toFilePath base) (toFilePath source)
          isCsv = map toLower (FilePath.takeExtension path) `elem` [".csv", ".tsv", ".ssv"]
          (bank, acct) = case T.splitOn "/" account of
            [_, b, a] -> (b, a)
            _ -> (account, account)
      header <- if isCsv then firstLine (toFilePath source) else return ""
      return (rulesTemplate bank acct path header)

-- | The first line of a file, tolerating encodings other than UTF-8.
firstLine :: FilePath -> IO T.Text
firstLine path = withFile path ReadMode $ \h -> do
  hSetEncoding h =<< mkTextEncoding "UTF-8//ROUNDTRIP"
  eof <- hIsEOF h
  if eof then return "" else T.hGetLine h
