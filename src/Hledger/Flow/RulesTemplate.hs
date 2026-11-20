{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A starter hledger rules file built from a statement's header row.
module Hledger.Flow.RulesTemplate
  ( splitHeader,
    looksLikeHeader,
    templateFields,
    rulesTemplate,
    rulesTemplateFor,
    rulesStarterFor,
    runRules,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, toLower)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hledger.Flow.BaseDir (relativeToBase')
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.Common (homeDirs, nativeShell, tildePath)
import Hledger.Flow.Import.ImportHelpers (defaultRulesCandidates, extractImportDirs)
import Hledger.Flow.Import.Inventory (findImportFiles, stageFiles)
import Hledger.Flow.Init.Example (isSampleStatement)
import Hledger.Flow.PathHelpers (AbsFile, writeNewUtf8File)
import Hledger.Flow.StatementScan
import Hledger.Flow.Status (flowCommand)
import Hledger.Flow.Status.Gather (accountName, cachedStatement)
import Hledger.Flow.Types (BaseDir, HledgerConf)
import Path
import qualified Path.IO as PathIO
import qualified Data.Text.IO as T
import System.IO (Newline (..), nativeNewline)
import System.IO.Error (tryIOError, isAlreadyExistsError)

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
    dedupe = snd . List.mapAccumL unique Set.empty
      where
        unique seen name = (Set.insert chosen seen, chosen)
          where
            chosen = available name (2 :: Int)
            available candidate suffix
              | Set.notMember candidate seen = candidate
              | otherwise = available (name <> "_" <> T.pack (show suffix)) (suffix + 1)

-- | Names hledger gives a meaning of its own, such as balance (assertions) or status.
isSpecialField :: T.Text -> Bool
isSpecialField n =
  n `elem` ["status", "code", "comment", "balance", "currency", "date2", "account", "amount", "date", "description"]
    || (T.dropWhileEnd isDigit n `elem` ["account", "amount", "balance", "comment", "currency"] && T.any isDigit n)

notCsv :: FilePath -> T.Text
notCsv path =
  T.pack path <> " isn't a CSV file, so there's no header row to build rules from. "
    <> "A preprocess script can convert it to CSV first: "
    <> docURL PreprocessScript

-- | The starter rules file for a statement, from its contents.
rulesTemplate :: T.Text -> T.Text -> FilePath -> T.Text -> Either T.Text T.Text
rulesTemplate bank account path contents
  | not (isDelimitedFile path) = Left (notCsv path)
  | otherwise = renderTemplate bank account path (parseRuleSample path contents)

renderTemplate :: T.Text -> T.Text -> FilePath -> Either T.Text DelimitedStatement -> Either T.Text T.Text
renderTemplate bank account path result = case result of
  Left problem -> Left (T.pack path <> ": " <> problem <> " Write the fields line yourself: " <> docURL RulesFiles)
  Right statement -> case statementHeader statement of
    Nothing -> headerlessTemplate bank account path statement
    Just names ->
      let fields = templateFields names
       in Right . T.unlines $
        [ "# Starter rules for " <> bank <> "/" <> account <> ", made by hledger-flow from the header row of",
          "# " <> T.pack path <> ". Check every line, then run: hledger-flow import",
          "# hledger's CSV rules reference: https://hledger.org/hledger.html#csv",
          "",
          "skip 1"
        ]
          ++ separatorRule (statementSeparator statement)
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
               starterAccountRule bank account,
               "account2 expenses:unknown"
             ]

-- Keep directory names on disk and make their suggested account usable as a shell argument.
starterAccountRule :: T.Text -> T.Text -> T.Text
starterAccountRule bank account = "account1 assets:" <> component "bank" bank <> ":" <> component "account" account
  where
    component fallback name = case filter (not . T.null) (T.split unsafe name) of
      [] -> fallback
      parts -> T.intercalate "-" parts
    unsafe c = not (isAlphaNum c || c `elem` ("-_." :: String))

separatorRule :: Char -> [T.Text]
separatorRule sep = ["separator " <> (if sep == '\t' then "TAB" else T.singleton sep) | sep /= ',']

headerlessTemplate :: T.Text -> T.Text -> FilePath -> DelimitedStatement -> Either T.Text T.Text
headerlessTemplate bank account path statement = case statementRows statement of
  [] -> Left ("No data rows in " <> T.pack path <> ".")
  sample : _ ->
    let columns = zip [1 :: Int ..] (List.transpose (statementRows statement))
        dateCandidates = [(n, cells) | (n, column) <- columns, Just cells <- [traverse parseDateCell column]]
        dateChoice = unique dateCandidates
        amountChoice = unique [n | (n, column) <- columns, all isSignedDecimal column]
        descriptionChoice = unique
          [ n | (n, column) <- columns, all (T.any isAlpha) column, Set.size (Set.fromList column) > 1
          ]
        field n
          | Just n == fmap fst dateChoice = "date"
          | Just n == amountChoice = "amount"
          | Just n == descriptionChoice = "description"
          | otherwise = columnName n
        dateRule = case dateChoice of
          Just (n, cells) -> case inferDateFormat cells of
            Just "%Y-%m-%d" -> []
            Just layout -> ["date-format " <> layout]
            Nothing -> ["# date-format (set the order and separator for " <> sampleValue n sample <> ")"]
          Nothing -> ["# Rename the transaction date column to date, then set date-format if needed."]
     in Right . T.unlines $
          [ "# Starter rules for " <> bank <> "/" <> account <> ", made by hledger-flow from",
            "# " <> T.pack path <> ", which has no header row.",
            "# The first row, by column:"
          ]
            ++ concat (zipWith commentValue [1 :: Int ..] sample)
            ++ ["# Check every line, then run: hledger-flow import", ""]
            ++ separatorRule (statementSeparator statement)
            ++ ["fields " <> T.intercalate ", " (map (field . fst) columns)]
            ++ dateRule
            ++ ["# Rename the amount column to amount." | amountChoice == Nothing]
            ++ ["# Rename the payee column to description." | descriptionChoice == Nothing]
            ++ ["", starterAccountRule bank account, "account2 expenses:unknown"]
  where
    unique [one] = Just one
    unique _ = Nothing
    columnName n = "column" <> T.pack (show n)
    sampleValue n = T.filter (/= '\n') . T.intercalate " " . take 1 . drop (n - 1)
    commentValue n value = case T.splitOn "\n" value of
      first : rest -> ("#   " <> columnName n <> " =" <> if T.null first then "" else " " <> first)
        : map ("#     " <>) rest
      [] -> []

-- | The first default rules candidate and the starter for the newest statement,
-- skipping init's sample statement when the account also has real ones.
rulesStarterFor :: BaseDir -> T.Text -> IO (Either T.Text (AbsFile, T.Text))
rulesStarterFor base requested = do
  let account = T.dropWhileEnd (== '/') (fromMaybe requested (T.stripPrefix "import/" requested))
      root = base </> [reldir|import|]
  files <- findImportFiles root
  let inputs = stageFiles root [reldir|1-in|] files
      found = filter ((== account) . accountName root) inputs
      statements = case filter (not . isSampleStatement) found of
        [] -> found
        real -> real
      known = List.nub (map (accountName root) inputs)
  case reverse statements of
    [] -> return . Left $
      "No statements found for " <> account <> ". Accounts with statements: "
        <> (if null known then "none" else T.intercalate ", " known) <> "."
    newest : _ -> case extractImportDirs root newest of
      Left problem -> return (Left problem)
      Right dirs -> do
        candidates <- defaultRulesCandidates dirs
        case candidates of
          target : _ -> do
            source <- fromMaybe newest <$> cachedStatement newest
            let path = relativeToBase' (toFilePath base) (toFilePath source)
                (bank, acct) = case T.splitOn "/" account of
                  [_, b, a] -> (b, a)
                  _ -> (account, account)
            if isDelimitedFile path
              then fmap (fmap (\template -> (target, template)) . renderTemplate bank acct path) (readRuleSample source)
              else return (Left (notCsv path))
          [] -> return (Left ("No rules file target for " <> account <> "."))

rulesTemplateFor :: BaseDir -> T.Text -> IO (Either T.Text T.Text)
rulesTemplateFor base = fmap (fmap snd) . rulesStarterFor base

runRules :: Maybe HledgerConf -> BaseDir -> T.Text -> IO ()
runRules conf base account = do
  result <- rulesStarterFor base account
  (target, template) <- either (ioError . userError . T.unpack) return result
  cwd <- PathIO.getCurrentDir
  homes <- homeDirs
  let path = toFilePath target
      shown = T.pack (if cwd == base then relativeToBase' (toFilePath base) path else tildePath homes path)
      command sub args = flowCommand nativeShell homes cwd base conf sub args
      -- Match the native line endings of status --rules-template on stdout.
      fileTemplate = case nativeNewline of
        LF -> template
        CRLF -> T.replace "\n" "\r\n" template
  written <- tryIOError (writeNewUtf8File target fileTemplate)
  case written of
    Left err -> do
      occupied <- PathIO.doesFileExist target
      let problem
            | occupied || isAlreadyExistsError err =
                shown <> " already exists. Delete it first or preview a fresh starter with: "
                  <> command "status" ["--rules-template", account]
            | otherwise = "Could not create " <> shown <> ": " <> T.pack (show err)
      ioError (userError (T.unpack problem))
    Right () -> do
      T.putStrLn ("Created " <> shown)
      T.putStrLn ("Edit it: check the fields line and the account names. Then run: " <> command "import" [])
