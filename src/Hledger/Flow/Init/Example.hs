{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Init.Example
  ( exampleImportFiles,
    exampleBaseFiles,
    starterOwner,
    exampleAccount,
    exampleAccountPath,
    sampleAccountDir,
    untouchedExample,
    directivesAreStarter,
    journalIsCommentsOnly,
  )
where

import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Hledger.Flow.Common (Shell (..), nativeShell)
import Hledger.Flow.DocHelpers (DocLink (Balances, ManualJournals, RulesFiles), docURL)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, pathOccupied)
import Hledger.Flow.Types (BaseDir)
import Path
import qualified Path.IO as PathIO
import System.Environment (lookupEnv)
import qualified System.FilePath as F
import qualified System.FilePath.Windows as Windows
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

starterOwner :: IO RelDir
starterOwner = do
  let variable = case nativeShell of
        Posix -> "USER"
        PowerShell -> "USERNAME"
  value <- either (const Nothing) id <$> tryIOError (lookupEnv variable)
  return (fromMaybe [reldir|me|] (value >>= validOwner))
  where
    validOwner name
      | Windows.isValid name,
        Windows.splitDirectories name == [name],
        not (any Windows.isPathSeparator name),
        T.strip text == text,
        not (T.isPrefixOf "." text || T.isPrefixOf "~" text),
        not (T.isSuffixOf "." text),
        ';' `notElem` name,
        not (T.isSuffixOf ".journal" folded),
        folded /= "_manual_",
        B.length (T.encodeUtf8 text) <= 255 = parseRelDir name
      | otherwise = Nothing
      where
        text = T.pack name
        folded = T.toCaseFold text

exampleAccount :: RelDir -> RelDir
exampleAccount owner = owner </> [reldir|mybank/checking|]

exampleAccountPath :: RelDir -> T.Text
exampleAccountPath account = "import/" <> T.pack (intercalate "/" parts)
  where
    parts = F.splitDirectories (F.dropTrailingPathSeparator (toFilePath account))

sampleName :: RelFile
sampleName = [relfile|hledger-flow-example.csv|]

rulesName :: RelFile
rulesName = [relfile|mybank-checking.rules|]

sampleCsv :: Integer -> T.Text
sampleCsv year = T.unlines $
  zipWith (<>) (sampleCsvRows year) [",Comment", ",\"" <> starterNotice <> "\"", ","]

sampleCsvRows :: Integer -> [T.Text]
sampleCsvRows year =
  ["Date,Description,Amount", date <> ",Example purchase,-5.00", date <> ",Example refund,5.00"]
  where
    date = T.pack (show year) <> "-01-02"

sampleRules :: T.Text
sampleRules = T.unlines
  [ "# Rules for the example statement.",
    "# Docs: " <> docURL RulesFiles,
    "skip 1",
    "fields date, description, amount",
    "currency USD",
    "account1 assets:mybank:checking",
    "account2 expenses:unknown",
    "",
    "if Example",
    " account2 expenses:example"
  ]

exampleImportFiles :: RelDir -> Integer -> [(RelFile, T.Text)]
exampleImportFiles starter year =
  (exampleAccount starter </> rulesName, starterComment "# " sampleRules)
    : paths ((samplePath, sampleCsv year) : manualFiles ++ balanceFiles)
  where
    account = F.dropTrailingPathSeparator (toFilePath (exampleAccount starter))
    bank = F.takeDirectory account
    owner = F.takeDirectory bank
    yearName = show year
    samplePath = account F.</> "1-in" F.</> yearName F.</> toFilePath sampleName
    date day = T.pack yearName <> "-" <> day
    entry day description postings = (date day <> " " <> description) : map ("    " <>) postings
    manual level phase help example =
      (level F.</> "_manual_" F.</> yearName F.</> (phase ++ "-import.journal"),
       journalExample ManualJournals help example)
    balance suffix help example =
      (account F.</> (yearName ++ "-" ++ suffix ++ ".journal"), journalExample Balances help example)
    manualFiles =
      [ manual "" "pre" ["Record an entry shared across owners before their imported activity."]
          (entry "01-01" "Fund the household cash box from my cash"
            ["assets:household:cash  150.00 USD", "assets:me:cash"]),
        manual "" "post" ["Record a transfer between owners after reviewing both sets of statements."]
          (entry "01-02" "Reimburse partner for shared groceries"
            ["assets:partner:cash  35.00 USD", "assets:me:cash"]),
        manual owner "pre" ["Fill a gap in a transfer between this owner's banks before their first statements."]
          (entry "01-01" "Move savings to a new bank"
            ["assets:otherbank:savings  75.00 USD", "assets:mybank:checking"]),
        manual owner "post" ["Record a transfer between this owner's banks after reviewing their statements."]
          (entry "01-02" "Move emergency savings back to checking"
            ["assets:mybank:checking  50.00 USD", "assets:otherbank:savings"]),
        manual bank "pre" ["Record a transfer between accounts at this bank before their imported activity."]
          (entry "01-01" "Move money from checking to savings"
            ["assets:mybank:savings  25.00 USD", "assets:mybank:checking"]),
        manual bank "post" ["Fill in a transfer missing from both exports after reviewing this bank's statements."]
          (entry "01-02" "Sweep savings into checking"
            ["assets:mybank:checking  12.00 USD", "assets:mybank:savings"]),
        manual account "pre"
          ["Fill a gap before this account's first statement.", "Put opening balances in the dedicated yearly opening file."]
          (entry "01-01" "Replace debit card"
            ["expenses:bank-fees  5.00 USD", "assets:mybank:checking"]),
        manual account "post"
          ["Assert this account's balance after its imported entries for the same day.",
           "Use the balance shown by your bank. Put closing balances in the dedicated yearly closing file."]
          (entry "01-02" "Check balance after the sample statement"
            ["assets:mybank:checking  0 USD = 0 USD"])
      ]
    balanceFiles =
      [ balance "opening" ["Record what this account already held just before its first imported transaction."]
          (entry "01-01" "Checking opening balance"
            ["assets:mybank:checking  100.00 USD", "equity:opening/closing balances"]),
        balance "closing"
          ["Close this account's balance to zero when you deliberately close your books.",
           "This example assumes the starter opening balance and sample statement."]
          (entry "12-31" "Checking closing balance"
            ["assets:mybank:checking  -100.00 USD = 0 USD", "equity:opening/closing balances"])
      ]

starterNotice :: T.Text
starterNotice = "Generated by hledger-flow init as a starter. This file is yours to edit. hledger-flow leaves it as you save it."

starterComment :: T.Text -> T.Text -> T.Text
starterComment prefix contents = prefix <> starterNotice <> "\n" <> contents

optionalIncludeNotice :: T.Text
optionalIncludeNotice = "; This file is optional. If absent, hledger-flow omits it from the include files it maintains."

optionalJournal :: T.Text -> T.Text
optionalJournal contents = starterComment "; " (optionalIncludeNotice <> "\n" <> contents)

journalExample :: DocLink -> [T.Text] -> [T.Text] -> T.Text
journalExample link help example = optionalJournal $ T.unlines $
  map ("; " <>) help ++ ["; Docs: " <> docURL link, "", "; Example. Remove its leading semicolons and adapt it."]
    ++ map ("; " <>) example

exampleBaseFiles :: Integer -> [(RelFile, T.Text)]
exampleBaseFiles year = paths $
  [("directives.journal", starterDirectives)]
    ++ [ (priceDir F.</> name, starterComment "; " (T.unlines
           [ "; This file is optional. Remove its include line from prices.journal to omit the pair.",
             "; Add market prices here.",
             "; P " <> date <> " " <> quote
           ]))
         | (name, quote) <- pairs
       ]
    ++ [(priceDir F.</> "prices.journal", optionalJournal (T.unlines ["include " <> T.pack name | (name, _) <- pairs]))]
  where
    priceDir = "prices" F.</> show year
    date = T.pack (show year) <> "-01-01"
    pairs = [("EUR-USD.prices", "EUR 1.10 USD"), ("BTC-USD.prices", "BTC 99000.00 USD")]

starterDirectives :: T.Text
starterDirectives = T.unlines
  [ "; Ledger-wide directives go here. https://hledger.org/dev/hledger.html#directives",
    "; " <> starterNotice,
    optionalIncludeNotice,
    "",
    "; Common commodities. https://hledger.org/dev/hledger.html#commodity-aliases",
    "commodity 1,000.00 USD  ; alias: US$",
    "commodity 1,000.00 EUR  ; alias: €",
    "commodity 1,000.00 GBP  ; alias: £",
    "commodity 1,000.00 CAD  ; alias: CA$",
    "commodity 1,000.00 AUD  ; alias: A$",
    "commodity 1,000. JPY    ; alias: JP¥",
    "commodity 1,000.00 CNY  ; alias: CN¥, alias: RMB",
    "commodity 1,000.00 INR  ; alias: ₹",
    "",
    "; Bitcoin and satoshis. https://hledger.org/dev/hledger.html#commodity-units",
    "commodity 1.00000000 BTC  ; alias: 100000000 SAT, alias: ₿",
    "commodity 1. SAT",
    "",
    "; Account types. https://hledger.org/dev/hledger.html#account-types",
    "account assets                  ; type: A",
    "account liabilities             ; type: L",
    "account equity                  ; type: E",
    "account revenues                ; type: R",
    "account expenses                ; type: X",
    "",
    "account assets:bank             ; type: C",
    "account assets:cash             ; type: C",
    "",
    "account equity:conversion       ; type: V",
    "account equity:unrealised-gain  ; type: U",
    "account revenues:gain           ; type: G"
  ]

paths :: [(FilePath, a)] -> [(RelFile, a)]
paths = mapMaybe (\(p, a) -> fmap (\path -> (path, a)) (parseRelFile p))

untouchedExample :: BaseDir -> [AbsFile] -> IO (Maybe RelDir)
untouchedExample base [source]
  | Just rel <- stripProperPrefix (importRoot base) source,
    [ownerName, "mybank", "checking", "1-in", yearName, name] <- F.splitDirectories (toFilePath rel),
    name == toFilePath sampleName,
    Just starter <- parseRelDir ownerName,
    Just year <- readMaybe yearName,
    Just generated <- parseRelFile ("3-journal" F.</> yearName F.</> "hledger-flow-example.journal"),
    Just yearIndex <- parseRelFile (yearName ++ "-include.journal") = do
      let account = sampleAccountDir base starter
      result <- tryIOError $ do
        let rules = account </> rulesName
            journal = account </> generated
            bank = parent account
            owner = parent bank
            indexes dir = [dir </> yearIndex, dir </> [relfile|all-years.journal|]]
            otherStarters =
              [ (file, contents)
                | (relative, contents) <- exampleImportFiles starter year,
                  let file = importRoot base </> relative,
                  isProperPrefixOf owner file, file /= source, file /= rules
              ]
            allowed = [source, rules, journal] ++ map fst otherStarters ++ concatMap indexes [owner, bank, account]
            dirs = concatMap (takeWhile (isProperPrefixOf owner) . iterate parent . parent) allowed
            optionalStarter (file, contents) = do
              exists <- PathIO.doesFileExist file
              if exists then matches file contents else pure True
        cleanPath <- allM plainDir [importRoot base, owner]
        cleanContents <- if cleanPath then containsOnly dirs allowed owner else return False
        if cleanContents then do
          current <- (&&) <$> matches source (sampleCsv year)
                          <*> matches rules (starterComment "# " sampleRules)
          sampleMatches <- if current then pure True
            else (&&) <$> matches source (T.unlines (sampleCsvRows year)) <*> matches rules sampleRules
          if sampleMatches then allM optionalStarter otherStarters else pure False
        else pure False
      return (case result of Right True -> Just (exampleAccount starter); _ -> Nothing)
  where
    matches file expected = (== T.encodeUtf8 expected) <$> B.readFile (toFilePath file)
untouchedExample _ _ = return Nothing

containsOnly :: [AbsDir] -> [AbsFile] -> AbsDir -> IO Bool
containsOnly allowedDirs allowedFiles dir = do
  (dirs, files) <- PathIO.listDir dir
  if all (`elem` allowedDirs) dirs && all (`elem` allowedFiles) files
    then do
      hasLinks <- (||) <$> (or <$> traverse PathIO.isSymlink dirs) <*> (or <$> traverse PathIO.isSymlink files)
      if hasLinks then return False else and <$> traverse (containsOnly allowedDirs allowedFiles) dirs
    else return False

importRoot :: BaseDir -> AbsDir
importRoot base = base </> [reldir|import|]

sampleAccountDir :: BaseDir -> RelDir -> AbsDir
sampleAccountDir base owner = importRoot base </> exampleAccount owner

-- Starter declarations and comments do not add transactions to the ledger.
directivesAreStarter :: BaseDir -> IO Bool
directivesAreStarter base = do
  let file = base </> [relfile|directives.journal|]
  exists <- pathOccupied file
  if exists
    then elem <$> readJournalBody file <*> pure [Just [], Just (journalBody starterDirectives)]
    else pure True

journalIsCommentsOnly :: AbsFile -> IO Bool
journalIsCommentsOnly file = (== Just []) <$> readJournalBody file

readJournalBody :: AbsFile -> IO (Maybe [T.Text])
readJournalBody file = do
  regular <- regularFile file
  if regular then Just . journalBody <$> TIO.readFile (toFilePath file) else pure Nothing

journalBody :: T.Text -> [T.Text]
journalBody = filter (\line -> not (T.null line || T.isPrefixOf ";" line)) . map T.strip . T.lines

plainDir :: AbsDir -> IO Bool
plainDir dir = do
  exists <- PathIO.doesDirExist dir
  if exists then not <$> PathIO.isSymlink dir else pure False

regularFile :: AbsFile -> IO Bool
regularFile file = do
  exists <- PathIO.doesFileExist file
  if exists then not <$> PathIO.isSymlink file else pure False

allM :: (a -> IO Bool) -> [a] -> IO Bool
allM check = fmap and . traverse check
