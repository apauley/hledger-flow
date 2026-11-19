{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Init.Example
  ( exampleImportFiles,
    exampleBaseFiles,
    sampleAccountDir,
    untouchedExample,
    directivesAreStarter,
    journalIsCommentsOnly,
  )
where

import qualified Data.ByteString as B
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Hledger.Flow.DocHelpers (DocLink (RulesFiles), docURL)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, pathOccupied)
import Hledger.Flow.Types (BaseDir)
import Path
import qualified Path.IO as PathIO
import qualified System.FilePath as F
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

exampleAccount :: RelDir
exampleAccount = [reldir|me/mybank/checking|]

sampleName :: RelFile
sampleName = [relfile|hledger-flow-example.csv|]

rulesName :: RelFile
rulesName = [relfile|mybank-checking.rules|]

sampleCsv :: Integer -> T.Text
sampleCsv year = T.unlines
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

exampleImportFiles :: Integer -> [(RelFile, T.Text)]
exampleImportFiles year =
  (exampleAccount </> rulesName, sampleRules)
    : paths
      [ (toFilePath exampleAccount F.</> "1-in" F.</> show year F.</> toFilePath sampleName, sampleCsv year),
        ("_manual_" F.</> show year F.</> "pre-import.journal", manual "before"),
        ("_manual_" F.</> show year F.</> "post-import.journal", manual "after")
      ]
  where
    manual place = T.unlines
      [ "; Put transactions here that should appear " <> place <> " imported statements.",
        "; " <> T.pack (show year) <> "-01-01 Opening balance",
        ";     assets:mybank:checking  100 USD",
        ";     equity:opening balances"
      ]

exampleBaseFiles :: Integer -> [(RelFile, T.Text)]
exampleBaseFiles year = paths $
  [("directives.journal", starterDirectives)]
    ++ [ (priceDir F.</> name, T.unlines ["; Add market prices here.", "; P " <> date <> " " <> quote])
         | (name, quote) <- pairs
       ]
    ++ [(priceDir F.</> "prices.journal", T.unlines ["include " <> T.pack name | (name, _) <- pairs])]
  where
    priceDir = "prices" F.</> show year
    date = T.pack (show year) <> "-01-01"
    pairs = [("EUR-USD.prices", "EUR 1.10 USD"), ("BTC-USD.prices", "BTC 99000.00 USD")]

starterDirectives :: T.Text
starterDirectives = T.unlines
  [ "; Ledger-wide directives go here. https://hledger.org/dev/hledger.html#directives",
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

untouchedExample :: BaseDir -> [AbsFile] -> IO Bool
untouchedExample base [source] = case stripProperPrefix account source of
  Just rel
    | ["1-in", yearName, name] <- F.splitDirectories (toFilePath rel),
      name == toFilePath sampleName,
      Just year <- readMaybe yearName,
      Just generated <- parseRelFile ("3-journal" F.</> yearName F.</> "hledger-flow-example.journal"),
      Just yearIndex <- parseRelFile (yearName ++ "-include.journal") -> do
        result <- tryIOError $ do
          let rules = account </> rulesName
              journal = account </> generated
              bank = parent account
              owner = parent bank
              indexes dir = [dir </> yearIndex, dir </> [relfile|all-years.journal|]]
              allowed = [source, rules, journal] ++ concatMap indexes [owner, bank, account]
              dirs = [bank, account, parent source, parent (parent source), parent journal, parent (parent journal)]
          cleanPath <- allM plainDir [importRoot base, owner]
          cleanContents <- if cleanPath then containsOnly dirs allowed owner else return False
          if cleanContents
            then (&&) <$> matches source (sampleCsv year) <*> matches rules sampleRules
            else return False
        return (either (const False) id result)
  _ -> return False
  where
    account = sampleAccountDir base
    matches file expected = (== T.encodeUtf8 expected) <$> B.readFile (toFilePath file)
untouchedExample _ _ = return False

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

sampleAccountDir :: BaseDir -> AbsDir
sampleAccountDir base = importRoot base </> exampleAccount

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
