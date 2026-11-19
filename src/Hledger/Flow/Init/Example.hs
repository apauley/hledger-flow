{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Init.Example
  ( exampleImportFiles,
    exampleBaseFiles,
    untouchedExample,
  )
where

import qualified Data.ByteString as B
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hledger.Flow.DocHelpers (DocLink (RulesFiles), docURL)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile)
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
exampleBaseFiles year = paths
  [ ("directives.journal", "; Ledger-wide directives go here.\n; commodity 1,000.00 USD\n; account assets:mybank:checking\n"),
    ("prices" F.</> show year F.</> "prices.journal",
      "; Add market prices here.\n; P " <> T.pack (show year) <> "-01-01 EUR 1.10 USD\n")
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
              allowed = [source, rules, journal, account </> yearIndex, account </> [relfile|all-years.journal|]]
              dirs = [parent source, parent (parent source), parent journal, parent (parent journal)]
              importRoot = base </> [reldir|import|]
          cleanPath <- not . or <$> traverse PathIO.isSymlink [account, parent account, parent (parent account), importRoot]
          cleanContents <- if cleanPath then containsOnly dirs allowed account else return False
          if cleanContents
            then (&&) <$> matches source (sampleCsv year) <*> matches rules sampleRules
            else return False
        return (either (const False) id result)
  _ -> return False
  where
    account = base </> [reldir|import|] </> exampleAccount
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
