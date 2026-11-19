{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.DocHelpers
  ( DocLink (..),
    docSlug,
    docURL,
    siteURL,
  )
where

import Data.Text (Text)

-- | Every documentation link the CLI prints. Each one is a permanent
-- @/go/<slug>/@ URL that redirects to the current page and section, so
-- reorganising the site never breaks links printed by released binaries.
-- Redirect targets live in @site/go-links.json@; a test checks every
-- constructor has one. Never rename a slug once released.
data DocLink
  = Docs
  | GettingStarted
  | IncrementalImportFallback
  | InputFiles
  | StatementYearFolders
  | RulesFiles
  | PriceFiles
  | ManualJournals
  | Balances
  | PreprocessScript
  | SetLedgerFile
  | StatusCommand
  | Reports
  | ReviewUnknownTransactions
  | ReviewTransfers
  deriving (Bounded, Enum, Eq, Show)

docSlug :: DocLink -> Text
docSlug Docs = "docs"
docSlug GettingStarted = "getting-started"
docSlug IncrementalImportFallback = "incremental-import-fallback"
docSlug InputFiles = "input-files"
docSlug StatementYearFolders = "statement-year-folders"
docSlug RulesFiles = "rules-files"
docSlug PriceFiles = "price-files"
docSlug ManualJournals = "manual-journals"
docSlug Balances = "balances"
docSlug PreprocessScript = "preprocess-script"
docSlug SetLedgerFile = "set-ledger-file"
docSlug StatusCommand = "status"
docSlug Reports = "reports"
docSlug ReviewUnknownTransactions = "review-unknown-transactions"
docSlug ReviewTransfers = "review-transfers"

siteURL :: Text
siteURL = "https://hledger-flow.github.io/"

docURL :: DocLink -> Text
docURL link = siteURL <> "go/" <> docSlug link <> "/"
