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
  | InputFiles
  | RulesFiles
  | PreprocessScript
  | SetLedgerFile
  deriving (Bounded, Enum, Eq, Show)

docSlug :: DocLink -> Text
docSlug Docs = "docs"
docSlug GettingStarted = "getting-started"
docSlug InputFiles = "input-files"
docSlug RulesFiles = "rules-files"
docSlug PreprocessScript = "preprocess-script"
docSlug SetLedgerFile = "set-ledger-file"

siteURL :: Text
siteURL = "https://hledger-flow.github.io/"

docURL :: DocLink -> Text
docURL link = siteURL <> "go/" <> docSlug link <> "/"
