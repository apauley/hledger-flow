{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.DocHelpers where

import Data.Text (Text)

docURL :: Text -> Text
docURL section = "https://github.com/apauley/hledger-flow/tree/master/docs#" <> section
