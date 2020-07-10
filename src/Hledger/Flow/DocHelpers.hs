{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.DocHelpers where

import Data.Text
import Turtle

docURL :: Line -> Text
docURL = format ("https://github.com/apauley/hledger-flow#"%l)
