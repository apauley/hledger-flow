{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.DocHelpers where

import qualified Data.Text as T (Text)
import qualified Turtle as Turtle (Line, format, l)
import Turtle ((%))

docURL :: Turtle.Line -> T.Text
docURL = Turtle.format ("https://github.com/apauley/hledger-flow#"%Turtle.l)
