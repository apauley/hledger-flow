{-# LANGUAGE OverloadedStrings #-}

module HledgerConfig.Unit (tests) where

import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Hledger.Flow.HledgerConfig
import Test.HUnit

fromText :: [T.Text] -> [ConfigConflict]
fromText = configConflicts "hledger.conf" . parseConfigLines True

tests :: Test
tests = "hledger config sources" ~: TestList
  [ TestList [TestCase $ do
      let problem = firstConfigConflict ImportConfig (fromText ["# comment", "[print]", input])
      fmap (T.takeWhile (/= '\n') . renderConfigConflict) problem @?=
        Just ("hledger.conf line 3 [print] adds a journal: " <> input)
    | input <- ["-f other", "-fother", "--file other", "--file=other"]],
    TestList [isNothing (firstConfigConflict ReportConfig (fromText ["[general]", option <> " \"-f=value\""])) ~?= True
    | option <- ["--alias", "--pivot", "-b", "--begin", "-e", "--end", "-p", "--period",
                 "--depth", "-o", "--output-file", "-O", "--output-format", "--rules",
                 "--rules-file", "--today", "--value", "--conf"]],
    isJust (firstConfigConflict ReportConfig (fromText ["[balancesheet]", "--flat -f other"])) ~?= True,
    isNothing (firstConfigConflict ImportConfig (fromText ["[balance]", "--file other"])) ~?= True,
    isNothing (firstConfigConflict ReportConfig (fromText ["[files]", "--file other"])) ~?= True,
    isNothing (firstConfigConflict ReportConfig (fromText ["# -f file", "--pretty # -f file"])) ~?= True,
    isNothing (firstConfigConflict BalanceConfig (fromText ["-- --file other"])) ~?= True,
    isJust (firstConfigConflict ReportConfig (fromText ["  [balance] @arguments"])) ~?= True
  ]
