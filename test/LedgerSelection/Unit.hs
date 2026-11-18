{-# LANGUAGE OverloadedStrings #-}

module LedgerSelection.Unit (tests) where

import Hledger.Flow.LedgerSelection (Coverage (..), coverageFor)
import Test.HUnit

tests :: Test
tests = "ledger coverage" ~: TestList
  [ coverageFor "index" Nothing ["wrapper", "nested", "index"] ~?= CoversLedger,
    coverageFor "index" (Just "directives") ["wrapper", "index", "directives"] ~?= CoversLedger,
    coverageFor "index" (Just "directives") ["wrapper", "index"] ~?= UnrelatedLedger,
    coverageFor "index" Nothing ["wrapper", "other"] ~?= UnrelatedLedger
  ]
