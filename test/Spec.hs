{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified BaseDir.Integration
import qualified CSVImport.Integration
import qualified CSVImport.Unit
import qualified Common.Integration
import qualified Common.Unit
import qualified ImportHelpers.Integration
import qualified PathHelpers.Unit
import qualified Status.Unit
import Test.HUnit
import System.Exit (exitFailure)

tests :: Test
tests =
  TestList
    [ Common.Unit.tests,
      Common.Integration.tests,
      PathHelpers.Unit.tests,
      BaseDir.Integration.tests,
      ImportHelpers.Integration.tests,
      CSVImport.Unit.tests,
      CSVImport.Integration.tests,
      Status.Unit.tests
    ]

main :: IO Counts
main = do
  errCounts <- runTestTT tests
  if (errors errCounts > 0 || failures errCounts > 0)
    then exitFailure
    else return errCounts
