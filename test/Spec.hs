{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified BaseDir.Integration
import qualified CSVImport.Integration
import qualified CSVImport.Unit
import qualified Common.Integration
import qualified Common.Unit
import qualified Environment.Unit
import qualified ImportHelpers.Integration
import qualified Incremental.Cache
import qualified Incremental.Dependencies
import qualified Incremental.Platform
import qualified Incremental.Snapshot
import qualified Init.Integration
import qualified LedgerSelection.Unit
import qualified PathHelpers.Unit
import qualified ReleaseNotice.Unit
import qualified RulesTemplate.Unit
import qualified RuntimeOptions.Unit
import qualified Status.Integration
import qualified Status.Unit
import Test.HUnit
import Control.Monad (unless, when)
import System.Environment (getArgs)
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
      Status.Unit.tests,
      Status.Integration.tests,
      Incremental.Cache.tests,
      Incremental.Dependencies.tests,
      Incremental.Platform.tests,
      Incremental.Snapshot.tests,
      Init.Integration.tests,
      LedgerSelection.Unit.tests,
      ReleaseNotice.Unit.tests,
      RulesTemplate.Unit.tests,
      RuntimeOptions.Unit.tests,
      Environment.Unit.tests
    ]

main :: IO ()
main = do
  child <- getArgs >>= Incremental.Snapshot.runWorker
  unless child $ do
    results <- runTestTT tests
    when (errors results > 0 || failures results > 0) exitFailure
