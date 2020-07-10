{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)

import qualified Common.Unit
import qualified Common.Integration
import qualified BaseDir.Integration
import qualified CSVImport.Unit
import qualified CSVImport.Integration

tests :: Test
tests = TestList [Common.Unit.tests, Common.Integration.tests, BaseDir.Integration.tests, CSVImport.Unit.tests, CSVImport.Integration.tests]

main :: IO Counts
main = do
  errCounts <- runTestTT tests
  if (errors errCounts > 0 || failures errCounts > 0)
    then exit $ ExitFailure 1
    else return errCounts
