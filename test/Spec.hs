{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)

import qualified Unit
import qualified Integration

tests = TestList [Unit.tests, Integration.tests]

main :: IO Counts
main = do
  counts <- runTestTT tests
  if (errors counts > 0 || failures counts > 0)
    then exit $ ExitFailure 1
    else return counts
