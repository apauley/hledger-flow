{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold

import Common

files = ["dir1/d1f1.journal", "dir1/d1f2.journal", "dir2/d2f1.journal", "dir2/d2f2.journal"] :: [FilePath]

test1 = TestCase (assertEqual "takeLast" [3,5,7] (takeLast 3 [1,3,5,7]))

testGroupBy = TestCase (do
                           let expected = [("dir1","dir1/d1f1.journal"), ("dir1","dir1/d1f2.journal"),
                                           ("dir2","dir2/d2f1.journal"), ("dir2","dir2/d2f2.journal")] :: [(FilePath, FilePath)]
                           let grouped = groupBy dirname $ select files :: Shell (Map.Map FilePath FilePath)
                           let shList = fmap Map.assocs grouped :: Shell [(FilePath, FilePath)]
                           actual <- single shList
                           assertEqual "Group Files by Dir" expected actual)

tests = TestList [test1, testGroupBy]

main :: IO Counts
main = do
  counts <- runTestTT tests
  if (errors counts > 0 || failures counts > 0)
    then exit $ ExitFailure 1
    else return counts
