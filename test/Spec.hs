{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold

import Common

test1 = TestCase (assertEqual "takeLast" [3,5,7] (takeLast 3 [1,3,5,7]))

testGroupShell = TestCase (do
                              let files = ["dir1/d1f1", "dir1/d1f2", "dir2/d2f1", "dir2/d2f2"] :: [FilePath]
                              let shFiles = select files :: Shell FilePath
                              let expected = [[("dir1","dir1/d1f1"), ("dir1","dir1/d1f2"), ("dir2","dir2/d2f1"), ("dir2","dir2/d2f2")]] :: [[(FilePath, FilePath)]]
                              let shList = fmap Map.assocs (groupShell dirname shFiles) :: Shell [(FilePath, FilePath)]
                              actual <- fold shList Fold.list
                              assertEqual "Group Files by Dir" expected actual)

tests = TestList [test1, testGroupShell]

main :: IO Counts
main = do
  counts <- runTestTT tests
  if (errors counts > 0 || failures counts > 0)
    then exit $ ExitFailure 1
    else return counts
