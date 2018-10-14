module Main where

import Test.HUnit

import Common
import CSVImport

test1 = TestCase (assertEqual "takeLast" [3,5,7] (takeLast 3 [1,3,5,7]))

tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests
