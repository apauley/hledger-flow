{-# LANGUAGE QuasiQuotes #-}

module PathHelpers.Unit where

import Hledger.Flow.PathHelpers
import Path
import Test.HUnit

testPathSize :: Test
testPathSize =
  TestCase
    ( do
        let d0 = [reldir|.|]
        let d1 = [reldir|d1|]
        let d1ond0 = d0 </> [reldir|d1|]
        let d2 = d1 </> [reldir|d2|]
        let d3 = d2 </> [reldir|d3|]
        assertEqual "Calculate the path size correctly" 0 (pathSize d0)
        assertEqual "Calculate the path size correctly" 1 (pathSize d1)
        assertEqual "Calculate the path size correctly" 1 (pathSize d1ond0)
        assertEqual "Calculate the path size correctly" 2 (pathSize d2)
        assertEqual "Calculate the path size correctly" 3 (pathSize d3)
    )

tests :: Test
tests = TestList [testPathSize]
