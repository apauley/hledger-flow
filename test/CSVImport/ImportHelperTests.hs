{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module CSVImport.ImportHelperTests where

import Test.HUnit

import Hledger.Flow.PathHelpers (RelFile)
import Hledger.Flow.Import.Types (InputFileBundle)
import Hledger.Flow.Import.ImportHelpers (groupIncludesUpTo)

import Path

journal2017 :: RelFile
journal2017 = [relfile|import/jane/bogartbank/savings/3-journals/2017/2017-12-30.journal|]

journals2018 :: [RelFile]
journals2018 = [
    [relfile|import/jane/bogartbank/savings/3-journals/2018/2018-01-30.journal|]
  , [relfile|import/jane/bogartbank/savings/3-journals/2018/2018-12-30.journal|]
  ]

journal2019 :: RelFile
journal2019 = [relfile|import/jane/bogartbank/savings/3-journals/2019/2019-01-30.journal|]

journalsSmall :: [RelFile]
journalsSmall = [journal2017] ++ journals2018 ++ [journal2019]

testGroupIncludesUpToTinySet :: Test
testGroupIncludesUpToTinySet = TestCase (
  do
    let expected = [
           ([relfile|import/jane/bogartbank/savings/2017-include.journal|], [journal2017])
         , ([relfile|import/jane/bogartbank/2017-include.journal|], [[relfile|import/jane/bogartbank/savings/2017-include.journal|]])
         , ([relfile|import/jane/2017-include.journal|],            [[relfile|import/jane/bogartbank/2017-include.journal|]])
         ] :: InputFileBundle

    let grouped = groupIncludesUpTo [reldir|import/jane|] [journal2017]
    assertEqual "groupIncludesUpTo: A single journal file grouping" expected grouped
  )

testGroupIncludesUpToSmallSet :: Test
testGroupIncludesUpToSmallSet = TestCase (
  do
    let expected = [
           ([relfile|import/jane/bogartbank/savings/2017-include.journal|], [journal2017])
         , ([relfile|import/jane/bogartbank/savings/2018-include.journal|], journals2018)
         , ([relfile|import/jane/bogartbank/savings/2019-include.journal|], [journal2019])
         , ([relfile|import/jane/bogartbank/2017-include.journal|], [[relfile|import/jane/bogartbank/savings/2017-include.journal|]])
         , ([relfile|import/jane/bogartbank/2018-include.journal|], [[relfile|import/jane/bogartbank/savings/2018-include.journal|]])
         , ([relfile|import/jane/bogartbank/2019-include.journal|], [[relfile|import/jane/bogartbank/savings/2019-include.journal|]])
         , ([relfile|import/jane/2017-include.journal|],            [[relfile|import/jane/bogartbank/2017-include.journal|]])
         , ([relfile|import/jane/2018-include.journal|],            [[relfile|import/jane/bogartbank/2018-include.journal|]])
         , ([relfile|import/jane/2019-include.journal|],            [[relfile|import/jane/bogartbank/2019-include.journal|]])
         ] :: InputFileBundle

    let grouped = groupIncludesUpTo [reldir|import/jane|] journalsSmall
    assertEqual "groupIncludesUpTo: A small set of yournal files - same account over 3 years" expected grouped
  )

tests :: Test
tests = TestList [testGroupIncludesUpToTinySet, testGroupIncludesUpToSmallSet]
