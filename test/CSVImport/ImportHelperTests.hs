{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CSVImport.ImportHelperTests where

import Hledger.Flow.Import.ImportHelpers (groupIncludesUpTo, includeFileName)
import Hledger.Flow.Import.Types (InputFileBundle)
import Path
import Test.HUnit
import TestHelpers

testToJournal :: Test
testToJournal =
  TestCase
    ( do
        let journal = toJournal [relfile|import/jane/bogartbank/investment/1-in/2020/2020-09-30.csv|]
        let expected = [relfile|import/jane/bogartbank/investment/3-journal/2020/2020-09-30.journal|]
        assertEqual "toJournal" expected journal
    )

testIncludeFileName :: Test
testIncludeFileName =
  TestCase
    ( do
        let includeFile = includeFileName [relfile|import/jane/bogartbank/investment/3-journals/2020/2020-09-30.journal|]
        assertEqual "includeFileName" [relfile|2020-include.journal|] includeFile
    )

testGroupIncludesUpToTinySet :: Test
testGroupIncludesUpToTinySet =
  TestCase
    ( do
        let expected =
              [ ([relfile|import/jane/bogartbank/savings/2017-include.journal|], [janeSavingsJournal2017]),
                ([relfile|import/jane/bogartbank/2017-include.journal|], [[relfile|import/jane/bogartbank/savings/2017-include.journal|]]),
                ([relfile|import/jane/2017-include.journal|], [[relfile|import/jane/bogartbank/2017-include.journal|]])
              ] ::
                InputFileBundle

        let grouped = groupIncludesUpTo [reldir|import/jane|] [janeSavingsJournal2017]
        assertEqual "groupIncludesUpTo: A single journal file grouping" expected grouped
    )

testGroupIncludesUpToSmallSet :: Test
testGroupIncludesUpToSmallSet =
  TestCase
    ( do
        let expected =
              [ ([relfile|import/jane/bogartbank/savings/2017-include.journal|], [janeSavingsJournal2017]),
                ([relfile|import/jane/bogartbank/savings/2018-include.journal|], janeSavingsJournals2018),
                ([relfile|import/jane/bogartbank/savings/2019-include.journal|], [janeSavingsJournal2019]),
                ([relfile|import/jane/bogartbank/2017-include.journal|], [[relfile|import/jane/bogartbank/savings/2017-include.journal|]]),
                ([relfile|import/jane/bogartbank/2018-include.journal|], [[relfile|import/jane/bogartbank/savings/2018-include.journal|]]),
                ([relfile|import/jane/bogartbank/2019-include.journal|], [[relfile|import/jane/bogartbank/savings/2019-include.journal|]]),
                ([relfile|import/jane/2017-include.journal|], [[relfile|import/jane/bogartbank/2017-include.journal|]]),
                ([relfile|import/jane/2018-include.journal|], [[relfile|import/jane/bogartbank/2018-include.journal|]]),
                ([relfile|import/jane/2019-include.journal|], [[relfile|import/jane/bogartbank/2019-include.journal|]])
              ] ::
                InputFileBundle

        let grouped = groupIncludesUpTo [reldir|import/jane|] janeSavingsJournals
        assertEqual "groupIncludesUpTo: A small set of journal files - same account over 3 years" expected grouped
    )

testGroupIncludesUpTo :: Test
testGroupIncludesUpTo =
  TestCase
    ( do
        let expected =
              [ ([relfile|import/john/bogartbank/savings/2017-include.journal|], johnSavingsJournals2017),
                ([relfile|import/john/bogartbank/savings/2018-include.journal|], johnSavingsJournals2018),
                ([relfile|import/john/bogartbank/checking/2018-include.journal|], johnCheckingJournals2018),
                ([relfile|import/john/bogartbank/checking/2019-include.journal|], johnCheckingJournals2019),
                ([relfile|import/john/otherbank/creditcard/2017-include.journal|], [johnCCJournal2017]),
                ([relfile|import/john/otherbank/creditcard/2018-include.journal|], [johnCCJournal2018]),
                ([relfile|import/john/otherbank/investments/2018-include.journal|], [johnInvestJournal2018]),
                ([relfile|import/john/otherbank/investments/2019-include.journal|], [johnInvestJournal2019]),
                ([relfile|import/jane/bogartbank/savings/2017-include.journal|], [janeSavingsJournal2017]),
                ([relfile|import/jane/bogartbank/savings/2018-include.journal|], janeSavingsJournals2018),
                ([relfile|import/jane/bogartbank/savings/2019-include.journal|], [janeSavingsJournal2019]),
                ([relfile|import/jane/otherbank/creditcard/2017-include.journal|], [janeCCJournal2017]),
                ([relfile|import/jane/otherbank/creditcard/2018-include.journal|], [janeCCJournal2018]),
                ([relfile|import/jane/otherbank/investments/2018-include.journal|], [janeInvestJournal2018]),
                ([relfile|import/jane/otherbank/investments/2019-include.journal|], [janeInvestJournal2019]),
                ([relfile|import/john/bogartbank/2017-include.journal|], [[relfile|import/john/bogartbank/savings/2017-include.journal|]]),
                ([relfile|import/john/bogartbank/2018-include.journal|], [[relfile|import/john/bogartbank/checking/2018-include.journal|], [relfile|import/john/bogartbank/savings/2018-include.journal|]]),
                ([relfile|import/john/bogartbank/2019-include.journal|], [[relfile|import/john/bogartbank/checking/2019-include.journal|]]),
                ([relfile|import/john/otherbank/2017-include.journal|], [[relfile|import/john/otherbank/creditcard/2017-include.journal|]]),
                ([relfile|import/john/otherbank/2018-include.journal|], [[relfile|import/john/otherbank/creditcard/2018-include.journal|], [relfile|import/john/otherbank/investments/2018-include.journal|]]),
                ([relfile|import/john/otherbank/2019-include.journal|], [[relfile|import/john/otherbank/investments/2019-include.journal|]]),
                ([relfile|import/jane/bogartbank/2017-include.journal|], [[relfile|import/jane/bogartbank/savings/2017-include.journal|]]),
                ([relfile|import/jane/bogartbank/2018-include.journal|], [[relfile|import/jane/bogartbank/savings/2018-include.journal|]]),
                ([relfile|import/jane/bogartbank/2019-include.journal|], [[relfile|import/jane/bogartbank/savings/2019-include.journal|]]),
                ([relfile|import/jane/otherbank/2017-include.journal|], [[relfile|import/jane/otherbank/creditcard/2017-include.journal|]]),
                ([relfile|import/jane/otherbank/2018-include.journal|], [[relfile|import/jane/otherbank/creditcard/2018-include.journal|], [relfile|import/jane/otherbank/investments/2018-include.journal|]]),
                ([relfile|import/jane/otherbank/2019-include.journal|], [[relfile|import/jane/otherbank/investments/2019-include.journal|]]),
                ([relfile|import/john/2017-include.journal|], [[relfile|import/john/bogartbank/2017-include.journal|], [relfile|import/john/otherbank/2017-include.journal|]]),
                ([relfile|import/john/2018-include.journal|], [[relfile|import/john/bogartbank/2018-include.journal|], [relfile|import/john/otherbank/2018-include.journal|]]),
                ([relfile|import/john/2019-include.journal|], [[relfile|import/john/bogartbank/2019-include.journal|], [relfile|import/john/otherbank/2019-include.journal|]]),
                ([relfile|import/jane/2017-include.journal|], [[relfile|import/jane/bogartbank/2017-include.journal|], [relfile|import/jane/otherbank/2017-include.journal|]]),
                ([relfile|import/jane/2018-include.journal|], [[relfile|import/jane/bogartbank/2018-include.journal|], [relfile|import/jane/otherbank/2018-include.journal|]]),
                ([relfile|import/jane/2019-include.journal|], [[relfile|import/jane/bogartbank/2019-include.journal|], [relfile|import/jane/otherbank/2019-include.journal|]]),
                ([relfile|import/2017-include.journal|], [[relfile|import/jane/2017-include.journal|], [relfile|import/john/2017-include.journal|]]),
                ([relfile|import/2018-include.journal|], [[relfile|import/jane/2018-include.journal|], [relfile|import/john/2018-include.journal|]]),
                ([relfile|import/2019-include.journal|], [[relfile|import/jane/2019-include.journal|], [relfile|import/john/2019-include.journal|]])
              ] ::
                InputFileBundle

        let grouped = groupIncludesUpTo [reldir|import|] journalFiles
        assertEqual "groupIncludesUpTo: A full set of journal files" expected grouped
    )

tests :: Test
tests = TestList [testToJournal, testIncludeFileName, testGroupIncludesUpToTinySet, testGroupIncludesUpToSmallSet, testGroupIncludesUpTo]
