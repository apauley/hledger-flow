{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module CSVImport.ImportHelperTurtleTests where

import Test.HUnit
import Path
import Turtle
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM

import TestHelpers
import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.Common
import Hledger.Flow.Import.Types (TurtleFileBundle)
import Hledger.Flow.Import.ImportHelpersTurtle (allYearIncludeFiles, groupIncludeFiles, toIncludeFiles, toIncludeLine, yearsIncludeMap)

import Data.Either
import qualified Data.Text as T

groupedJaneBogart :: TurtleFileBundle
groupedJaneBogart = [
  ("./import/jane/bogartbank/checking/2018-include.journal",
   ["import/jane/bogartbank/checking/3-journal/2018/2018-12-30.journal"]),
  ("./import/jane/bogartbank/checking/2019-include.journal",
   ["import/jane/bogartbank/checking/3-journal/2019/2019-01-30.journal"]),
  ("./import/jane/bogartbank/savings/2017-include.journal",
   ["import/jane/bogartbank/savings/3-journal/2017/2017-12-30.journal"]),
  ("./import/jane/bogartbank/savings/2018-include.journal",
   ["import/jane/bogartbank/savings/3-journal/2018/2018-01-30.journal"])]

groupedJaneOther :: TurtleFileBundle
groupedJaneOther = [
  ("./import/jane/otherbank/creditcard/2017-include.journal",
   ["import/jane/otherbank/creditcard/3-journal/2017/2017-12-30.journal"]),
  ("./import/jane/otherbank/creditcard/2018-include.journal",
   ["import/jane/otherbank/creditcard/3-journal/2018/2018-01-30.journal"]),
  ("./import/jane/otherbank/investments/2018-include.journal",
   ["import/jane/otherbank/investments/3-journal/2018/2018-12-30.journal"]),
  ("./import/jane/otherbank/investments/2019-include.journal",
   ["import/jane/otherbank/investments/3-journal/2019/2019-01-30.journal"])]

groupedJohnBogart :: TurtleFileBundle
groupedJohnBogart = [
  ("./import/john/bogartbank/checking/2018-include.journal",
   ["import/john/bogartbank/checking/3-journal/2018/2018-11-30.journal",
    "import/john/bogartbank/checking/3-journal/2018/2018-10-30.journal",
    "import/john/bogartbank/checking/3-journal/2018/2018-12-30.journal"]),
  ("./import/john/bogartbank/checking/2019-include.journal",
   ["import/john/bogartbank/checking/3-journal/2019/2019-01-30.journal",
    "import/john/bogartbank/checking/3-journal/2019/2019-02-30.journal"]),
  ("./import/john/bogartbank/savings/2017-include.journal",
   ["import/john/bogartbank/savings/3-journal/2017/2017-11-30.journal",
    "import/john/bogartbank/savings/3-journal/2017/2017-12-30.journal"]),
  ("./import/john/bogartbank/savings/2018-include.journal",
   ["import/john/bogartbank/savings/3-journal/2018/2018-02-30.journal",
    "import/john/bogartbank/savings/3-journal/2018/2018-01-30.journal"])]

groupedJohnOther :: TurtleFileBundle
groupedJohnOther = [
  ("./import/john/otherbank/creditcard/2017-include.journal",
   ["import/john/otherbank/creditcard/3-journal/2017/2017-12-30.journal"]),
  ("./import/john/otherbank/creditcard/2018-include.journal",
   ["import/john/otherbank/creditcard/3-journal/2018/2018-01-30.journal"]),
  ("./import/john/otherbank/investments/2018-include.journal",
   ["import/john/otherbank/investments/3-journal/2018/2018-12-30.journal"]),
  ("./import/john/otherbank/investments/2019-include.journal",
   ["import/john/otherbank/investments/3-journal/2019/2019-01-30.journal"])]

groupedIncludeFiles :: TurtleFileBundle
groupedIncludeFiles = groupedJaneBogart <> groupedJaneOther <>
                      groupedJohnBogart <> groupedJohnOther

testYearsIncludeMap :: Test
testYearsIncludeMap = TestCase (
  do
    let maps = allYearIncludeFiles groupedJohnOther
    let yearGrouping = [("./import/john/otherbank/creditcard/all-years.journal",
                         ["./import/john/otherbank/creditcard/2017-include.journal",
                          "./import/john/otherbank/creditcard/2018-include.journal"]),
                        ("./import/john/otherbank/investments/all-years.journal",
                         ["./import/john/otherbank/investments/2018-include.journal",
                          "./import/john/otherbank/investments/2019-include.journal"])]
    let expected = (groupedJohnOther, yearGrouping)
    assertEqual "An augmented map with grouped years per level added" expected maps
  )

testYearsIncludeGrouping :: Test
testYearsIncludeGrouping = TestCase (
  do
    let yearsMap = yearsIncludeMap (Map.keys groupedJohnOther)
    let expected = [("./import/john/otherbank/creditcard/all-years.journal",
                     ["./import/john/otherbank/creditcard/2017-include.journal",
                      "./import/john/otherbank/creditcard/2018-include.journal"]),
                    ("./import/john/otherbank/investments/all-years.journal",
                     ["./import/john/otherbank/investments/2018-include.journal",
                      "./import/john/otherbank/investments/2019-include.journal"])]
    assertEqual "A basic map with grouped years per level" expected yearsMap
  )

testGroupIncludeFilesTinySet :: Test
testGroupIncludeFilesTinySet = TestCase (
  do
    let journals1 = [   "import/jane/bogartbank/savings/3-journals/2017/2017-12-30.journal"]
    let expected1 = [("./import/jane/bogartbank/savings/2017-include.journal", journals1)] :: TurtleFileBundle
    let expectedAllYears1 = [("./import/jane/bogartbank/savings/all-years.journal", ["./import/jane/bogartbank/savings/2017-include.journal"])]
    let (group1, allYears1) = groupIncludeFiles journals1
    assertEqual "groupIncludeFiles small allYears 1" expectedAllYears1 allYears1
    assertEqual "groupIncludeFiles small set 1" expected1 group1

    let journals2 = [(fst . head . Map.toList) expected1] :: [TurtlePath]
    let expected2 = [("./import/jane/bogartbank/2017-include.journal", journals2)] :: TurtleFileBundle
    let expectedAllYears2 = [("./import/jane/bogartbank/all-years.journal", ["./import/jane/bogartbank/2017-include.journal"])]
    let (group2, allYears2) = groupIncludeFiles journals2
    assertEqual "groupIncludeFiles small allYears 2" expectedAllYears2 allYears2
    assertEqual "groupIncludeFiles small set 2" expected2 group2

    let journals3 = [(fst . head . Map.toList) expected2] :: [TurtlePath]
    let expected3 = [("./import/jane/2017-include.journal", journals3)] :: TurtleFileBundle
    let expectedAllYears3 = [("./import/jane/all-years.journal", ["./import/jane/2017-include.journal"])]
    let (group3, allYears3) = groupIncludeFiles journals3
    assertEqual "groupIncludeFiles small allYears 3" expectedAllYears3 allYears3
    assertEqual "groupIncludeFiles small set 3" expected3 group3
  )

testGroupIncludeFilesSmallSet :: Test
testGroupIncludeFilesSmallSet = TestCase (
  do
    let (group1, allYears1) = groupIncludeFiles (toJournals inputJaneBogart)
    let expectedAllYears1 = [
          ("./import/jane/bogartbank/checking/all-years.journal",
           ["./import/jane/bogartbank/checking/2018-include.journal",
            "./import/jane/bogartbank/checking/2019-include.journal"]),
          ("./import/jane/bogartbank/savings/all-years.journal",
           ["./import/jane/bogartbank/savings/2017-include.journal",
            "./import/jane/bogartbank/savings/2018-include.journal"])]

    assertEqual "groupIncludeFiles Jane AllYears 1" expectedAllYears1 allYears1
    assertEqual "groupIncludeFiles Jane 1" groupedJaneBogart group1

    let (group2, allYears2) = groupIncludeFiles (Map.keys group1)
    let expectedAllYears2 = [
          ("./import/jane/bogartbank/all-years.journal",
           ["./import/jane/bogartbank/2017-include.journal",
            "./import/jane/bogartbank/2018-include.journal",
            "./import/jane/bogartbank/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 2" expectedAllYears2 allYears2
    let expectedGroup2 = [
          ("./import/jane/bogartbank/2017-include.journal",
           ["./import/jane/bogartbank/savings/2017-include.journal"]),
          ("./import/jane/bogartbank/2018-include.journal",
           ["./import/jane/bogartbank/checking/2018-include.journal",
            "./import/jane/bogartbank/savings/2018-include.journal"]),
          ("./import/jane/bogartbank/2019-include.journal",
           ["./import/jane/bogartbank/checking/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane 2" expectedGroup2 group2

    let (group3, allYears3) = groupIncludeFiles (Map.keys group2)
    let expectedAllYears3 = [
          ("./import/jane/all-years.journal",
           ["./import/jane/2017-include.journal",
            "./import/jane/2018-include.journal",
            "./import/jane/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 3" expectedAllYears3 allYears3
    let expectedGroup3 = [
          ("./import/jane/2017-include.journal",
           ["./import/jane/bogartbank/2017-include.journal"]),
          ("./import/jane/2018-include.journal",
           ["./import/jane/bogartbank/2018-include.journal"]),
          ("./import/jane/2019-include.journal",
           ["./import/jane/bogartbank/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane 3" expectedGroup3 group3

    let (group4, allYears4) = groupIncludeFiles (Map.keys group3)
    let expectedAllYears4 = [
          ("./import/all-years.journal",
           ["./import/2017-include.journal",
            "./import/2018-include.journal",
            "./import/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 4" expectedAllYears4 allYears4
    let expectedGroup4 = [
          ("./import/2017-include.journal",
           ["./import/jane/2017-include.journal"]),
          ("./import/2018-include.journal",
           ["./import/jane/2018-include.journal"]),
          ("./import/2019-include.journal",
           ["./import/jane/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane 4" expectedGroup4 group4

    let (group5, allYears5) = groupIncludeFiles (Map.keys group4)
    let expectedAllYears5 = [("./all-years.journal", ["./2017-include.journal", "./2018-include.journal", "./2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 5" expectedAllYears5 allYears5
    let expectedGroup5 = [
          ("./2017-include.journal",
           ["./import/2017-include.journal"]),
          ("./2018-include.journal",
           ["./import/2018-include.journal"]),
          ("./2019-include.journal",
           ["./import/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane 5" expectedGroup5 group5
 )

testGroupIncludeFiles :: Test
testGroupIncludeFiles = TestCase (
  do
    let (group1, allYears1) = groupIncludeFiles journalFiles
    let expectedAllYears1 = [
          ("./import/jane/bogartbank/checking/all-years.journal",
            ["./import/jane/bogartbank/checking/2018-include.journal",
             "./import/jane/bogartbank/checking/2019-include.journal"]),
          ("./import/jane/bogartbank/savings/all-years.journal",
            ["./import/jane/bogartbank/savings/2017-include.journal",
             "./import/jane/bogartbank/savings/2018-include.journal"]),
          ("./import/jane/otherbank/creditcard/all-years.journal",
            ["./import/jane/otherbank/creditcard/2017-include.journal",
             "./import/jane/otherbank/creditcard/2018-include.journal"]),
          ("./import/jane/otherbank/investments/all-years.journal",
            ["./import/jane/otherbank/investments/2018-include.journal",
             "./import/jane/otherbank/investments/2019-include.journal"]),
          ("./import/john/bogartbank/checking/all-years.journal",
            ["./import/john/bogartbank/checking/2018-include.journal",
             "./import/john/bogartbank/checking/2019-include.journal"]),
          ("./import/john/bogartbank/savings/all-years.journal",
            ["./import/john/bogartbank/savings/2017-include.journal",
             "./import/john/bogartbank/savings/2018-include.journal"]),
          ("./import/john/otherbank/creditcard/all-years.journal",
            ["./import/john/otherbank/creditcard/2017-include.journal",
             "./import/john/otherbank/creditcard/2018-include.journal"]),
          ("./import/john/otherbank/investments/all-years.journal",
            ["./import/john/otherbank/investments/2018-include.journal",
             "./import/john/otherbank/investments/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 1" expectedAllYears1 allYears1
    assertEqual "groupIncludeFiles 1" groupedIncludeFiles group1

    let (group2, allYears2) = groupIncludeFiles (Map.keys group1)
    let expectedAllYears2 = [
          ("./import/jane/bogartbank/all-years.journal",
            ["./import/jane/bogartbank/2017-include.journal",
             "./import/jane/bogartbank/2018-include.journal",
             "./import/jane/bogartbank/2019-include.journal"]),
          ("./import/jane/otherbank/all-years.journal",
            ["./import/jane/otherbank/2017-include.journal",
             "./import/jane/otherbank/2018-include.journal",
             "./import/jane/otherbank/2019-include.journal"]),
          ("./import/john/bogartbank/all-years.journal",
            ["./import/john/bogartbank/2017-include.journal",
             "./import/john/bogartbank/2018-include.journal",
             "./import/john/bogartbank/2019-include.journal"]),
          ("./import/john/otherbank/all-years.journal",
            ["./import/john/otherbank/2017-include.journal",
             "./import/john/otherbank/2018-include.journal",
             "./import/john/otherbank/2019-include.journal"])]

    assertEqual "groupIncludeFiles Jane AllYears 2" expectedAllYears2 allYears2
    let expectedGroup2 = [("./import/jane/bogartbank/2017-include.journal",
                           ["./import/jane/bogartbank/savings/2017-include.journal"]),
                          ("./import/jane/bogartbank/2018-include.journal",
                           ["./import/jane/bogartbank/checking/2018-include.journal",
                            "./import/jane/bogartbank/savings/2018-include.journal"]),
                          ("./import/jane/bogartbank/2019-include.journal",
                           ["./import/jane/bogartbank/checking/2019-include.journal"]),

                          ("./import/jane/otherbank/2017-include.journal",
                           ["./import/jane/otherbank/creditcard/2017-include.journal"]),
                          ("./import/jane/otherbank/2018-include.journal",
                           ["./import/jane/otherbank/creditcard/2018-include.journal",
                            "./import/jane/otherbank/investments/2018-include.journal"]),
                          ("./import/jane/otherbank/2019-include.journal",
                           ["./import/jane/otherbank/investments/2019-include.journal"]),

                          ("./import/john/bogartbank/2017-include.journal",
                           ["./import/john/bogartbank/savings/2017-include.journal"]),
                          ("./import/john/bogartbank/2018-include.journal",
                           ["./import/john/bogartbank/checking/2018-include.journal",
                            "./import/john/bogartbank/savings/2018-include.journal"]),
                          ("./import/john/bogartbank/2019-include.journal",
                           ["./import/john/bogartbank/checking/2019-include.journal"]),

                          ("./import/john/otherbank/2017-include.journal",
                           ["./import/john/otherbank/creditcard/2017-include.journal"]),
                          ("./import/john/otherbank/2018-include.journal",
                           ["./import/john/otherbank/creditcard/2018-include.journal",
                            "./import/john/otherbank/investments/2018-include.journal"]),
                          ("./import/john/otherbank/2019-include.journal",
                           ["./import/john/otherbank/investments/2019-include.journal"])]
    assertEqual "groupIncludeFiles 2 - diff 1" [] (expectedGroup2 Map.\\ group2)
    assertEqual "groupIncludeFiles 2 - diff 2" [] (group2 Map.\\ expectedGroup2)
    assertEqual "groupIncludeFiles 2" expectedGroup2 group2

    let (group3, allYears3) = groupIncludeFiles (Map.keys group2)
    let expectedAllYears3 = [
          ("./import/jane/all-years.journal",
            ["./import/jane/2017-include.journal",
             "./import/jane/2018-include.journal",
             "./import/jane/2019-include.journal"]),
          ("./import/john/all-years.journal",
            ["./import/john/2017-include.journal",
             "./import/john/2018-include.journal",
             "./import/john/2019-include.journal"])]

    assertEqual "groupIncludeFiles Jane AllYears 3" expectedAllYears3 allYears3
    let expectedGroup3 = [("./import/jane/2017-include.journal",
                           ["./import/jane/bogartbank/2017-include.journal",
                            "./import/jane/otherbank/2017-include.journal"]),
                          ("./import/jane/2018-include.journal",
                           ["./import/jane/bogartbank/2018-include.journal",
                            "./import/jane/otherbank/2018-include.journal"]),
                          ("./import/jane/2019-include.journal",
                           ["./import/jane/bogartbank/2019-include.journal",
                            "./import/jane/otherbank/2019-include.journal"]),
                          ("./import/john/2017-include.journal",
                           ["./import/john/bogartbank/2017-include.journal",
                            "./import/john/otherbank/2017-include.journal"]),
                          ("./import/john/2018-include.journal",
                           ["./import/john/bogartbank/2018-include.journal",
                            "./import/john/otherbank/2018-include.journal"]),
                          ("./import/john/2019-include.journal",
                           ["./import/john/bogartbank/2019-include.journal",
                            "./import/john/otherbank/2019-include.journal"])]
    assertEqual "groupIncludeFiles 3 - diff 1" [] (expectedGroup3 Map.\\ group3)
    assertEqual "groupIncludeFiles 3 - diff 2" [] (group3 Map.\\ expectedGroup3)
    assertEqual "groupIncludeFiles 3" expectedGroup3 group3

    let (group4, allYears4) = groupIncludeFiles (Map.keys group3)
    let expectedAllYears4 = [
          ("./import/all-years.journal",
            ["./import/2017-include.journal",
             "./import/2018-include.journal",
             "./import/2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 4" expectedAllYears4 allYears4
    let expectedGroup4 = [("./import/2017-include.journal",
                           ["./import/jane/2017-include.journal",
                            "./import/john/2017-include.journal"]),
                          ("./import/2018-include.journal",
                           ["./import/jane/2018-include.journal",
                            "./import/john/2018-include.journal"]),
                          ("./import/2019-include.journal",
                           ["./import/jane/2019-include.journal",
                            "./import/john/2019-include.journal"])]
    assertEqual "groupIncludeFiles 4 - diff 1" [] (expectedGroup4 Map.\\ group4)
    assertEqual "groupIncludeFiles 4 - diff 2" [] (group4 Map.\\ expectedGroup4)
    assertEqual "groupIncludeFiles 4" expectedGroup4 group4

    let (group5, allYears5) = groupIncludeFiles (Map.keys group4)
    let expectedAllYears5 = [("./all-years.journal", ["./2017-include.journal", "./2018-include.journal", "./2019-include.journal"])]
    assertEqual "groupIncludeFiles Jane AllYears 5" expectedAllYears5 allYears5
    let expectedGroup5 = [
          ("./2017-include.journal",
           ["./import/2017-include.journal"]),
          ("./2018-include.journal",
           ["./import/2018-include.journal"]),
          ("./2019-include.journal",
           ["./import/2019-include.journal"])]
    assertEqual "groupIncludeFiles 5 - diff 1" [] (expectedGroup5 Map.\\ group5)
    assertEqual "groupIncludeFiles 5 - diff 2" [] (group5 Map.\\ expectedGroup5)
    assertEqual "groupIncludeFiles 5" expectedGroup5 group5
  )

testIncludeYears :: Test
testIncludeYears = TestCase (
  do
    let txterr = "Some text without years"
    let expectederr = ["Unable to extract years from the following text:", txterr, "Errors:"]
    let actualerr = (init . head) $ map (T.lines) $ lefts [includeYears' txterr] :: [Text]
    assertEqual "Get a list of years from an include file - error case" expectederr actualerr

    let txt1 = "### Generated by hledger-flow - DO NOT EDIT ###\n\n" <>
          "!include import/2014-include.journal\n" <>
          "!include import/2015-include.journal\n" <>
          "!include import/2016-include.journal\n" <>
          "!include import/2017-include.journal\n" <>
          "!include import/2018-include.journal\n" <>
          "!include import/2019-include.journal"

    let expected1 = Right [2014..2019]
    let actual1 = includeYears' txt1
    assertEqual "Get a list of years from an include file - success case 1" expected1 actual1

    let txt2 = "!include 2019-include.journal"

    let expected2 = Right [2019]
    let actual2 = includeYears' txt2
    assertEqual "Get a list of years from an include file - success case 2" expected2 actual2
  )

testToIncludeLine :: Test
testToIncludeLine = TestCase (
  do
    let expected = "!include file1.journal"
    let relativeWithTrailingSlash = toIncludeLine "./base/dir/" "./base/dir/file1.journal"
    assertEqual "Include line - relative base dir with trailing slash" expected relativeWithTrailingSlash

    let relativeNoTrailingSlash = toIncludeLine "./base/dir" "./base/dir/file1.journal"
    assertEqual "Include line - relative base dir without a trailing slash" expected relativeNoTrailingSlash

    let absoluteWithTrailingSlash = toIncludeLine "/base/dir/" "/base/dir/file1.journal"
    assertEqual "Include line - absolute base dir with trailing slash" expected absoluteWithTrailingSlash

    let absoluteNoTrailingSlash = toIncludeLine "/base/dir" "/base/dir/file1.journal"
    assertEqual "Include line - absolute base dir without a trailing slash" expected absoluteNoTrailingSlash
  )

testToIncludeFiles :: Test
testToIncludeFiles = TestCase (
  do
    let expected = [
          ("./import/john/bogartbank/checking/2018-include.journal",
           "### Generated by hledger-flow - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/checking/3-journal/2018/2018-10-30.journal\n" <>
           "!include import/john/bogartbank/checking/3-journal/2018/2018-11-30.journal\n" <>
           "!include import/john/bogartbank/checking/3-journal/2018/2018-12-30.journal\n"),
          ("./import/john/bogartbank/checking/2019-include.journal",
           "### Generated by hledger-flow - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/checking/3-journal/2019/2019-01-30.journal\n" <>
           "!include import/john/bogartbank/checking/3-journal/2019/2019-02-30.journal\n"),
          ("./import/john/bogartbank/savings/2017-include.journal",
           "### Generated by hledger-flow - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/savings/3-journal/2017/2017-11-30.journal\n" <>
           "!include import/john/bogartbank/savings/3-journal/2017/2017-12-30.journal\n"),
          ("./import/john/bogartbank/savings/2018-include.journal",
           "### Generated by hledger-flow - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/savings/3-journal/2018/2018-01-30.journal\n" <>
           "!include import/john/bogartbank/savings/3-journal/2018/2018-02-30.journal\n")]

    ch <- newTChanIO
    txt <- toIncludeFiles (defaultOpts [absdir|/|]) ch groupedJohnBogart
    assertEqual "Convert a grouped map of paths, to a map with text contents for each file" expected txt)

tests :: Test
tests = TestList [testYearsIncludeMap, testYearsIncludeGrouping, testGroupIncludeFilesTinySet, testGroupIncludeFilesSmallSet, testGroupIncludeFiles, testIncludeYears, testToIncludeLine, testToIncludeFiles]
