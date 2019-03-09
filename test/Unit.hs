{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Unit where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Integration

import TestHelpers
import Hledger.MakeItSo.Common

groupedJaneBogart :: Map.Map FilePath [FilePath]
groupedJaneBogart = [
  ("./import/jane/bogartbank/checking/3-journal/2018-include.journal",
    ["import/jane/bogartbank/checking/3-journal/2018/2018-12-30.journal"]),
  ("./import/jane/bogartbank/checking/3-journal/2019-include.journal",
    ["import/jane/bogartbank/checking/3-journal/2019/2019-01-30.journal"]),
  ("./import/jane/bogartbank/savings/3-journal/2017-include.journal",
    ["import/jane/bogartbank/savings/3-journal/2017/2017-12-30.journal"]),
  ("./import/jane/bogartbank/savings/3-journal/2018-include.journal",
    ["import/jane/bogartbank/savings/3-journal/2018/2018-01-30.journal"])]

groupedJaneOther :: Map.Map FilePath [FilePath]
groupedJaneOther = [
  ("./import/jane/otherbank/creditcard/3-journal/2017-include.journal",
    ["import/jane/otherbank/creditcard/3-journal/2017/2017-12-30.journal"]),
  ("./import/jane/otherbank/creditcard/3-journal/2018-include.journal",
    ["import/jane/otherbank/creditcard/3-journal/2018/2018-01-30.journal"]),
  ("./import/jane/otherbank/investments/3-journal/2018-include.journal",
    ["import/jane/otherbank/investments/3-journal/2018/2018-12-30.journal"]),
  ("./import/jane/otherbank/investments/3-journal/2019-include.journal",
    ["import/jane/otherbank/investments/3-journal/2019/2019-01-30.journal"])]

groupedJohnBogart :: Map.Map FilePath [FilePath]
groupedJohnBogart = [
  ("./import/john/bogartbank/checking/3-journal/2018-include.journal",
    ["import/john/bogartbank/checking/3-journal/2018/2018-11-30.journal",
     "import/john/bogartbank/checking/3-journal/2018/2018-10-30.journal",
     "import/john/bogartbank/checking/3-journal/2018/2018-12-30.journal"]),
  ("./import/john/bogartbank/checking/3-journal/2019-include.journal",
    ["import/john/bogartbank/checking/3-journal/2019/2019-01-30.journal",
     "import/john/bogartbank/checking/3-journal/2019/2019-02-30.journal"]),
  ("./import/john/bogartbank/savings/3-journal/2017-include.journal",
    ["import/john/bogartbank/savings/3-journal/2017/2017-11-30.journal",
     "import/john/bogartbank/savings/3-journal/2017/2017-12-30.journal"]),
  ("./import/john/bogartbank/savings/3-journal/2018-include.journal",
    ["import/john/bogartbank/savings/3-journal/2018/2018-02-30.journal",
     "import/john/bogartbank/savings/3-journal/2018/2018-01-30.journal"])]

groupedJohnOther :: Map.Map FilePath [FilePath]
groupedJohnOther = [
  ("./import/john/otherbank/creditcard/3-journal/2017-include.journal",
    ["import/john/otherbank/creditcard/3-journal/2017/2017-12-30.journal"]),
  ("./import/john/otherbank/creditcard/3-journal/2018-include.journal",
    ["import/john/otherbank/creditcard/3-journal/2018/2018-01-30.journal"]),
  ("./import/john/otherbank/investments/3-journal/2018-include.journal",
    ["import/john/otherbank/investments/3-journal/2018/2018-12-30.journal"]),
  ("./import/john/otherbank/investments/3-journal/2019-include.journal",
    ["import/john/otherbank/investments/3-journal/2019/2019-01-30.journal"])]

groupedIncludeFiles :: Map.Map FilePath [FilePath]
groupedIncludeFiles = groupedJaneBogart <> groupedJaneOther <>
                      groupedJohnBogart <> groupedJohnOther

testGroupIncludeFiles = TestCase (
  do
    let group1 = groupIncludeFiles journalFiles :: Map.Map FilePath [FilePath]
    assertEqual "groupIncludeFiles 1" groupedIncludeFiles group1

    let group2 = groupIncludeFiles (Map.keys group1) :: Map.Map FilePath [FilePath]
    let expectedGroup2 = [("./import/jane/bogartbank/checking/3-journal-include.journal",
                           ["./import/jane/bogartbank/checking/3-journal/2018-include.journal",
                            "./import/jane/bogartbank/checking/3-journal/2019-include.journal"]),
                          ("./import/jane/bogartbank/savings/3-journal-include.journal",
                           ["./import/jane/bogartbank/savings/3-journal/2017-include.journal",
                            "./import/jane/bogartbank/savings/3-journal/2018-include.journal"]),
                          ("./import/jane/otherbank/creditcard/3-journal-include.journal",
                           ["./import/jane/otherbank/creditcard/3-journal/2017-include.journal",
                            "./import/jane/otherbank/creditcard/3-journal/2018-include.journal"]),
                          ("./import/jane/otherbank/investments/3-journal-include.journal",
                           ["./import/jane/otherbank/investments/3-journal/2018-include.journal",
                            "./import/jane/otherbank/investments/3-journal/2019-include.journal"]),
                          ("./import/john/bogartbank/checking/3-journal-include.journal",
                           ["./import/john/bogartbank/checking/3-journal/2018-include.journal",
                            "./import/john/bogartbank/checking/3-journal/2019-include.journal"]),
                          ("./import/john/bogartbank/savings/3-journal-include.journal",
                           ["./import/john/bogartbank/savings/3-journal/2017-include.journal",
                            "./import/john/bogartbank/savings/3-journal/2018-include.journal"]),
                          ("./import/john/otherbank/creditcard/3-journal-include.journal",
                           ["./import/john/otherbank/creditcard/3-journal/2017-include.journal",
                            "./import/john/otherbank/creditcard/3-journal/2018-include.journal"]),
                          ("./import/john/otherbank/investments/3-journal-include.journal",
                           ["./import/john/otherbank/investments/3-journal/2018-include.journal",
                            "./import/john/otherbank/investments/3-journal/2019-include.journal"])]
    assertEqual "groupIncludeFiles 2" expectedGroup2 group2

    let group3 = groupIncludeFiles (Map.keys group2) :: Map.Map FilePath [FilePath]
    let expectedGroup3 = [("./import/jane/bogartbank/checking-include.journal",
                           ["./import/jane/bogartbank/checking/3-journal-include.journal"]),
                          ("./import/jane/bogartbank/savings-include.journal",
                           ["./import/jane/bogartbank/savings/3-journal-include.journal"]),
                          ("./import/jane/otherbank/creditcard-include.journal",
                           ["./import/jane/otherbank/creditcard/3-journal-include.journal"]),
                          ("./import/jane/otherbank/investments-include.journal",
                           ["./import/jane/otherbank/investments/3-journal-include.journal"]),
                          ("./import/john/bogartbank/checking-include.journal",
                           ["./import/john/bogartbank/checking/3-journal-include.journal"]),
                          ("./import/john/bogartbank/savings-include.journal",
                           ["./import/john/bogartbank/savings/3-journal-include.journal"]),
                          ("./import/john/otherbank/creditcard-include.journal",
                           ["./import/john/otherbank/creditcard/3-journal-include.journal"]),
                          ("./import/john/otherbank/investments-include.journal",
                           ["./import/john/otherbank/investments/3-journal-include.journal"])]
    assertEqual "groupIncludeFiles 3" expectedGroup3 group3

    let group4 = groupIncludeFiles (Map.keys group3) :: Map.Map FilePath [FilePath]
    let expectedGroup4 = [("./import/jane/bogartbank-include.journal",
                           ["./import/jane/bogartbank/checking-include.journal",
                            "./import/jane/bogartbank/savings-include.journal"]),
                          ("./import/jane/otherbank-include.journal",
                           ["./import/jane/otherbank/creditcard-include.journal",
                            "./import/jane/otherbank/investments-include.journal"]),
                          ("./import/john/bogartbank-include.journal",
                           ["./import/john/bogartbank/checking-include.journal",
                            "./import/john/bogartbank/savings-include.journal"]),
                          ("./import/john/otherbank-include.journal",
                           ["./import/john/otherbank/creditcard-include.journal",
                            "./import/john/otherbank/investments-include.journal"])]
    assertEqual "groupIncludeFiles 4" expectedGroup4 group4

    let group5 = groupIncludeFiles (Map.keys group4) :: Map.Map FilePath [FilePath]
    let expectedGroup5 = [("./import/jane-include.journal",
                           ["./import/jane/bogartbank-include.journal",
                            "./import/jane/otherbank-include.journal"]),
                          ("./import/john-include.journal",
                           ["./import/john/bogartbank-include.journal",
                            "./import/john/otherbank-include.journal"])]
    assertEqual "groupIncludeFiles 5" expectedGroup5 group5

    let group6 = groupIncludeFiles (Map.keys group5) :: Map.Map FilePath [FilePath]
    let expectedGroup6 = [("./import-include.journal",
                           ["./import/jane-include.journal",
                            "./import/john-include.journal"])]
    assertEqual "groupIncludeFiles 6" expectedGroup6 group6
  )

testGroupPairs = TestCase (do
                              let actual = groupPairs . pairBy includeFilePath $ journalFiles
                              assertEqual "Group files, paired by the directories they live in" groupedIncludeFiles actual)

testToIncludeLine = TestCase (do
                                 let expected = "!include file1.journal"
                                 let actual = toIncludeLine "./base/dir/" "./base/dir/file1.journal"
                                 assertEqual "Include line" expected actual)
testToIncludeFiles = TestCase (
  do
    let expected = [
          ("./import/john/bogartbank/checking/3-journal/2018-include.journal",
            "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
            "!include import/john/bogartbank/checking/3-journal/2018/2018-10-30.journal\n" <>
            "!include import/john/bogartbank/checking/3-journal/2018/2018-11-30.journal\n" <>
            "!include import/john/bogartbank/checking/3-journal/2018/2018-12-30.journal\n"),
          ("./import/john/bogartbank/checking/3-journal/2019-include.journal",
            "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
            "!include import/john/bogartbank/checking/3-journal/2019/2019-01-30.journal\n" <>
            "!include import/john/bogartbank/checking/3-journal/2019/2019-02-30.journal\n"),
          ("./import/john/bogartbank/savings/3-journal/2017-include.journal",
            "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
            "!include import/john/bogartbank/savings/3-journal/2017/2017-11-30.journal\n" <>
            "!include import/john/bogartbank/savings/3-journal/2017/2017-12-30.journal\n"),
          ("./import/john/bogartbank/savings/3-journal/2018-include.journal",
            "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
            "!include import/john/bogartbank/savings/3-journal/2018/2018-01-30.journal\n" <>
            "!include import/john/bogartbank/savings/3-journal/2018/2018-02-30.journal\n")]

    txt <- single $ toIncludeFiles groupedJohnBogart
    assertEqual "Convert a grouped map of paths, to a map with text contents for each file" expected txt)

tests = TestList [testGroupIncludeFiles, testGroupPairs, testToIncludeLine, testToIncludeFiles]
