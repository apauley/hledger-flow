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
  ("./import/jane/bogartbank/checking/2018-include.journal",
   ["import/jane/bogartbank/checking/3-journal/2018/2018-12-30.journal"]),
  ("./import/jane/bogartbank/checking/2019-include.journal",
   ["import/jane/bogartbank/checking/3-journal/2019/2019-01-30.journal"]),
  ("./import/jane/bogartbank/savings/2017-include.journal",
   ["import/jane/bogartbank/savings/3-journal/2017/2017-12-30.journal"])]

groupedJaneOther :: Map.Map FilePath [FilePath]
groupedJaneOther = [
  ("./import/jane/otherbank/creditcard/2017-include.journal",
   ["import/jane/otherbank/creditcard/3-journal/2017/2017-12-30.journal"]),
  ("./import/jane/otherbank/creditcard/2018-include.journal",
   ["import/jane/otherbank/creditcard/3-journal/2018/2018-01-30.journal"]),
  ("./import/jane/otherbank/investments/2018-include.journal",
   ["import/jane/otherbank/investments/3-journal/2018/2018-12-30.journal"]),
  ("./import/jane/otherbank/investments/2019-include.journal",
   ["import/jane/otherbank/investments/3-journal/2019/2019-01-30.journal"])]

groupedJohnBogart :: Map.Map FilePath [FilePath]
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

groupedJohnOther :: Map.Map FilePath [FilePath]
groupedJohnOther = [
  ("./import/john/otherbank/creditcard/2017-include.journal",
   ["import/john/otherbank/creditcard/3-journal/2017/2017-12-30.journal"]),
  ("./import/john/otherbank/creditcard/2018-include.journal",
   ["import/john/otherbank/creditcard/3-journal/2018/2018-01-30.journal"]),
  ("./import/john/otherbank/investments/2018-include.journal",
   ["import/john/otherbank/investments/3-journal/2018/2018-12-30.journal"]),
  ("./import/john/otherbank/investments/2019-include.journal",
   ["import/john/otherbank/investments/3-journal/2019/2019-01-30.journal"])]

groupedIncludeFiles :: Map.Map FilePath [FilePath]
groupedIncludeFiles = groupedJaneBogart <> groupedJaneOther <>
                      groupedJohnBogart <> groupedJohnOther

testGroupSmall = TestCase (
  do
    assertEqual "groupIncludeFiles small set" groupedJaneBogart (groupIncludeFiles inputJaneBogart)
  )

testGroupIncludeFiles = TestCase (
  do
    let group1 = groupIncludeFiles journalFiles :: Map.Map FilePath [FilePath]
    assertEqual "groupIncludeFiles 1" groupedIncludeFiles group1

    let group2 = groupIncludeFiles (Map.keys group1) :: Map.Map FilePath [FilePath]
    let expectedGroup2 = [("./import/jane/bogartbank/2017-include.journal",
                           ["./import/jane/bogartbank/savings/2017-include.journal"]),
                          ("./import/jane/bogartbank/2018-include.journal",
                           ["./import/jane/bogartbank/checking/2018-include.journal",
                            "./import/jane/bogartbank/savings/2018-include.journal"]),

                          ("./import/jane/otherbank/2017-include.journal",
                           ["./import/jane/otherbank/creditcard/2017-include.journal"]),
                          ("./import/jane/otherbank/2018-include.journal",
                           ["./import/jane/otherbank/creditcard/2018-include.journal",
                            "./import/jane/otherbank/investments/2018-include.journal"]),
                          ("./import/jane/otherbank/2019-include.journal",
                           ["./import/jane/otherbank/investments/2019-include.journal"]),

                          ("./import/john/bogartbank/2017-include.journal",
                           ["./import/john/bogartbank/savings/2017-include.journal",
                            "./import/john/bogartbank/creditcard/2017-include.journal"]),
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
    assertEqual "groupIncludeFiles 2" expectedGroup2 group2

    let group3 = groupIncludeFiles (Map.keys group2) :: Map.Map FilePath [FilePath]
    let expectedGroup3 = [("./import/jane/2017-include.journal",
                           ["./import/jane/bogartbank/2017-include.journal",
                            "./import/jane/otherbank/2017-include.journal"]),
                          ("./import/jane/2018-include.journal",
                           ["./import/jane/bogartbank/2018-include.journal",
                            "./import/jane/otherbank/2018-include.journal"]),
                          ("./import/jane/2019-include.journal",
                           ["./import/jane/otherbank/2019-include.journal"]),
                          ("./import/john/2017-include.journal",
                           ["./import/john/bogartbank/2017-include.journal",
                            "./import/john/otherbank/2017-include.journal"]),
                          ("./import/john/2018-include.journal",
                           ["./import/john/bogartbank/2018-include.journal",
                            "./import/john/otherbank/2018-include.journal"]),
                          ("./import/john/2019-include.journal",
                           ["./import/john/bogartbank/2019-include.journal",
                            "./import/john/otherbank/2019-include.journal"])]
    assertEqual "groupIncludeFiles 3" expectedGroup3 group3

    let group4 = groupIncludeFiles (Map.keys group3) :: Map.Map FilePath [FilePath]
    let expectedGroup4 = [("./import/2017-include.journal",
                           ["./import/jane/2017-include.journal",
                            "./import/john/2017-include.journal"]),
                          ("./import/2018-include.journal",
                           ["./import/jane/2018-include.journal",
                            "./import/john/2018-include.journal"]),
                          ("./import/2019-include.journal",
                           ["./import/jane/2019-include.journal",
                            "./import/john/2019-include.journal"])]
    assertEqual "groupIncludeFiles 4" expectedGroup4 group4

    let group5 = groupIncludeFiles (Map.keys group4) :: Map.Map FilePath [FilePath]
    let expectedGroup5 = [("./makeitso.journal",
                           ["./import/2017-include.journal",
                            "./import/2018-include.journal",
                            "./import/2019-include.journal"])]
    assertEqual "groupIncludeFiles 5" expectedGroup5 group5
  )

testGroupPairs = TestCase (do
                              let actual = groupPairs . pairBy includeFilePath $ journalFiles
                              assertEqual "Group files, paired by the directories they live in" groupedIncludeFiles actual)

testRelativeToBase = TestCase (
  do
    let expected = "file1.journal"
    let relativeWithTrailingSlash = relativeToBase' "./base/dir/" "./base/dir/file1.journal"
    assertEqual "relative base dir with trailing slash" expected relativeWithTrailingSlash

    let relativeNoTrailingSlash = relativeToBase' "./base/dir" "./base/dir/file1.journal"
    assertEqual "relative base dir without a trailing slash" expected relativeNoTrailingSlash

    let absoluteWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/file1.journal"
    assertEqual "absolute base dir with trailing slash" expected absoluteWithTrailingSlash

    let absoluteNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir/file1.journal"
    assertEqual "absolute base dir without a trailing slash" expected absoluteNoTrailingSlash

    let mismatch = relativeToBase' "/base/dir" "/unrelated/dir/file1.journal"
    assertEqual "A basedir with no shared prefix should return the supplied file unchanged" "/unrelated/dir/file1.journal" mismatch
  )

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

testToIncludeFiles = TestCase (
  do
    let expected = [
          ("./import/john/bogartbank/checking/2018-include.journal",
           "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/checking/3-journal/2018/2018-10-30.journal\n" <>
           "!include import/john/bogartbank/checking/3-journal/2018/2018-11-30.journal\n" <>
           "!include import/john/bogartbank/checking/3-journal/2018/2018-12-30.journal\n"),
          ("./import/john/bogartbank/checking/2019-include.journal",
           "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/checking/3-journal/2019/2019-01-30.journal\n" <>
           "!include import/john/bogartbank/checking/3-journal/2019/2019-02-30.journal\n"),
          ("./import/john/bogartbank/savings/2017-include.journal",
           "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/savings/3-journal/2017/2017-11-30.journal\n" <>
           "!include import/john/bogartbank/savings/3-journal/2017/2017-12-30.journal\n"),
          ("./import/john/bogartbank/savings/2018-include.journal",
           "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n" <>
           "!include import/john/bogartbank/savings/3-journal/2018/2018-01-30.journal\n" <>
           "!include import/john/bogartbank/savings/3-journal/2018/2018-02-30.journal\n")]

    txt <- single $ toIncludeFiles (defaultOpts ".") groupedJohnBogart
    assertEqual "Convert a grouped map of paths, to a map with text contents for each file" expected txt)

tests = TestList [testGroupSmall, testGroupIncludeFiles, testGroupPairs, testRelativeToBase, testToIncludeLine, testToIncludeFiles]
