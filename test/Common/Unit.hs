{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.Unit where

import Hledger.Flow.BaseDir (relativeToBase')
import Hledger.Flow.Common
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.RuntimeOptions (RuntimeOptions (hledgerConf))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Path ((</>), reldir)
import Test.HUnit
import TestHelpers (defaultOpts, testRoot)
import qualified System.FilePath as FilePath

testDocURL :: Test
testDocURL = TestCase $
  assertEqual "Input files documentation URL"
    "https://github.com/apauley/hledger-flow/tree/master/docs#input-files"
    (docURL "input-files")

testShowCmdArgs :: Test
testShowCmdArgs =
  TestCase
    ( do
        let opts = ["--number", "/tmp/file with spaces"]
        let expected = "--number '/tmp/file with spaces'"
        let actual = showCmdArgs opts
        assertEqual "Convert command-line arguments to text" expected actual
        mapM_ (\(arg, quoted) -> assertEqual ("Quote literal argument " ++ show arg) quoted (escapeArg arg))
          ([ ("", "''"),
            ("Morgan's Saturday", "'Morgan'\"'\"'s Saturday'"),
            ("O'Brien", "'O'\"'\"'Brien'"),
            ("$(echo injected);*", "'$(echo injected);*'"),
            ("a\tb\nc", "'a\tb\nc'"),
            ("a\"b", "'a\"b'"),
            ("a\\b", "'a\\b'"),
            ("--file=import/2016-include.journal", "--file=import/2016-include.journal")
          ] :: [(T.Text, T.Text)])
    )

testHledgerConfArgs :: Test
testHledgerConfArgs =
  TestCase
    ( do
        let optsWithoutConf = defaultOpts (testRoot </> [reldir|base|])
        assertEqual "No hledger config should produce no hledger config args" [] (hledgerConfArgs optsWithoutConf)

        let optsWithConf = optsWithoutConf {hledgerConf = Just "/tmp/hledger.conf"}
        assertEqual "Configured hledger config should produce --conf args" ["--conf", "/tmp/hledger.conf"] (hledgerConfArgs optsWithConf)
    )

testRelativeToBase :: Test
testRelativeToBase =
  TestCase
    ( do
        let expected = "file1.journal"
        let relativeWithTrailingSlash = relativeToBase' "./base/dir/" "./base/dir/file1.journal"
        assertEqual "relative base dir with trailing slash" expected relativeWithTrailingSlash

        let relativeNoTrailingSlash = relativeToBase' "./base/dir" "./base/dir/file1.journal"
        assertEqual "relative base dir without a trailing slash" expected relativeNoTrailingSlash

        let absoluteWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/file1.journal"
        assertEqual "absolute base dir with trailing slash" expected absoluteWithTrailingSlash

        let absoluteNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir/file1.journal"
        assertEqual "absolute base dir without a trailing slash" expected absoluteNoTrailingSlash

        let absoluteTwiceNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir"
        assertEqual "absolute base dir without a trailing slash supplied twice" "./" absoluteTwiceNoTrailingSlash

        let absoluteTwiceWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/"
        assertEqual "absolute base dir with a trailing slash supplied twice" "./" absoluteTwiceWithTrailingSlash

        let absoluteTwiceNoTrailingSlashOnSecondParam = relativeToBase' "/base/dir/" "/base/dir"
        assertEqual "absolute base dir supplied twice, but the second param has no slash" "./" absoluteTwiceNoTrailingSlashOnSecondParam

        let mismatch = relativeToBase' "/base/dir" "/unrelated/dir/file1.journal"
        assertEqual "A basedir with no shared prefix should return the supplied file unchanged" "/unrelated/dir/file1.journal" mismatch

        assertEqual "Do not strip part of a sibling directory name" "base.other/file"
          (relativeToBase' "base" "base.other/file")
        assertEqual "Retain the trailing separator for a parent directory" "../"
          (relativeToBase' "base" "base/..")
        if FilePath.pathSeparator == '\\'
          then assertEqual "A UNC share is not relative to the current drive root" "//server/share/file"
                 (relativeToBase' "/" "//server/share/file")
          else return ()
    )

testExtractDigits :: Test
testExtractDigits =
  TestCase
    ( do
        let txt1 = "A number: 321\nAnother number is 42, so is 0"

        let expected1 = Right 321420
        let actual1 = extractDigits txt1
        assertEqual "Extract digits from text 1" expected1 actual1

        let txt2 = "No numbers in this line"

        let expected2 = Left "input does not start with a digit"
        let actual2 = extractDigits txt2
        assertEqual "Extract digits from text 2" expected2 actual2
    )

testGroupValuesBy :: Test
testGroupValuesBy =
  TestCase
    ( do
        let values = ["b2", "a1", "b1", "a2"]
        let grouped = groupValuesBy (take 1) values
        let normalize = Map.map List.sort
        let expected = Map.fromList [("a", ["a1", "a2"]), ("b", ["b1", "b2"])]
        assertEqual "Group values by key" (normalize expected) (normalize grouped)
    )

tests :: Test
tests = TestList [testDocURL, testShowCmdArgs, testHledgerConfArgs, testRelativeToBase, testExtractDigits, testGroupValuesBy]
