{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Common.Unit where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)

import TestHelpers
import Hledger.Flow.Common

testShowCmdArgs = TestCase (
  do
    let options = ["--number", "/tmp/file with spaces"]
    let expected = "--number '/tmp/file with spaces'"
    let actual = showCmdArgs options
    assertEqual "Convert command-line arguments to text" expected actual)

tests = TestList [testShowCmdArgs]
