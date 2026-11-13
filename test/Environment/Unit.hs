{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Environment.Unit (tests) where

import qualified Data.Text as T
import Hledger.Flow.Environment
import Hledger.Flow.RuntimeOptions
import Hledger.Flow.Types (HledgerConf (..))
import Path
import Test.HUnit
import qualified System.FilePath as FilePath
import TestHelpers (defaultOpts, testRoot)

opts :: RuntimeOptions
opts = defaultOpts (testRoot </> [reldir|fin|])

testImportLines :: Test
testImportLines = TestCase $ do
  let out = renderSettings id (importSettings [] opts {importStartYear = Just 2024, batchSize = 20})
      line l = filter ((l <> " ") `T.isPrefixOf`) out
  assertEqual "one line per setting" 8 (length out)
  assertBool "full hledger path and version" (any ((T.pack (toFilePath [relfile|path/to/hledger|]) <> "  (1.2.3)") `T.isInfixOf`) (line "hledger"))
  assertBool "flag beside its value" (any ("2024 onwards" `T.isInfixOf`) (line "Start year") && any ("--start-year" `T.isSuffixOf`) (line "Start year"))
  assertBool "whole ledger" (any (". (whole ledger)" `T.isInfixOf`) (line "Run from"))
  -- The longest labels are 12 characters, so every value starts in column 14.
  assertBool "values line up" (all (\l -> T.index l 13 == ' ' && T.index l 14 /= ' ') out)

testMissingHledger :: Test
testMissingHledger = TestCase $ do
  let out = renderSettings (\v -> "<" <> v <> ">") (environmentSettings [] (sysInfo opts) Nothing Nothing)
  assertBool "missing hledger styled as a problem" (any ("<not found" `T.isInfixOf`) out)
  assertBool "only that line styled" (length (filter ("<" `T.isInfixOf`) out) == 1)

testHledgerConf :: Test
testHledgerConf = TestCase $ do
  let confLine c = filter ("hledger.conf " `T.isPrefixOf`) (renderSettings id (environmentSettings [] (sysInfo opts) Nothing c))
  assertBool "no config" (any ("  none" `T.isInfixOf`) (confLine Nothing))
  assertBool "given config" (any ("  /given.conf " `T.isInfixOf`) (confLine (Just (GivenConf "/given.conf"))))
  assertBool "found config is named" (any ("/home/me/.hledger.conf  (found by hledger)" `T.isInfixOf`) (confLine (Just (FoundConf "/home/me/.hledger.conf"))))

testHomePaths :: Test
testHomePaths = TestCase $ do
  let out = renderSettings id (importSettings [toFilePath testRoot] opts)
      line l = filter ((l <> " ") `T.isPrefixOf`) out
  assertBool "hledger under home" (any ((T.pack (FilePath.joinPath ["~", "path", "to", "hledger"]) <> "  (1.2.3)") `T.isInfixOf`) (line "hledger"))
  assertBool "ledger under home" (any ((T.pack (FilePath.joinPath ["~", "fin"])) `T.isSuffixOf`) (line "Ledger"))

testReportTables :: Test
testReportTables = TestCase $ do
  let out = renderSettings id (reportSettings [] opts {prettyReports = False, sequential = True})
  assertBool "ascii tables" (any ("ASCII" `T.isInfixOf`) out)
  assertBool "sequential" (any ("one file at a time" `T.isInfixOf`) out)
  assertBool "no import settings" (not (any ("Start year" `T.isPrefixOf`) out))

tests :: Test
tests = TestList [testImportLines, testMissingHledger, testHledgerConf, testHomePaths, testReportTables]
