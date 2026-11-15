{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Incremental.Cache (tests) where

import Control.Exception (IOException, try)
import qualified Data.Text.IO as T
import Hledger.Flow.Import.Cache
import Hledger.Flow.Import.Fingerprint
import qualified Path as P
import qualified Path.IO as P
import Test.HUnit

tests :: Test
tests = TestLabel "incremental records" $ TestCase $ P.withSystemTempDir "records" $ \base -> do
  let source = base P.</> [P.relfile|import/alice/bank/card/1-in/2026/march.csv|]
      other = base P.</> [P.relfile|import/alice/bank/card/1-in/2026/march.tsv|]
      out = base P.</> [P.relfile|march.journal|]
      ignored = base P.</> [P.relfile|.hledger-flow/cache/.gitignore|]
      bytes = "2026-03-01 coffee\n"
  record <- recordPath base source
  another <- recordPath base other
  assertBool "the source extension distinguishes records" (another /= record)
  assertEqual "missing record" Nothing =<< readRecord record
  initializeCache base
  assertEqual "self-ignoring cache" "*\n" =<< T.readFile (P.toFilePath ignored)
  T.writeFile (P.toFilePath ignored) "mine"
  initializeCache base
  assertEqual "preserve custom ignore" "mine" =<< T.readFile (P.toFilePath ignored)
  digest <- installOutput False out $ \staged -> T.writeFile (P.toFilePath staged) bytes
  assertEqual "published digest is the staged bytes" digest =<< digestFile out
  let stage = StageRecord [("source", "march.csv")] digest
      entry = StatementRecord Nothing (Just stage)
  saveRecord record entry
  assertEqual "read published record" (Just entry) =<< readRecord record
  assertBool "verified output" =<< stageMatches (stageInputs stage) out (Just stage)
  before <- P.getModificationTime out
  _ <- installOutput True out $ \staged -> T.writeFile (P.toFilePath staged) bytes
  assertEqual "preserve identical intermediate" before =<< P.getModificationTime out
  _ <- try (installOutput False out $ \_ -> ioError (userError "conversion failed")) :: IO (Either IOException Digest)
  assertEqual "failed conversion leaves installed output" digest =<< digestFile out
  T.writeFile (P.toFilePath out) "another importer"
  assertBool "intervening writer invalidates record" . not =<< stageMatches (stageInputs stage) out (Just stage)
  digestB <- installOutput False out $ \staged -> T.writeFile (P.toFilePath staged) "output from writer B"
  let stageB = StageRecord [("source", "B")] digestB
  saveRecord record (StatementRecord Nothing (Just stageB))
  -- Writer A, paused after installing A, saves its record after B installs B.
  saveRecord record entry
  late <- readRecord record
  assertBool "late publication must miss on B's output" . not =<< stageMatches (stageInputs stage) out (late >>= journalRecord)
  T.writeFile (P.toFilePath record) "{}"
  assertEqual "invalid record" Nothing =<< readRecord record
