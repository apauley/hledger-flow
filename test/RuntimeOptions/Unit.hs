{-# LANGUAGE QuasiQuotes #-}

module RuntimeOptions.Unit (tests) where

import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as Types
import Path
import Test.HUnit
import TestHelpers (defaultOpts, testRoot)

tests :: Test
tests = TestCase $ do
  let base = testRoot </> [reldir|ledger|]
      run = [reldir|import/owner|]
      opts = (defaultOpts base) {importRunDir = run, batchSize = 19, prettyReports = False}
      flags o = (Types.verbose o, Types.sequential o, Types.prettyReports o)
  assertEqual "base directory" base (Types.baseDir opts)
  assertEqual "import directory" run (Types.importRunDir opts)
  assertEqual "batch size" 19 (Types.batchSize opts)
  assertEqual "verbose alone" (True, False, False) (flags opts {verbose = True})
  assertEqual "sequential alone" (False, True, False) (flags opts {sequential = True})
  assertEqual "pretty reports alone" (False, False, True) (flags opts {prettyReports = True})
  assertEqual "unrelated flags" (False, False, False) (flags opts {onlyNewFiles = True, showOptions = True})
