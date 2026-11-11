{-# LANGUAGE QuasiQuotes #-}

module Init.Integration (tests) where

import Hledger.Flow.Init (InitOutcome (..), initLedger)
import Path
import Path.IO (doesDirExist)
import Test.HUnit hiding (Path)
import TestHelpers (withTestDir)

testInit :: Test
testInit = TestCase $ withTestDir $ \tmp -> do
  let fresh = tmp </> [reldir|fin|]
  (@?= Created fresh Nothing) =<< initLedger fresh
  doesDirExist (fresh </> [reldir|import|]) >>= assertBool "import/ created"
  (@?= AlreadyLedger fresh) =<< initLedger fresh
  (@?= InsideLedger fresh) =<< initLedger (fresh </> [reldir|import/me|])
  let nested = fresh </> [reldir|elsewhere|]
  (@?= Created nested (Just fresh)) =<< initLedger nested

tests :: Test
tests = TestList [testInit]
