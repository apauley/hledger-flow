{-# LANGUAGE QuasiQuotes #-}

module Init.Integration (tests) where

import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Init (InitOutcome (..), initLedger)
import Hledger.Flow.Init.Example (exampleBaseFiles, exampleImportFiles)
import Path
import Path.IO (doesDirExist, doesFileExist)
import Test.HUnit hiding (Path)
import TestHelpers (withTestDir)

testInit :: Test
testInit = TestCase $ withTestDir $ \tmp -> do
  let fresh = tmp </> [reldir|fin|]
  year <- currentYear
  let created = [(name, True) | (name, _) <- exampleBaseFiles year]
  (@?= Created fresh Nothing created) =<< initLedger fresh
  mapM_ (\(file, _) -> doesFileExist (fresh </> [reldir|import|] </> file) >>= assertBool (toFilePath file))
    (exampleImportFiles year)
  doesDirExist (fresh </> [reldir|import|]) >>= assertBool "import/ created"
  (@?= AlreadyLedger fresh) =<< initLedger fresh
  (@?= InsideLedger fresh) =<< initLedger (fresh </> [reldir|import/me|])
  let nested = fresh </> [reldir|elsewhere|]
  (@?= Created nested (Just fresh) created) =<< initLedger nested

tests :: Test
tests = TestList [testInit]
