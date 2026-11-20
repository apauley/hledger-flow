{-# LANGUAGE QuasiQuotes #-}

module Init.Integration (tests) where

import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Init (InitMode (WriteLedger), InitOutcome (..), initLedger)
import Hledger.Flow.Init.Example (exampleAccount, exampleBaseFiles, exampleImportFiles, starterOwner)
import Path
import Path.IO (doesDirExist, doesFileExist)
import Test.HUnit hiding (Path)
import TestHelpers (withTestDir)

testInit :: Test
testInit = TestCase $ withTestDir $ \tmp -> do
  let fresh = tmp </> [reldir|fin|]
  year <- currentYear
  owner <- starterOwner
  let created = [(name, True) | (name, _) <- exampleBaseFiles year]
             ++ [([reldir|import|] </> name, True) | (name, _) <- exampleImportFiles owner year]
  (@?= StarterLedger Nothing (exampleAccount owner) created) =<< initLedger WriteLedger fresh
  mapM_ (\(file, _) -> doesFileExist (fresh </> [reldir|import|] </> file) >>= assertBool (toFilePath file))
    (exampleImportFiles owner year)
  doesDirExist (fresh </> [reldir|import|]) >>= assertBool "import/ created"
  (@?= AlreadyLedger fresh) =<< initLedger WriteLedger fresh
  (@?= InsideLedger fresh) =<< initLedger WriteLedger (fresh </> [reldir|import/me|])
  let nested = fresh </> [reldir|elsewhere|]
  (@?= StarterLedger (Just fresh) (exampleAccount owner) created) =<< initLedger WriteLedger nested

tests :: Test
tests = TestList [testInit]
