{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module PathHelpers.Compatibility (tests) where

import Hledger.Flow.PathHelpers
import Path
import Test.HUnit
import TestHelpers (testRoot)

-- Exercise the published aliases while callers migrate to Path.
tests :: Test
tests = TestCase $ do
  let file = [relfile|statements/April 1.csv|]
      dir = parent file
      check label parse valid invalid = do
        assertEqual (label ++ " parses") (Just valid) (parse (toFilePath valid))
        assertEqual (label ++ " rejects the wrong base") Nothing (parse invalid)
        assertEqual (label ++ " renders") (toFilePath valid) (pathToTurtle valid)
  check "absolute file" fromTurtleAbsFile (testRoot </> file) (toFilePath file)
  check "relative file" fromTurtleRelFile file (toFilePath (testRoot </> file))
  check "absolute directory" fromTurtleAbsDir (testRoot </> dir) (toFilePath dir)
  check "relative directory" fromTurtleRelDir dir (toFilePath (testRoot </> dir))
