module CSVImport.Unit where

import qualified CSVImport.ImportHelperTests
import qualified CSVImport.ImportHelperTurtleTests
import Test.HUnit

tests :: Test
tests = TestList [CSVImport.ImportHelperTests.tests, CSVImport.ImportHelperTurtleTests.tests]
