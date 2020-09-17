module CSVImport.Unit where

import Test.HUnit

import qualified CSVImport.ImportHelperTests
import qualified CSVImport.ImportHelperTurtleTests

tests :: Test
tests = TestList [CSVImport.ImportHelperTests.tests, CSVImport.ImportHelperTurtleTests.tests]
