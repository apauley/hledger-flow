module CSVImport.Unit where

import qualified CSVImport.ImportHelperTests
import qualified CSVImport.IncludeFilesTests
import Test.HUnit

tests :: Test
tests = TestList [CSVImport.ImportHelperTests.tests, CSVImport.IncludeFilesTests.tests]
