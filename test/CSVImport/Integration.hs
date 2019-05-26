{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module CSVImport.Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Data.List as List (sort)

import TestHelpers
import Hledger.Flow.Common
import Control.Concurrent.STM

testExtraIncludesForFile :: Test
testExtraIncludesForFile = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "hlflow")
        let importedJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let accountDir = "import/john/bogartbank/savings"
        let opening = tmpdir </> accountDir </> "2017-opening.journal"
        let closing = tmpdir </> accountDir </> "2017-closing.journal"
        let hidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        touchAll $ importedJournals ++ hidden

        let accountInclude = tmpdir </> accountDir </> "2017-include.journal"
        let expectedEmpty = [(accountInclude, [])]

        ch <- liftIO newTChanIO

        extraOpening1 <- extraIncludesForFile (defaultOpts tmpdir) ch accountInclude ["opening.journal"] []
        liftIO $ assertEqual "The opening journal should not be included when it is not on disk" expectedEmpty extraOpening1

        extraClosing1 <- extraIncludesForFile (defaultOpts tmpdir) ch accountInclude ["closing.journal"] []
        liftIO $ assertEqual "The closing journal should not be included when it is not on disk" expectedEmpty extraClosing1

        touchAll [opening, closing]

        extraOpening2 <- extraIncludesForFile (defaultOpts tmpdir) ch accountInclude ["opening.journal"] []
        liftIO $ assertEqual "The opening journal should be included when it is on disk" [(accountInclude, [opening])] extraOpening2

        extraClosing2 <- extraIncludesForFile (defaultOpts tmpdir) ch accountInclude ["closing.journal"] []
        liftIO $ assertEqual "The closing journal should be included when it is on disk" [(accountInclude, [closing])] extraClosing2
     ))

testIncludesPrePost :: Test
testIncludesPrePost = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "hlflow")
        let ownerDir = tmpdir </> "import/john"
        let includeFile = ownerDir </> "2019-include.journal"
        let pre  = ownerDir </> "_manual_" </> "2019" </> "pre-import.journal"
        let post = ownerDir </> "_manual_" </> "2019" </> "post-import.journal"
        touchAll [pre, post]

        let includeMap = Map.singleton includeFile [ownerDir </> "bank1" </> "2019-include.journal",
                                                    ownerDir </> "bank2" </> "2019-include.journal"]

        ch <- liftIO newTChanIO
        fileMap <- toIncludeFiles (defaultOpts tmpdir) ch includeMap
        let expectedText = includePreamble <> "\n"
              <> "!include _manual_/2019/pre-import.journal\n"
              <> "!include bank1/2019-include.journal\n"
              <> "!include bank2/2019-include.journal\n"
              <> "!include _manual_/2019/post-import.journal\n"
        let expectedMap = Map.singleton includeFile expectedText
        liftIO $ assertEqual "All pre/post files on disk should be included" expectedMap fileMap
     ))

testIncludesOpeningClosing :: Test
testIncludesOpeningClosing = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "hlflow")
        let ownerDir = tmpdir </> "import/john"
        let accountDir = ownerDir </> "bank1" </> "savings"
        let includeFile = accountDir </> "2019-include.journal"
        let opening = accountDir </> "2019-opening.journal"
        let closing = accountDir </> "2019-closing.journal"
        touchAll [opening, closing]

        let includeMap = Map.singleton includeFile [accountDir </> "3-journal" </> "2019" </> "2019-01-30.journal"]

        ch <- liftIO newTChanIO
        fileMap <- toIncludeFiles (defaultOpts tmpdir) ch includeMap
        let expectedText = includePreamble <> "\n"
              <> "!include 2019-opening.journal\n"
              <> "!include 3-journal/2019/2019-01-30.journal\n"
              <> "!include 2019-closing.journal\n"
        let expectedMap = Map.singleton includeFile expectedText
        liftIO $ assertEqual "All pre/post files on disk should be included" expectedMap fileMap
     ))

testWriteIncludeFiles :: Test
testWriteIncludeFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "hlflow")
        let importedJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let extras = map (tmpdir </>) extraFiles :: [FilePath]
        let hidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        touchAll $ importedJournals ++ extras ++ hidden

        let jane1 = tmpdir </> "import/jane/bogartbank/checking/2018-include.journal"
        let jane2 = tmpdir </> "import/jane/bogartbank/checking/2019-include.journal"
        let jane3 = tmpdir </> "import/jane/bogartbank/savings/2017-include.journal"
        let jane4 = tmpdir </> "import/jane/bogartbank/savings/2018-include.journal"
        let jane5 = tmpdir </> "import/jane/otherbank/creditcard/2017-include.journal"
        let jane6 = tmpdir </> "import/jane/otherbank/creditcard/2018-include.journal"
        let jane7 = tmpdir </> "import/jane/otherbank/investments/2018-include.journal"
        let jane8 = tmpdir </> "import/jane/otherbank/investments/2019-include.journal"

        let john1 = tmpdir </> "import/john/bogartbank/checking/2018-include.journal"
        let john2 = tmpdir </> "import/john/bogartbank/checking/2019-include.journal"
        let john3 = tmpdir </> "import/john/bogartbank/savings/2017-include.journal"
        let john4 = tmpdir </> "import/john/bogartbank/savings/2018-include.journal"
        let john5 = tmpdir </> "import/john/otherbank/creditcard/2017-include.journal"
        let john6 = tmpdir </> "import/john/otherbank/creditcard/2018-include.journal"
        let john7 = tmpdir </> "import/john/otherbank/investments/2018-include.journal"
        let john8 = tmpdir </> "import/john/otherbank/investments/2019-include.journal"
        let expectedIncludes = [jane1, jane2, jane3, jane4, jane5, jane6, jane7, jane8,
                                john1, john2, john3, john4, john5, john6, john7, john8]

        ch <- liftIO newTChanIO
        reportedAsWritten <- single $ groupAndWriteIncludeFiles (defaultOpts tmpdir) ch importedJournals
        liftIO $ assertEqual "groupAndWriteIncludeFiles should return which files it wrote" expectedIncludes reportedAsWritten

        let allYears = [tmpdir </> "import/jane/bogartbank/checking/all-years.journal",
                        tmpdir </> "import/jane/bogartbank/savings/all-years.journal",
                        tmpdir </> "import/jane/otherbank/creditcard/all-years.journal",
                        tmpdir </> "import/jane/otherbank/investments/all-years.journal",
                        tmpdir </> "import/john/bogartbank/checking/all-years.journal",
                        tmpdir </> "import/john/bogartbank/savings/all-years.journal",
                        tmpdir </> "import/john/otherbank/creditcard/all-years.journal",
                        tmpdir </> "import/john/otherbank/investments/all-years.journal"]
        let expectedOnDisk = List.sort $ reportedAsWritten ++ extras ++ importedJournals ++ allYears
        allFilesOnDisk <- single $ sort $ onlyFiles $ lstree tmpdir
        liftIO $ assertEqual "The actual files on disk should match what groupAndWriteIncludeFiles reported" expectedOnDisk allFilesOnDisk

        let expectedJohn1Contents = includePreamble <> "\n"
              <> "!include 3-journal/2018/2018-10-30.journal\n"
              <> "!include 3-journal/2018/2018-11-30.journal\n"
              <> "!include 3-journal/2018/2018-12-30.journal\n"
        actualJohn1Contents <- liftIO $ readTextFile john1
        liftIO $ assertEqual "John1: The include file contents should be the journal files" expectedJohn1Contents actualJohn1Contents

        let expectedJohn2Contents = includePreamble <> "\n"
              <> "!include 3-journal/2019/2019-01-30.journal\n"
              <> "!include 3-journal/2019/2019-02-30.journal\n"
        actualJohn2Contents <- liftIO $ readTextFile john2
        liftIO $ assertEqual "John2: The include file contents should be the journal files" expectedJohn2Contents actualJohn2Contents

        let expectedJohn3Contents = includePreamble <> "\n"
              <> "!include 2017-opening.journal\n"
              <> "!include 3-journal/2017/2017-11-30.journal\n"
              <> "!include 3-journal/2017/2017-12-30.journal\n"
        actualJohn3Contents <- liftIO $ readTextFile john3
        liftIO $ assertEqual "John3: The include file contents should be the journal files" expectedJohn3Contents actualJohn3Contents

        let expectedJohn4Contents = includePreamble <> "\n"
              <> "!include 3-journal/2018/2018-01-30.journal\n"
              <> "!include 3-journal/2018/2018-02-30.journal\n"
        actualJohn4Contents <- liftIO $ readTextFile john4
        liftIO $ assertEqual "John4: The include file contents should be the journal files" expectedJohn4Contents actualJohn4Contents

        let expectedJane7Contents = includePreamble <> "\n"
              <> "!include 3-journal/2018/2018-12-30.journal\n"
        actualJane7Contents <- liftIO $ readTextFile jane7
        liftIO $ assertEqual "Jane7: The include file contents should be the journal files" expectedJane7Contents actualJane7Contents
     )
  )

tests :: Test
tests = TestList [testExtraIncludesForFile, testIncludesPrePost, testIncludesOpeningClosing, testWriteIncludeFiles]
