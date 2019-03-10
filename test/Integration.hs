{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Map.Strict as Map
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Data.List as List (sort)

import TestHelpers
import Hledger.MakeItSo.Common

testHiddenFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
        let tmpJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let tmpExtras = map (tmpdir </>) extraFiles :: [FilePath]
        let tmpHidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        let onDisk = List.sort $ tmpJournals ++ tmpExtras ++ tmpHidden
        touchAll onDisk
        filtered <- (fmap List.sort) $ shellToList $ onlyFiles $ select onDisk
        let expected = List.sort $ tmpExtras ++ tmpJournals
        liftIO $ assertEqual "Hidden files should be excluded" expected filtered
     )
  )

testDirOrPwd = TestCase (
  sh (
      do
        currentDir <- fmap (\p -> directory (p </> "t")) pwd
        tmpdir <- using (mktempdir "." "makeitso")
        let fooDir = collapse $ currentDir </> tmpdir </> "foo/"
        let barDir = collapse $ currentDir </> tmpdir </> "bar/"
        mkdir fooDir
        mkdir barDir
        d1 <- liftIO $ dirOrPwd Nothing
        liftIO $ assertEqual "dirOrPwd returns pwd as a fallback" currentDir d1
        liftIO $ assertEqual "dirOrPwd assumes the fallback is a directory" (directory d1) d1
        d2 <- liftIO $ dirOrPwd $ Just $ tmpdir </> "foo"
        liftIO $ assertEqual "dirOrPwd returns the supplied dir - no trailing slash supplied" fooDir d2
        liftIO $ assertEqual "dirOrPwd assumes the supplied dir is a directory - no trailing slash supplied" (directory d2) d2
        d3 <- liftIO $ dirOrPwd $ Just $ tmpdir </> "bar/"
        liftIO $ assertEqual "dirOrPwd returns the supplied dir - trailing slash supplied" barDir d3
        liftIO $ assertEqual "dirOrPwd assumes the supplied dir is a directory - trailing slash supplied" (directory d3) d3
     )
  )

testFilterPaths = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
        let tmpJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let tmpExtras = map (tmpdir </>) extraFiles :: [FilePath]
        let tmpHidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        let onDisk = List.sort $ tmpJournals ++ tmpExtras ++ tmpHidden
        touchAll onDisk

        let nonExistant = map (tmpdir </>) ["where", "is", "my", "mind"]
        let toFilter = nonExistant ++ onDisk
        filtered <- single $ filterPaths testfile toFilter
        let actual = List.sort filtered
        liftIO $ assertEqual "The filtered paths should exclude files not actually on disk" onDisk actual
     )
  )

testExtraIncludesForFile = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
        let importedJournals = map (tmpdir </>) journalFiles :: [FilePath]
        let accountDir = "import/john/bogartbank/savings"
        let opening = tmpdir </> accountDir </> "2017-opening.journal"
        let closing = tmpdir </> accountDir </> "2017-closing.journal"
        let hidden = map (tmpdir </>) hiddenFiles :: [FilePath]
        touchAll $ importedJournals ++ hidden

        let accountInclude = tmpdir </> accountDir </> "2017-include.journal"
        let expectedEmpty = [(accountInclude, [])]

        extraOpening1 <- extraIncludesForFile (defaultOpts tmpdir) accountInclude ["opening.journal"] []
        liftIO $ assertEqual "The opening journal should not be included when it is not on disk" expectedEmpty extraOpening1

        extraClosing1 <- extraIncludesForFile (defaultOpts tmpdir) accountInclude ["closing.journal"] []
        liftIO $ assertEqual "The closing journal should not be included when it is not on disk" expectedEmpty extraClosing1

        touchAll [opening, closing]

        extraOpening2 <- extraIncludesForFile (defaultOpts tmpdir) accountInclude ["opening.journal"] []
        liftIO $ assertEqual "The opening journal should be included when it is on disk" [(accountInclude, [opening])] extraOpening2

        extraClosing2 <- extraIncludesForFile (defaultOpts tmpdir) accountInclude ["closing.journal"] []
        liftIO $ assertEqual "The closing journal should be included when it is on disk" [(accountInclude, [closing])] extraClosing2

        let oldJournalOpening = tmpdir </> accountDir </> "3-journal" </> "2017-opening.journal"
        superTouch oldJournalOpening
        extraJournalOpening <- extraIncludesForFile (defaultOpts tmpdir) accountInclude ["opening.journal"] []
        liftIO $ assertEqual "The old opening file in 3-journal should be included when it is on disk" [(accountInclude, [oldJournalOpening, opening])] extraJournalOpening
     ))

testWriteIncludeFiles = TestCase (
  sh (
      do
        tmpdir <- using (mktempdir "." "makeitso")
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

        reportedAsWritten <- single $ groupAndWriteIncludeFiles (defaultOpts tmpdir) importedJournals
        liftIO $ assertEqual "groupAndWriteIncludeFiles should return which files it wrote" expectedIncludes reportedAsWritten

        let expectedOnDisk = List.sort $ reportedAsWritten ++ extras ++ importedJournals
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

tests = TestList [testDirOrPwd, testExtraIncludesForFile, testHiddenFiles, testFilterPaths, testWriteIncludeFiles]
