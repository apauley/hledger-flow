{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module CSVImport.Integration (tests) where

import Control.Concurrent.STM
import qualified Data.List as List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import Hledger.Flow.Import.ImportHelpersTurtle (extraIncludesForFile, groupAndWriteIncludeFiles, includePreamble, toIncludeFiles, writeIncludesUpTo, writeToplevelAllYearsInclude)
import Hledger.Flow.PathHelpers
import qualified Path
import qualified Path.IO as PathIO
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Test.HUnit
import TestHelpers (defaultOpts, withTestDir)
import TestHelpersTurtle (extraFiles, hiddenFiles, journalFiles, touchAll)
import Prelude hiding (readFile, writeFile)

testExtraIncludesForFile :: Test
testExtraIncludesForFile =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let importedJournals = map (tmpdir </>) journalFiles :: [TurtlePath]
    let accountDir = "import/john/bogartbank/savings"
    let opening = tmpdir </> accountDir </> "2017-opening.journal"
    let closing = tmpdir </> accountDir </> "2017-closing.journal"
    let hidden = map (tmpdir </>) hiddenFiles :: [TurtlePath]
    touchAll $ importedJournals ++ hidden

    let accountInclude = tmpdir </> accountDir </> "2017-include.journal"
    let expectedEmpty = [(accountInclude, [])]

    ch <- newTChanIO

    extraOpening1 <- extraIncludesForFile (defaultOpts tmpdirAbsPath) ch accountInclude ["opening.journal"] [] []
    assertEqual "The opening journal should not be included when it is not on disk" expectedEmpty extraOpening1

    extraClosing1 <- extraIncludesForFile (defaultOpts tmpdirAbsPath) ch accountInclude ["closing.journal"] [] []
    assertEqual "The closing journal should not be included when it is not on disk" expectedEmpty extraClosing1

    touchAll [opening, closing]

    extraOpening2 <- extraIncludesForFile (defaultOpts tmpdirAbsPath) ch accountInclude ["opening.journal"] [] []
    assertEqual "The opening journal should be included when it is on disk" [(accountInclude, [FilePath.normalise opening])] (Map.map (map FilePath.normalise) extraOpening2)

    extraClosing2 <- extraIncludesForFile (defaultOpts tmpdirAbsPath) ch accountInclude ["closing.journal"] [] []
    assertEqual "The closing journal should be included when it is on disk" [(accountInclude, [FilePath.normalise closing])] (Map.map (map FilePath.normalise) extraClosing2)

testExtraIncludesPrices :: Test
testExtraIncludesPrices =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let importedJournals = map (tmpdir </>) journalFiles :: [TurtlePath]
    touchAll $ importedJournals

    let priceFile = "prices" </> "2020" </> "prices.journal"

    let includeFile = tmpdir </> "import" </> "2020-include.journal"
    let expectedEmpty = [(includeFile, [])]

    ch <- newTChanIO

    price1 <- extraIncludesForFile (defaultOpts tmpdirAbsPath) ch includeFile [] [] ["prices.journal"]
    assertEqual "The price file should not be included when it is not on disk" expectedEmpty price1

    touchAll [tmpdir </> priceFile]
    let expectedPricePath = tmpdir </> "import" </> ".." </> priceFile

    price2 <- extraIncludesForFile (defaultOpts tmpdirAbsPath) ch includeFile [] [] ["prices.journal"]
    assertEqual "The price file should be included when it is on disk" [(includeFile, [FilePath.normalise expectedPricePath])] (Map.map (map FilePath.normalise) price2)

testIncludesPrePost :: Test
testIncludesPrePost =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let ownerDir = tmpdir </> "import" </> "john"
    let includeFile = ownerDir </> "2019-include.journal"
    let pre = ownerDir </> "_manual_" </> "2019" </> "pre-import.journal"
    let post = ownerDir </> "_manual_" </> "2019" </> "post-import.journal"
    touchAll [pre, post]

    let includeMap =
          Map.singleton
            includeFile
            [ ownerDir </> "bank1" </> "2019-include.journal",
              ownerDir </> "bank2" </> "2019-include.journal"
            ]

    ch <- newTChanIO
    fileMap <- toIncludeFiles (defaultOpts tmpdirAbsPath) ch includeMap
    let expectedText =
          includePreamble
            <> "\n"
            <> "include _manual_/2019/pre-import.journal\n"
            <> "include bank1/2019-include.journal\n"
            <> "include bank2/2019-include.journal\n"
            <> "include _manual_/2019/post-import.journal\n"
    let expectedMap = Map.singleton includeFile expectedText
    assertEqual "All pre/post files on disk should be included" expectedMap fileMap

testIncludesOpeningClosing :: Test
testIncludesOpeningClosing =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let ownerDir = tmpdir </> "import/john"
    let accountDir = ownerDir </> "bank1" </> "savings"
    let includeFile = accountDir </> "2019-include.journal"
    let opening = accountDir </> "2019-opening.journal"
    let closing = accountDir </> "2019-closing.journal"
    touchAll [opening, closing]

    let includeMap = Map.singleton includeFile [accountDir </> "3-journal" </> "2019" </> "2019-01-30.journal"]

    ch <- newTChanIO
    fileMap <- toIncludeFiles (defaultOpts tmpdirAbsPath) ch includeMap
    let expectedText =
          includePreamble
            <> "\n"
            <> "include 2019-opening.journal\n"
            <> "include 3-journal/2019/2019-01-30.journal\n"
            <> "include 2019-closing.journal\n"
    let expectedMap = Map.singleton includeFile expectedText
    assertEqual "All opening/closing files on disk should be included" expectedMap fileMap

testIncludesPrices :: Test
testIncludesPrices =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let importDir = tmpdir </> "import"
    let includeFile = importDir </> "2020-include.journal"
    let prices = tmpdir </> "prices" </> "2020" </> "prices.journal"
    let pre = importDir </> "_manual_" </> "2020" </> "pre-import.journal"
    let post = importDir </> "_manual_" </> "2020" </> "post-import.journal"
    touchAll [prices, pre, post]

    let includeMap = Map.singleton includeFile [importDir </> "john" </> "2020-include.journal"]

    ch <- newTChanIO
    fileMap <- toIncludeFiles (defaultOpts tmpdirAbsPath) ch includeMap
    let expectedText =
          includePreamble
            <> "\n"
            <> "include _manual_/2020/pre-import.journal\n"
            <> "include john/2020-include.journal\n"
            <> "include ../prices/2020/prices.journal\n"
            <> "include _manual_/2020/post-import.journal\n"
    let expectedMap = Map.singleton includeFile expectedText
    assertEqual "The price file should be included together with any pre/post files" expectedMap fileMap

testWriteIncludeFiles :: Test
testWriteIncludeFiles =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let importedJournals = map (tmpdir </>) journalFiles :: [TurtlePath]
    let extras = map (tmpdir </>) extraFiles :: [TurtlePath]
    let hidden = map (tmpdir </>) hiddenFiles :: [TurtlePath]
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
    let expectedIncludes =
          [ jane1,
            jane2,
            jane3,
            jane4,
            jane5,
            jane6,
            jane7,
            jane8,
            john1,
            john2,
            john3,
            john4,
            john5,
            john6,
            john7,
            john8
          ]

    ch <- newTChanIO
    reportedAsWritten <- groupAndWriteIncludeFiles (defaultOpts tmpdirAbsPath) ch importedJournals
    assertEqual "groupAndWriteIncludeFiles should return which files it wrote" expectedIncludes reportedAsWritten

    let allYears =
          [ tmpdir </> "import/jane/bogartbank/checking/all-years.journal",
            tmpdir </> "import/jane/bogartbank/savings/all-years.journal",
            tmpdir </> "import/jane/otherbank/creditcard/all-years.journal",
            tmpdir </> "import/jane/otherbank/investments/all-years.journal",
            tmpdir </> "import/john/bogartbank/checking/all-years.journal",
            tmpdir </> "import/john/bogartbank/savings/all-years.journal",
            tmpdir </> "import/john/otherbank/creditcard/all-years.journal",
            tmpdir </> "import/john/otherbank/investments/all-years.journal"
          ]
    let expectedOnDisk = List.sort $ reportedAsWritten ++ extras ++ importedJournals ++ allYears
    (_, allFiles) <- PathIO.listDirRecur tmpdirAbsPath
    let allFilesOnDisk = map Path.toFilePath (excludeHiddenFiles allFiles)
    assertEqual "The actual files on disk should match what groupAndWriteIncludeFiles reported" (List.sort $ map FilePath.normalise expectedOnDisk) (List.sort $ map FilePath.normalise allFilesOnDisk)

    let expectedJohn1Contents =
          includePreamble
            <> "\n"
            <> "include 3-journal/2018/2018-10-30.journal\n"
            <> "include 3-journal/2018/2018-11-30.journal\n"
            <> "include 3-journal/2018/2018-12-30.journal\n"
    actualJohn1Contents <- T.readFile john1
    assertEqual "John1: The include file contents should be the journal files" expectedJohn1Contents actualJohn1Contents

    let expectedJohn2Contents =
          includePreamble
            <> "\n"
            <> "include 3-journal/2019/2019-01-30.journal\n"
            <> "include 3-journal/2019/2019-02-30.journal\n"
    actualJohn2Contents <- T.readFile john2
    assertEqual "John2: The include file contents should be the journal files" expectedJohn2Contents actualJohn2Contents

    let expectedJohn3Contents =
          includePreamble
            <> "\n"
            <> "include 2017-opening.journal\n"
            <> "include 3-journal/2017/2017-11-30.journal\n"
            <> "include 3-journal/2017/2017-12-30.journal\n"
    actualJohn3Contents <- T.readFile john3
    assertEqual "John3: The include file contents should be the journal files" expectedJohn3Contents actualJohn3Contents

    let expectedJohn4Contents =
          includePreamble
            <> "\n"
            <> "include 3-journal/2018/2018-01-30.journal\n"
            <> "include 3-journal/2018/2018-02-30.journal\n"
    actualJohn4Contents <- T.readFile john4
    assertEqual "John4: The include file contents should be the journal files" expectedJohn4Contents actualJohn4Contents

    let expectedJane7Contents =
          includePreamble
            <> "\n"
            <> "include 3-journal/2018/2018-12-30.journal\n"
    actualJane7Contents <- T.readFile jane7
    assertEqual "Jane7: The include file contents should be the journal files" expectedJane7Contents actualJane7Contents

testWriteToplevelAllYearsInclude :: Test
testWriteToplevelAllYearsInclude =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    written1 <- writeToplevelAllYearsInclude (defaultOpts tmpdirAbsPath)
    let expectedFile = tmpdir </> "all-years.journal"
    assertEqual "Should write the top-level all-years include file" [expectedFile] written1
    let expectedNoDirectives =
          includePreamble
            <> "\n"
            <> "include import/all-years.journal\n"
    actualNoDirectives <- T.readFile expectedFile
    assertEqual "Top-level include should not reference directives when missing" expectedNoDirectives actualNoDirectives

    let directives = tmpdir </> "directives.journal"
    touchAll [directives]
    written2 <- writeToplevelAllYearsInclude (defaultOpts tmpdirAbsPath)
    assertEqual "Should overwrite the top-level all-years include file" [expectedFile] written2
    let expectedWithDirectives =
          includePreamble
            <> "\n"
            <> "include directives.journal\n"
            <> "include import/all-years.journal\n"
    actualWithDirectives <- T.readFile expectedFile
    assertEqual "Top-level include should reference directives when present" expectedWithDirectives actualWithDirectives

testWriteIncludesUpTo :: Test
testWriteIncludesUpTo =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let journal = tmpdir </> "import/john/mybank/checking/3-journal/2019/2019-01-01.journal"
    touchAll [journal]

    ch <- newTChanIO
    let stopAt = FilePath.addTrailingPathSeparator (tmpdir </> "import")
    written <- writeIncludesUpTo (defaultOpts tmpdirAbsPath) ch stopAt [journal]
    let expectedFinal = tmpdir </> "import/2019-include.journal"
    assertEqual "writeIncludesUpTo should stop at the requested directory" [expectedFinal] written

    finalContents <- T.readFile expectedFinal
    let expectedContents =
          includePreamble
            <> "\n"
            <> "include john/2019-include.journal\n"
    assertEqual "Top-level year include should reference the owner include" expectedContents finalContents

tests :: Test
tests =
  TestList
    [ testExtraIncludesForFile,
      testExtraIncludesPrices,
      testIncludesPrePost,
      testIncludesOpeningClosing,
      testIncludesPrices,
      testWriteIncludeFiles,
      testWriteToplevelAllYearsInclude,
      testWriteIncludesUpTo
    ]
