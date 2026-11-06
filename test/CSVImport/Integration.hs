{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module CSVImport.Integration (tests) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import Hledger.Flow.Import.ImportHelpersTurtle (extraIncludesForFile, includePreamble, toIncludeFiles, writeIncludesFromInventory, writeToplevelAllYearsInclude)
import Hledger.Flow.Import.Inventory (findImportFiles)
import Hledger.Flow.RuntimeOptions (RuntimeOptions)
import Hledger.Flow.Types (LogMessage)
import Hledger.Flow.PathHelpers
import qualified Path
import qualified Path.IO as PathIO
import Data.Either (isLeft)
import System.IO.Error (tryIOError)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Test.HUnit
import TestHelpers (defaultOpts, withTestDir, touchAll)
import Prelude hiding (readFile, writeFile)

-- Refresh the inventory exactly as the CLI does before writing includes.
writeIncludes :: RuntimeOptions -> TChan LogMessage -> AbsDir -> [AbsFile] -> IO [AbsFile]
writeIncludes opts ch stopAt journals = do
  inventory <- findImportFiles stopAt
  writeIncludesFromInventory opts ch stopAt inventory journals

testExtraIncludesForFile :: Test
testExtraIncludesForFile =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let accountDir = "import/john/bogartbank/savings"
    let opening = tmpdir </> accountDir </> "2017-opening.journal"
    let closing = tmpdir </> accountDir </> "2017-closing.journal"

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


    let priceFile = "prices" </> "2020" </> "prices.journal"

    let includeFile = tmpdir </> "import" </> "2020-include.journal"
    let expectedEmpty = [(includeFile, [])]

    Path.parseAbsDir (tmpdir </> "import") >>= PathIO.createDir
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

testWriteIncludes :: Test
testWriteIncludes =
  TestCase $ withTestDir $ \tmpdirAbsPath -> do
    let tmpdir = Path.toFilePath tmpdirAbsPath

    let journal = tmpdir </> "import/john/mybank/checking/3-journal/2019/2019-01-01.journal"
    touchAll [journal]

    ch <- newTChanIO
    stopAt <- Path.parseAbsDir (tmpdir </> "import")
    input <- Path.parseAbsFile journal
    expectedFinal <- Path.parseAbsFile (tmpdir </> "import/2019-include.journal")
    narrower <- Path.parseAbsDir (tmpdir </> "import" </> "other")
    rejected <- tryIOError (writeIncludesFromInventory (defaultOpts tmpdirAbsPath) ch narrower [] [input])
    assertBool "Reject a journal outside the boundary before writing" (isLeft rejected)
    PathIO.doesFileExist expectedFinal >>= assertBool "No ancestor include written on rejection" . not

    written <- writeIncludes (defaultOpts tmpdirAbsPath) ch stopAt [input]
    assertEqual "writeIncludes should stop at the requested directory" [expectedFinal] written

    finalContents <- T.readFile (Path.toFilePath expectedFinal)
    let expectedContents =
          includePreamble
            <> "\n"
            <> "include john/2019-include.journal\n"
    assertEqual "Top-level year include should reference the owner include" expectedContents finalContents

    _ <- writeIncludes (defaultOpts tmpdirAbsPath) ch stopAt []
    emptyIndex <- T.readFile (tmpdir </> "import" </> "all-years.journal")
    assertEqual "An empty import has a readable index" (includePreamble <> "\n") emptyIndex

testDiscoveredManualIncludes :: Test
testDiscoveredManualIncludes = TestCase $ withTestDir $ \base -> do
  let root = Path.toFilePath base </> "import"
      owner = root </> "john"
      account = owner </> "bank" </> "checking"
      journal = account </> "3-journal/2020/march.journal"
      pre = owner </> "_manual_/2020/pre-import.journal"
      post = owner </> "_manual_/2020/post-import.journal"
      unrelated = owner </> "2021-include.journal"
  touchAll [journal, pre, post, unrelated,
            owner </> "_manual_/2022/notes.journal",
            owner </> "_manual_/20x3/post-import.journal",
            account </> ".trash/2024/_manual_/2024/post-import.journal"]
  T.writeFile unrelated "; handwritten include, leave alone\n"
  stopAt <- Path.parseAbsDir root
  input <- Path.parseAbsFile journal
  ch <- newTChanIO
  _ <- writeIncludes (defaultOpts base) ch stopAt [input]
  contents <- T.readFile (owner </> "2020-include.journal")
  assertEqual "Discovered manual files surround generated children exactly once"
    (includePreamble <> "\ninclude _manual_/2020/pre-import.journal\ninclude bank/2020-include.journal\ninclude _manual_/2020/post-import.journal\n") contents
  index <- T.readFile (root </> "all-years.journal")
  assertEqual "Only supported manual paths establish years"
    (includePreamble <> "\ninclude 2020-include.journal\n") index
  mapM_ (\file -> Path.parseAbsFile file >>= PathIO.removeFile) ([journal, pre, post] :: [FilePath])
  _ <- writeIncludes (defaultOpts base) ch stopAt []
  mapM_ (\file -> do
      actual <- T.readFile file
      assertEqual "Retired year and account ledgers become empty" (includePreamble <> "\n") actual)
    ([owner </> "2020-include.journal", account </> "all-years.journal", root </> "2020-include.journal"] :: [FilePath])
  T.readFile unrelated >>= assertEqual "Do not clear handwritten indexes" "; handwritten include, leave alone\n"

tests :: Test
tests =
  TestList
    [ testExtraIncludesForFile,
      testExtraIncludesPrices,
      testIncludesPrePost,
      testIncludesOpeningClosing,
      testIncludesPrices,
      testWriteToplevelAllYearsInclude,
      testWriteIncludes,
      testDiscoveredManualIncludes
    ]
