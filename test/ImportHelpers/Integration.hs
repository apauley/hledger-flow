{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ImportHelpers.Integration (tests) where

import qualified Data.List as List
import qualified Data.Text.IO as T
import Hledger.Flow.Import.ImportHelpers (findInputFiles, findJournalFiles)
import Hledger.Flow.PathHelpers (AbsFile)
import Path
import Path.IO
import Test.HUnit

touchFile :: AbsFile -> IO ()
touchFile f = do
  createDirIfMissing True (parent f)
  T.writeFile (toFilePath f) "x"

testFindInputFiles :: Test
testFindInputFiles =
  TestCase
    ( do
        cwd <- getCurrentDir
        let tmpbase = cwd </> [reldir|test|] </> [reldir|tmp|]
        createDirIfMissing True tmpbase
        withTempDir tmpbase "hlflowtest" $ \tmpdir -> do
          let file2018 = tmpdir </> [relfile|import/john/mybank/checking/1-in/2018/2018-01-01.csv|]
          let file2019 = tmpdir </> [relfile|import/john/mybank/checking/1-in/2019/2019-01-01.csv|]
          let file2020 = tmpdir </> [relfile|import/john/mybank/checking/1-in/2020/2020-01-01.csv|]
          let hidden = tmpdir </> [relfile|import/john/mybank/checking/1-in/2019/.hidden.csv|]
          let wrongYear = tmpdir </> [relfile|import/john/mybank/checking/1-in/20a0/20a0-01-01.csv|]
          let nested = tmpdir </> [relfile|import/john/mybank/checking/1-in/2019/subdir/2019-02-01.csv|]
          let preprocessed = tmpdir </> [relfile|import/john/mybank/checking/2-preprocessed/2019/pre.csv|]
          let manual = tmpdir </> [relfile|import/john/_manual_/2019/manual.journal|]
          let journal = tmpdir </> [relfile|import/john/mybank/checking/3-journal/2019/2019-01-01.journal|]

          mapM_ touchFile [file2018, file2019, file2020, hidden, wrongYear, nested, preprocessed, manual, journal]

          actual <- findInputFiles 2019 tmpdir
          let expected = List.sort [file2019, file2020]
          assertEqual "findInputFiles should return only year directories >= startYear, excluding hidden and excluded dirs" expected (List.sort actual)
    )

testFindJournalFiles :: Test
testFindJournalFiles =
  TestCase
    ( do
        cwd <- getCurrentDir
        let tmpbase = cwd </> [reldir|test|] </> [reldir|tmp|]
        createDirIfMissing True tmpbase
        withTempDir tmpbase "hlflowtest" $ \tmpdir -> do
          let journal2018 = tmpdir </> [relfile|import/john/mybank/checking/3-journal/2018/2018-01-01.journal|]
          let journal2019 = tmpdir </> [relfile|import/john/mybank/checking/3-journal/2019/2019-01-01.journal|]
          let hidden = tmpdir </> [relfile|import/john/mybank/checking/3-journal/2019/.hidden.journal|]
          let wrongYear = tmpdir </> [relfile|import/john/mybank/checking/3-journal/20a0/20a0-01-01.journal|]
          let nested = tmpdir </> [relfile|import/john/mybank/checking/3-journal/2019/subdir/2019-02-01.journal|]
          let input = tmpdir </> [relfile|import/john/mybank/checking/1-in/2019/2019-01-01.csv|]
          let preprocessed = tmpdir </> [relfile|import/john/mybank/checking/2-preprocessed/2019/pre.csv|]

          mapM_ touchFile [journal2018, journal2019, hidden, wrongYear, nested, input, preprocessed]

          actual <- findJournalFiles tmpdir
          let expected = List.sort [journal2018, journal2019]
          assertEqual "findJournalFiles should return only journal directory files, excluding hidden and excluded dirs" expected (List.sort actual)
    )

tests :: Test
tests = TestList [testFindInputFiles, testFindJournalFiles]
