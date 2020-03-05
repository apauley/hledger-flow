{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Common.Integration (tests) where

import Test.HUnit
import Turtle
import Prelude hiding (FilePath)
import qualified Data.List as List (sort)
import qualified Data.Text as T

import TestHelpers
import Hledger.Flow.Common

testHiddenFiles :: Test
testHiddenFiles = TestCase (
  sh (
      do
        let tmpbase = "." </> "test" </> "tmp"
        mktree tmpbase
        tmpdir <- using (mktempdir tmpbase "hlflowtest")
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

assertSubDirsForDetermineBaseDir :: FilePath -> FilePath -> [FilePath] -> IO ()
assertSubDirsForDetermineBaseDir initialPwd expectedBaseDir importDirs = do
  _ <- sequence $ map (assertDetermineBaseDir initialPwd expectedBaseDir) importDirs
  return ()

assertDetermineBaseDir :: FilePath -> FilePath -> FilePath -> IO ()
assertDetermineBaseDir initialPwd expectedBaseDir subDir = do
  cd initialPwd
  (bd1, runDir1) <- determineBaseDir $ Just subDir
  assertFindTestFileUsingRundir bd1 runDir1

  cd subDir
  (bd2, runDir2) <- determineBaseDir Nothing
  assertFindTestFileUsingRundir bd2 runDir2

  (bd3, runDir3) <- determineBaseDir $ Just "."
  assertFindTestFileUsingRundir bd3 runDir3

  (bd4, runDir4) <- determineBaseDir $ Just "./"
  assertFindTestFileUsingRundir bd4 runDir4

  cd initialPwd
  let msg = format ("determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - "%fp) subDir
  _ <- sequence $ map (assertEqual (T.unpack msg) expectedBaseDir) [bd1, bd2, bd3, bd4]
  return ()

assertFindTestFileUsingRundir :: FilePath -> FilePath -> IO ()
assertFindTestFileUsingRundir baseDir runDir = do
  let absRunDir = baseDir </> runDir
  let dirTestMsg = format ("Directories should end with a slash: "%fp)
  _ <- sequence $ map (\dir -> assertEqual (T.unpack $ dirTestMsg dir) (forceTrailingSlash dir) dir) [baseDir, runDir, absRunDir]

  found <- single $ fmap head $ shellToList $ find (has "test-file.txt") absRunDir
  fileContents <- readTextFile found
  assertEqual "We should find our test file by searching from the returned runDir" (format ("The expected base dir is "%fp) baseDir) fileContents

assertCurrentDirVariations :: FilePath -> FilePath -> IO ()
assertCurrentDirVariations absoluteTempDir bdRelativeToTempDir = do
  let absBaseDirNoTrailingSlash = absoluteTempDir </> bdRelativeToTempDir
  let absBaseDirWithTrailingSlash = forceTrailingSlash absBaseDirNoTrailingSlash
  assertEqual "Test the test: with and without slashes should really be that" '/' (T.last $ format fp absBaseDirWithTrailingSlash)
  assertBool "Test the test: with and without slashes should really be that" ('/' /= (T.last $ format fp absBaseDirNoTrailingSlash))

  cd absBaseDirNoTrailingSlash
  (bd1, runDir1) <- liftIO $ determineBaseDir Nothing
  (bd2, runDir2) <- liftIO $ determineBaseDir $ Just "."
  (bd3, runDir3) <- liftIO $ determineBaseDir $ Just "./"
  (bd4, runDir4) <- liftIO $ determineBaseDir $ Just absBaseDirNoTrailingSlash
  (bd5, runDir5) <- liftIO $ determineBaseDir $ Just absBaseDirWithTrailingSlash

  let msg label dir = format ("When pwd is the base dir, determineBaseDir returns the same "%s%", regardless of the input variation. "%s%":  "%fp) label label dir
  _ <- sequence $ map (\dir -> assertEqual (T.unpack $ msg "baseDir" dir) (forceTrailingSlash dir) dir) [bd1, bd2, bd3, bd4,bd5]
  _ <- sequence $ map (\dir -> assertEqual (T.unpack $ msg "runDir" dir) "./" dir) [runDir1, runDir2, runDir3, runDir4, runDir5]
  -- _ <- sequence $ map (\dir -> assertEqual (T.unpack $ msg "runDir" dir) "./" dir) [runDir2, runDir3, runDir4, runDir5]
  return ()

testDetermineBaseDir :: Test
testDetermineBaseDir = TestCase (
  sh (
      do
        error1 <- liftIO $ determineBaseDirFromAbsoluteStartDir "/path/to/dir"
        liftIO $ assertEqual "determineBaseDir produces an error message when given a non-existant dir" (Left "The provided directory does not exist: /path/to/dir") error1

        initialPwd <- fmap forceTrailingSlash pwd
        let tmpbase = initialPwd </> "test" </> "tmp"
        mktree tmpbase
        absoluteTempDir <- fmap forceTrailingSlash $ using (mktempdir tmpbase "hlflowtest")

        let unrelatedDir = absoluteTempDir </> "unrelated"
        mkdir unrelatedDir

        bdUnrelated <- liftIO $ determineBaseDirFromAbsoluteStartDir unrelatedDir
        liftIO $ assertEqual "determineBaseDir produces an error message when it cannot find a baseDir" (Left $ errorMessageBaseDir unrelatedDir) bdUnrelated

        let baseDir = "bd1"
        let importDir = baseDir </> "import"
        let ownerDir = importDir </> "john"
        let bankDir = ownerDir </> "mybank"
        let accDir = bankDir </> "myacc"
        let inDir = accDir </> "1-in"
        let yearDir = inDir </> "2019"
        let subDirs = [yearDir, inDir, accDir, bankDir, ownerDir, importDir, baseDir]

        mktree $ absoluteTempDir </> yearDir

        let fictionalDir = absoluteTempDir </> ownerDir </> "fictionalDir"
        errorSub <- liftIO $ determineBaseDirFromAbsoluteStartDir fictionalDir
        liftIO $ assertEqual "determineBaseDir produces an error message when given a non-existant subdir of a valid basedir" (Left $ format ("The provided directory does not exist: "%fp) fictionalDir) errorSub

        let relativeTempDir = foldl1 mappend (dropWhile (\el -> el `elem` (splitDirectories initialPwd)) (splitDirectories absoluteTempDir))


        let subDirsRelativeToTop = map (relativeTempDir </>) subDirs
        let absoluteSubDirs = map (absoluteTempDir </>) subDirs

        let absoluteBaseDir = forceTrailingSlash $ absoluteTempDir </> baseDir

        liftIO $ assertCurrentDirVariations absoluteTempDir baseDir

        liftIO $ writeTextFile (absoluteTempDir </> yearDir </> "test-file.txt") $ format ("The expected base dir is "%fp) absoluteBaseDir

        liftIO $ assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir subDirs
        liftIO $ assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir absoluteSubDirs
        liftIO $ assertSubDirsForDetermineBaseDir initialPwd absoluteBaseDir subDirsRelativeToTop
     )
  )

testFilterPaths :: Test
testFilterPaths = TestCase (
  sh (
      do
        let tmpbase = "." </> "test" </> "tmp"
        mktree tmpbase
        tmpdir <- using (mktempdir tmpbase "hlflowtest")
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

tests :: Test
tests = TestList [testDetermineBaseDir, testHiddenFiles, testFilterPaths]
