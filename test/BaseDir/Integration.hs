{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module BaseDir.Integration (tests) where

import Control.Exception (try)
import Test.HUnit

import Path
import Path.IO
import Prelude hiding (FilePath)

import qualified Turtle as Turtle
import qualified Data.Text as T

import Hledger.Flow.Common
import Hledger.Flow.Types (BaseDir, RunDir)
import Hledger.Flow.BaseDir (determineBaseDir)
import Hledger.Flow.PathHelpers

assertSubDirsForDetermineBaseDir :: AbsDir -> BaseDir -> [Path.Path b Dir] -> IO ()
assertSubDirsForDetermineBaseDir initialPwd expectedBaseDir importDirs = do
  _ <- sequence $ map (assertDetermineBaseDir initialPwd expectedBaseDir) importDirs
  return ()

assertDetermineBaseDir :: AbsDir -> BaseDir -> Path.Path b Dir -> IO ()
assertDetermineBaseDir initialPwd expectedBaseDir subDir = do
  setCurrentDir initialPwd
  (bd1, runDir1) <- determineBaseDir $ Just $ pathToTurtle subDir
  assertFindTestFileUsingRundir bd1 runDir1

  setCurrentDir subDir
  (bd2, runDir2) <- determineBaseDir Nothing
  assertFindTestFileUsingRundir bd2 runDir2

  (bd3, runDir3) <- determineBaseDir $ Just "."
  assertFindTestFileUsingRundir bd3 runDir3

  (bd4, runDir4) <- determineBaseDir $ Just "./"
  assertFindTestFileUsingRundir bd4 runDir4

  setCurrentDir initialPwd
  let msg dir = "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - " ++ (show dir)
  _ <- sequence $ map (\dir -> assertEqual (msg dir) expectedBaseDir dir) [bd1, bd2, bd3, bd4]
  return ()

assertFindTestFileUsingRundir :: BaseDir -> RunDir -> IO ()
assertFindTestFileUsingRundir baseDir runDir = do
  let absRunDir = baseDir </> runDir

  found <- Turtle.single $ fmap head $ shellToList $ Turtle.find (Turtle.has "test-file.txt") $ pathToTurtle absRunDir
  fileContents <- Turtle.readTextFile found
  assertEqual "We should find our test file by searching from the returned runDir" (T.pack $ "The expected base dir is " ++ show baseDir) fileContents

assertCurrentDirVariations :: AbsDir -> RelDir -> IO ()
assertCurrentDirVariations absoluteTempDir bdRelativeToTempDir = do
  let absBaseDir = absoluteTempDir </> bdRelativeToTempDir

  setCurrentDir absBaseDir
  (bd1, runDir1) <- determineBaseDir Nothing
  (bd2, runDir2) <- determineBaseDir $ Just "."
  (bd3, runDir3) <- determineBaseDir $ Just "./"
  (bd4, runDir4) <- determineBaseDir $ Just $ pathToTurtle absBaseDir

  let msg label dir = "When pwd is the base dir, determineBaseDir returns the same " ++ label ++ ", regardless of the input variation. " ++ (show dir)
  _ <- sequence $ map (\dir -> assertEqual (msg "baseDir" dir) absBaseDir dir) [bd1, bd2, bd3, bd4]
  _ <- sequence $ map (\dir -> assertEqual (msg "runDir" dir) [reldir|.|] dir) [runDir1, runDir2, runDir3, runDir4]
  return ()

testBaseDirWithTempDir :: AbsDir -> AbsDir -> IO ()
testBaseDirWithTempDir initialPwd absoluteTempDir = do
  error1 <- try $ determineBaseDir $ Just "/path/to/dir"
  assertEqual "determineBaseDir produces an error message when given a non-existant dir" (Left $ InvalidTurtleDir "/path/to/dir") error1

  let unrelatedDir = absoluteTempDir </> [reldir|unrelated|]
  createDir unrelatedDir

  bdUnrelated <- try $ determineBaseDir $ Just (pathToTurtle unrelatedDir)
  assertEqual "determineBaseDir produces an error message when it cannot find a baseDir" (Left $ MissingBaseDir unrelatedDir) bdUnrelated

  let baseDir = [reldir|bd1|]
  let importDir = baseDir </> [reldir|import|]
  let ownerDir = importDir </> [reldir|john|]
  let bankDir = ownerDir </> [reldir|mybank|]
  let accDir = bankDir </> [reldir|myacc|]
  let inDir = accDir </> [reldir|1-in|]
  let yearDir = inDir </> [reldir|2019|]
  let subDirs = [yearDir, inDir, accDir, bankDir, ownerDir, importDir, baseDir] :: [RelDir]

  createDirIfMissing True $ absoluteTempDir </> yearDir

  let fictionalDir = absoluteTempDir </> ownerDir </> [reldir|fictionalDir|]
  errorSub <- try $ determineBaseDir $ Just $ pathToTurtle fictionalDir
  assertEqual "determineBaseDir produces an error message when given a non-existant subdir of a valid basedir" (Left $ InvalidTurtleDir $ pathToTurtle fictionalDir) errorSub

  assertCurrentDirVariations absoluteTempDir baseDir

  relativeTempDir <- makeRelative initialPwd absoluteTempDir
  let subDirsRelativeToTop = map (relativeTempDir </>) subDirs
  let absoluteSubDirs = map (absoluteTempDir </>) subDirs

  let absoluteBaseDir = absoluteTempDir </> baseDir

  Turtle.writeTextFile (pathToTurtle $ absoluteTempDir </> yearDir </> [relfile|test-file.txt|]) (T.pack $ "The expected base dir is " ++ show absoluteBaseDir)

  assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir subDirs
  assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir absoluteSubDirs
  assertSubDirsForDetermineBaseDir initialPwd absoluteBaseDir subDirsRelativeToTop
  return ()

testDetermineBaseDir :: Test
testDetermineBaseDir = TestCase (
  do
    initialPwd <- getCurrentDir
    let tmpbase = initialPwd </> [reldir|test|] </> [reldir|tmp|]
    createDirIfMissing True tmpbase
    withTempDir tmpbase "hlflowtest" $ testBaseDirWithTempDir initialPwd
  )

tests :: Test
tests = TestList [testDetermineBaseDir]
