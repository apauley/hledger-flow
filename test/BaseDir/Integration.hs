{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BaseDir.Integration (tests) where

import Control.Exception (try)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.BaseDir (determineBaseDir, effectiveRunDir)
import Hledger.Flow.Common
import Hledger.Flow.PathHelpers
import Hledger.Flow.Types (BaseDir, RunDir)
import Path
import Path.IO
import Test.HUnit
import qualified Turtle
import Prelude hiding (readFile, writeFile)

assertSubDirsForDetermineBaseDir :: AbsDir -> BaseDir -> [Path.Path b Dir] -> IO ()
assertSubDirsForDetermineBaseDir initialPwd expectedBaseDir importDirs = do
  sequence_ $ map (assertDetermineBaseDir initialPwd expectedBaseDir) importDirs

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
  let msg dir = "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - " ++ show dir
  sequence_ $ map (\dir -> assertEqual (msg dir) expectedBaseDir dir) [bd1, bd2, bd3, bd4]

assertFindTestFileUsingRundir :: BaseDir -> RunDir -> IO ()
assertFindTestFileUsingRundir baseDir runDir = do
  let absRunDir = baseDir </> runDir
  foundFiles <- Turtle.single $ shellToList $ Turtle.find (Turtle.has "test-file.txt") $ pathToTurtle absRunDir
  case foundFiles of
    (found : _) -> do
      fileContents <- T.readFile found
      assertEqual "We should find our test file by searching from the returned runDir" (T.pack $ "The expected base dir is " ++ show baseDir) fileContents
    [] -> assertFailure "Expected to find test-file.txt but search returned no results"

assertCurrentDirVariations :: AbsDir -> RelDir -> IO ()
assertCurrentDirVariations absoluteTempDir bdRelativeToTempDir = do
  let absBaseDir = absoluteTempDir </> bdRelativeToTempDir
  withCurrentDir absBaseDir $ do
    (bd1, runDir1) <- determineBaseDir Nothing
    (bd2, runDir2) <- determineBaseDir $ Just "."
    (bd3, runDir3) <- determineBaseDir $ Just "./"
    (bd4, runDir4) <- determineBaseDir $ Just $ pathToTurtle absBaseDir

    let msg label dir = "When pwd is the base dir, determineBaseDir returns the same " ++ label ++ ", regardless of the input variation. " ++ show dir
    sequence_ $ map (\dir -> assertEqual (msg "baseDir" dir) absBaseDir dir) [bd1, bd2, bd3, bd4]
    sequence_ $ map (\dir -> assertEqual (msg "runDir" dir) [reldir|.|] dir) [runDir1, runDir2, runDir3, runDir4]

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

  T.writeFile (pathToTurtle $ absoluteTempDir </> yearDir </> [relfile|test-file.txt|]) (T.pack $ "The expected base dir is " ++ show absoluteBaseDir)

  assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir subDirs
  assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir absoluteSubDirs
  assertSubDirsForDetermineBaseDir initialPwd absoluteBaseDir subDirsRelativeToTop
  return ()

assertRunDirs :: RelDir -> [RelDir] -> [RelDir] -> IO ()
assertRunDirs accDir businessAsUsualRundirs specialTreatmentRundirs = do
  sequence_ $ map (assertRunDir id "Normal rundirs should not be modified") businessAsUsualRundirs
  sequence_ $ map (assertRunDir (\_ -> accDir) "Rundirs deeper than account-level should return the account dir instead") specialTreatmentRundirs

assertRunDir :: (RelDir -> RelDir) -> String -> RelDir -> IO ()
assertRunDir expectedRunDir msg subDir = do
  (_, runDir) <- determineBaseDir $ Just $ pathToTurtle subDir
  assertEqual msg (expectedRunDir subDir) runDir

testRunDirsWithTempDir :: AbsDir -> IO ()
testRunDirsWithTempDir absoluteTempDir = do
  let baseDir = absoluteTempDir </> [reldir|bd1|]

  let importDir = [reldir|import|]
  let ownerDir = importDir </> [reldir|john|]
  let bankDir = ownerDir </> [reldir|mybank|]
  let accDir = bankDir </> [reldir|myacc|]
  let inDir = accDir </> [reldir|1-in|]
  let yearDir = inDir </> [reldir|2019|]

  createDirIfMissing True $ baseDir </> yearDir

  withCurrentDir baseDir $ assertRunDirs accDir [accDir, bankDir, ownerDir, importDir] [yearDir, inDir]

testRunDirs :: Test
testRunDirs =
  TestCase
    ( do
        initialPwd <- getCurrentDir
        let tmpbase = initialPwd </> [reldir|test|] </> [reldir|tmp|]
        withTempDir tmpbase "hlflowtest" testRunDirsWithTempDir
    )

testDetermineBaseDir :: Test
testDetermineBaseDir =
  TestCase
    ( do
        initialPwd <- getCurrentDir
        let tmpbase = initialPwd </> [reldir|test|] </> [reldir|tmp|]
        createDirIfMissing True tmpbase
        withTempDir tmpbase "hlflowtest" $ testBaseDirWithTempDir initialPwd
    )

testEffectiveRunDir :: Test
testEffectiveRunDir =
  TestCase
    ( do
        let base = [absdir|/tmp/hlflow-base|]
        let expectedBaseImport = base </> [reldir|import|]
        assertEqual "A runDir of '.' should map to base/import" expectedBaseImport (effectiveRunDir base [reldir|.|])

        let runImport = [reldir|import|]
        assertEqual "A runDir under base should be preserved" (base </> runImport) (effectiveRunDir base runImport)

        let runOwner = [reldir|import/john|]
        assertEqual "A nested runDir should be preserved" (base </> runOwner) (effectiveRunDir base runOwner)
    )

tests :: Test
tests = TestList [testDetermineBaseDir, testRunDirs, testEffectiveRunDir]
