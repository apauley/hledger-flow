{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.BaseDir
  ( determineBaseDir,
    relativeToBase,
    relativeToBase',
    turtleBaseDir,
    effectiveRunDir,
  )
where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.PathHelpers
import Hledger.Flow.Types (BaseDir, HasBaseDir, RunDir, baseDir)
import Path
import Path.IO
import qualified System.FilePath as FilePath

determineBaseDir :: Maybe TurtlePath -> IO (BaseDir, RunDir)
determineBaseDir suppliedDir = do
  pwd <- getCurrentDir
  determineBaseDir' pwd suppliedDir

determineBaseDir' :: AbsDir -> Maybe TurtlePath -> IO (BaseDir, RunDir)
determineBaseDir' pwd (Just suppliedDir) = do
  absDir <- turtleToAbsDir pwd suppliedDir
  determineBaseDirFromStartDir absDir
determineBaseDir' pwd Nothing = determineBaseDirFromStartDir pwd

determineBaseDirFromStartDir :: AbsDir -> IO (BaseDir, RunDir)
determineBaseDirFromStartDir startDir = determineBaseDirFromStartDir' startDir startDir

determineBaseDirFromStartDir' :: (MonadIO m, MonadThrow m) => AbsDir -> AbsDir -> m (BaseDir, RunDir)
determineBaseDirFromStartDir' startDir possibleBaseDir = do
  Control.Monad.when (parent possibleBaseDir == possibleBaseDir) $ throwM (MissingBaseDir startDir)
  foundBaseDir <- doesDirExist $ possibleBaseDir </> [reldir|import|]
  if foundBaseDir
    then do
      runDir <- limitRunDir possibleBaseDir startDir
      return (possibleBaseDir, runDir)
    else determineBaseDirFromStartDir' startDir $ parent possibleBaseDir

-- | We have unexpected behaviour when the runDir is deeper than the account directory,
-- e.g. "1-in" or the year directory. Specifically, include files are generated incorrectly
-- and some journals are written entirely outside of the baseDir.
-- limitRunDir can possibly removed if the above is fixed.
limitRunDir :: (MonadIO m, MonadThrow m) => BaseDir -> AbsDir -> m RunDir
limitRunDir bd absRunDir = do
  rel <- makeRelative bd absRunDir
  let runDirDepth = pathSize rel
  let fun = composeN (runDirDepth - 4) parent
  let newRunDir = fun rel
  when (runDirDepth > 4) $ do
    let msg = T.pack $ "Changing runDir from " ++ show rel ++ " to " ++ show newRunDir :: T.Text
    liftIO $ T.putStrLn msg
  return newRunDir

composeN :: Int -> (a -> a) -> (a -> a)
composeN n f
  | n < 1 = id
  | n == 1 = f
  | otherwise = composeN (n - 1) (f . f)

relativeToBase :: (HasBaseDir o) => o -> TurtlePath -> TurtlePath
relativeToBase opts = relativeToBase' $ pathToTurtle (baseDir opts)

relativeToBase' :: TurtlePath -> TurtlePath -> TurtlePath
relativeToBase' bd p
  | forceTrailingSlash bd == forceTrailingSlash p = "./"
  | otherwise =
      case stripPrefix (FilePath.splitPath (forceTrailingSlash bd)) (FilePath.splitPath (FilePath.normalise p)) of
        Nothing -> p
        Just parts ->
          let suffix = concat parts
           in if FilePath.takeFileName suffix == ".." then suffix ++ "/" else suffix

turtleBaseDir :: (HasBaseDir o) => o -> TurtlePath
turtleBaseDir opts = pathToTurtle $ baseDir opts

effectiveRunDir :: BaseDir -> RunDir -> AbsDir
effectiveRunDir bd rd = do
  let baseImportDir = bd </> [Path.reldir|import|]
  let absRunDir = bd </> rd
  if absRunDir == bd then baseImportDir else absRunDir
