{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.BaseDir (
    determineBaseDir
  , relativeToBase
  , relativeToBase'
  , turtleBaseDir
  , effectiveRunDir
) where

import Path
import Path.IO
import Hledger.Flow.Types (HasBaseDir, BaseDir, RunDir, baseDir)
import Hledger.Flow.PathHelpers

import Data.Maybe

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (when)

import qualified Turtle (liftIO, repr, stripPrefix)
import qualified Data.Text as T
import qualified Data.Text.IO as T

determineBaseDir :: Maybe TurtlePath -> IO (BaseDir, RunDir)
determineBaseDir suppliedDir = do
  pwd <- getCurrentDir
  determineBaseDir' pwd suppliedDir

determineBaseDir' :: AbsDir -> Maybe TurtlePath -> IO (BaseDir, RunDir)
determineBaseDir' pwd (Just suppliedDir) = do
  absDir <- turtleToAbsDir pwd suppliedDir
  determineBaseDirFromStartDir absDir
determineBaseDir' pwd Nothing = determineBaseDirFromStartDir pwd

determineBaseDirFromStartDir ::  AbsDir -> IO (BaseDir, RunDir)
determineBaseDirFromStartDir startDir = determineBaseDirFromStartDir' startDir startDir

determineBaseDirFromStartDir' :: (MonadIO m, MonadThrow m) => AbsDir -> AbsDir -> m (BaseDir, RunDir)
determineBaseDirFromStartDir' startDir possibleBaseDir = do
  Control.Monad.when (parent possibleBaseDir == possibleBaseDir) $ throwM (MissingBaseDir startDir)
  foundBaseDir <- doesDirExist $ possibleBaseDir </> [reldir|import|]
  if foundBaseDir then
    do
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
    let msg = T.pack $ "Changing runDir from " ++ Turtle.repr rel ++ " to " ++ Turtle.repr newRunDir :: T.Text
    Turtle.liftIO $ T.putStrLn msg
  return newRunDir

composeN :: Int -> (a -> a) -> (a -> a)
composeN n f | n < 1      = id
             | n == 1     = f
             | otherwise = composeN (n-1) (f . f)

relativeToBase :: HasBaseDir o => o -> TurtlePath -> TurtlePath
relativeToBase opts = relativeToBase' $ pathToTurtle (baseDir opts)

relativeToBase' :: TurtlePath -> TurtlePath -> TurtlePath
relativeToBase' bd p = if forceTrailingSlash bd == forceTrailingSlash p then "./" else
  fromMaybe p $ Turtle.stripPrefix (forceTrailingSlash bd) p

turtleBaseDir :: HasBaseDir o => o -> TurtlePath
turtleBaseDir opts = pathToTurtle $ baseDir opts

effectiveRunDir :: BaseDir -> RunDir -> AbsDir
effectiveRunDir bd rd = do
  let baseImportDir = bd </> [Path.reldir|import|]
  let absRunDir = bd </> rd
  if absRunDir == bd then baseImportDir else absRunDir
