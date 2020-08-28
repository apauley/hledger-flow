{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.BaseDir where

import Path
import Path.IO
import Hledger.Flow.Types (HasBaseDir, BaseDir, RunDir, baseDir)
import Hledger.Flow.PathHelpers

import Data.Maybe

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)


import qualified Turtle as Turtle (stripPrefix)

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
  _ <- if (parent possibleBaseDir == possibleBaseDir) then throwM (MissingBaseDir startDir) else return ()
  foundBaseDir <- doesDirExist $ possibleBaseDir </> [reldir|import|]
  if foundBaseDir then
    do
      runDir <- makeRelative possibleBaseDir startDir
      return (possibleBaseDir, runDir)
    else determineBaseDirFromStartDir' startDir $ parent possibleBaseDir

relativeToBase :: HasBaseDir o => o -> TurtlePath -> TurtlePath
relativeToBase opts = relativeToBase' $ pathToTurtle (baseDir opts)

relativeToBase' :: TurtlePath -> TurtlePath -> TurtlePath
relativeToBase' bd p = if forceTrailingSlash bd == forceTrailingSlash p then "./" else
  fromMaybe p $ Turtle.stripPrefix (forceTrailingSlash bd) p

turtleBaseDir :: HasBaseDir o => o -> TurtlePath
turtleBaseDir opts = pathToTurtle $ baseDir opts

effectiveRunDir :: BaseDir -> RunDir -> Bool -> AbsDir
effectiveRunDir bd rd useRunDir = do
  let baseImportDir = bd </> [Path.reldir|import|]
  let absRunDir = bd </> rd
  if useRunDir
    then if absRunDir == bd then baseImportDir else absRunDir
    else baseImportDir
