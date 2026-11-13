{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.BaseDir
  ( determineBaseDir,
    findBaseDir,
    relativeToBase,
    relativeToBase',
    baseDirFilePath,
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

determineBaseDir :: Maybe FilePath -> IO (BaseDir, RunDir)
determineBaseDir suppliedDir = do
  pwd <- getCurrentDir
  determineBaseDir' pwd suppliedDir

determineBaseDir' :: AbsDir -> Maybe FilePath -> IO (BaseDir, RunDir)
determineBaseDir' pwd (Just suppliedDir) = do
  absDir <- resolveExistingDir pwd suppliedDir
  determineBaseDirFromStartDir absDir
determineBaseDir' pwd Nothing = determineBaseDirFromStartDir pwd

determineBaseDirFromStartDir :: AbsDir -> IO (BaseDir, RunDir)
determineBaseDirFromStartDir startDir = determineBaseDirFromStartDir' startDir startDir

determineBaseDirFromStartDir' :: (MonadIO m, MonadThrow m) => AbsDir -> AbsDir -> m (BaseDir, RunDir)
determineBaseDirFromStartDir' startDir possibleBaseDir = do
  found <- liftIO $ findBaseDir possibleBaseDir
  case found of
    Nothing -> throwM (MissingBaseDir startDir)
    Just bd -> do
      runDir <- limitRunDir bd startDir
      return (bd, runDir)

-- | The nearest directory at or above the start directory that contains @import/@.
findBaseDir :: AbsDir -> IO (Maybe BaseDir)
findBaseDir dir = do
  found <- doesDirExist $ dir </> [reldir|import|]
  if found
    then return (Just dir)
    else if parent dir == dir then return Nothing else findBaseDir (parent dir)

-- | A directory below import/owner/bank/account selects that account.
limitRunDir :: (MonadIO m, MonadThrow m) => BaseDir -> AbsDir -> m RunDir
limitRunDir bd absRunDir = do
  rel <- makeRelative bd absRunDir
  let runDirDepth = pathSize rel
  let newRunDir = limitDepth runDirDepth rel
  when (runDirDepth > 4) $ do
    let msg = T.pack $ "Changing runDir from " ++ show rel ++ " to " ++ show newRunDir :: T.Text
    liftIO $ T.putStrLn msg
  return newRunDir

  where
    limitDepth depth dir
      | depth > 4 = limitDepth (depth - 1) (parent dir)
      | otherwise = dir

relativeToBase :: (HasBaseDir o) => o -> FilePath -> FilePath
relativeToBase opts = relativeToBase' $ baseDirFilePath opts

relativeToBase' :: FilePath -> FilePath -> FilePath
relativeToBase' bd p
  | forceTrailingSlash bd == forceTrailingSlash p = "./"
  | otherwise =
      case stripPrefix (FilePath.splitPath (forceTrailingSlash bd)) (FilePath.splitPath (FilePath.normalise p)) of
        Nothing -> p
        Just parts ->
          let suffix = concat parts
           in if FilePath.takeFileName suffix == ".." then suffix ++ "/" else suffix

baseDirFilePath :: (HasBaseDir o) => o -> FilePath
baseDirFilePath opts = toFilePath $ baseDir opts

effectiveRunDir :: BaseDir -> RunDir -> AbsDir
effectiveRunDir bd rd = do
  let baseImportDir = bd </> [Path.reldir|import|]
  let absRunDir = bd </> rd
  if absRunDir == bd then baseImportDir else absRunDir
