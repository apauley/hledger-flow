{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.PathHelpers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Hledger.Flow.DocHelpers (docURL)
import qualified Path
import qualified Path.IO as Path
import System.Directory (doesDirectoryExist)
import qualified System.FilePath as FilePath

type AbsFile = Path.Path Path.Abs Path.File

type RelFile = Path.Path Path.Rel Path.File

type AbsDir = Path.Path Path.Abs Path.Dir

type RelDir = Path.Path Path.Rel Path.Dir

data PathException = MissingBaseDir AbsDir | InvalidTurtleDir FilePath
  deriving (Eq)

instance Show PathException where
  show (MissingBaseDir d) =
    "Unable to find an import directory at "
      ++ show d
      ++ " (or in any of its parent directories).\n\n"
      ++ "Have a look at the documentation for more information:\n"
      ++ T.unpack (docURL "getting-started")
  show (InvalidTurtleDir d) = "Expected a directory but got this instead: " ++ d

instance Exception PathException

fromTurtleAbsFile :: (MonadThrow m) => FilePath -> m AbsFile
fromTurtleAbsFile turtlePath = Path.parseAbsFile turtlePath

fromTurtleRelFile :: (MonadThrow m) => FilePath -> m RelFile
fromTurtleRelFile turtlePath = Path.parseRelFile turtlePath

fromTurtleAbsDir :: (MonadThrow m) => FilePath -> m AbsDir
fromTurtleAbsDir turtlePath = Path.parseAbsDir turtlePath

fromTurtleRelDir :: (MonadThrow m) => FilePath -> m RelDir
fromTurtleRelDir turtlePath = Path.parseRelDir turtlePath

turtleToAbsDir :: (MonadIO m, MonadThrow m) => AbsDir -> FilePath -> m AbsDir
turtleToAbsDir baseDir p = do
  isDir <- liftIO (doesDirectoryExist p)
  if isDir
    then Path.resolveDir baseDir p
    else throwM $ InvalidTurtleDir p

pathToTurtle :: Path.Path b t -> FilePath
pathToTurtle = Path.toFilePath

forceTrailingSlash :: FilePath -> FilePath
forceTrailingSlash = FilePath.addTrailingPathSeparator . FilePath.normalise

pathSize :: Path.Path b Path.Dir -> Int
pathSize p = pathSize' p 0

pathSize' :: Path.Path b Path.Dir -> Int -> Int
pathSize' p count = if Path.parent p == p then count else pathSize' (Path.parent p) (count + 1)
