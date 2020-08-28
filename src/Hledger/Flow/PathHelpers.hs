{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.PathHelpers where

import Control.Monad.Catch (MonadThrow, Exception, throwM)
import Control.Monad.IO.Class (MonadIO)

import qualified Data.Text as T
import qualified Path as Path
import qualified Path.IO as Path
import qualified Turtle as Turtle

import Hledger.Flow.DocHelpers (docURL)

type TurtlePath = Turtle.FilePath

type AbsFile = Path.Path Path.Abs Path.File
type RelFile = Path.Path Path.Rel Path.File
type AbsDir = Path.Path Path.Abs Path.Dir
type RelDir = Path.Path Path.Rel Path.Dir

data PathException = MissingBaseDir AbsDir | InvalidTurtleDir TurtlePath
  deriving (Eq)

instance Show PathException where
  show (MissingBaseDir d) = "Unable to find an import directory at " ++ show d ++
    " (or in any of its parent directories).\n\n" ++
    "Have a look at the documentation for more information:\n" ++
    T.unpack (docURL "getting-started")
  show (InvalidTurtleDir  d) = "Expected a directory but got this instead: " ++ Turtle.encodeString d

instance Exception PathException

fromTurtleAbsFile :: MonadThrow m => TurtlePath -> m AbsFile
fromTurtleAbsFile turtlePath = Path.parseAbsFile $ Turtle.encodeString turtlePath

fromTurtleRelFile :: MonadThrow m => TurtlePath -> m RelFile
fromTurtleRelFile turtlePath = Path.parseRelFile $ Turtle.encodeString turtlePath

fromTurtleAbsDir :: MonadThrow m => TurtlePath -> m AbsDir
fromTurtleAbsDir turtlePath = Path.parseAbsDir $ Turtle.encodeString turtlePath

fromTurtleRelDir :: MonadThrow m => TurtlePath -> m RelDir
fromTurtleRelDir turtlePath = Path.parseRelDir $ Turtle.encodeString turtlePath

turtleToAbsDir :: (MonadIO m, MonadThrow m) => AbsDir -> TurtlePath -> m AbsDir
turtleToAbsDir baseDir p = do
  isDir <- Turtle.testdir p
  if isDir
    then Path.resolveDir baseDir $ Turtle.encodeString p
    else throwM $ InvalidTurtleDir p

pathToTurtle :: Path.Path b t -> TurtlePath
pathToTurtle = Turtle.decodeString . Path.toFilePath

forceTrailingSlash :: TurtlePath -> TurtlePath
forceTrailingSlash p = Turtle.directory (p Turtle.</> "temp")
