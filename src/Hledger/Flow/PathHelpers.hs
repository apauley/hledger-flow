{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.PathHelpers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Hledger.Flow.DocHelpers (docURL)
import Path ((</>))
import qualified Path
import qualified Path.IO as Path
import qualified Turtle

type TurtlePath = Turtle.FilePath

type AbsFile = Path.Path Path.Abs Path.File

type RelFile = Path.Path Path.Rel Path.File

type AbsDir = Path.Path Path.Abs Path.Dir

type RelDir = Path.Path Path.Rel Path.Dir

data PathException = MissingBaseDir AbsDir | InvalidTurtleDir TurtlePath
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

fromTurtleAbsFile :: (MonadThrow m) => TurtlePath -> m AbsFile
fromTurtleAbsFile turtlePath = Path.parseAbsFile turtlePath

fromTurtleRelFile :: (MonadThrow m) => TurtlePath -> m RelFile
fromTurtleRelFile turtlePath = Path.parseRelFile turtlePath

fromTurtleAbsDir :: (MonadThrow m) => TurtlePath -> m AbsDir
fromTurtleAbsDir turtlePath = Path.parseAbsDir turtlePath

fromTurtleRelDir :: (MonadThrow m) => TurtlePath -> m RelDir
fromTurtleRelDir turtlePath = Path.parseRelDir turtlePath

turtleToAbsDir :: (MonadIO m, MonadThrow m) => AbsDir -> TurtlePath -> m AbsDir
turtleToAbsDir baseDir p = do
  isDir <- Turtle.testdir p
  if isDir
    then Path.resolveDir baseDir p
    else throwM $ InvalidTurtleDir p

pathToTurtle :: Path.Path b t -> TurtlePath
pathToTurtle = Path.toFilePath

forceTrailingSlash :: TurtlePath -> TurtlePath
forceTrailingSlash p = Turtle.directory (p Turtle.</> "temp")

pathSize :: Path.Path b Path.Dir -> Int
pathSize p = pathSize' p 0

pathSize' :: Path.Path b Path.Dir -> Int -> Int
pathSize' p count = if Path.parent p == p then count else pathSize' (Path.parent p) (count + 1)

-- | Do a recursive search starting from the given directory.
-- Return all files contained in each directory which matches the given predicate.
findFilesIn ::
  (MonadIO m) =>
  -- | Do we want the files in this directory?
  (AbsDir -> Bool) ->
  -- | Exclude these directory names
  [RelDir] ->
  -- | Top of the search tree
  AbsDir ->
  -- | Absolute paths to all files in the directories which match the predicate
  m [AbsFile]
findFilesIn includePred excludeDirs = Path.walkDirAccum (Just excludeHandler) accumulator
  where
    excludeHandler currentDir _ _ = return $ Path.WalkExclude (map (currentDir </>) excludeDirs)
    accumulator currentDir _ files =
      if includePred currentDir
        then return $ excludeHiddenFiles files
        else return []

excludeHiddenFiles :: [AbsFile] -> [AbsFile]
excludeHiddenFiles = filter (\f -> head (Path.toFilePath (Path.filename f)) /= '.')
