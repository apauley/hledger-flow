{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.PathHelpers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import qualified Path
import qualified Path.IO as Path
import System.Directory (doesDirectoryExist)
import qualified System.FilePath as FilePath

type AbsFile = Path.Path Path.Abs Path.File

type RelFile = Path.Path Path.Rel Path.File

type AbsDir = Path.Path Path.Abs Path.Dir

type RelDir = Path.Path Path.Rel Path.Dir

-- | Publish a complete file by renaming it on the destination filesystem.
withStagedOutput :: AbsFile -> (AbsFile -> IO ()) -> IO ()
withStagedOutput target writeOutput = do
  let dir = Path.parent target
  Path.createDirIfMissing True dir
  Path.withTempDir dir ".hledger-flow-" $ \tmp -> do
    let staged = tmp Path.</> Path.filename target
    writeOutput staged
    Path.renameFile staged target

-- | Match one literal path in an hledger glob. A range of one character
-- keeps hledger's include reader from treating the encoded spelling as a filename.
literalGlobPath :: FilePath -> FilePath
literalGlobPath = concatMap escape
  where
    escape c
      | c `elem` ("*?[{<" :: String) = ['[', c, '-', c, ']']
      | otherwise = [c]

data PathException = MissingBaseDir AbsDir | InvalidTurtleDir FilePath
  deriving (Eq)

instance Show PathException where
  show (MissingBaseDir d) =
    "No hledger-flow ledger here or in any parent folder (looked from "
      ++ Path.toFilePath d
      ++ ").\n"
      ++ "Run `hledger-flow init` to start one here, or pass the ledger's folder as an argument.\n"
      ++ "Docs: "
      ++ T.unpack (docURL GettingStarted)
  show (InvalidTurtleDir d) = "Not a folder: " ++ d

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
