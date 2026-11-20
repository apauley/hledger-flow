{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.PathHelpers where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import qualified Path
import qualified Path.IO as Path
import System.Directory (doesDirectoryExist, doesPathExist, pathIsSymbolicLink)
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.FilePath as FilePath
#ifdef mingw32_HOST_OS
import qualified System.Win32.File as Win32
#else
import qualified System.Posix.Files as Posix
#endif

type AbsFile = Path.Path Path.Abs Path.File

type RelFile = Path.Path Path.Rel Path.File

type AbsDir = Path.Path Path.Abs Path.Dir

type RelDir = Path.Path Path.Rel Path.Dir

-- | Publish a complete file by renaming it on the destination filesystem.
withStagedOutput :: AbsFile -> (AbsFile -> IO a) -> IO a
withStagedOutput target writeOutput = do
  let dir = Path.parent target
  Path.createDirIfMissing True dir
  Path.withTempDir dir ".hledger-flow-" $ \tmp -> do
    let staged = tmp Path.</> Path.filename target
    result <- writeOutput staged
    Path.renameFile staged target
    return result

-- | Use this operation's own filesystem timestamp, even if another process
-- publishes a marker at the same destination.
writeTimestampMarker :: AbsFile -> IO UTCTime
writeTimestampMarker target = withStagedOutput target $ \stage -> do
  B.writeFile (Path.toFilePath stage) B.empty
  Path.getModificationTime stage

-- | Install complete UTF-8 bytes at an unused path. The final operation must
-- fail for an existing entry, including a dangling symlink.
writeNewUtf8File :: AbsFile -> T.Text -> IO ()
writeNewUtf8File target contents = do
  let dir = Path.parent target
  Path.createDirIfMissing True dir
  Path.withTempDir dir ".hledger-flow-" $ \tmp -> do
    let staged = tmp Path.</> Path.filename target
    B.writeFile (Path.toFilePath staged) (T.encodeUtf8 contents)
    publishNewFile staged target

-- | Publish a staged file exclusively, without replacing an existing target.
publishNewFile :: AbsFile -> AbsFile -> IO ()
publishNewFile staged target =
#ifdef mingw32_HOST_OS
  Win32.moveFile (extendedWindowsPath src) (extendedWindowsPath dest)
#else
  Posix.createLink src dest
#endif
  where
    src = Path.toFilePath staged
    dest = Path.toFilePath target

#ifdef mingw32_HOST_OS
-- The raw Win32 move needs the file namespace for paths beyond MAX_PATH.
-- GHC file writes already use that namespace for ordinary absolute paths.
extendedWindowsPath :: FilePath -> FilePath
extendedWindowsPath path = case FilePath.normalise path of
  absolute@('\\' : '\\' : marker : '\\' : _) | marker == '?' || marker == '.' -> absolute
  '\\' : '\\' : unc -> "\\\\?\\UNC\\" ++ unc
  absolute -> "\\\\?\\" ++ absolute
#endif

pathOccupied :: Path.Path Path.Abs t -> IO Bool
pathOccupied file = do
  let name = FilePath.dropTrailingPathSeparator (Path.toFilePath file)
  exists <- doesPathExist name
  if exists then return True else
    pathIsSymbolicLink name `catchIOError` \err ->
      if isDoesNotExistError err then return False else ioError err

isGlobMetacharacter :: Char -> Bool
isGlobMetacharacter c = c `elem` ("*?[{<" :: String)

-- | Match one literal path in an hledger glob. A range of one character
-- keeps hledger's include reader from treating the encoded spelling as a filename.
literalGlobPath :: FilePath -> FilePath
literalGlobPath = concatMap escape
  where
    escape c
      | isGlobMetacharacter c = ['[', c, '-', c, ']']
      | otherwise = [c]

data PathException = MissingBaseDir AbsDir | NotADirectory FilePath | CannotCreate String FilePath
  deriving (Eq)

instance Show PathException where
  show = renderPathException id

initAdvice :: String
initAdvice = "Run `hledger-flow init --dry-run` to preview the files, then `hledger-flow init` to create them."

renderPathException :: (FilePath -> FilePath) -> PathException -> String
renderPathException display (MissingBaseDir d) =
  "No hledger-flow ledger here or in any parent folder (looked from "
    ++ display (FilePath.dropTrailingPathSeparator (Path.toFilePath d))
    ++ ").\n"
    ++ initAdvice ++ "\nTo use an existing ledger, pass its folder as an argument.\n"
    ++ "Docs: "
    ++ T.unpack (docURL GettingStarted)
renderPathException display (NotADirectory d) = "Not a folder: " ++ display d
renderPathException display (CannotCreate what d) = "Cannot create " ++ what ++ " at " ++ display d

instance Exception PathException

resolveExistingDir :: (MonadIO m, MonadThrow m) => AbsDir -> FilePath -> m AbsDir
resolveExistingDir baseDir p = do
  isDir <- liftIO (doesDirectoryExist p)
  if isDir
    then Path.resolveDir baseDir p
    else throwM $ NotADirectory p

forceTrailingSlash :: FilePath -> FilePath
forceTrailingSlash = FilePath.addTrailingPathSeparator . FilePath.normalise

pathSize :: Path.Path b Path.Dir -> Int
pathSize p = pathSize' p 0

pathSize' :: Path.Path b Path.Dir -> Int -> Int
pathSize' p count = if Path.parent p == p then count else pathSize' (Path.parent p) (count + 1)
