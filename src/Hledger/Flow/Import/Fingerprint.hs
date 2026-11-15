{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hledger.Flow.Import.Fingerprint
  ( Digest (..), FileObservation (..), FileStamp (..),
    copyDigest, digestBytes, digestFile, observeFile, statFile
  ) where

import qualified Crypto.Hash.SHA256 as SHA
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Hledger.Flow.PathHelpers (AbsFile)
import Path (toFilePath)
import System.IO (Handle, IOMode (ReadMode, WriteMode), withBinaryFile)
#ifdef mingw32_HOST_OS
import qualified System.Win32.File as Win
import System.Win32.Time (FILETIME (..))
import System.Win32.Types (withHandleToHANDLE)
#else
import Control.Exception (bracket, bracketOnError)
import System.IO (hClose)
import qualified System.Posix.Files as Posix
import qualified System.Posix.IO as Posix
#endif

-- | Only the content digest is persisted across invocations.
newtype Digest = Digest {digestHex :: Text}
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data FileStamp = FileStamp
  { stampIdentity :: !(Integer, Integer),
    stampSize :: !Integer,
    stampModified :: !Rational,
    stampChanged :: !(Maybe Rational)
  }
  deriving (Eq, Show)

data FileObservation = FileObservation
  { fileStamp :: !FileStamp,
    observationDigest :: !Digest
  }
  deriving (Eq, Show)

hexDigest :: BS.ByteString -> Digest
hexDigest = Digest . decodeLatin1 . BL.toStrict . Builder.toLazyByteString
          . BS.foldr (mappend . Builder.word8HexFixed) mempty

digestBytes :: BS.ByteString -> Digest
digestBytes = hexDigest . SHA.hash

digestFile :: AbsFile -> IO Digest
digestFile path = withBinaryFile (toFilePath path) ReadMode (digestHandle (const (pure ())))

copyDigest :: AbsFile -> AbsFile -> IO Digest
copyDigest source destination =
  withBinaryFile (toFilePath source) ReadMode $ \input ->
    withBinaryFile (toFilePath destination) WriteMode $ \output ->
      digestHandle (BS.hPut output) input

digestHandle :: (BS.ByteString -> IO ()) -> Handle -> IO Digest
digestHandle consume handle = loop SHA.init
  where
    loop !context = do
      chunk <- BS.hGetSome handle (64 * 1024)
      if BS.null chunk
        then pure $! hexDigest (SHA.finalize context)
        else consume chunk >> loop (SHA.update context chunk)

-- | Reject an observation if the open file changed while it was being hashed.
observeFile :: AbsFile -> IO FileObservation
observeFile path = withStampedHandle path $ \handle readStamp -> do
  before <- readStamp
  digest <- digestHandle (const (pure ())) handle
  after <- readStamp
  if before == after
    then pure (FileObservation after digest)
    else ioError (userError ("File changed while hashing " ++ toFilePath path))

statFile :: AbsFile -> IO FileStamp
statFile path = withStampedHandle path $ \_ readStamp -> readStamp

withStampedHandle :: AbsFile -> (Handle -> IO FileStamp -> IO a) -> IO a
#ifdef mingw32_HOST_OS
withStampedHandle path action =
  withBinaryFile (toFilePath path) ReadMode $ \handle -> action handle $
    withHandleToHANDLE handle $ \native -> do
      info <- Win.getFileInformationByHandle native
      let FILETIME ticks = Win.bhfiLastWriteTime info
      pure FileStamp
        { stampIdentity = (toInteger (Win.bhfiVolumeSerialNumber info), toInteger (Win.bhfiFileIndex info)),
          stampSize = toInteger (Win.bhfiSize info),
          stampModified = fromIntegral ticks / 10000000,
          stampChanged = Nothing
        }
#else
withStampedHandle path action = bracket acquire (hClose . snd) $ \(fd, handle) ->
  action handle $ do
    info <- Posix.getFdStatus fd
    pure FileStamp
      { stampIdentity = (toInteger (Posix.deviceID info), toInteger (Posix.fileID info)),
        stampSize = toInteger (Posix.fileSize info),
        stampModified = toRational (Posix.modificationTimeHiRes info),
        stampChanged = Just (toRational (Posix.statusChangeTimeHiRes info))
      }
  where
    -- fdToHandle transfers ownership. Keep the borrowed fd for fstat until hClose.
    acquire = bracketOnError
      (Posix.openFd (toFilePath path) Posix.ReadOnly Posix.defaultFileFlags {Posix.cloexec = True})
      Posix.closeFd
      (\fd -> do handle <- Posix.fdToHandle fd
                 pure (fd, handle))
#endif
