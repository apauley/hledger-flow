{-# LANGUAGE CPP #-}

module Hledger.Flow.Import.PrivateDirectory (createPrivateDirectory) where

import Control.Monad (unless)
import Hledger.Flow.PathHelpers (AbsDir)
import Path (toFilePath)
import System.FilePath (dropTrailingPathSeparator)
#ifdef mingw32_HOST_OS
import Foreign.C.String (CWString, withCWString)
import System.Win32.Types (DWORD, failWith)

foreign import ccall safe "hf_create_private_directory"
  createDirectoryWithDACL :: CWString -> IO DWORD
#else
import Control.Exception (bracket, bracketOnError)
import Data.Bits ((.&.))
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix
import qualified System.Posix.IO as Posix
import qualified System.Posix.User as Posix
#ifdef darwin_HOST_OS
import Foreign.C.Error (throwErrnoPathIfMinus1_)
import Foreign.C.Types (CInt (..))
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "hf_verify_private_acl"
  verifyPrivateACL :: Fd -> IO CInt
#endif
#endif

-- | Create a fresh directory and verify privacy before any copies are made.
createPrivateDirectory :: AbsDir -> IO ()
#ifdef mingw32_HOST_OS
createPrivateDirectory path =
  withCWString (dropTrailingPathSeparator (toFilePath path)) $ \name -> do
    err <- createDirectoryWithDACL name
    unless (err == 0) (failWith ("Create private directory " ++ toFilePath path) err)
#else
createPrivateDirectory path =
  bracketOnError (Posix.createDirectory name 0o700) (const (Posix.removeDirectory name)) $ \() ->
    bracket (Posix.openFd name Posix.ReadOnly flags) Posix.closeFd $ \fd -> do
      status <- Posix.getFdStatus fd
      owner <- Posix.getEffectiveUserID
      unless (Posix.fileOwner status == owner && Posix.fileMode status .&. 0o777 == 0o700) $
        ioError (userError ("Cannot verify owner-only permissions on " ++ name))
#ifdef darwin_HOST_OS
      throwErrnoPathIfMinus1_ "Verify private directory ACL" name (verifyPrivateACL fd)
#endif
  where
    name = dropTrailingPathSeparator (toFilePath path)
    flags = Posix.defaultFileFlags {Posix.nofollow = True, Posix.directory = True, Posix.cloexec = True}
#endif
