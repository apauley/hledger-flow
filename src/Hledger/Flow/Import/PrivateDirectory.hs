{-# LANGUAGE CPP #-}

module Hledger.Flow.Import.PrivateDirectory (createPrivateDirectory) where

import Hledger.Flow.PathHelpers (AbsDir)
import Path (toFilePath)
#ifdef mingw32_HOST_OS
import Control.Monad (unless)
import Foreign.C.String (CWString, withCWString)
import qualified Path
import System.FilePath (dropTrailingPathSeparator)
import System.Win32.Types (DWORD, failWith)

foreign import ccall unsafe "hf_create_private_directory"
  createDirectoryWithDACL :: CWString -> CWString -> IO DWORD
#else
import qualified System.Posix.Directory as Posix
#endif

-- | Create a fresh directory with private permissions already in effect.
createPrivateDirectory :: AbsDir -> IO ()
#ifdef mingw32_HOST_OS
createPrivateDirectory path =
  withCWString (dropTrailingPathSeparator (toFilePath (Path.parent path))) $ \parent ->
    withCWString (dropTrailingPathSeparator (toFilePath path)) $ \name -> do
      err <- createDirectoryWithDACL parent name
      unless (err == 0) (failWith ("Create private directory " ++ toFilePath path) err)
#else
createPrivateDirectory path = Posix.createDirectory (toFilePath path) 0o700
#endif
