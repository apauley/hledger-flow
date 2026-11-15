{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Cache (cacheDir, initializeCache) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hLock)
import Hledger.Flow.PathHelpers (AbsDir, withStagedOutput)
import qualified Path as P
import qualified Path.IO as P
import System.IO (IOMode (AppendMode), withBinaryFile)

cacheDir :: AbsDir -> AbsDir
cacheDir base = base P.</> [P.reldir|.hledger-flow/cache|]

initializeCache :: AbsDir -> IO ()
initializeCache base = do
  let cache = cacheDir base
      ignore = cache P.</> [P.relfile|.gitignore|]
      lock = cache P.</> [P.relfile|initialize.lock|]
  exists <- P.doesFileExist ignore
  unless exists $ do
    P.createDirIfMissing True cache
    -- Serialize first use across processes. Closing the handle releases the lock.
    withBinaryFile (P.toFilePath lock) AppendMode $ \handle -> do
      hLock handle ExclusiveLock
      initialized <- P.doesFileExist ignore
      unless initialized $ withStagedOutput ignore $ \stage ->
        BS.writeFile (P.toFilePath stage) "*\n"
