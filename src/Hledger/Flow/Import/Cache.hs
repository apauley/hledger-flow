{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.Cache
  ( StageRecord (..), StatementRecord (..), StageInputs
  , initializeCache, recordPath, readRecord, saveRecord
  , stageMatches, installOutput, digestIfPresent
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hLock)
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.PathHelpers
import qualified Path as P
import qualified Path.IO as P
import System.IO (IOMode (AppendMode), withBinaryFile)
import System.IO.Error (tryIOError)

type StageInputs = [(Text, Text)]

data StageRecord = StageRecord
  { stageInputs :: StageInputs
  , stageOutput :: Digest
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data StatementRecord = StatementRecord
  { preprocessRecord :: Maybe StageRecord
  , journalRecord :: Maybe StageRecord
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Bump when a conversion's semantics, cache key or serialization changes.
recordVersion :: Int
recordVersion = 1

recordPath :: AbsDir -> AbsFile -> IO AbsFile
recordPath base source = do
  let year = P.parent source
      account = P.parent (P.parent year)
  group <- P.stripProperPrefix (base P.</> [P.reldir|import|]) account
  name <- P.addExtension ".json" (P.filename source)
  pure $ base P.</> [P.reldir|.hledger-flow/cache/import|] P.</> group
              P.</> P.dirname year P.</> name

initializeCache :: AbsDir -> IO ()
initializeCache base = do
  let cache = base P.</> [P.reldir|.hledger-flow/cache|]
      ignore = cache P.</> [P.relfile|.gitignore|]
      lock = cache P.</> [P.relfile|initialize.lock|]
  exists <- P.doesFileExist ignore
  unless exists $ do
    P.createDirIfMissing True cache
    -- Serialize first use across imports. Closing the handle releases the lock.
    withBinaryFile (P.toFilePath lock) AppendMode $ \handle -> do
      hLock handle ExclusiveLock
      initialized <- P.doesFileExist ignore
      unless initialized $ withStagedOutput ignore $ \stage ->
        BS.writeFile (P.toFilePath stage) "*\n"

readRecord :: AbsFile -> IO (Maybe StatementRecord)
readRecord path = do
  result <- tryIOError (BS.readFile (P.toFilePath path))
  pure $ case result of
    Right bytes | Right (version, record) <- eitherDecodeStrict' bytes
                , version == recordVersion -> Just record
    _ -> Nothing

saveRecord :: AbsFile -> StatementRecord -> IO ()
saveRecord path record = withStagedOutput path $ \stage ->
  BL.writeFile (P.toFilePath stage) (encode (recordVersion, record))

digestIfPresent :: AbsFile -> IO (Maybe Digest)
digestIfPresent path = either (const Nothing) Just <$> tryIOError (digestFile path)

stageMatches :: StageInputs -> AbsFile -> Maybe StageRecord -> IO Bool
stageMatches inputs output (Just record)
  | stageInputs record == inputs = (== Just (stageOutput record)) <$> digestIfPresent output
stageMatches _ _ _ = pure False

-- The writer owns its staged file until it returns. Hash it before publishing:
-- another process can install a different output as soon as the rename finishes.
installOutput :: Bool -> AbsFile -> (AbsFile -> IO ()) -> IO Digest
installOutput keepIdentical target writeOutput = do
  let dir = P.parent target
  P.createDirIfMissing True dir
  P.withTempDir dir ".hledger-flow-" $ \tmp -> do
    let staged = tmp P.</> P.filename target
    writeOutput staged
    digest <- digestFile staged
    existing <- if keepIdentical then digestIfPresent target else pure Nothing
    unless (existing == Just digest) $ P.renameFile staged target
    pure digest
