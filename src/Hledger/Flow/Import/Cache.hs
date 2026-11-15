{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.Cache
  ( StageRecord (..), StatementRecord (..), StageInputs
  , recordPath, readRecord, saveRecord
  , stageMiss, installOutput, digestIfPresent
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hledger.Flow.Cache (cacheDir)
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.PathHelpers
import qualified Path as P
import qualified Path.IO as P
import System.IO.Error (isDoesNotExistError, tryIOError)

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
recordVersion = 2  -- Earlier snapshots could overwrite a differently spelled input.

recordPath :: AbsDir -> AbsFile -> IO AbsFile
recordPath base source = do
  let year = P.parent source
      account = P.parent (P.parent year)
  group <- P.stripProperPrefix (base P.</> [P.reldir|import|]) account
  pure $ cacheDir base P.</> [P.reldir|import|] P.</> group
              P.</> P.dirname year P.</> P.filename source

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

-- Return the first difference in the same ordered key used for cache lookup.
-- File dependencies use their resolved path as the key, including nested rules.
stageMiss :: StageInputs -> AbsFile -> Maybe StageRecord -> IO (Maybe Text)
stageMiss _ _ Nothing = pure (Just "no valid record for this stage")
stageMiss inputs output (Just record) =
  case changedInput (stageInputs record) inputs of
    Just key -> pure (Just ("changed input: " <> key))
    Nothing -> do
      result <- tryIOError (digestFile output)
      pure $ case result of
        Left err | isDoesNotExistError err -> Just ("missing output: " <> path)
                 | otherwise -> Just ("could not read output: " <> path)
        Right digest | digest /= stageOutput record -> Just ("changed output: " <> path)
                     | otherwise -> Nothing
  where
    path = T.pack (P.toFilePath output)
    changedInput [] [] = Nothing
    changedInput ((key,_):_) [] = Just key
    changedInput [] ((key,_):_) = Just key
    changedInput (old:xs) (new:ys)
      | old == new = changedInput xs ys
      | otherwise = Just (fst new)

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
