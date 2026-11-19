{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Flow.LedgerSelection
  ( LedgerFile (..),
    Coverage (..),
    LedgerSelection (..),
    selectLedger,
    journalInputs,
    coverageFor,
    ledgerFileProblem,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict', encodeFile)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import Hledger.Flow.Common (configuredLedgerFile, hledgerFileArgument, runProcessCaptured)
import Hledger.Flow.InputSnapshot
import Hledger.Flow.PathHelpers (AbsFile, withStagedOutput)
import Hledger.Flow.Types (BaseDir, HledgerInfo (..))
import Path
import qualified System.Directory as Directory
import System.Exit (ExitCode (..))
import qualified System.FilePath as F

data Coverage = CoversLedger | UnrelatedLedger | UnreadableLedger T.Text
  deriving (Eq, Show)

data LedgerFile = LedgerFile
  { lfValue :: FilePath,
    lfExists :: Bool,
    lfCoverage :: Coverage
  }
  deriving (Eq, Show)

data LedgerSelection = LedgerSelection
  { selectedLedgerFile :: Maybe LedgerFile,
    aggregateJournal :: Maybe AbsFile,
    selectionError :: Maybe T.Text
  }
  deriving (Show)

data CachedInputs = CachedInputs FilePath FilePath T.Text InputSnapshot
  deriving (Generic)

instance FromJSON CachedInputs
instance ToJSON CachedInputs

journalInputs :: HledgerInfo -> [AbsFile] -> IO (Either T.Text [AbsFile])
journalInputs info files = do
  let args = ["--no-conf", "files"] ++ concatMap (\file -> ["--file", hledgerFileArgument info (toFilePath file)]) files
  result <- try (runProcessCaptured (T.pack (toFilePath (hlPath info)), args, Nothing))
  case result of
    Left err -> return (Left (T.pack (show (err :: IOException))))
    Right (ExitFailure _, _, err) -> return (Left (T.strip err))
    Right (ExitSuccess, out, _) -> do
      let paths = filter (not . T.null) (map (T.dropWhileEnd (== '\r')) (T.lines out))
      case paths of
        [] -> return (Left "Could not read the paths printed by hledger files.")
        _ -> do
          resolved <- try (traverse (resolveLedgerPath . T.unpack) paths)
          return (either (Left . T.pack . show) Right (resolved :: Either IOException [AbsFile]))

coverageFor :: FilePath -> Maybe FilePath -> [FilePath] -> Coverage
coverageFor index directives files
  | index `elem` files && maybe True (`elem` files) directives = CoversLedger
  | otherwise = UnrelatedLedger

ledgerFileProblem :: LedgerFile -> T.Text
ledgerFileProblem file
  | not (lfExists file) = "LEDGER_FILE is set to " <> T.pack (lfValue file) <> ", which doesn't exist."
  | otherwise = case lfCoverage file of
      CoversLedger -> ""
      UnrelatedLedger -> "LEDGER_FILE (" <> T.pack (lfValue file) <> ") does not include this ledger."
      UnreadableLedger err -> "Could not read LEDGER_FILE (" <> T.pack (lfValue file) <> "): " <> err

selectLedger :: Maybe HledgerInfo -> BaseDir -> IO LedgerSelection
selectLedger info base = do
  let root = base </> [relfile|all-years.journal|]
  rootExists <- Directory.doesFileExist (toFilePath root)
  configured <- configuredLedgerFile
  let missingRoot = "No all-years.journal in " <> T.pack (toFilePath base) <> " yet. Run `hledger-flow import` first."
      fallback value
        | rootExists = LedgerSelection value (Just root) Nothing
        | otherwise = LedgerSelection value Nothing (Just missingRoot)
  case configured of
    Nothing -> return (fallback Nothing)
    Just (value, path) -> do
      exists <- Directory.doesFileExist path
      if not exists
        then do
          let file = LedgerFile value False UnrelatedLedger
          return (LedgerSelection (Just file) Nothing (Just (ledgerFileProblem file <>
            " Point it at " <> T.pack (toFilePath root) <> ", or unset it.")))
        else do
          absolute <- resolveLedgerPath path
          canonical <- Directory.canonicalizePath (toFilePath absolute)
          rootPath <- Directory.canonicalizePath (toFilePath root)
          coverage <- if rootExists && canonical == rootPath
            then return CoversLedger
            else case info of
              Nothing -> return (UnreadableLedger "hledger is unavailable.")
              Just hl -> discoverCoverage hl base absolute
          let file = Just (LedgerFile value True coverage)
          return $ case coverage of
            CoversLedger -> LedgerSelection file (Just absolute) Nothing
            _ -> fallback file

-- Path excludes parent references. Resolve only the directory when needed
-- so hledger still reads a file symlink relative to its original location.
resolveLedgerPath :: FilePath -> IO AbsFile
resolveLedgerPath path = do
  absolute <- Directory.makeAbsolute path
  case parseAbsFile absolute of
    Just file -> return file
    Nothing -> do
      dir <- Directory.canonicalizePath (F.takeDirectory absolute)
      parseAbsFile (dir F.</> F.takeFileName absolute)

discoverCoverage :: HledgerInfo -> BaseDir -> AbsFile -> IO Coverage
discoverCoverage info base journal = do
  let cache = base </> [relfile|.hledger-flow/ledger-coverage.json|]
      path = toFilePath journal
      executable = toFilePath (hlPath info)
      version = hlVersion info
  cached <- try (decodeFileStrict' (toFilePath cache))
  reusable <- case cached of
    Right (Just (CachedInputs oldPath oldExecutable oldVersion snapshot))
      | (oldPath, oldExecutable, oldVersion) == (path, executable, version) -> do
          unchanged <- inputsMatch snapshot
          return (if unchanged then Just (inputTargets snapshot) else Nothing)
    Left (_ :: IOException) -> return Nothing
    _ -> return Nothing
  inputs <- case reusable of
    Just paths -> return (Right paths)
    Nothing -> do
      started <- getCurrentTime
      files <- journalInputs info [journal]
      case files of
        Left err -> return (Left err)
        Right paths -> do
          snapshot <- try (snapshotInputs paths)
          case snapshot of
            Right saved -> do
              unchanged <- inputsUnchanged saved started
              when unchanged $ do
                _ <- try (withStagedOutput cache (\stage ->
                  encodeFile (toFilePath stage) (CachedInputs path executable version saved))) :: IO (Either IOException ())
                return ()
              return (Right (inputTargets saved))
            Left (_ :: IOException) -> Right <$> traverse (Directory.canonicalizePath . toFilePath) paths
  case inputs of
    Left err -> return (UnreadableLedger err)
    Right paths -> do
      index <- Directory.canonicalizePath (toFilePath (base </> [relfile|import/all-years.journal|]))
      let directives = toFilePath (base </> [relfile|directives.journal|])
      hasDirectives <- Directory.doesFileExist directives
      required <- if hasDirectives then Just <$> Directory.canonicalizePath directives else return Nothing
      return (coverageFor index required paths)
