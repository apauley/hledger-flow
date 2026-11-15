{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.ReleaseNotice
  ( ReleaseResult (..), withReleaseNotice, withCachedReleaseNotice,
    prepareCheck, refreshIfDue, cachedRelease,
    finishCommand, renderUpdate
  ) where

import Control.Concurrent.Async (async, waitCatch)
import Control.Exception (catch, throwIO, try)
import Control.Monad (guard, join, void, when)
import Data.Aeson (FromJSON, ToJSON, decode, decodeFileStrict, encode, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Version (Version, parseVersion, showVersion)
import GHC.Generics (Generic)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hTryLock)
import Hledger.Flow.Cache (cacheDir, initializeCache)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, withStagedOutput)
import qualified Path as P
import Hledger.Flow.Status.Render (useColor, yellow)
import Network.HTTP.Client (HttpException, checkResponse, httpLbs, parseRequest, requestHeaders, responseBody, throwErrorStatusCodes)
import Network.HTTP.Client.TLS (getGlobalManager)
import Paths_hledger_flow (version)
import System.Directory (canonicalizePath)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (IOMode (AppendMode), hIsTerminalDevice, stderr, stdout, withBinaryFile)
import System.IO.Error (catchIOError)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

data ReleaseResult = UpToDate | UpdateAvailable !Version !T.Text | CheckUnavailable
  deriving (Eq, Show)

-- The cached version is compared with the binary reading it on every run.
data Observation = Observation
  { latestVersion :: !Version
  , checkedAt :: !UTCTime
  , formulaConfirmed :: !Bool
  } deriving (Generic, FromJSON, ToJSON)

data ReleaseCache = ReleaseCache
  { lastSuccess :: !(Maybe Observation)
  , lastFailure :: !(Maybe UTCTime)
  } deriving (Generic, FromJSON, ToJSON)

type Fetch = String -> IO (Maybe BL.ByteString)

cacheFile :: AbsDir -> AbsFile
cacheFile base = cacheDir base P.</> [P.relfile|release-check.json|]

readCache :: AbsDir -> IO ReleaseCache
readCache base = do
  record <- decodeFileStrict (P.toFilePath (cacheFile base)) `catchIOError` const (pure Nothing)
  pure (fromMaybe (ReleaseCache Nothing Nothing) record)

saveCache :: AbsDir -> ReleaseCache -> IO ()
saveCache base record = withStagedOutput (cacheFile base) $ \stage ->
  BL.writeFile (P.toFilePath stage) (encode record)

refreshDue :: UTCTime -> ReleaseCache -> Bool
refreshDue now record =
  not (maybe False (recent (24 * 60 * 60) . checkedAt) (lastSuccess record)
    || maybe False (recent (15 * 60)) (lastFailure record))
  where
    recent :: NominalDiffTime -> UTCTime -> Bool
    recent limit stamp = let age = diffUTCTime now stamp in age >= 0 && age < limit

cachedRelease :: AbsDir -> IO FilePath -> IO ReleaseResult
cachedRelease base executable = readCache base >>= releaseResult executable

releaseResult :: IO FilePath -> ReleaseCache -> IO ReleaseResult
releaseResult executable record = case lastSuccess record of
  Nothing -> pure CheckUnavailable
  Just observation
    | latestVersion observation <= version -> pure UpToDate
    | otherwise -> do
        owned <- if formulaConfirmed observation then brewOwned executable else pure False
        let advice = if owned
              then "Upgrade with: brew update && brew upgrade ferntrail/tap/hledger-flow"
              else "https://github.com/apauley/hledger-flow/releases/latest"
        pure (UpdateAvailable (latestVersion observation) advice)

withReleaseNotice :: AbsDir -> IO () -> IO ()
withReleaseNotice base = withNotice (prepareCheck base getCurrentTime fetch getExecutablePath)

withCachedReleaseNotice :: AbsDir -> IO () -> IO ()
withCachedReleaseNotice base = withNotice (pure (cachedRelease base getExecutablePath))

withNotice :: IO (IO ReleaseResult) -> IO () -> IO ()
withNotice prepare command = do
  finish <- setup `catchIOError` const (pure (pure ()))
  finishCommand finish command
  where
    setup = do
      disabled <- lookupEnv "HLEDGER_FLOW_NO_UPDATE_CHECK"
      outTTY <- hIsTerminalDevice stdout
      errTTY <- hIsTerminalDevice stderr
      if disabled /= Just "1" && outTTY && errTTY
        then do
          result <- prepare
          pure (result >>= display)
        else pure (pure ())
    display (UpdateAvailable latest advice) = do
      color <- useColor stderr
      T.hPutStrLn stderr ("\n" <> renderUpdate color latest advice)
    display _ = pure ()

prepareCheck :: AbsDir -> IO UTCTime -> Fetch -> IO FilePath -> IO (IO ReleaseResult)
prepareCheck base clock get executable = do
  record <- readCache base
  now <- clock
  if refreshDue now record then do
    initializeCache base
    worker <- async (refreshIfDue base clock get executable)
    pure $ do
      case lastSuccess record of
        Nothing -> void (timeout 250000 (waitCatch worker))
        Just _ -> pure ()
      cachedRelease base executable
  else pure (releaseResult executable record)

-- Called after initializeCache. Keep the lock file in place so all processes
-- contend on the same file even as the JSON record is atomically replaced.
refreshIfDue :: AbsDir -> IO UTCTime -> Fetch -> IO FilePath -> IO ()
refreshIfDue base clock get executable = do
  let lock = cacheDir base P.</> [P.relfile|release-check.lock|]
  withBinaryFile (P.toFilePath lock) AppendMode $ \handle -> do
    acquired <- hTryLock handle ExclusiveLock
    when acquired $ do
      -- A different process may have published since our first read.
      current <- readCache base
      started <- clock
      when (refreshDue started current) (refreshCache base clock get executable current)

renderUpdate :: Bool -> Version -> T.Text -> T.Text
renderUpdate color latest advice =
  "  " <> yellow color ("↑ " <> availableMessage latest) <> "\n    " <> advice

availableMessage :: Version -> T.Text
availableMessage latest = "hledger-flow " <> renderVersion latest <> " is available (installed "
                       <> renderVersion version <> ")."

renderVersion :: Version -> T.Text
renderVersion = T.pack . showVersion

-- The caller has already rendered ordinary errors as diagnostics + ExitCode.
-- Catch just ExitCode here so that interrupts skip the notice altogether.
finishCommand :: IO () -> IO () -> IO ()
finishCommand finish command = do
  outcome <- try command :: IO (Either ExitCode ())
  finish `catchIOError` const (pure ())
  either throwIO pure outcome

fetch :: String -> IO (Maybe BL.ByteString)
fetch url = request `catch` ignoreHttpError
  where
    request = do
      manager <- getGlobalManager
      req <- parseRequest url
      let githubRequest = req { requestHeaders = [("User-Agent", "hledger-flow")]
                              , checkResponse = throwErrorStatusCodes }
      response <- httpLbs githubRequest manager
      pure (Just (responseBody response))
    ignoreHttpError :: HttpException -> IO (Maybe BL.ByteString)
    ignoreHttpError _ = pure Nothing

refreshCache :: AbsDir -> IO UTCTime -> Fetch -> IO FilePath -> ReleaseCache -> IO ()
refreshCache base clock get executable previous = do
  result <- timeout 3000000 (get "https://api.github.com/repos/apauley/hledger-flow/releases/latest")
  now <- clock
  case join result >>= releaseVersion of
    Nothing -> saveCache base previous { lastFailure = Just now }
    Just latest -> do
      let save confirmed = saveCache base (ReleaseCache (Just (Observation latest now confirmed)) Nothing)
      -- A short command can exit during the optional formula lookup.
      save False
      when (latest > version) $ do
        owned <- brewOwned executable
        when owned $ do
          response <- timeout 1000000 (get "https://raw.githubusercontent.com/FernTrail/homebrew-tap/main/Formula/hledger-flow.rb")
          let published = do
                body <- join response
                contents <- either (const Nothing) Just (T.decodeUtf8' (BL.toStrict body))
                let tagLine = "tag = \"v" <> renderVersion latest <> "\""
                pure (tagLine `elem` map T.strip (T.lines contents))
          when (published == Just True) (save True)

brewOwned :: IO FilePath -> IO Bool
brewOwned executable =
  (executable >>= canonicalizePath >>= ownsHomebrewBinary) `catchIOError` const (pure False)

releaseVersion :: BL.ByteString -> Maybe Version
releaseVersion bytes = do
  value <- decode bytes
  tag <- parseMaybe (withObject "release" $ \o -> do
    draft <- o .:? "draft" .!= False
    prerelease <- o .:? "prerelease" .!= False
    guard (not draft && not prerelease)
    o .: "tag_name") value
  number <- T.stripPrefix "v" tag
  guard (T.all (\c -> isDigit c || c == '.') number)
  listToMaybe [v | (v, "") <- readP_to_S (parseVersion <* eof) (T.unpack number)]

ownsHomebrewBinary :: FilePath -> IO Bool
ownsHomebrewBinary path
  | map takeFileName [path, bin, rack, takeDirectory rack] /=
      ["hledger-flow", "bin", "hledger-flow", "Cellar"] = pure False
  | otherwise = do
      receipt <- decodeFileStrict (keg </> "INSTALL_RECEIPT.json")
      let tap = receipt >>= parseMaybe (withObject "receipt" $ \o ->
                  o .: "source" >>= withObject "source" (.: "tap"))
      pure $ fmap T.toCaseFold tap == Just "ferntrail/tap"
  where
    bin = takeDirectory path
    keg = takeDirectory bin
    rack = takeDirectory keg
