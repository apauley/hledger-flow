{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.ReleaseNotice
  ( ReleaseResult (..), withReleaseNotice, startCheck,
    finishCommand, checkRelease, renderUpdate
  ) where

import Control.Concurrent.Async (async, poll)
import Control.Exception (catch, evaluate, throwIO, try)
import Control.Monad (guard, join)
import Data.Aeson (decode, decodeFileStrict, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Version (Version, parseVersion, showVersion)
import Hledger.Flow.Status.Render (useColor, yellow)
import Network.HTTP.Client (HttpException, checkResponse, httpLbs, parseRequest, requestHeaders, responseBody, throwErrorStatusCodes)
import Network.HTTP.Client.TLS (getGlobalManager)
import Paths_hledger_flow (version)
import System.Directory (canonicalizePath)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hIsTerminalDevice, stderr, stdout)
import System.IO.Error (catchIOError)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

data ReleaseResult = UpToDate | UpdateAvailable !Version !T.Text | CheckUnavailable
  deriving (Eq, Show)

withReleaseNotice :: IO () -> IO ()
withReleaseNotice command = do
  finish <- setup `catchIOError` const (pure (pure ()))
  finishCommand finish command
  where
    setup = do
      disabled <- lookupEnv "HLEDGER_FLOW_NO_UPDATE_CHECK"
      outTTY <- hIsTerminalDevice stdout
      errTTY <- hIsTerminalDevice stderr
      if disabled /= Just "1" && outTTY && errTTY
        then startCheck (checkRelease fetch getExecutablePath)
        else pure (pure ())

-- The CLI owns the worker. Let process exit handle any pending network cleanup.
startCheck :: IO ReleaseResult -> IO (IO ())
startCheck check = do
  worker <- async (check >>= evaluate)
  pure $ do
    result <- poll worker
    case result of
      Just (Right (UpdateAvailable latest advice)) -> do
        color <- useColor stderr
        T.hPutStrLn stderr ("\n" <> renderUpdate color latest advice)
      _ -> pure ()

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

checkRelease :: (String -> IO (Maybe BL.ByteString)) -> IO FilePath -> IO ReleaseResult
checkRelease get executable = do
  result <- timeout 3000000 (get "https://api.github.com/repos/apauley/hledger-flow/releases/latest")
  case join result >>= releaseVersion of
    Just latest | latest > version -> do
      confirmed <- timeout 1000000 (homebrewOffers latest `catchIOError` const (pure False))
      let advice = if confirmed == Just True
            then "Upgrade with: brew update && brew upgrade ferntrail/tap/hledger-flow"
            else "https://github.com/apauley/hledger-flow/releases/latest"
      pure (UpdateAvailable latest advice)
    Just _ -> pure UpToDate
    Nothing -> pure CheckUnavailable
  where
    homebrewOffers latest = do
      path <- executable >>= canonicalizePath
      owned <- ownsHomebrewBinary path
      if owned then do
        formula <- get "https://raw.githubusercontent.com/FernTrail/homebrew-tap/main/Formula/hledger-flow.rb"
        let tagLine = "tag = \"v" <> renderVersion latest <> "\""
        pure $ fromMaybe False $ do
          body <- formula
          contents <- either (const Nothing) Just (T.decodeUtf8' (BL.toStrict body))
          pure $ tagLine `elem` map T.strip (T.lines contents)
      else pure False

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
