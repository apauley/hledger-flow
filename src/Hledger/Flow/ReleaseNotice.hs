{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.ReleaseNotice
  ( withReleaseNotice, startCheck, finishCommand, checkRelease
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

-- The worker belongs to this CLI invocation. Do not join it or synchronously
-- cancel it on exit: either would put network cleanup on the command's path.
startCheck :: IO (Maybe T.Text) -> IO (IO ())
startCheck check = do
  worker <- async $ do
    notice <- check
    traverse evaluate notice
  pure $ do
    result <- poll worker
    case result of
      Just (Right (Just notice)) -> T.hPutStrLn stderr notice
      _ -> pure ()

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

checkRelease :: (String -> IO (Maybe BL.ByteString)) -> IO FilePath -> IO (Maybe T.Text)
checkRelease get executable = do
  result <- timeout 3000000 (get "https://api.github.com/repos/apauley/hledger-flow/releases/latest")
  case join result >>= releaseVersion of
    Just latest | latest > version -> do
      confirmed <- timeout 1000000 (homebrewOffers latest `catchIOError` const (pure False))
      let advice = if confirmed == Just True
            then "Upgrade with: brew update && brew upgrade ferntrail/tap/hledger-flow"
            else "https://github.com/apauley/hledger-flow/releases/latest"
      pure $ Just $ "hledger-flow " <> render latest <> " is available (installed "
                 <> render version <> ").\n" <> advice
    _ -> pure Nothing
  where
    render = T.pack . showVersion
    homebrewOffers latest = do
      path <- executable >>= canonicalizePath
      owned <- ownsHomebrewBinary path
      if owned then do
        formula <- get "https://raw.githubusercontent.com/FernTrail/homebrew-tap/main/Formula/hledger-flow.rb"
        let tagLine = "tag = \"v" <> render latest <> "\""
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
