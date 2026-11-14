{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Logging where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, tryJust)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GHC.IO.Handle.FD as H
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleIntensity (..), ConsoleLayer (..), SGR (..), hSupportsANSIColor, setSGRCode)
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush)
import Hledger.Flow.Types

-- | Colour only on a terminal, and only when NO_COLOR is unset or empty.
useColor :: Handle -> IO Bool
useColor h = do
  noColor <- lookupEnv "NO_COLOR"
  supported <- hSupportsANSIColor h
  return (supported && maybe True null noColor)

paint :: Bool -> [SGR] -> T.Text -> T.Text
paint False _ txt = txt
paint True sgr txt = T.pack (setSGRCode sgr) <> txt <> resetCode

resetCode :: T.Text
resetCode = T.pack (setSGRCode [Reset])

green, red, yellow, cyan, bold, dim :: Bool -> T.Text -> T.Text
green c = paint c [SetColor Foreground Vivid Green]
red c = paint c [SetColor Foreground Vivid Red]
yellow c = paint c [SetColor Foreground Vivid Yellow]
cyan c = paint c [SetColor Foreground Vivid Cyan]
bold c = paint c [SetConsoleIntensity BoldIntensity]
dim c = paint c [SetConsoleIntensity FaintIntensity]

channelOut :: TChan LogMessage -> T.Text -> IO ()
channelOut ch txt = atomically $ writeTChan ch $ StdOut txt

channelOutLn :: TChan LogMessage -> T.Text -> IO ()
channelOutLn ch txt = channelOut ch (txt <> "\n")

channelErr :: TChan LogMessage -> T.Text -> IO ()
channelErr ch txt = atomically $ writeTChan ch $ StdErr txt

channelErrLn :: TChan LogMessage -> T.Text -> IO ()
channelErrLn ch txt = channelErr ch (txt <> "\n")

-- | Mark the first line as hledger-flow's own, so it stands apart from what other programs print.
-- Callers indent any further lines.
verboseLine :: T.Text -> IO T.Text
verboseLine msg = do
  color <- useColor H.stderr
  return (dim color "hledger-flow:" <> " " <> msg <> "\n")

logToChannel :: TChan LogMessage -> T.Text -> IO ()
logToChannel ch msg = verboseLine msg >>= channelErr ch

-- | In verbose mode, show one item's lines together under a heading. With 'streaming' they
-- appear as they're logged; otherwise they're held back until the item is done, so items
-- that run in parallel don't interleave.
withLogBlock :: (HasVerbosity o) => o -> Bool -> TChan LogMessage -> T.Text -> (TChan LogMessage -> IO a) -> IO a
withLogBlock opts streaming ch heading action
  | not (verbose opts) = action ch
  | otherwise = do
      color <- useColor H.stderr
      line <- verboseLine (bold color heading)
      if streaming
        then channelErr ch ("\n" <> line) >> action ch
        else do
          buffer <- newTChanIO
          result <- tryJust synchronous (action buffer)
          atomically $ writeTChan ch (StdErr ("\n" <> line)) >> drain buffer
          either throwIO return result
  where
    drain buffer = tryReadTChan buffer >>= maybe (return ()) (\msg -> writeTChan ch msg >> drain buffer)

-- | Errors from the action itself, not cancellation from outside.
synchronous :: SomeException -> Maybe SomeException
synchronous err = case fromException err :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just err

withConsoleLogging :: (TChan LogMessage -> IO a) -> IO a
withConsoleLogging action = do
  ch <- newTChanIO
  withAsync (consoleChannelLoop ch) $ \logger -> do
    -- Drain queued diagnostics on failure, but allow Ctrl-C to cancel a blocked writer.
    result <- tryJust synchronous (action ch)
    terminateChannelLoop ch
    wait logger
    either throwIO return result

consoleChannelLoop :: TChan LogMessage -> IO ()
consoleChannelLoop ch = do
  logMsg <- atomically $ readTChan ch
  case logMsg of
    StdOut msg -> do
      T.hPutStr H.stdout msg
      -- Keep stdout and stderr lines in the order they were logged.
      hFlush H.stdout
      consoleChannelLoop ch
    StdErr msg -> do
      T.hPutStr H.stderr msg
      consoleChannelLoop ch
    Terminate -> return ()

terminateChannelLoop :: TChan LogMessage -> IO ()
terminateChannelLoop ch = atomically $ writeTChan ch Terminate

logVerbose :: (HasVerbosity o) => o -> TChan LogMessage -> T.Text -> IO ()
logVerbose opts ch msg = when (verbose opts) $ logToChannel ch msg
