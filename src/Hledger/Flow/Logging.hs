{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Logging where

import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime (getZonedTime)
import qualified GHC.IO.Handle.FD as H
import Hledger.Flow.Types
import Turtle ((%))
import qualified Turtle

dummyLogger :: TChan LogMessage -> T.Text -> IO ()
dummyLogger _ _ = return ()

channelOut :: TChan LogMessage -> T.Text -> IO ()
channelOut ch txt = atomically $ writeTChan ch $ StdOut txt

channelOutLn :: TChan LogMessage -> T.Text -> IO ()
channelOutLn ch txt = channelOut ch (txt <> "\n")

channelErr :: TChan LogMessage -> T.Text -> IO ()
channelErr ch txt = atomically $ writeTChan ch $ StdErr txt

channelErrLn :: TChan LogMessage -> T.Text -> IO ()
channelErrLn ch txt = channelErr ch (txt <> "\n")

logToChannel :: TChan LogMessage -> T.Text -> IO ()
logToChannel ch msg = do
  ts <- timestampPrefix msg
  channelErrLn ch ts

timestampPrefix :: T.Text -> IO T.Text
timestampPrefix txt = do
  t <- getZonedTime
  return $ Turtle.format (Turtle.s % "\thledger-flow " % Turtle.s) (Turtle.repr t) txt

consoleChannelLoop :: TChan LogMessage -> IO ()
consoleChannelLoop ch = do
  logMsg <- atomically $ readTChan ch
  case logMsg of
    StdOut msg -> do
      T.hPutStr H.stdout msg
      consoleChannelLoop ch
    StdErr msg -> do
      T.hPutStr H.stderr msg
      consoleChannelLoop ch
    Terminate -> return ()

terminateChannelLoop :: TChan LogMessage -> IO ()
terminateChannelLoop ch = atomically $ writeTChan ch Terminate

logVerbose :: (HasVerbosity o) => o -> TChan LogMessage -> T.Text -> IO ()
logVerbose opts ch msg = when (verbose opts) $ logToChannel ch msg
