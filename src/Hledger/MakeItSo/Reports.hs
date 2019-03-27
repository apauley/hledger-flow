{-# LANGUAGE OverloadedStrings #-}

module Hledger.MakeItSo.Reports
    ( generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Hledger.MakeItSo.Report.Types
import Hledger.MakeItSo.Common
import Control.Concurrent.STM

generateReports :: ReportOptions -> IO ()
generateReports opts = sh (
  do
    ch <- liftIO newTChanIO
    logHandle <- fork $ consoleChannelLoop ch
    liftIO $ logVerbose opts ch "Something will be here Real Soon Now (tm)"
    liftIO $ channelOut ch "Report generation has not been implemented. Yet. https://github.com/apauley/hledger-makeitso/pull/4"
    liftIO $ terminateChannelLoop ch
    wait logHandle
  )
