{-# LANGUAGE OverloadedStrings #-}

module Hledger.MakeItSo.Reports
    ( generateReports
    ) where

import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)
import Hledger.MakeItSo.Report.Types
import Hledger.MakeItSo.Common

generateReports :: ReportOptions -> IO ()
generateReports opts = do
  logVerbose opts "Something will be here Real Soon Now (tm)"
  putStrLn "Report generation has not been implemented. Yet. https://github.com/apauley/hledger-makeitso/pull/4"
