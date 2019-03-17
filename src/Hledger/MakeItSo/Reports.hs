{-# LANGUAGE OverloadedStrings #-}

module Hledger.MakeItSo.Reports
    ( generateReports
    ) where

import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)
import Hledger.MakeItSo.Report.Types

generateReports :: ReportOptions -> IO ()
generateReports _ = putStrLn "Generate reports: not yet implemented"
