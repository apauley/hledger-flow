module Hledger.MakeItSo.Report.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data ReportOptions = ReportOptions { baseDir :: FilePath, verbose :: Bool }
