module Hledger.MakeItSo.Report.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Hledger.MakeItSo.Types

data ReportOptions = ReportOptions { baseDir :: FilePath, verbose :: Bool }
  deriving (Show)

instance HasVerbosity ReportOptions where
  verbose (ReportOptions _ v) = v

instance HasBaseDir ReportOptions where
  baseDir (ReportOptions bd _) = bd
