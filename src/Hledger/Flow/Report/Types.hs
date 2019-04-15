module Hledger.Flow.Report.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Hledger.Flow.Types

data ReportOptions = ReportOptions { baseDir :: FilePath
                                   , verbose :: Bool
                                   , showOptions :: Bool
                                   , sequential :: Bool }
  deriving (Show)

instance HasVerbosity ReportOptions where
  verbose (ReportOptions _ v _ _) = v

instance HasBaseDir ReportOptions where
  baseDir (ReportOptions bd _ _ _) = bd
