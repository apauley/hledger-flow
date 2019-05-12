module Hledger.Flow.Report.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Hledger.Flow.Types

data ReportOptions = ReportOptions { baseDir :: FilePath
                                   , hfVersion :: Text
                                   , hledgerInfo :: HledgerInfo
                                   , verbose :: Bool
                                   , showOptions :: Bool
                                   , sequential :: Bool
                                   }
  deriving (Show)

instance HasVerbosity ReportOptions where
  verbose (ReportOptions _ _ _ v _ _) = v

instance HasSequential ReportOptions where
  sequential (ReportOptions _ _ _ _ _ sq) = sq

instance HasBaseDir ReportOptions where
  baseDir (ReportOptions bd _ _ _ _ _) = bd
