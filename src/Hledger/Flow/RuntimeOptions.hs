module Hledger.Flow.RuntimeOptions
where

import qualified Data.Text as T
import Prelude hiding (putStrLn)
import Hledger.Flow.Types
import Hledger.Flow.Internals (SystemInfo)

data RuntimeOptions = RuntimeOptions { baseDir :: BaseDir
                                     , importRunDir :: RunDir
                                     , importStartYear :: Maybe Integer
                                     , onlyNewFiles :: Bool
                                     , hfVersion :: T.Text
                                     , hledgerInfo :: HledgerInfo
                                     , sysInfo :: SystemInfo
                                     , verbose :: Bool
                                     , showOptions :: Bool
                                     , sequential :: Bool
                                     , batchSize :: Int
                                     , prettyReports :: Bool
                                     }
  deriving (Show)

instance HasVerbosity RuntimeOptions where
  verbose (RuntimeOptions _ _ _ _ _ _ _ v _ _ _ _) = v

instance HasSequential RuntimeOptions where
  sequential (RuntimeOptions _ _ _ _ _ _ _ _ _ sq _ _) = sq

instance HasBatchSize RuntimeOptions where
  batchSize (RuntimeOptions _ _ _ _ _ _ _ _ _ _ bs _) = bs

instance HasBaseDir RuntimeOptions where
  baseDir (RuntimeOptions bd _ _ _ _ _ _ _ _ _ _ _) = bd

instance HasRunDir RuntimeOptions where
  importRunDir (RuntimeOptions _ rd _ _ _ _ _ _ _ _ _ _) = rd

instance HasPrettyReports RuntimeOptions where
  prettyReports (RuntimeOptions _ _ _ _ _ _ _ _ _ _ _ pr) = pr

