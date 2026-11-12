{-# LANGUAGE FlexibleInstances #-}

module Hledger.Flow.Types where

import qualified Data.Text as T
import Hledger.Flow.PathHelpers
import Data.Time.Clock (NominalDiffTime)
import System.Exit (ExitCode)
import System.IO (Handle)

type BaseDir = AbsDir

type RunDir = RelDir

data LogMessage = StdOut T.Text | StdErr T.Text | Terminate deriving (Show)

type FullOutput = (ExitCode, T.Text, T.Text)

type FullTimedOutput = (FullOutput, NominalDiffTime)

type ProcInput = (T.Text, [T.Text], Maybe Handle)

-- | The hledger config file hledger runs with: given with --hledger-conf, or found by hledger's own search.
data HledgerConf = GivenConf FilePath | FoundConf FilePath deriving (Eq, Show)

hledgerConfPath :: HledgerConf -> FilePath
hledgerConfPath (GivenConf f) = f
hledgerConfPath (FoundConf f) = f

data HledgerInfo = HledgerInfo
  { hlPath :: AbsFile,
    hlVersion :: T.Text
  }
  deriving (Show)

class HasVerbosity a where
  verbose :: a -> Bool

class HasBaseDir a where
  baseDir :: a -> BaseDir

class HasRunDir a where
  importRunDir :: a -> RunDir

class HasSequential a where
  sequential :: a -> Bool

class HasBatchSize a where
  batchSize :: a -> Int

class HasPrettyReports a where
  prettyReports :: a -> Bool
