module Hledger.Flow.RuntimeOptions where

import qualified Data.Text as T
import Hledger.Flow.PathHelpers (AbsFile)
import Hledger.Flow.Internals (SystemInfo)
import Hledger.Flow.Types (BaseDir, HledgerConf, HledgerInfo (..), RunDir)
import qualified Hledger.Flow.Types as Types

data RuntimeOptions = RuntimeOptions
  { baseDir :: BaseDir,
    importRunDir :: RunDir,
    importStartYear :: Maybe Integer,
    onlyNewFiles :: Bool,
    hfVersion :: T.Text,
    hledgerExecutable :: AbsFile,
    hledgerVersion :: IO T.Text,
    hledgerConf :: Maybe HledgerConf,
    sysInfo :: SystemInfo,
    verbose :: Bool,
    showOptions :: Bool,
    sequential :: Bool,
    batchSize :: Int,
    prettyReports :: Bool
  }

hledgerInfo :: RuntimeOptions -> IO HledgerInfo
hledgerInfo opts = HledgerInfo (hledgerExecutable opts) <$> hledgerVersion opts

instance Types.HasVerbosity RuntimeOptions where
  verbose RuntimeOptions {verbose = value} = value

instance Types.HasSequential RuntimeOptions where
  sequential RuntimeOptions {sequential = value} = value

instance Types.HasBatchSize RuntimeOptions where
  batchSize RuntimeOptions {batchSize = value} = value

instance Types.HasBaseDir RuntimeOptions where
  baseDir RuntimeOptions {baseDir = value} = value

instance Types.HasRunDir RuntimeOptions where
  importRunDir RuntimeOptions {importRunDir = value} = value

instance Types.HasPrettyReports RuntimeOptions where
  prettyReports RuntimeOptions {prettyReports = value} = value
