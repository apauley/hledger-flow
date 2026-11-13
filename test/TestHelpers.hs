{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestHelpers where

import Hledger.Flow.Internals (SystemInfo (..), versionInfo)
import Hledger.Flow.PathHelpers (AbsDir)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import Path
import qualified Path.IO as PathIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified System.Info as Sys

withTestDir :: (AbsDir -> IO ()) -> IO ()
withTestDir action = do
  cwd <- PathIO.getCurrentDir
  let tmpbase = cwd </> [reldir|test/tmp|]
  PathIO.createDirIfMissing True tmpbase
  PathIO.withTempDir tmpbase "hlflowtest" action

defaultHlInfo :: FlowTypes.HledgerInfo
defaultHlInfo = FlowTypes.HledgerInfo (testRoot </> [relfile|path/to/hledger|]) "1.2.3"

-- A fictional absolute root for fixtures; no files are created here.
testRoot :: Path Abs Dir
#ifdef mingw32_HOST_OS
testRoot = [absdir|C:/|]
#else
testRoot = [absdir|/|]
#endif

testSystemInfo :: SystemInfo
testSystemInfo =
  SystemInfo
    { os = Sys.os,
      arch = Sys.arch,
      compilerName = Sys.compilerName,
      compilerVersion = Sys.compilerVersion,
      cores = 1,
      availableCores = 1
    }

defaultOpts :: FlowTypes.BaseDir -> RuntimeOptions
defaultOpts bd =
  RuntimeOptions
    { baseDir = bd,
      importRunDir = [reldir|./|],
      importStartYear = Nothing,
      onlyNewFiles = False,
      hfVersion = versionInfo testSystemInfo,
      hledgerExecutable = FlowTypes.hlPath defaultHlInfo,
      hledgerVersion = return (FlowTypes.hlVersion defaultHlInfo),
      hledgerConf = Nothing,
      sysInfo = testSystemInfo,
      verbose = False,
      showOptions = False,
      sequential = False,
      batchSize = 1,
      prettyReports = True
    }

touchAll :: [FilePath] -> IO ()
touchAll = mapM_ $ \file -> do
  createDirectoryIfMissing True (takeDirectory file)
  appendFile file ""
