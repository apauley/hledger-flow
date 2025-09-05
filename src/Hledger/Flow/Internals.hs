{-# LANGUAGE TemplateHaskell #-}

module Hledger.Flow.Internals where

import qualified Data.Text as T
import Data.Version (Version, showVersion)
import Development.GitRev
import GHC.Conc (getNumCapabilities, getNumProcessors)
import Paths_hledger_flow (version)
import qualified System.Info as Sys

data SystemInfo = SystemInfo
  { os :: String,
    arch :: String,
    compilerName :: String,
    compilerVersion :: Version,
    cores :: Int,
    availableCores :: Int
  }
  deriving (Show)

versionInfo :: SystemInfo -> T.Text
versionInfo sysInfo =
  T.pack
    ( "hledger-flow "
        ++ showVersion version
        ++ " "
        ++ os sysInfo
        ++ " "
        ++ arch sysInfo
        ++ " "
        ++ compilerName sysInfo
        ++ " "
        ++ showVersion (compilerVersion sysInfo)
        ++ " "
        ++ $(gitHash)
    )

systemInfo :: IO SystemInfo
systemInfo = do
  processors <- getNumProcessors
  available <- getNumCapabilities
  return
    SystemInfo
      { os = Sys.os,
        arch = Sys.arch,
        compilerName = Sys.compilerName,
        compilerVersion = Sys.compilerVersion,
        cores = processors,
        availableCores = available
      }
