{-# LANGUAGE TemplateHaskell #-}

module Hledger.Flow.Internals where

import Development.GitRev
import Data.Version (Version, showVersion)
import Paths_hledger_flow (version)

import qualified Data.Text as T
import qualified System.Info as Sys

data SystemInfo = SystemInfo { os :: String
                             , arch :: String
                             , compilerName :: String
                             , compilerVersion :: Version
                             }
                deriving (Show)

versionInfo :: T.Text
versionInfo = T.pack ("hledger-flow " ++ showVersion version ++ " " ++
                       os systemInfo ++ " " ++ arch systemInfo ++ " " ++
                       compilerName systemInfo ++ " " ++
                       showVersion (compilerVersion systemInfo) ++
                       " " ++ $(gitHash))

systemInfo :: SystemInfo
systemInfo = SystemInfo { os = Sys.os
                        , arch = Sys.arch
                        , compilerName = Sys.compilerName
                        , compilerVersion = Sys.compilerVersion
                        }
