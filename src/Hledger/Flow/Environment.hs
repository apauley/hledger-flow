{-# LANGUAGE OverloadedStrings #-}

-- | The versions, paths and settings a command runs with, for debugging and bug reports.
module Hledger.Flow.Environment
  ( Setting (..),
    environmentSettings,
    coresSetting,
    importSettings,
    reportSettings,
    renderSettings,
    environmentJSON,
  )
where

import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import Data.Version (showVersion)
import Hledger.Flow.Common (tildePath)
import Hledger.Flow.Internals (SystemInfo (..), gitCommit)
import Hledger.Flow.RuntimeOptions
import Hledger.Flow.Status (hledgerVersionNumber)
import Hledger.Flow.Types (HledgerConf (..), HledgerInfo (..), hledgerConfPath)
import Path (toFilePath)
import Paths_hledger_flow (version)
import System.FilePath (dropTrailingPathSeparator)

-- | One line: a label, its value and the flags that change it.
data Setting = Setting {sLabel :: T.Text, sValue :: T.Text, sFlags :: T.Text, sProblem :: Bool}

setting :: T.Text -> T.Text -> T.Text -> Setting
setting l v f = Setting l v f False

-- | What every command shares: hledger-flow, hledger, and its config.
-- Paths under one of @homes@ show as @~@.
environmentSettings :: [FilePath] -> SystemInfo -> Maybe HledgerInfo -> Maybe HledgerConf -> [Setting]
environmentSettings homes sys hledger conf =
  [ setting "hledger-flow" (T.pack (showVersion version <> " (" <> gitCommit <> "), " <> compilerName sys <> " " <> showVersion (compilerVersion sys) <> ", " <> os sys <> " " <> arch sys)) "",
    case hledger of
      Just hl -> setting "hledger" (shown (toFilePath (hlPath hl)) <> "  (" <> hledgerVersionNumber (hlVersion hl) <> ")") "--hledger-path"
      Nothing -> Setting "hledger" "not found" "--hledger-path" True,
    setting "hledger.conf" confText "--hledger-conf"
  ]
  where
    shown = T.pack . tildePath homes
    confText = case conf of
      Nothing -> "none"
      Just (GivenConf f) -> shown f
      Just (FoundConf f) -> shown f <> "  (found by hledger)"

coresSetting :: SystemInfo -> Setting
coresSetting sys = setting "Cores" (tshow (cores sys) <> ", using " <> tshow (availableCores sys)) ""

optsEnvironment :: [FilePath] -> HledgerInfo -> RuntimeOptions -> [Setting]
optsEnvironment homes info o =
  environmentSettings homes (sysInfo o) (Just info) (hledgerConf o)
    ++ [setting "Ledger" (T.pack (tildePath homes (dropTrailingPathSeparator (toFilePath (baseDir o))))) ""]

parallelism :: RuntimeOptions -> Setting
parallelism o = setting "Parallelism" value "--batch-size, --sequential"
  where
    sys = sysInfo o
    value
      | sequential o = "one file at a time"
      | otherwise = tshow (batchSize o) <> " files at a time, " <> tshow (availableCores sys) <> " of " <> tshow (cores sys) <> " cores"

importSettings :: [FilePath] -> HledgerInfo -> RuntimeOptions -> [Setting]
importSettings homes info o =
  optsEnvironment homes info o
    ++ [ setting "Run from" runFrom "",
         setting "Start year" (maybe "all years" (\y -> tshow y <> " onwards") (importStartYear o)) "--start-year",
         setting "Files" (if onlyNewFiles o then "only new or changed" else "regenerate all") "--new-files-only",
         parallelism o
       ]
  where
    runFrom = case dropTrailingPathSeparator (toFilePath (importRunDir o)) of
      "." -> ". (whole ledger)"
      dir -> T.pack dir

reportSettings :: [FilePath] -> HledgerInfo -> RuntimeOptions -> [Setting]
reportSettings homes info o =
  optsEnvironment homes info o
    ++ [ setting "Tables" (if prettyReports o then "pretty" else "ASCII") "--ascii-reports",
         parallelism o
       ]

-- | Aligned lines; @problem@ styles values that need attention, such as a missing hledger.
renderSettings :: (T.Text -> T.Text) -> [Setting] -> [T.Text]
renderSettings problem settings = map line settings
  where
    width f = maximum (0 : map (T.length . f) settings)
    -- A long value without flags, such as the ledger path, mustn't push the flags column right.
    valueWidth = maximum (0 : [T.length (sValue s) | s <- settings, not (T.null (sFlags s))])
    line s =
      let value = T.justifyLeft valueWidth ' ' (sValue s)
          styled = if sProblem s then problem value else value
       in T.stripEnd (T.justifyLeft (width sLabel) ' ' (sLabel s) <> "  " <> styled <> "   " <> sFlags s)

environmentJSON :: SystemInfo -> Maybe HledgerConf -> Value
environmentJSON sys conf =
  object
    [ "hledgerFlowVersion" .= showVersion version,
      "commit" .= gitCommit,
      "compiler" .= (compilerName sys <> " " <> showVersion (compilerVersion sys)),
      "os" .= os sys,
      "arch" .= arch sys,
      "cores" .= cores sys,
      "availableCores" .= availableCores sys,
      "hledgerConf" .= fmap hledgerConfPath conf
    ]

tshow :: Show a => a -> T.Text
tshow = T.pack . show
