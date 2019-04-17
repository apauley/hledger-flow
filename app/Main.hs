{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle hiding (switch)
import Prelude hiding (FilePath, putStrLn)

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Hledger.Flow.Import.Types as IT
import qualified Hledger.Flow.Report.Types as RT
import Hledger.Flow.Common
import Hledger.Flow.Reports
import Hledger.Flow.CSVImport

data SubcommandParams = SubcommandParams { maybeBaseDir :: Maybe FilePath
                                         , hledgerPathOpt :: Maybe FilePath
                                         , verbose :: Bool
                                         , showOpts :: Bool
                                         , sequential :: Bool
                                         }
                      deriving (Show)
data Command = Version (Maybe Text) | Import SubcommandParams | Report SubcommandParams deriving (Show)

main :: IO ()
main = do
  cmd <- options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" parser
  case cmd of
    Version _        -> stdout $ select versionInfo
    Import subParams -> toImportOptions subParams >>= importCSVs
    Report subParams -> toReportOptions subParams >>= generateReports

toImportOptions :: SubcommandParams -> IO IT.ImportOptions
toImportOptions params = do
  bd <- dirOrPwd $ maybeBaseDir params
  hli <- hledgerInfoFromPath $ hledgerPathOpt params
  return IT.ImportOptions { IT.baseDir = bd
                          , IT.hledgerInfo = hli
                          , IT.verbose = verbose params
                          , IT.showOptions = showOpts params
                          , IT.sequential = sequential params }

toReportOptions :: SubcommandParams -> IO RT.ReportOptions
toReportOptions params = do
  bd <- dirOrPwd $ maybeBaseDir params
  hli <- hledgerInfoFromPath $ hledgerPathOpt params
  return RT.ReportOptions { RT.baseDir = bd
                          , RT.hledgerInfo = hli
                          , RT.verbose = verbose params
                          , RT.showOptions = showOpts params
                          , RT.sequential = sequential params }

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)
  <|> fmap Version (subcommand "version" "Display version information" noArgs)

subcommandParser :: Parser SubcommandParams
subcommandParser = SubcommandParams
  <$> optional (argPath "basedir" "The hledger-flow base directory")
  <*> optional (optPath "hledger-path" 'H' "The full path to an hledger executable")
  <*> switch (long "verbose" <> short 'v' <> help "Print more verbose output")
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> switch (long "sequential" <> help "Disable parallel processing")

noArgs :: Parser (Maybe Text)
noArgs = optional (argText "" "")
