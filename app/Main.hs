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
                                         , showOpts :: Bool
                                         , sequential :: Bool
                                         }
                      deriving (Show)
data Command = Import SubcommandParams | Report SubcommandParams deriving (Show)

data MainParams = MainParams { verbose :: Bool } deriving (Show)
data BaseCommand = Version | Command { mainParams :: MainParams, command :: Command } deriving (Show)

main :: IO ()
main = do
  cmd <- options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" baseCommandParser
  case cmd of
    Version                                -> stdout $ select versionInfo
    Command mainParams' (Import subParams) -> toImportOptions mainParams' subParams >>= importCSVs
    Command mainParams' (Report subParams) -> toReportOptions mainParams' subParams >>= generateReports

toImportOptions :: MainParams -> SubcommandParams -> IO IT.ImportOptions
toImportOptions mainParams' params = do
  bd <- determineBaseDir $ maybeBaseDir params
  hli <- hledgerInfoFromPath $ hledgerPathOpt params
  return IT.ImportOptions { IT.baseDir = bd
                          , IT.hledgerInfo = hli
                          , IT.verbose = verbose mainParams'
                          , IT.showOptions = showOpts params
                          , IT.sequential = sequential params }

toReportOptions :: MainParams -> SubcommandParams -> IO RT.ReportOptions
toReportOptions mainParams' params = do
  bd <- determineBaseDir $ maybeBaseDir params
  hli <- hledgerInfoFromPath $ hledgerPathOpt params
  return RT.ReportOptions { RT.baseDir = bd
                          , RT.hledgerInfo = hli
                          , RT.verbose = verbose mainParams'
                          , RT.showOptions = showOpts params
                          , RT.sequential = sequential params }

baseCommandParser :: Parser BaseCommand
baseCommandParser = (Command <$> verboseParser <*> commandParser)
  <|> flag' Version (long "version" <> short 'V' <> help "Display version information")

commandParser :: Parser Command
commandParser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> switch (long "verbose" <> short 'v' <> help "Print more verbose output")

subcommandParser :: Parser SubcommandParams
subcommandParser = SubcommandParams
  <$> optional (argPath "basedir" "The hledger-flow base directory")
  <*> optional (optPath "hledger-path" 'H' "The full path to an hledger executable")
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> switch (long "sequential" <> help "Disable parallel processing")
