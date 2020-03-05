{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle hiding (switch)
import Prelude hiding (FilePath, putStrLn)

import Options.Applicative

import Hledger.Flow.Common
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Reports
import Hledger.Flow.CSVImport

data ImportParams = ImportParams { maybeImportBaseDir :: Maybe FilePath
                                 , maybeRunDir :: Maybe FilePath } deriving (Show)

data ReportParams = ReportParams { maybeReportBaseDir :: Maybe FilePath } deriving (Show)

data Command = Import ImportParams | Report ReportParams deriving (Show)

data MainParams = MainParams { verbosity :: Int
                             , hledgerPathOpt :: Maybe FilePath
                             , showOpts :: Bool
                             , sequential :: Bool
                             } deriving (Show)
data BaseCommand = Version | Command { mainParams :: MainParams, command :: Command } deriving (Show)

main :: IO ()
main = do
  cmd <- options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" baseCommandParser
  case cmd of
    Version                                -> stdout $ select versionInfo
    Command mainParams' (Import subParams) -> toRuntimeOptionsImport mainParams' subParams >>= importCSVs
    Command mainParams' (Report subParams) -> toRuntimeOptionsReport mainParams' subParams >>= generateReports

toRuntimeOptionsImport :: MainParams -> ImportParams -> IO RT.RuntimeOptions
toRuntimeOptionsImport mainParams' subParams' = do
  (bd, _runDir) <- determineBaseDir $ maybeImportBaseDir subParams'
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = maybeRunDir subParams'
                           , RT.hfVersion = versionInfo'
                           , RT.hledgerInfo = hli
                           , RT.sysInfo = systemInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams' }

toRuntimeOptionsReport :: MainParams -> ReportParams -> IO RT.RuntimeOptions
toRuntimeOptionsReport mainParams' subParams' = do
  (bd, _) <- determineBaseDir $ maybeReportBaseDir subParams'
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = Nothing
                           , RT.hfVersion = versionInfo'
                           , RT.hledgerInfo = hli
                           , RT.sysInfo = systemInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams' }

baseCommandParser :: Parser BaseCommand
baseCommandParser = (Command <$> verboseParser <*> commandParser)
  <|> flag' Version (long "version" <> short 'V' <> help "Display version information")

commandParser :: Parser Command
commandParser = fmap Import (subcommand "import" "Uses hledger with your own rules and/or scripts to convert electronic statements into categorised journal files" subcommandParserImport)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParserReport)

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print more verbose output")))
  <*> optional (optPath "hledger-path" 'H' "The full path to an hledger executable")
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> switch (long "sequential" <> help "Disable parallel processing")

subcommandParserImport :: Parser ImportParams
subcommandParserImport = ImportParams
  <$> optional (argPath "dir" "The directory to import. Defaults to the current directory. Use the hledger-flow base directory for a full import.")
  <*> optional (strOption (long "experimental-rundir" <> help "A subdirectory of the base where the command will restrict itself to"))

subcommandParserReport :: Parser ReportParams
subcommandParserReport = ReportParams
  <$> optional (argPath "basedir" "The hledger-flow base directory")
