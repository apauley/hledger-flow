{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Parsing ( parseStartYear )
import Path ( reldir )

import qualified Turtle hiding (switch)
import Prelude hiding (putStrLn)

import Options.Applicative
    ( auto,
      optional,
      Alternative(many, (<|>)),
      Parser,
      flag',
      help,
      long,
      metavar,
      option,
      short,
      str,
      switch )

import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.Common ( hledgerInfoFromPath )
import Hledger.Flow.Internals (versionInfo, systemInfo)
import Hledger.Flow.BaseDir ( determineBaseDir )
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Reports ( generateReports )
import Hledger.Flow.Import.CSVImport ( importCSVs )

import qualified Data.Text.IO as T

data ImportParams = ImportParams { maybeImportBaseDir :: Maybe TurtlePath
                                 , importStartYear :: Maybe String
                                 , onlyNewFiles :: Bool
                                 } deriving (Show)

data ReportParams = ReportParams { maybeReportBaseDir :: Maybe TurtlePath
                                 , asciiReports :: Bool
                                 } deriving (Show)

data Command = Import ImportParams | Report ReportParams deriving (Show)

data MainParams = MainParams { verbosity :: Int
                             , hledgerPathOpt :: Maybe TurtlePath
                             , showOpts :: Bool
                             , batchSize :: Maybe Int
                             , sequential :: Bool
                             } deriving (Show)
data BaseCommand = Version | Command { mainParams :: MainParams, command :: Command } deriving (Show)

main :: IO ()
main = do
  cmd <- Turtle.options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" baseCommandParser
  case cmd of
    Version -> do
      sysInfo <- systemInfo
      T.putStrLn $ versionInfo sysInfo 
    Command mainParams' (Import subParams) -> toRuntimeOptionsImport mainParams' subParams >>= importCSVs
    Command mainParams' (Report subParams) -> toRuntimeOptionsReport mainParams' subParams >>= generateReports

defaultBatchSize :: Int
defaultBatchSize = 20

determineBatchSize :: MainParams -> IO Int
determineBatchSize mainParams' =
  case (batchSize mainParams') of
    Nothing   -> return defaultBatchSize
    Just size -> return size

toRuntimeOptionsImport :: MainParams -> ImportParams -> IO RT.RuntimeOptions
toRuntimeOptionsImport mainParams' subParams' = do
  startYear <- parseStartYear $ importStartYear subParams'
  let maybeBD = maybeImportBaseDir subParams' :: Maybe TurtlePath
  (bd, runDir) <- determineBaseDir maybeBD
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  size <- determineBatchSize mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = runDir
                           , RT.importStartYear = startYear
                           , RT.onlyNewFiles = onlyNewFiles subParams'
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerInfo = hli
                           , RT.sysInfo = sysInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams'
                           , RT.batchSize = size
                           , RT.prettyReports = True
                           }

toRuntimeOptionsReport :: MainParams -> ReportParams -> IO RT.RuntimeOptions
toRuntimeOptionsReport mainParams' subParams' = do
  let maybeBD = maybeReportBaseDir subParams' :: Maybe TurtlePath
  (bd, _) <- determineBaseDir maybeBD
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  size <- determineBatchSize mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = [reldir|.|]
                           , RT.importStartYear = Nothing
                           , RT.onlyNewFiles = False
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerInfo = hli
                           , RT.sysInfo = sysInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams'
                           , RT.batchSize = size
                           , RT.prettyReports = not(asciiReports subParams')
                           }

baseCommandParser :: Parser BaseCommand
baseCommandParser = (Command <$> verboseParser <*> commandParser)
  <|> flag' Version (long "version" <> short 'V' <> help "Display version information")

commandParser :: Parser Command
commandParser = fmap Import (Turtle.subcommand "import" "Uses hledger with your own rules and/or scripts to convert electronic statements into categorised journal files" subcommandParserImport)
  <|> fmap Report (Turtle.subcommand "report" "Generate Reports" subcommandParserReport)

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print more verbose output")))
  <*> optional (Turtle.optPath "hledger-path" 'H' "The full path to an hledger executable")
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> optional (option auto (long "batch-size" <> metavar "SIZE" <> help ("Parallel processing of files are done in batches of the specified size. Default: " <> show defaultBatchSize <> ". Ignored during sequential processing.")))
  <*> switch (long "sequential" <> help "Disable parallel processing")

subcommandParserImport :: Parser ImportParams
subcommandParserImport = ImportParams
  <$> optional (Turtle.argPath "dir" "The directory to import. Use the base directory for a full import or a sub-directory for a partial import. Defaults to the current directory.")
  <*> optional (option str (long "start-year" <> metavar "YEAR" <> help "Import only from the specified year and onwards, ignoring previous years. By default all available years are imported. Valid values include a 4-digit year or 'current' for the current year"))
  <*> switch (long "new-files-only" <> help "Don't regenerate transaction files if they are already present. This applies to hledger journal files as well as files produced by the preprocess and construct scripts.")

subcommandParserReport :: Parser ReportParams
subcommandParserReport = ReportParams
  <$> optional (Turtle.argPath "basedir" "The hledger-flow base directory")
  <*> switch (long "ascii-reports" <> help "If to avoid using hledger --pretty-tables flag when generating reports.")
