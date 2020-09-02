{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Path
import qualified Turtle hiding (switch)
import Prelude hiding (putStrLn)

import Options.Applicative

import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.Common
import Hledger.Flow.BaseDir
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Reports
import Hledger.Flow.CSVImport

import Control.Monad (when)
import qualified Data.Text.IO as T

data ImportParams = ImportParams { maybeImportBaseDir :: Maybe TurtlePath
                                 , importUseRunDir :: Bool
                                 , onlyNewFiles :: Bool
                                 } deriving (Show)

data ReportParams = ReportParams { maybeReportBaseDir :: Maybe TurtlePath } deriving (Show)

data Command = Import ImportParams | Report ReportParams deriving (Show)

data MainParams = MainParams { verbosity :: Int
                             , hledgerPathOpt :: Maybe TurtlePath
                             , showOpts :: Bool
                             , sequential :: Bool
                             } deriving (Show)
data BaseCommand = Version | Command { mainParams :: MainParams, command :: Command } deriving (Show)

main :: IO ()
main = do
  cmd <- Turtle.options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" baseCommandParser
  case cmd of
    Version                                -> Turtle.stdout $ Turtle.select versionInfo
    Command mainParams' (Import subParams) -> toRuntimeOptionsImport mainParams' subParams >>= importCSVs
    Command mainParams' (Report subParams) -> toRuntimeOptionsReport mainParams' subParams >>= generateReports

toRuntimeOptionsImport :: MainParams -> ImportParams -> IO RT.RuntimeOptions
toRuntimeOptionsImport mainParams' subParams' = do
  let maybeBD = maybeImportBaseDir subParams' :: Maybe TurtlePath
  Control.Monad.when (importUseRunDir subParams') $ do
    T.putStrLn "The enable-future-rundir option is now the default, no need to specify it. This option is currently being ignored and will be removed in future."
  (bd, runDir) <- determineBaseDir maybeBD
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = runDir
                           , RT.onlyNewFiles = onlyNewFiles subParams'
                           , RT.hfVersion = versionInfo'
                           , RT.hledgerInfo = hli
                           , RT.sysInfo = systemInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams' }

toRuntimeOptionsReport :: MainParams -> ReportParams -> IO RT.RuntimeOptions
toRuntimeOptionsReport mainParams' subParams' = do
  let maybeBD = maybeReportBaseDir subParams' :: Maybe TurtlePath
  (bd, _) <- determineBaseDir maybeBD
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = [reldir|.|]
                           , RT.onlyNewFiles = False
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
commandParser = fmap Import (Turtle.subcommand "import" "Uses hledger with your own rules and/or scripts to convert electronic statements into categorised journal files" subcommandParserImport)
  <|> fmap Report (Turtle.subcommand "report" "Generate Reports" subcommandParserReport)

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print more verbose output")))
  <*> optional (Turtle.optPath "hledger-path" 'H' "The full path to an hledger executable")
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> switch (long "sequential" <> help "Disable parallel processing")

subcommandParserImport :: Parser ImportParams
subcommandParserImport = ImportParams
  <$> optional (Turtle.argPath "dir" "The directory to import. Use the base directory for a full import or a sub-directory for a partial import. Defaults to the current directory. This behaviour is changing: see --enable-future-rundir")
  <*> switch (long "enable-future-rundir" <> help "This switch is currently being ignored, since the behaviour it previously enabled is now the default. It will be removed in future.")
  <*> switch (long "new-files-only" <> help "Don't regenerate transaction files if they are already present. This applies to hledger journal files as well as files produced by the preprocess and construct scripts.")

subcommandParserReport :: Parser ReportParams
subcommandParserReport = ReportParams
  <$> optional (Turtle.argPath "basedir" "The hledger-flow base directory")
