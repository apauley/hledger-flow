{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle hiding (switch)
import Prelude hiding (FilePath, putStrLn)

import Options.Applicative
import Data.Semigroup ((<>))

import Hledger.Flow.Common
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Reports
import Hledger.Flow.CSVImport

data SubcommandParams = SubcommandParams { maybeBaseDir :: Maybe FilePath } deriving (Show)
data Command = Import SubcommandParams | Report SubcommandParams deriving (Show)

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
    Command mainParams' (Import subParams) -> toRuntimeOptions mainParams' subParams >>= importCSVs
    Command mainParams' (Report subParams) -> toRuntimeOptions mainParams' subParams >>= generateReports

toRuntimeOptions :: MainParams -> SubcommandParams -> IO RT.RuntimeOptions
toRuntimeOptions mainParams' subParams' = do
  bd <- determineBaseDir $ maybeBaseDir subParams'
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  return RT.RuntimeOptions { RT.baseDir = bd
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
commandParser = fmap Import (subcommand "import" "Converts electronic transactions into categorised journal files" subcommandParser)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print more verbose output")))
  <*> optional (optPath "hledger-path" 'H' "The full path to an hledger executable")
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> switch (long "sequential" <> help "Disable parallel processing")

subcommandParser :: Parser SubcommandParams
subcommandParser = SubcommandParams
  <$> optional (argPath "basedir" "The hledger-flow base directory")
