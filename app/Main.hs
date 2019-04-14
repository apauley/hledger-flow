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

type SubcommandParams = (Maybe FilePath, Bool, Bool)
data Command = Version (Maybe Text) | Import SubcommandParams | Report SubcommandParams deriving (Show)

main :: IO ()
main = do
  cmd <- options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" parser
  case cmd of
    Version _        -> stdout $ select versionInfo
    Import subParams -> toImportOptions subParams >>= importCSVs
    Report subParams -> toReportOptions subParams >>= generateReports

toImportOptions :: SubcommandParams -> IO IT.ImportOptions
toImportOptions (maybeBaseDir, verbose, sequential) = do
  bd <- dirOrPwd maybeBaseDir
  return IT.ImportOptions {IT.baseDir = bd, IT.verbose = verbose, IT.sequential = sequential}

toReportOptions :: SubcommandParams -> IO RT.ReportOptions
toReportOptions (maybeBaseDir, verbose, sequential) = do
  bd <- dirOrPwd maybeBaseDir
  return RT.ReportOptions {RT.baseDir = bd, RT.verbose = verbose, RT.sequential = sequential}

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)
  <|> fmap Version (subcommand "version" "Display version information" noArgs)

subcommandParser :: Parser SubcommandParams
subcommandParser = (,,)
  <$> optional (argPath "basedir" "The hledger-flow base directory")
  <*> switch (long "verbose" <> short 'v' <> help "Print more verbose output")
  <*> switch (long "sequential" <> help "Disable parallel processing")

noArgs :: Parser (Maybe Text)
noArgs = optional (argText "" "")
