{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import qualified Hledger.Flow.Import.Types as IT
import qualified Hledger.Flow.Report.Types as RT
import Hledger.Flow.Common
import Hledger.Flow.Reports
import Hledger.Flow.CSVImport

type SubcommandParams = (Maybe FilePath, Bool)
data Command = Version (Maybe Text) | Import SubcommandParams | Report SubcommandParams deriving (Show)

main :: IO ()
main = do
  cmd <- options "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme" parser
  case cmd of
    Version _        -> stdout $ select versionInfo
    Import subParams -> toImportOptions subParams >>= importCSVs
    Report subParams -> toReportOptions subParams >>= generateReports

toImportOptions :: SubcommandParams -> IO IT.ImportOptions
toImportOptions (maybeBaseDir, verbose) = do
  bd <- dirOrPwd maybeBaseDir
  return IT.ImportOptions {IT.baseDir = bd, IT.verbose = verbose}

toReportOptions :: SubcommandParams -> IO RT.ReportOptions
toReportOptions (maybeBaseDir, verbose) = do
  bd <- dirOrPwd maybeBaseDir
  return RT.ReportOptions {RT.baseDir = bd, RT.verbose = verbose}

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)
  <|> fmap Version (subcommand "version" "Display version information" (optional (argText "" "")))

subcommandParser :: Parser SubcommandParams
subcommandParser = (,) <$> optional (argPath "basedir" "The hledger-flow base directory")
                       <*> switch  "verbose" 'v' "Print more verbose output"
