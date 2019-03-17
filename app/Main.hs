{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Maybe (fromMaybe)
import qualified Hledger.MakeItSo.Import.Types as IT
import qualified Hledger.MakeItSo.Report.Types as RT
import Hledger.MakeItSo.Common
import Hledger.MakeItSo.Reports
import Hledger.MakeItSo.CSVImport

type SubcommandParams = (Maybe FilePath, Maybe Bool)
data Command = Import SubcommandParams | Report SubcommandParams deriving (Show)

main :: IO ()
main = do
  cmd <- options "Manage your hledger CSV imports and classification:\nhttps://github.com/apauley/hledger-makeitso#readme" parser
  case cmd of
    Import subParams -> toImportOptions subParams >>= importCSVs
    Report subParams -> toReportOptions subParams >>= generateReports

toImportOptions :: SubcommandParams -> IO IT.ImportOptions
toImportOptions (maybeBaseDir, maybeVerbose) = do
  bd <- dirOrPwd maybeBaseDir
  return IT.ImportOptions {IT.baseDir = bd, IT.verbose = fromMaybe False maybeVerbose}

toReportOptions :: SubcommandParams -> IO RT.ReportOptions
toReportOptions (maybeBaseDir, maybeVerbose) = do
  bd <- dirOrPwd maybeBaseDir
  return RT.ReportOptions {RT.baseDir = bd, RT.verbose = fromMaybe False maybeVerbose}

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
     <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)

subcommandParser :: Parser SubcommandParams
subcommandParser = (,) <$> optional (argPath "basedir" "The hledger-makeitso base directory")
                       <*> optional (switch  "verbose" 'v' "Print more verbose output")
