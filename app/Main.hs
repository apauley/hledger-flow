{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Maybe (fromMaybe)
import Hledger.MakeItSo.Data.Types
import Common
import Reports
import CSVImport

type SubcommandParams = (Maybe FilePath, Maybe Bool)
data Command = Import SubcommandParams | Report SubcommandParams deriving (Show)

main :: IO ()
main = do
  cmd <- options "Manage your hledger CSV imports and classification:\nhttps://github.com/apauley/hledger-makeitso#readme" parser
  case cmd of
    Import subParams -> toHMISOptions subParams >>= importCSVs
    Report subParams -> toHMISOptions subParams >>= generateReports

toHMISOptions :: SubcommandParams -> IO HMISOptions
toHMISOptions (maybeBaseDir, maybeVerbose) = do
  bd <- dirOrPwd maybeBaseDir
  return HMISOptions {baseDir = bd, verbose = fromMaybe False maybeVerbose}

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
     <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)

subcommandParser :: Parser SubcommandParams
subcommandParser = (,) <$> optional (argPath "basedir" "The hledger-makeitso base directory")
                       <*> optional (switch  "verbose" 'v' "Print more verbose output")
