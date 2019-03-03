{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Maybe
import Hledger.MakeItSo.Data.Types
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
  bd <- toBaseDir maybeBaseDir
  let v = case maybeVerbose of
        Nothing    -> 0
        Just False -> 0
        Just True  -> 1
  return HMISOptions {baseDir = bd, verbosityLevel = v}

toBaseDir :: Maybe FilePath -> IO FilePath
toBaseDir maybeBaseDir = fromMaybe pwd $ fmap return maybeBaseDir

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" subcommandParser)
     <|> fmap Report (subcommand "report" "Generate Reports" subcommandParser)

subcommandParser :: Parser SubcommandParams
subcommandParser = (,) <$> optional (argPath "basedir" "The hledger-makeitso base directory")
                       <*> optional (switch  "verbose" 'v' "Print more verbose output")
