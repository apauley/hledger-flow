{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Maybe
import Reports
import CSVImport

data Command = Import (Maybe FilePath) | Report (Maybe FilePath) deriving (Show)

main :: IO ()
main = do
  cmd <- options "Manage your hledger CSV imports and classification:\nhttps://github.com/apauley/hledger-makeitso#readme" parser
  case cmd of
    Import maybeBaseDir -> baseDir maybeBaseDir >>= importCSVs
    Report maybeBaseDir -> baseDir maybeBaseDir >>= generateReports

baseDir :: Maybe FilePath -> IO FilePath
baseDir maybeBaseDir = fromMaybe pwd $ fmap return maybeBaseDir

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" optionalBaseDir)
     <|> fmap Report (subcommand "report" "Generate Reports" optionalBaseDir)

optionalBaseDir :: Parser (Maybe FilePath)
optionalBaseDir = optional (argPath "basedir" "The hledger-makeitso base directory")
