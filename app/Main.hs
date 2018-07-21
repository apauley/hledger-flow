{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

data Command = Import (Maybe FilePath) | Report (Maybe FilePath) deriving (Show)

main :: IO ()
main = do
  x <- options "Manage your hledger CSV imports and classification: https://github.com/apauley/hledger-makeitso#readme" parser
  case x of
    Import t -> putStrLn "Import: not yet implemented"
    Report t -> putStrLn "Report: not yet implemented"

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files" optionalBaseDir)
     <|> fmap Report (subcommand "report" "Generate Reports" optionalBaseDir)

optionalBaseDir = optional (argPath "basedir" "The hledger-makeitso base directory")
