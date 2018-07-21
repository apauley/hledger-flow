{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

data Command = Import Text | Report Text deriving (Show)

main :: IO ()
main = do
  x <- options "Manage your hledger CSV imports and classification: https://github.com/apauley/hledger-makeitso#readme" parser
  case x of
    Import t -> putStrLn t
    Report t -> putStrLn t

parser :: Parser Command
parser = fmap Import (subcommand "import" "Converts CSV transactions into categorised journal files"  (argText "i" "Import placeholder"))
     <|> fmap Report (subcommand "report" "Generate Reports" (argText "r" "Report placeholder"))
