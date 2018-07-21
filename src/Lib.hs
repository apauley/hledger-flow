{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( importCSVs
    , generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

importCSVs :: Maybe FilePath -> IO ()
importCSVs fp = putStrLn "Import CSV files: not yet implemented"

generateReports :: Maybe FilePath -> IO ()
generateReports fp = putStrLn "Generate reports: not yet implemented"
