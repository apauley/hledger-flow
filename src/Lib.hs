{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( importCSVs
    , generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

importCSVs :: FilePath -> IO ()
importCSVs baseDir = view $ ls (baseDir </> "import")

generateReports :: FilePath -> IO ()
generateReports fp = putStrLn "Generate reports: not yet implemented"
