{-# LANGUAGE OverloadedStrings #-}

module Reports
    ( generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

generateReports :: FilePath -> IO ()
generateReports _ = putStrLn "Generate reports: not yet implemented"
