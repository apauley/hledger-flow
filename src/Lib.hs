{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

generateReports :: FilePath -> IO ()
generateReports fp = putStrLn "Generate reports: not yet implemented"
