{-# LANGUAGE OverloadedStrings #-}

module Reports
    ( generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)
import Hledger.MakeItSo.Data.Types

generateReports :: HMISOptions -> IO ()
generateReports _ = putStrLn "Generate reports: not yet implemented"
