{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

importCSVs :: FilePath -> IO ()
importCSVs baseDir = view $ ls (baseDir </> "import")
