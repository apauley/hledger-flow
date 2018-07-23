{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Common

importCSVs :: FilePath -> IO ()
importCSVs baseDir = view $ validDirs $ ls (baseDir </> "import")
