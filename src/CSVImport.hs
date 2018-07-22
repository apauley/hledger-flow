{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)

importCSVs :: FilePath -> IO ()
importCSVs baseDir = view $ validDirs $ ls (baseDir </> "import")

validDirs :: Shell FilePath -> Shell FilePath
validDirs = findtree (plus $ noneOf "._") -- TODO: Do an actual dir check
