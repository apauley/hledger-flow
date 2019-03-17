module Hledger.MakeItSo.Import.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data ImportOptions = ImportOptions { baseDir :: FilePath, verbose :: Bool }

data ImportDirs = ImportDirs { importDir  :: FilePath
                             , ownerDir   :: FilePath
                             , bankDir    :: FilePath
                             , accountDir :: FilePath
                             , stateDir   :: FilePath
                             , yearDir    :: FilePath
                             } deriving (Show)
