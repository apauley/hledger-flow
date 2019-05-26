module Hledger.Flow.Import.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data ImportDirs = ImportDirs { importDir  :: FilePath
                             , ownerDir   :: FilePath
                             , bankDir    :: FilePath
                             , accountDir :: FilePath
                             , stateDir   :: FilePath
                             , yearDir    :: FilePath
                             } deriving (Show)
