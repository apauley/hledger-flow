module Hledger.MakeItSo.Data.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data HMISOptions = HMISOptions { baseDir :: FilePath, verbose :: Bool }
