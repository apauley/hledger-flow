module Hledger.MakeItSo.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

class HasVerbosity a where
  verbose :: a -> Bool

class HasBaseDir a where
  baseDir :: a -> FilePath
