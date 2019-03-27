module Hledger.MakeItSo.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data LogMessage = StdOut Text | StdErr Text | Terminate deriving (Show)

class HasVerbosity a where
  verbose :: a -> Bool

class HasBaseDir a where
  baseDir :: a -> FilePath
