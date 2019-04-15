{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hledger.Flow.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data LogMessage = StdOut Text | StdErr Text | Terminate deriving (Show)
type FullOutput = (ExitCode, Text, Text)
type FullTimedOutput = (FullOutput, NominalDiffTime)

class HasVerbosity a where
  verbose :: a -> Bool

class HasBaseDir a where
  baseDir :: a -> FilePath

class HasExitCode a where
  exitCode :: a -> ExitCode

instance HasExitCode ExitCode where
  exitCode c = c

instance HasExitCode FullOutput where
  exitCode (c, _, _) = c
