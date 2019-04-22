{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hledger.Flow.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)

data LogMessage = StdOut Text | StdErr Text | Terminate deriving (Show)
type FullOutput = (ExitCode, Text, Text)
type FullTimedOutput = (FullOutput, NominalDiffTime)

type ProcFun = Text -> [Text] -> Shell Line -> IO FullOutput
type ProcInput = (Text, [Text], Shell Line)

data HledgerInfo = HledgerInfo { hlPath :: FilePath
                               , hlVersion :: Text
                               }
                 deriving (Show)

class HasVerbosity a where
  verbose :: a -> Bool

class HasBaseDir a where
  baseDir :: a -> FilePath

class HasSequential a where
  sequential :: a -> Bool
