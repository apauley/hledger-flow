{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hledger.Flow.Types
where

import Turtle
import Data.Version

import Hledger.Flow.PathHelpers

type BaseDir = AbsDir
type RunDir = RelDir

data LogMessage = StdOut Text | StdErr Text | Terminate deriving (Show)
type FullOutput = (ExitCode, Text, Text)
type FullTimedOutput = (FullOutput, NominalDiffTime)

type ProcFun = Text -> [Text] -> Shell Line -> IO FullOutput
type ProcInput = (Text, [Text], Shell Line)

data HledgerInfo = HledgerInfo { hlPath :: TurtlePath
                               , hlVersion :: Text
                               }
                 deriving (Show)

data SystemInfo = SystemInfo { os :: String
                             , arch :: String
                             , compilerName :: String
                             , compilerVersion :: Version
                             }
                deriving (Show)

class HasVerbosity a where
  verbose :: a -> Bool

class HasBaseDir a where
  baseDir :: a -> BaseDir

class HasRunDir a where
  importRunDir :: a -> RunDir

class HasSequential a where
  sequential :: a -> Bool
