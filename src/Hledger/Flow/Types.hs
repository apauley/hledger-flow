{-# LANGUAGE FlexibleInstances #-}

module Hledger.Flow.Types where

import qualified Turtle (ExitCode, NominalDiffTime, Shell, Line)
import qualified Data.Text as T
import Data.Version

import Hledger.Flow.PathHelpers

type BaseDir = AbsDir
type RunDir = RelDir

data LogMessage = StdOut T.Text | StdErr T.Text | Terminate deriving (Show)
type FullOutput = (Turtle.ExitCode, T.Text, T.Text)
type FullTimedOutput = (FullOutput, Turtle.NominalDiffTime)

type ProcFun = T.Text -> [T.Text] -> Turtle.Shell Turtle.Line -> IO FullOutput
type ProcInput = (T.Text, [T.Text], Turtle.Shell Turtle.Line)

data HledgerInfo = HledgerInfo { hlPath :: AbsFile
                               , hlVersion :: T.Text
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
