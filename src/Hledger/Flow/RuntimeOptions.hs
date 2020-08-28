module Hledger.Flow.RuntimeOptions
where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Hledger.Flow.Types

data RuntimeOptions = RuntimeOptions { baseDir :: BaseDir
                                     , importRunDir :: RunDir
                                     , useRunDir :: Bool
                                     , hfVersion :: Text
                                     , hledgerInfo :: HledgerInfo
                                     , sysInfo :: SystemInfo
                                     , verbose :: Bool
                                     , showOptions :: Bool
                                     , sequential :: Bool
                                     }
  deriving (Show)

instance HasVerbosity RuntimeOptions where
  verbose (RuntimeOptions _ _ _ _ _ _ v _ _) = v

instance HasSequential RuntimeOptions where
  sequential (RuntimeOptions _ _ _ _ _ _ _ _ sq) = sq

instance HasBaseDir RuntimeOptions where
  baseDir (RuntimeOptions bd _ _ _ _ _ _ _ _) = bd

instance HasRunDir RuntimeOptions where
  importRunDir (RuntimeOptions _ rd _ _ _ _ _ _ _) = rd
