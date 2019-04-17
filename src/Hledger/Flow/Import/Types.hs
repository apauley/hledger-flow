module Hledger.Flow.Import.Types
where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Hledger.Flow.Types

data ImportOptions = ImportOptions { baseDir :: FilePath
                                   , verbose :: Bool
                                   , showOptions :: Bool
                                   , sequential :: Bool }
  deriving (Show)

instance HasVerbosity ImportOptions where
  verbose (ImportOptions _ v _ _) = v

instance HasSequential ImportOptions where
  sequential (ImportOptions _ _ _ s) = s

instance HasBaseDir ImportOptions where
  baseDir (ImportOptions bd _ _ _) = bd

data ImportDirs = ImportDirs { importDir  :: FilePath
                             , ownerDir   :: FilePath
                             , bankDir    :: FilePath
                             , accountDir :: FilePath
                             , stateDir   :: FilePath
                             , yearDir    :: FilePath
                             } deriving (Show)
