module Hledger.Flow.Import.Types where

import qualified Data.Map.Strict as Map
import Hledger.Flow.PathHelpers (RelFile, TurtlePath)

type TurtleFileBundle = Map.Map TurtlePath [TurtlePath]

type InputFileBundle = Map.Map RelFile [RelFile]

data ImportDirs = ImportDirs
  { importDir :: TurtlePath,
    ownerDir :: TurtlePath,
    bankDir :: TurtlePath,
    accountDir :: TurtlePath,
    stateDir :: TurtlePath,
    yearDir :: TurtlePath
  }
  deriving (Show)
