module Hledger.Flow.Import.Types where

import qualified Data.Map.Strict as Map
import Hledger.Flow.PathHelpers (RelFile)
import Path (Dir, Path)

type TurtleFileBundle = Map.Map FilePath [FilePath]

type InputFileBundle = Map.Map RelFile [RelFile]

data ImportDirs b = ImportDirs
  { importDir :: Path b Dir,
    ownerDir :: Path b Dir,
    bankDir :: Path b Dir,
    accountDir :: Path b Dir,
    stateDir :: Path b Dir,
    yearDir :: Path b Dir
  }
  deriving (Eq, Show)
