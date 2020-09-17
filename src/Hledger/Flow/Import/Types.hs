module Hledger.Flow.Import.Types where

import Hledger.Flow.PathHelpers (RelFile, TurtlePath)
import qualified Data.Map.Strict as Map

type TurtleFileBundle = Map.Map TurtlePath [TurtlePath]
type InputFileBundle = Map.Map RelFile [RelFile]

data ImportDirs = ImportDirs { importDir  :: TurtlePath
                             , ownerDir   :: TurtlePath
                             , bankDir    :: TurtlePath
                             , accountDir :: TurtlePath
                             , stateDir   :: TurtlePath
                             , yearDir    :: TurtlePath
                             } deriving (Show)
