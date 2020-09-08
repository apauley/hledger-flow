module Hledger.Flow.Import.Types
where

import Hledger.Flow.PathHelpers (TurtlePath)
import qualified Data.Map.Strict as Map

type InputFileBundle = Map.Map TurtlePath [TurtlePath]

data ImportDirs = ImportDirs { importDir  :: TurtlePath
                             , ownerDir   :: TurtlePath
                             , bankDir    :: TurtlePath
                             , accountDir :: TurtlePath
                             , stateDir   :: TurtlePath
                             , yearDir    :: TurtlePath
                             } deriving (Show)
