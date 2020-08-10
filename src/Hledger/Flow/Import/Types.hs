module Hledger.Flow.Import.Types
where

import Hledger.Flow.PathHelpers (TurtlePath)

data ImportDirs = ImportDirs { importDir  :: TurtlePath
                             , ownerDir   :: TurtlePath
                             , bankDir    :: TurtlePath
                             , accountDir :: TurtlePath
                             , stateDir   :: TurtlePath
                             , yearDir    :: TurtlePath
                             } deriving (Show)
