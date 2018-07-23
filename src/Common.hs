{-# LANGUAGE OverloadedStrings #-}

module Common
    ( onlyDirs
    , validDirs
    , filterPaths
    , echoShell
    , printShell
    ) where

import Turtle
import Prelude hiding (FilePath)

onlyDirs :: Shell FilePath -> Shell FilePath
onlyDirs = filterPaths isDirectory

filterPaths :: (FileStatus -> Bool) -> Shell FilePath -> Shell FilePath
filterPaths filepred files = do
  path <- files
  filestat <- stat path
  if (filepred filestat) then select [path] else select []

validDirs :: Shell FilePath -> Shell FilePath
validDirs = excludeWeirdPaths . onlyDirs

excludeWeirdPaths :: Shell FilePath -> Shell FilePath
excludeWeirdPaths = findtree (suffix $ noneOf "_")

echoShell :: Line -> Shell ()
echoShell line = liftIO $ echo line

printShell :: Show a => a -> Shell ()
printShell o = liftIO $ print o
