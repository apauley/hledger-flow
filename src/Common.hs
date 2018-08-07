{-# LANGUAGE OverloadedStrings #-}

module Common
    ( lsDirs
    , onlyDirs
    , onlyFiles
    , validDirs
    , filterPaths
    , searchUp
    , echoShell
    , printShell
    , basenameLine
    , buildFilename
    ) where

import Turtle
import Prelude hiding (FilePath)
import Data.Text (intercalate)

lsDirs :: FilePath -> Shell FilePath
lsDirs = validDirs . ls

onlyDirs :: Shell FilePath -> Shell FilePath
onlyDirs = filterPaths isDirectory

onlyFiles :: Shell FilePath -> Shell FilePath
onlyFiles = filterPaths isRegularFile

filterPaths :: (FileStatus -> Bool) -> Shell FilePath -> Shell FilePath
filterPaths filepred files = do
  path <- files
  filestat <- stat path
  if (filepred filestat) then select [path] else select []

validDirs :: Shell FilePath -> Shell FilePath
validDirs = excludeWeirdPaths . onlyDirs

excludeWeirdPaths :: Shell FilePath -> Shell FilePath
excludeWeirdPaths = findtree (suffix $ noneOf "_")

searchUp :: Int -> FilePath -> FilePath -> Shell FilePath
searchUp remainingLevels dir filename = do
  let filepath = dir </> filename
  exists <- testfile filepath
  if (exists || remainingLevels == 0)
    then return filepath
    else searchUp (remainingLevels - 1) (parent dir) filename

echoShell :: Line -> Shell ()
echoShell line = liftIO $ echo line

printShell :: Show a => a -> Shell ()
printShell o = liftIO $ print o

basenameLine :: FilePath -> Shell Line
basenameLine path = case (textToLine $ format fp $ basename path) of
  Nothing -> die $ format ("Unable to determine basename from path: "%fp%"\n") path
  Just bn -> return bn

buildFilename :: [Line] -> Text -> FilePath
buildFilename identifiers extension = fromText (intercalate "-" (map lineToText identifiers)) <.> extension
