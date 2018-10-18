{-# LANGUAGE OverloadedStrings #-}

module Common
    ( docURL
    , lsDirs
    , onlyDirs
    , onlyFiles
    , validDirs
    , filterPaths
    , basenameLine
    , buildFilename
    , shellToList
    , takeLast
    , firstLine
    , firstExistingFile
    , toIncludeLines
    , groupValuesBy
    , groupPairs
    , pairBy
    , aggregateFileName
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)
import Data.Text (intercalate)
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map

import Data.Function (on)
import qualified Data.List as List (sortBy, groupBy)
import Data.Ord (comparing)

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\l -> (fst . head $ l, map snd l)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

docURL :: Line -> Text
docURL = format ("https://github.com/apauley/hledger-makeitso#"%l)

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

firstExistingFile :: [FilePath] -> Shell (Maybe FilePath)
firstExistingFile files = do
  case files of
    []   -> return Nothing
    f:fs -> do
      exists <- testfile f
      if exists then return (Just f) else firstExistingFile fs

basenameLine :: FilePath -> Shell Line
basenameLine path = case (textToLine $ format fp $ basename path) of
  Nothing -> die $ format ("Unable to determine basename from path: "%fp%"\n") path
  Just bn -> return bn

buildFilename :: [Line] -> Text -> FilePath
buildFilename identifiers extension = fromText (intercalate "-" (map lineToText identifiers)) <.> extension

shellToList :: Shell FilePath -> Shell [FilePath]
shellToList files = fold files Fold.list

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

firstLine :: Text -> Line
firstLine = NonEmpty.head . textToLines

toIncludeLines :: Shell FilePath -> Shell Line
toIncludeLines paths = do
  journalFile <- paths
  return $ fromMaybe "" $ textToLine $ format ("!include "%fp) journalFile

aggregateFileName :: FilePath -> FilePath
aggregateFileName = (<.> "journal") . dirname
