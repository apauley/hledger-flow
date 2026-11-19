{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.InputSnapshot
  ( InputSnapshot,
    snapshotInputs,
    inputTargets,
    inputsMatch,
    inputsUnchanged,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Hledger.Flow.PathHelpers (AbsFile, isGlobMetacharacter)
import Path (toFilePath)
import qualified System.Directory as Directory
import qualified System.FilePath as F

data Entry = Entry FilePath FilePath UTCTime
  deriving (Eq, Generic, Show)

instance FromJSON Entry
instance ToJSON Entry

data InputSnapshot = InputSnapshot [Entry] [Entry]
  deriving (Eq, Generic, Show)

instance FromJSON InputSnapshot
instance ToJSON InputSnapshot

entry :: FilePath -> IO Entry
entry file = Entry file <$> Directory.canonicalizePath file <*> Directory.getModificationTime file

snapshotInputs :: [AbsFile] -> IO InputSnapshot
snapshotInputs inputs = do
  let files = nub (map toFilePath inputs)
  inputsWithTargets <- traverse entry files
  watched <- concat <$> traverse (\(Entry _ target _) -> globDirectories target) inputsWithTargets
  InputSnapshot inputsWithTargets <$> traverse entry (nub watched)

inputTargets :: InputSnapshot -> [FilePath]
inputTargets (InputSnapshot files _) = [target | Entry _ target _ <- files]

inputsMatch :: InputSnapshot -> IO Bool
inputsMatch (InputSnapshot files dirs) = do
  let entries = files ++ dirs
  result <- try (traverse (\(Entry file _ _) -> entry file) entries)
  return $ either (const False :: IOException -> Bool) (== entries) result

inputsUnchanged :: InputSnapshot -> UTCTime -> IO Bool
inputsUnchanged snapshot@(InputSnapshot files dirs) since =
  if all (\(Entry _ _ modified) -> modified < since) (files ++ dirs)
    then inputsMatch snapshot
    else return False

globDirectories :: FilePath -> IO [FilePath]
globDirectories file = do
  contents <- T.readFile file
  let includes =
        [ T.unpack (readerPath arg)
          | line <- T.lines (T.dropWhile (== '\xFEFF') contents),
            Just rest <- [T.stripPrefix "include" line],
            Just (first, _) <- [T.uncons rest],
            isSpace first,
            let arg = T.strip (T.takeWhile (/= ';') rest),
            not (T.null arg)
        ]
  concat <$> mapM watch includes
  where
    -- hledger-flow emits singleton character ranges for literal metacharacters.
    -- Track wildcard branches conservatively, including parents of missing branches.
    readerPath arg =
      let (prefix, path) = T.breakOn ":" arg
       in if prefix `elem` ["journal", "timeclock", "timedot", "csv", "tsv", "ssv"]
            then fromMaybe arg (T.stripPrefix ":" path)
            else arg
    watch ('~' : separator : path) | separator `elem` ("/\\" :: String) =
      Directory.getHomeDirectory >>= \home -> watchUnder home path
    watch path = watchUnder (F.takeDirectory file) path
    watchUnder parent path =
      let parts = concatMap expandRecursive (F.splitDirectories path)
          (fixed, patternParts) = span (not . snd . decodeLiteral) parts
          root = (if F.isAbsolute path then id else (parent F.</>))
            (F.joinPath (map (fst . decodeLiteral) fixed))
       in case patternParts of
            [] -> return []
            _ -> watchFrom Set.empty root patternParts

-- hledger also interprets ** with a filename suffix as a recursive glob.
expandRecursive :: String -> [String]
expandRecursive = go []
  where
    go before ('*' : '*' : suffix@(_ : _)) = (reverse before ++ "**") : go [] ('*' : suffix)
    go before (c : rest) = go (c : before) rest
    go before [] = [reverse before]

watchFrom :: Set.Set (FilePath, [FilePath]) -> FilePath -> [FilePath] -> IO [FilePath]
watchFrom _ _ [] = return []
watchFrom visited dir (part : rest) = do
  exists <- Directory.doesDirectoryExist dir
  if not exists
    then return []
    else do
      resolved <- Directory.canonicalizePath dir
      if Set.member (resolved, part : rest) visited
        then return [dir]
        else do
          let next = Set.insert (resolved, part : rest) visited
              (literal, glob) = decodeLiteral part
          descend <- case (part, glob, rest) of
            (_, _, []) | part /= "**" -> return []
            ("**", _, _) -> do
              children <- subdirs dir
              here <- watchFrom visited dir rest
              below <- concat <$> traverse (\child -> watchFrom next child (part : rest)) children
              return (here ++ below)
            (_, True, _) -> subdirs dir >>= fmap concat . traverse (\child -> watchFrom next child rest)
            _ -> watchFrom next (dir F.</> literal) rest
          return (dir : descend)
  where
    subdirs p = Directory.listDirectory p >>= filterM Directory.doesDirectoryExist . map (p F.</>)

decodeLiteral :: String -> (String, Bool)
decodeLiteral ('[' : x : '-' : y : ']' : rest) | x == y, isGlobMetacharacter x =
  let (decoded, glob) = decodeLiteral rest in (x : decoded, glob)
decodeLiteral (x : rest) =
  let (decoded, glob) = decodeLiteral rest in (x : decoded, isGlobMetacharacter x || glob)
decodeLiteral [] = ("", False)
