{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Discover first, then classify paths relative to the known import root.
module Hledger.Flow.Import.Inventory
  ( findImportFiles,
    stageFiles,
    inImportScope,
    generatedOutput,
    expectedOutputs,
    classifyGeneratedOutputs,
    conflictingOutputs,
    canonicalFile,
    manualIncludeFiles,
    isManualJournal,
    managedIncludeFiles,
    moveToTrash,
  )
where

import Control.Exception (bracket_)
import Control.Monad (forM)
import Data.Char (isDigit)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir)
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.IO.Error (catchIOError, isAlreadyExistsError, isDoesNotExistError)

findImportFiles :: AbsDir -> IO [AbsFile]
findImportFiles root = List.sort <$> PathIO.walkDirAccum (Just exclude) collect root
  where
    exclude _ dirs _ = return $ PathIO.WalkExclude $
      filter (ignored . FilePath.dropTrailingPathSeparator . toFilePath . dirname) dirs
    collect _ _ files = return $ filter (not . hidden . toFilePath . filename) files
    ignored name = hidden name || name == "__pycache__"
    hidden ('.' : _) = True
    hidden _ = False

relativeParts :: AbsDir -> AbsFile -> [String]
relativeParts root file = maybe [] (FilePath.splitDirectories . toFilePath) (stripProperPrefix root file)

isYear :: String -> Bool
isYear year = length year == 4 && all isDigit year

stageFiles :: AbsDir -> RelDir -> [AbsFile] -> [AbsFile]
stageFiles root stage = filter matches
  where
    name = FilePath.dropTrailingPathSeparator (toFilePath stage)
    matches file = case relativeParts root file of
      [_, _, _, actualStage, year, _] -> actualStage == name && isYear year
      _ -> False

-- Call only on classified stage files, which have a numeric year parent.
inImportScope :: AbsDir -> Integer -> AbsFile -> Bool
inImportScope scope startYear file =
  isProperPrefixOf scope file && read year >= startYear
  where
    year = FilePath.dropTrailingPathSeparator (toFilePath (dirname (parent file)))

generatedOutput :: RelDir -> String -> AbsFile -> IO AbsFile
generatedOutput stage extension source = do
  name <- parseRelFile (FilePath.replaceExtension (toFilePath (filename source)) extension)
  return (parent (parent (parent source)) </> stage </> dirname (parent source) </> name)

expectedOutputs :: [AbsFile] -> IO (Map.Map AbsFile [AbsFile])
expectedOutputs sources = do
  pairs <- outputPairs sources
  canonical <- forM pairs $ \(output, source) -> do
    key <- canonicalFile output
    return (key, [source])
  return (Map.fromListWith (++) canonical)

-- | Orphaned generated files and the source-backed journals to include.
classifyGeneratedOutputs :: AbsDir -> [AbsFile] -> [AbsFile] -> IO ([AbsFile], [AbsFile])
classifyGeneratedOutputs root inventory sources = do
  expected <- expectedOutputs sources
  let staged stage ext = filter ((== ext) . FilePath.takeExtension . toFilePath) (stageFiles root stage inventory)
      cached = staged [reldir|2-preprocessed|] ".csv"
      journals = staged [reldir|3-journal|] ".journal"
  classified <- forM (cached ++ journals) $ \file -> do
    key <- canonicalFile file
    return (file, Map.member key expected)
  return ([file | (file, False) <- classified],
          [file | (file, True) <- classified, FilePath.takeExtension (toFilePath file) == ".journal"])

canonicalFile :: AbsFile -> IO AbsFile
canonicalFile file = Directory.canonicalizePath (toFilePath file) >>= parseAbsFile

outputPairs :: [AbsFile] -> IO [(AbsFile, AbsFile)]
outputPairs sources = concat <$> mapM outputs sources
  where
    outputs source = do
      csv <- generatedOutput [reldir|2-preprocessed|] ".csv" source
      journal <- generatedOutput [reldir|3-journal|] ".journal" source
      return [(csv, source), (journal, source)]

-- Nonexistent outputs cannot be canonicalized for case or Unicode aliases.
-- Probe their names in a temporary directory on the destination filesystem.
conflictingOutputs :: [AbsFile] -> IO [(AbsFile, [AbsFile])]
conflictingOutputs sources = do
  pairs <- outputPairs sources
  grouped <- forM pairs $ \pair@(output, _) -> do
    directory <- Directory.canonicalizePath (toFilePath (parent output)) >>= parseAbsDir
    return (directory, [pair])
  fmap concat $ forM (Map.toAscList (Map.fromListWith (++) grouped)) $ \(directory, outputs) -> do
    existing <- nearestDirectory directory
    PathIO.withTempDir existing ".hledger-flow-destinations-" $ \tmp -> do
      names <- forM outputs $ \pair@(output, _) -> do
        name <- parseRelDir (toFilePath (filename output))
        let probe = tmp </> name
        PathIO.createDirIfMissing True probe
        key <- Directory.canonicalizePath (toFilePath probe)
        return (key, [pair])
      return [(output, source : map snd rest)
             | (output, source) : rest <- Map.elems (Map.fromListWith (++) names), not (null rest)]
  where
    nearestDirectory directory = do
      exists <- PathIO.doesDirExist directory
      if exists then return directory else nearestDirectory (parent directory)

-- Seed the include at the manual file's own level; its contents are added by
-- the existing pre/post renderer, keeping ordering and avoiding duplicates.
manualIncludeFiles :: AbsDir -> [AbsFile] -> IO [AbsFile]
manualIncludeFiles root files = Set.toAscList . Set.fromList <$> mapM includePath (filter (isManualJournal root) files)
  where
    includePath file = do
      let year = FilePath.dropTrailingPathSeparator (toFilePath (dirname (parent file)))
      name <- parseRelFile (year ++ "-include.journal")
      return (parent (parent (parent file)) </> name)

-- | A @_manual_@ pre- or post-import journal that import includes.
isManualJournal :: AbsDir -> AbsFile -> Bool
isManualJournal root file = case reverse (relativeParts root file) of
  name : year : "_manual_" : ancestors ->
    length ancestors <= 3 && isYear year
      && name `elem` ["pre-import.journal", "post-import.journal"]
  _ -> False

managedIncludeFiles :: AbsDir -> [AbsFile] -> [AbsFile]
managedIncludeFiles root = filter supported
  where
    supported file = case reverse (relativeParts root file) of
      name : ancestors -> length ancestors <= 3
        && (name == "all-years.journal" || (isYear (take 4 name) && drop 4 name == "-include.journal"))
      _ -> False

moveToTrash :: AbsFile -> IO AbsFile
moveToTrash file = do
  let year = dirname (parent file)
      stage = dirname (parent (parent file))
      account = parent (parent (parent file))
      trash = account </> [reldir|.trash|]
      lock = trash </> [reldir|.move-lock|]
      destination = trash </> year </> stage
      acquire = Directory.createDirectory (toFilePath lock) `catchIOError` \err ->
        if isAlreadyExistsError err
          then ioError (userError ("Trash is in use: " ++ toFilePath lock
            ++ ". If no import is running, remove this empty lock directory and retry."))
          else ioError err
  PathIO.createDirIfMissing True trash
  -- Serialize destination selection and rename so two imports cannot replace
  -- each other's recovery files. An interrupted move leaves a recoverable lock.
  bracket_ acquire (PathIO.removeDir lock) $ do
    PathIO.createDirIfMissing True destination
    target <- available destination (0 :: Integer)
    PathIO.renameFile file target
    return target
  where
    available destination n = do
      name <- parseRelFile (toFilePath (filename file) ++ if n == 0 then "" else "." ++ show n)
      let target = destination </> name
      exists <- Directory.doesPathExist (toFilePath target)
      -- A dangling symlink also occupies the destination name.
      link <- PathIO.isSymlink target `catchIOError` \err ->
        if isDoesNotExistError err then return False else ioError err
      if exists || link then available destination (n + 1) else return target
