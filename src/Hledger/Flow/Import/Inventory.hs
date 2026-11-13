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
    reconciliationInventory,
    conflictingOutputs,
    cannotAlias,
    canonicalFile,
    manualIncludeFiles,
    isManualJournal,
    managedIncludeFiles,
    moveToTrash,
  )
where

import Control.Exception (bracket_)
import Control.Monad (filterM, forM)
import Data.Char (isAscii, isDigit, isPrint, toLower)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hledger.Flow.Common (groupValuesBy, homeDirs, tildePath)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir)
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.IO.Error (catchIOError, isAlreadyExistsError, isDoesNotExistError, tryIOError)

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
generatedOutput stage extension source =
  -- One parse instead of chained Path.parent calls, which each renormalise the whole path.
  parseAbsFile (account FilePath.</> toFilePath stage FilePath.</> FilePath.takeFileName yearDir FilePath.</> name)
  where
    file = toFilePath source
    yearDir = FilePath.takeDirectory file
    account = FilePath.takeDirectory (FilePath.takeDirectory yearDir)
    name = FilePath.replaceExtension (FilePath.takeFileName file) extension

expectedOutputs :: [AbsFile] -> IO (Map.Map AbsFile [AbsFile])
expectedOutputs sources = outputPairs sources >>= canonicalOutputs

canonicalOutputs :: [(AbsFile, AbsFile)] -> IO (Map.Map AbsFile [AbsFile])
canonicalOutputs pairs = do
  canonical <- forM pairs $ \(output, source) -> do
    key <- canonicalFile output
    return (key, [source])
  return (Map.fromListWith (++) canonical)

-- | Orphaned generated files and the source-backed journals to include.
classifyGeneratedOutputs :: AbsDir -> [AbsFile] -> [AbsFile] -> IO ([AbsFile], [AbsFile])
classifyGeneratedOutputs root inventory sources = do
  pairs <- outputPairs sources
  let expected = Set.fromList (map (toFilePath . fst) pairs)
      staged stage ext = filter ((== ext) . FilePath.takeExtension . toFilePath) (stageFiles root stage inventory)
      cached = staged [reldir|2-preprocessed|] ".csv"
      journals = staged [reldir|3-journal|] ".journal"
      unmatched = filter ((`Set.notMember` expected) . toFilePath) (cached ++ journals)
  -- A differently spelled output can alias any expected file, even one which
  -- also matched literally (for example, through a symlink).
  orphans <- if null unmatched then return [] else do
    resolved <- Map.keysSet <$> canonicalOutputs pairs
    filterM (fmap (`Set.notMember` resolved) . canonicalFile) unmatched
  let retired = Set.fromList orphans
      active = filter (`Set.notMember` retired) journals
      preferExpected a b = if rank a <= rank b then a else b
      rank file = (Set.notMember (toFilePath file) expected, file)
  -- Resolve year directories once, including symlinked directories. Names
  -- came from listing the directory, so only symlinks need a full lookup.
  groups <- forM (Map.toList (groupValuesBy (FilePath.takeDirectory . toFilePath) active)) $ \(dir, files) -> do
    resolvedDir <- Directory.canonicalizePath dir
    forM files $ \file -> do
      link <- PathIO.isSymlink file
      key <- if link then Directory.canonicalizePath (toFilePath file)
             else return (resolvedDir FilePath.</> FilePath.takeFileName (toFilePath file))
      return (key, file)
  let chosen = Map.fromListWith preferExpected (concat groups)
  return (orphans, List.sort (Map.elems chosen))

-- | Inventory, orphaned outputs and source-backed journals for reconciliation.
reconciliationInventory :: AbsDir -> Maybe [AbsFile] -> IO ([AbsFile], [AbsFile], [AbsFile])
reconciliationInventory root cached = case cached of
  Nothing -> refresh
  Just files -> do
    result@(_, orphans, _) <- classify files
    if null orphans then return result else refresh
  where
    -- A source may have been restored since discovery.
    refresh = findImportFiles root >>= classify
    classify files = do
      (orphans, journals) <- classifyGeneratedOutputs root files (stageFiles root [reldir|1-in|] files)
      return (files, orphans, journals)

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
  -- Resolve each parent once, including links shared by different accounts.
  let byParent = groupValuesBy (parent . fst) pairs
  groups <- forM (Map.toList byParent) $ \(dir, outputs) -> do
    resolved <- Directory.canonicalizePath (toFilePath dir) >>= parseAbsDir
    return (resolved, outputs)
  concat <$> mapM checkDirectory (Map.toList (Map.fromListWith (++) groups))
  where
    checkDirectory (_, [_]) = return []
    checkDirectory (_, outputs) | cannotAlias (outputNames outputs) = return []
    checkDirectory (directory, outputs) = do
      let names = outputNames outputs
          distinct = Set.fromList names
      listed <- tryIOError (Directory.listDirectory (toFilePath directory))
      case listed of
        Right entries
          | Set.size distinct == length names && distinct `Set.isSubsetOf` Set.fromList entries -> return []
        _ -> do
          existing <- nearestDirectory directory
          PathIO.withTempDir existing ".hledger-flow-destinations-" $ \tmp -> do
            let probes = map (toFilePath tmp FilePath.</>) names
            mapM_ (Directory.createDirectoryIfMissing False) probes
            entries <- Directory.listDirectory (toFilePath tmp)
            if length entries == length names then return [] else do
              -- Resolve individual probes only when some names actually alias.
              keys <- mapM Directory.canonicalizePath probes
              let groups = Map.fromListWith (++) (zip keys (map (:[]) outputs))
              return [(output, source : map snd rest)
                     | (output, source) : rest <- Map.elems groups, not (null rest)]

    outputNames = map (toFilePath . filename . fst)

    nearestDirectory directory = do
      exists <- PathIO.doesDirExist directory
      if exists then return directory else nearestDirectory (parent directory)

-- | Names that stay distinct on any filesystem, so probing them is unnecessary:
-- distinct even ignoring case, plain ASCII (no Unicode normalisation), no
-- @~@ (Windows short names) and no trailing dot or space (Windows strips them).
cannotAlias :: [String] -> Bool
cannotAlias names = all plain names && Set.size (Set.fromList (map (map toLower) names)) == length names
  where
    plain name = not (null name) && all (\c -> isAscii c && isPrint c && c /= '~') name
      && last name `notElem` (". " :: String)

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
          then homeDirs >>= \homes -> ioError (userError ("Trash is in use: " ++ tildePath homes (toFilePath lock)
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
