{-# LANGUAGE OverloadedStrings #-}

-- | Functions which currently uses TurtlePath and will be replaced with Path eventually
module Hledger.Flow.Import.ImportHelpersTurtle
  ( allYearIncludeFiles,
    extractImportDirs,
    extraIncludesForFile,
    groupIncludeFiles,
    groupAndWriteIncludeFiles,
    includePreamble,
    toIncludeFiles,
    toIncludeLine,
    writeIncludesUpTo,
    writeToplevelAllYearsInclude,
    yearsIncludeMap,
  )
where

import Control.Concurrent.STM (TChan)
import qualified Data.List as List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hledger.Flow.BaseDir (relativeToBase, relativeToBase', turtleBaseDir)
import Hledger.Flow.Common (allYearsFileName, directivesFile, filterPaths, groupValuesBy, writeFiles, writeFiles')
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging (logVerbose)
import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.Types
import Turtle ((%), (<.>), (</>))
import qualified Turtle

extractImportDirs :: TurtlePath -> Either T.Text ImportDirs
extractImportDirs inputFile = do
  case importDirBreakdown inputFile of
    [bd, owner, bank, account, filestate, year] -> Right $ ImportDirs bd owner bank account filestate year
    _ -> do
      Left $
        Turtle.format
          ( "I couldn't find the right number of directories between \"import\" and the input file:\n"
              % Turtle.fp
              % "\n\nhledger-flow expects to find input files in this structure:\n"
              % "import/owner/bank/account/filestate/year/trxfile\n\n"
              % "Have a look at the documentation for a detailed explanation:\n"
              % Turtle.s
          )
          inputFile
          (docURL "input-files")

importDirBreakdown :: TurtlePath -> [TurtlePath]
importDirBreakdown = importDirBreakdown' []

importDirBreakdown' :: [TurtlePath] -> TurtlePath -> [TurtlePath]
importDirBreakdown' acc path = do
  let dir = Turtle.directory path
  if Turtle.dirname dir == "import" || (Turtle.dirname dir == "")
    then dir : acc
    else importDirBreakdown' (dir : acc) $ Turtle.parent dir

groupIncludeFiles :: [TurtlePath] -> (TurtleFileBundle, TurtleFileBundle)
groupIncludeFiles = allYearIncludeFiles . groupIncludeFilesPerYear . filter isJournalFile

isJournalFile :: TurtlePath -> Bool
isJournalFile f = Turtle.extension f == Just "journal"

allYearIncludeFiles :: TurtleFileBundle -> (TurtleFileBundle, TurtleFileBundle)
allYearIncludeFiles m = (m, yearsIncludeMap $ Map.keys m)

yearsIncludeMap :: [TurtlePath] -> TurtleFileBundle
yearsIncludeMap = groupValuesBy allYearsPath

allYearsPath :: TurtlePath -> TurtlePath
allYearsPath = allYearsPath' Turtle.directory

allYearsPath' :: (TurtlePath -> TurtlePath) -> TurtlePath -> TurtlePath
allYearsPath' dir p = dir p </> allYearsFileName

includeFileName :: TurtlePath -> TurtlePath
includeFileName = (<.> "journal") . T.unpack . (Turtle.format (Turtle.fp % "-include")) . Turtle.dirname

groupIncludeFilesPerYear :: [TurtlePath] -> TurtleFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p : _) = case extractImportDirs p of
  Right _ -> groupValuesBy initialIncludeFilePath ps
  Left _ -> groupValuesBy parentIncludeFilePath ps

initialIncludeFilePath :: TurtlePath -> TurtlePath
initialIncludeFilePath p = (Turtle.parent . Turtle.parent . Turtle.parent) p </> includeFileName p

parentIncludeFilePath :: TurtlePath -> TurtlePath
parentIncludeFilePath p = (Turtle.parent . Turtle.parent) p </> Turtle.filename p

toIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtleFileBundle -> IO (Map.Map TurtlePath T.Text)
toIncludeFiles opts ch m = do
  preMap <- extraIncludes opts ch (Map.keys m) ["opening.journal"] ["pre-import.journal"] []
  postMap <- extraIncludes opts ch (Map.keys m) ["closing.journal"] ["post-import.journal"] ["prices.journal"]
  return $ (addPreamble . toIncludeFiles' preMap postMap) m

toIncludeFiles' :: TurtleFileBundle -> TurtleFileBundle -> TurtleFileBundle -> Map.Map TurtlePath T.Text
toIncludeFiles' preMap postMap = Map.mapWithKey $ generatedIncludeText preMap postMap

addPreamble :: Map.Map TurtlePath T.Text -> Map.Map TurtlePath T.Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: TurtlePath -> TurtlePath -> T.Text
toIncludeLine base file = Turtle.format ("include " % Turtle.fp) $ relativeToBase' base file

generatedIncludeText :: TurtleFileBundle -> TurtleFileBundle -> TurtlePath -> [TurtlePath] -> T.Text
generatedIncludeText preMap postMap outputFile fs = do
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let files = List.nub . List.sort $ fs
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let lns = map (toIncludeLine $ Turtle.directory outputFile) $ preFiles ++ files ++ postFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: T.Text
includePreamble = "### Generated by hledger-flow - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [TurtlePath] -> IO [TurtlePath]
groupAndWriteIncludeFiles opts ch = writeFileMap opts ch . groupIncludeFiles

writeFileMap :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> (TurtleFileBundle, TurtleFileBundle) -> IO [TurtlePath]
writeFileMap opts ch (m, allYears) = do
  _ <- writeFiles' $ (addPreamble . toIncludeFiles' Map.empty Map.empty) allYears
  writeFiles . (toIncludeFiles opts ch) $ m

writeIncludesUpTo :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtlePath -> [TurtlePath] -> IO [TurtlePath]
writeIncludesUpTo _ _ _ [] = return []
writeIncludesUpTo opts ch stopAt journalFiles = do
  let shouldStop = any (\dir -> dir == stopAt) $ map Turtle.parent journalFiles
  if shouldStop
    then return journalFiles
    else do
      newJournalFiles <- groupAndWriteIncludeFiles opts ch journalFiles
      writeIncludesUpTo opts ch stopAt newJournalFiles

writeToplevelAllYearsInclude :: (HasBaseDir o, HasVerbosity o) => o -> IO [TurtlePath]
writeToplevelAllYearsInclude opts = do
  directivesExists <- Turtle.testfile (directivesFile opts)
  let preMap = if directivesExists then Map.singleton (turtleBaseDir opts </> allYearsFileName) [directivesFile opts] else Map.empty
  let allTop = Map.singleton (turtleBaseDir opts </> allYearsFileName) ["import" </> allYearsFileName]
  writeFiles' $ (addPreamble . toIncludeFiles' preMap Map.empty) allTop

extraIncludes :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [TurtlePath] -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO TurtleFileBundle
extraIncludes opts ch = extraIncludes' opts ch Map.empty

extraIncludes' :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtleFileBundle -> [TurtlePath] -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO TurtleFileBundle
extraIncludes' _ _ acc [] _ _ _ = return acc
extraIncludes' opts ch acc (file : files) extraSuffixes manualFiles prices = do
  extra <- extraIncludesForFile opts ch file extraSuffixes manualFiles prices
  extraIncludes' opts ch (Map.unionWith (++) acc extra) files extraSuffixes manualFiles prices

extraIncludesForFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> TurtlePath -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO TurtleFileBundle
extraIncludesForFile opts ch file extraSuffixes manualFiles prices = do
  let dirprefix = T.unpack $ fst $ T.breakOn "-" $ Turtle.format Turtle.fp $ Turtle.basename file
  let fileNames = map (T.unpack . Turtle.format (Turtle.fp % "-" % Turtle.s) dirprefix) extraSuffixes
  let suffixFiles = map (Turtle.directory file </>) fileNames
  let suffixDirFiles = map (((Turtle.directory file </> "_manual_") </> dirprefix) </>) manualFiles
  let priceFiles = map ((((Turtle.directory file </> "..") </> "prices") </> dirprefix) </>) prices
  let extraFiles = suffixFiles ++ suffixDirFiles ++ priceFiles
  filtered <- Turtle.single $ filterPaths Turtle.testfile extraFiles
  let logMsg =
        Turtle.format
          ("Looking for possible extra include files for '" % Turtle.fp % "' among these " % Turtle.d % " options: " % Turtle.s % ". Found " % Turtle.d % ": " % Turtle.s)
          (relativeToBase opts file)
          (length extraFiles)
          (Turtle.repr $ relativeFilesAsText opts extraFiles)
          (length filtered)
          (Turtle.repr $ relativeFilesAsText opts filtered)
  logVerbose opts ch logMsg
  return $ Map.fromList [(file, filtered)]

relativeFilesAsText :: (HasBaseDir o) => o -> [TurtlePath] -> [T.Text]
relativeFilesAsText opts = map (Turtle.format Turtle.fp . relativeToBase opts)
