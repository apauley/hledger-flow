{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Common where

import Path (absfile, relfile)
import qualified Path.IO as Path

import qualified Turtle
import Turtle ((%), (</>), (<.>))

import Prelude hiding (putStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified GHC.IO.Handle.FD as H

import Data.Char (isDigit)
import Data.Either

import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as Map

import Data.Function (on)
import qualified Data.List as List (null, sortBy, groupBy)
import Data.Ord (comparing)

import Hledger.Flow.Types

import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, TurtlePath, fromTurtleAbsFile, pathToTurtle)
import Hledger.Flow.BaseDir (turtleBaseDir, relativeToBase)

import Control.Concurrent.STM

hledgerPathFromOption :: Maybe TurtlePath -> IO AbsFile
hledgerPathFromOption pathOption = do
  case pathOption of
    Just h  -> do
      hlAbs <- fromTurtleAbsFile h
      isOnDisk <- Path.doesFileExist hlAbs
      if isOnDisk then return hlAbs else do
        let msg = Turtle.format ("Unable to find hledger at "%Turtle.fp) h
        errExit' 1 (T.hPutStrLn H.stderr) msg hlAbs
    Nothing -> do
      maybeH <- Path.findExecutable [relfile|hledger|]
      case maybeH of
        Just h  -> return h
        Nothing -> do
          let msg = "Unable to find hledger in your path.\n"
                <> "You need to either install hledger, or add it to your PATH, or provide the path to an hledger executable.\n\n"
                <> "There are a number of installation options on the hledger website: https://hledger.org/download.html"
          errExit' 1 (T.hPutStrLn H.stderr) msg [absfile|/hledger|]

hledgerVersionFromPath :: TurtlePath -> IO T.Text
hledgerVersionFromPath hlp = fmap (T.strip . Turtle.linesToText) (Turtle.single $ shellToList $ Turtle.inproc (Turtle.format Turtle.fp hlp) ["--version"] Turtle.empty)

hledgerInfoFromPath :: Maybe TurtlePath -> IO HledgerInfo
hledgerInfoFromPath pathOption = do
  hlp <- hledgerPathFromOption pathOption
  hlv <- hledgerVersionFromPath $ pathToTurtle hlp
  return $ HledgerInfo hlp hlv

showCmdArgs :: [T.Text] -> T.Text
showCmdArgs args = T.intercalate " " (map escapeArg args)

escapeArg :: T.Text -> T.Text
escapeArg a = if T.count " " a > 0 then "'" <> a <> "'" else a

errExit :: Int -> TChan LogMessage -> T.Text -> a -> IO a
errExit exitStatus ch = errExit' exitStatus (channelErrLn ch)

errExit' :: Int -> (T.Text -> IO ()) -> T.Text -> a -> IO a
errExit' exitStatus logFun errorMessage dummyReturnValue = do
  logFun errorMessage
  Turtle.sleep 0.1
  _ <- Turtle.exit $ Turtle.ExitFailure exitStatus
  return dummyReturnValue

descriptiveOutput :: T.Text -> T.Text -> T.Text
descriptiveOutput outputLabel outTxt = do
  if not (T.null outTxt)
    then Turtle.format (Turtle.s%":\n"%Turtle.s%"\n") outputLabel outTxt
    else ""

logTimedAction :: HasVerbosity o => o -> TChan LogMessage -> T.Text -> [T.Text]
  -> (TChan LogMessage -> T.Text -> IO ()) -> (TChan LogMessage -> T.Text -> IO ())
  -> IO FullOutput
  -> IO FullTimedOutput
logTimedAction opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger action = do
  logVerbose opts ch $ Turtle.format ("Begin: "%Turtle.s) cmdLabel
  if (List.null extraCmdLabels) then return () else logVerbose opts ch $ T.intercalate "\n" extraCmdLabels
  timed@((ec, stdOut, stdErr), diff) <- Turtle.time action
  stdoutLogger ch stdOut
  stderrLogger ch stdErr
  logVerbose opts ch $ Turtle.format ("End:   "%Turtle.s%" "%Turtle.s%" ("%Turtle.s%")") cmdLabel (Turtle.repr ec) (Turtle.repr diff)
  return timed

timeAndExitOnErr :: (HasSequential o, HasVerbosity o) => o -> TChan LogMessage -> T.Text
  -> (TChan LogMessage -> T.Text -> IO ()) -> (TChan LogMessage -> T.Text -> IO ())
  -> ProcFun -> ProcInput
  -> IO FullTimedOutput
timeAndExitOnErr opts ch cmdLabel = timeAndExitOnErr' opts ch cmdLabel []

timeAndExitOnErr' :: (HasSequential o, HasVerbosity o) => o -> TChan LogMessage -> T.Text -> [T.Text]
  -> (TChan LogMessage -> T.Text -> IO ()) -> (TChan LogMessage -> T.Text -> IO ())
  -> ProcFun -> ProcInput
  -> IO FullTimedOutput
timeAndExitOnErr' opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger procFun (cmd, args, stdInput) = do
  let action = procFun cmd args stdInput
  timed@((ec, stdOut, stdErr), _) <- logTimedAction opts ch cmdLabel extraCmdLabels stdoutLogger stderrLogger action
  case ec of
    Turtle.ExitFailure i -> do
      let cmdText = Turtle.format (Turtle.s%" "%Turtle.s) cmd $ showCmdArgs args
      let msgOut = descriptiveOutput "Standard output" stdOut
      let msgErr = descriptiveOutput "Error output" stdErr

      let exitMsg = Turtle.format ("\n=== Begin Error: "%Turtle.s%" ===\nExternal command:\n"%Turtle.s%"\nExit code "%Turtle.d%"\n"
                            %Turtle.s%Turtle.s%"=== End Error: "%Turtle.s%" ===\n") cmdLabel cmdText i msgOut msgErr cmdLabel
      errExit i ch exitMsg timed
    Turtle.ExitSuccess -> return timed

procWithEmptyOutput :: ProcFun
procWithEmptyOutput cmd args stdinput = do
  ec <- Turtle.proc cmd args stdinput
  return (ec, T.empty, T.empty)

parAwareProc :: HasSequential o => o -> ProcFun
parAwareProc opts = if (sequential opts) then procWithEmptyOutput else Turtle.procStrictWithErr

parAwareActions :: HasSequential o => o -> [IO a] -> IO [a]
parAwareActions opts = parAwareFun opts
  where
    parAwareFun op = if (sequential op) then sequence else Turtle.single . shellToList . Turtle.parallel

inprocWithErrFun :: (T.Text -> IO ()) -> ProcInput -> Turtle.Shell Turtle.Line
inprocWithErrFun errFun (cmd, args, standardInput) = do
  result <- Turtle.inprocWithErr cmd args standardInput
  case result of
    Right ln -> return ln
    Left  ln -> do
      (Turtle.liftIO . errFun . Turtle.lineToText) ln
      Turtle.empty

verboseTestFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> TurtlePath -> IO Bool
verboseTestFile opts ch p = do
  fileExists <- Turtle.testfile p
  let rel = relativeToBase opts p
  if fileExists
    then logVerbose opts ch $ Turtle.format ("Found '"%Turtle.fp%"'") rel
    else logVerbose opts ch $ Turtle.format ("Looked for but did not find '"%Turtle.fp%"'") rel
  return fileExists

groupPairs' :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupPairs' = map (\ll -> (fst . head $ ll, map snd ll)) . List.groupBy ((==) `on` fst)
              . List.sortBy (comparing fst)

groupPairs :: (Eq a, Ord a) => [(a, b)] -> Map.Map a [b]
groupPairs = Map.fromList . groupPairs'

pairBy :: (a -> b) -> [a] -> [(b, a)]
pairBy keyFun = map (\v -> (keyFun v, v))

groupValuesBy :: (Ord k, Ord v) => (v -> k) -> [v] -> Map.Map k [v]
groupValuesBy keyFun = groupPairs . pairBy keyFun

allYearsFileName :: TurtlePath
allYearsFileName = "all-years" <.> "journal"

lsDirs :: TurtlePath -> Turtle.Shell TurtlePath
lsDirs = onlyDirs . Turtle.ls

onlyDirs :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
onlyDirs = excludeHiddenFiles . excludeWeirdPaths . filterPathsByFileStatus Turtle.isDirectory

onlyFiles :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
onlyFiles = excludeHiddenFiles . filterPathsByFileStatus Turtle.isRegularFile

filterPathsByFileStatus :: (Turtle.FileStatus -> Bool) -> Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
filterPathsByFileStatus filepred files = do
  files' <- shellToList files
  filtered <- filterPathsByFileStatus' filepred [] files'
  Turtle.select filtered

filterPathsByFileStatus' :: (Turtle.FileStatus -> Bool) -> [TurtlePath] -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPathsByFileStatus' _ acc [] = return acc
filterPathsByFileStatus' filepred acc (file:files) = do
  filestat <- Turtle.stat file
  let filtered = if (filepred filestat) then file:acc else acc
  filterPathsByFileStatus' filepred filtered files

filterPaths :: (TurtlePath -> IO Bool) -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPaths = filterPaths' []

filterPaths' :: [TurtlePath] -> (TurtlePath -> IO Bool) -> [TurtlePath] -> Turtle.Shell [TurtlePath]
filterPaths' acc _ [] = return acc
filterPaths' acc filepred (file:files) = do
  shouldInclude <- Turtle.liftIO $ filepred file
  let filtered = if shouldInclude then file:acc else acc
  filterPaths' filtered filepred files

excludeHiddenFiles :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
excludeHiddenFiles paths = do
  p <- paths
  case (Turtle.match (Turtle.prefix ".") $ Turtle.format Turtle.fp $ Turtle.filename p) of
    [] -> Turtle.select [p]
    _  -> Turtle.select []

excludeWeirdPaths :: Turtle.Shell TurtlePath -> Turtle.Shell TurtlePath
excludeWeirdPaths = Turtle.findtree (Turtle.suffix $ Turtle.noneOf "_")

firstExistingFile :: [TurtlePath] -> IO (Maybe TurtlePath)
firstExistingFile files = do
  case files of
    []   -> return Nothing
    file:fs -> do
      exists <- Turtle.testfile file
      if exists then return (Just file) else firstExistingFile fs

basenameLine :: TurtlePath -> Turtle.Shell Turtle.Line
basenameLine path = case (Turtle.textToLine $ Turtle.format Turtle.fp $ Turtle.basename path) of
  Nothing -> Turtle.die $ Turtle.format ("Unable to determine basename from path: "%Turtle.fp%"\n") path
  Just bn -> return bn

buildFilename :: [Turtle.Line] -> T.Text -> TurtlePath
buildFilename identifiers ext = Turtle.fromText (T.intercalate "-" (map Turtle.lineToText identifiers)) <.> ext

shellToList :: Turtle.Shell a -> Turtle.Shell [a]
shellToList files = Turtle.fold files Fold.list

writeFiles :: IO (Map.Map TurtlePath T.Text) -> IO [TurtlePath]
writeFiles fileMap = do
  m <- fileMap
  writeFiles' m

writeFiles' :: Map.Map TurtlePath T.Text -> IO [TurtlePath]
writeFiles' fileMap = do
  writeTextMap fileMap
  return $ Map.keys fileMap

writeTextMap :: Map.Map TurtlePath T.Text -> IO ()
writeTextMap = Map.foldlWithKey (\a k v -> a <> Turtle.writeTextFile k v) (return ())

changeExtension :: T.Text -> TurtlePath -> TurtlePath
changeExtension ext path = (Turtle.dropExtension path) <.> ext

changePathAndExtension :: TurtlePath -> T.Text -> TurtlePath -> TurtlePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeOutputPath :: TurtlePath -> TurtlePath -> TurtlePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ Turtle.splitDirectories srcFile
  where changeSrcDir file = if file == "1-in/" || file == "2-preprocessed/" then newOutputLocation else file

listOwners :: HasBaseDir o => o -> Turtle.Shell TurtlePath
listOwners opts = fmap Turtle.basename $ lsDirs $ (turtleBaseDir opts) </> "import"

intPath :: Integer -> TurtlePath
intPath = Turtle.fromText . (Turtle.format Turtle.d)

includeYears :: TChan LogMessage -> TurtlePath -> IO [Integer]
includeYears ch includeFile = do
  txt <- Turtle.readTextFile includeFile
  case includeYears' txt of
    Left  msg   -> do
      channelErrLn ch msg
      return []
    Right years -> return years

includeYears' :: T.Text -> Either T.Text [Integer]
includeYears' txt = case partitionEithers (includeYears'' txt) of
  (errors, []) -> do
    let msg = Turtle.format ("Unable to extract years from the following text:\n"%Turtle.s%"\nErrors:\n"%Turtle.s) txt (T.intercalate "\n" $ map T.pack errors)
    Left msg
  (_, years) -> Right years

includeYears'' :: T.Text -> [Either String Integer]
includeYears'' txt = map extractDigits (T.lines txt)

extractDigits :: T.Text -> Either String Integer
extractDigits txt = fmap fst $ (T.decimal . (T.filter isDigit)) txt
