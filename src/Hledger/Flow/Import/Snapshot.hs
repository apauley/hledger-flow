{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Flow.Import.Snapshot
  ( SnapshotRun, SnapshotInput (..), PreparedSnapshot, snapshotInputs, snapshotManifest,
    withSnapshotRun, withSnapshotRunUsing, withSnapshot,
    translateDiagnostic, windowsSnapshotPathFits
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception hiding (handle)
import Control.Monad (filterM, forM_, unless, void, when)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAsciiLower, isDigit)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hLock, hTryLock)
import Hledger.Flow.Import.Dependencies
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.PrivateDirectory
import Hledger.Flow.PathHelpers (AbsDir, AbsFile)
import Numeric (showHex)
import Path
import qualified Path.IO as P
import qualified System.Directory as D
import qualified System.FilePath as FP
import System.IO
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO as Posix
#endif
import Text.Read (readMaybe)
import System.IO.Error

data SnapshotInput
  = RulesInput AbsFile AbsFile (Maybe AbsFile) (Maybe AbsFile) -- data, rules, directives, config
  | ConstructInput (Maybe AbsFile)
  deriving (Eq, Show)

data PreparedSnapshot = PreparedSnapshot
  { snapshotInputs :: SnapshotInput,
    snapshotManifest :: Map.Map AbsFile Digest,
    originalPaths :: Map.Map AbsFile AbsFile
  }

data SnapshotRun = SnapshotRun
  { ledger :: AbsDir,
    registration :: MVar (Maybe (AbsDir, Handle)),
    nextTree :: IORef Word32,
    makePrivate :: AbsDir -> IO ()
  }

withSnapshotRun :: AbsDir -> (SnapshotRun -> IO a) -> IO a
withSnapshotRun = withSnapshotRunUsing createPrivateDirectory

withSnapshotRunUsing :: (AbsDir -> IO ()) -> AbsDir -> (SnapshotRun -> IO a) -> IO a
withSnapshotRunUsing create base use = do
  run <- SnapshotRun base <$> newMVar Nothing <*> newIORef 0 <*> pure create
  let area = snapshotArea run
  -- Failure to claim an abandoned tree leaves it for a later invocation.
  void $ tryIOError $ do
    exists <- P.doesDirExist area
    when exists $ withRegistration area $ reap area
  use run `finally` (readMVar (registration run) >>= mapM_ (releaseRun area))

snapshotArea :: SnapshotRun -> AbsDir
snapshotArea run = ledger run </> [reldir|.hledger-flow/cache/snapshots|]

-- GHC prevents two writable opens of the same file within one process. Wait
-- for that case too (concurrent imports in one process). The OS lock also
-- serializes independent processes, including while an initializer is paused.
withRegistration :: AbsDir -> IO a -> IO a
withRegistration area action = bracket open hClose $ \handle -> hLock handle ExclusiveLock >> action
  where
    file = toFilePath (area </> [relfile|registration.lock|])
    open = catchIOError (openLockFile file) $ \e ->
      if isAlreadyInUseError e then threadDelay 1000 >> open else ioError e

-- A surviving hook must not retain its killed parent's lifetime lock.
openLockFile :: FilePath -> IO Handle
#ifdef mingw32_HOST_OS
openLockFile file = openBinaryFile file ReadWriteMode
#else
openLockFile file = bracketOnError (Posix.openFd file Posix.ReadWrite flags) Posix.closeFd Posix.fdToHandle
  where flags = Posix.defaultFileFlags {Posix.creat = Just 0o666, Posix.cloexec = True}
#endif

shortName :: Char -> Word32 -> String
shortName prefix n = prefix : replicate (8 - length digits) '0' ++ digits
  where digits = showHex n ""

lockFile :: AbsDir -> FilePath
lockFile dir = toFilePath (dir </> [relfile|run.lock|])

acquireTree :: SnapshotRun -> IO AbsDir
acquireTree run = do
  dir <- modifyMVarMasked (registration run) $ \current -> case current of
    Just registered -> pure (current, fst registered)
    Nothing -> do
      registered <- catchIOError register $ \e -> reject "private staging unavailable" (T.pack (displayException e))
      pure (Just registered, fst registered)
  n <- atomicModifyIORef' (nextTree run) $ \i -> (i + 1, i)
  tree <- (dir </>) <$> parseRelDir (shortName 't' n)
  P.createDir tree
  pure tree
  where
    area = snapshotArea run
    register = do
      P.createDirIfMissing True area
      withRegistration area (createRun 0)
    createRun n = do
      dir <- (area </>) <$> parseRelDir (shortName 'r' n)
      result <- tryIOError (makePrivate run dir)
      case result of
        Left e | isAlreadyExistsError e && n < maxBound -> createRun (n + 1)
        Left e -> ioError e
        Right () -> flip onException (P.removeDirRecur dir) $
          bracketOnError (openLockFile (lockFile dir)) hClose $ \handle -> do
            hLock handle ExclusiveLock
            pure (dir, handle)

releaseRun :: AbsDir -> (AbsDir, Handle) -> IO ()
releaseRun area (dir, handle) = withRegistration area $
  (clearRun dir `finally` hClose handle) >> removeEmptyRun dir

-- Keep the ownership marker until every byte of statement data is gone. A
-- process killed during reclamation leaves a lockable tree for the next run.
clearRun :: AbsDir -> IO ()
clearRun dir = do
  entries <- D.listDirectory (toFilePath dir)
  forM_ (filter (/= "run.lock") entries) $ \entry ->
    D.removePathForcibly (toFilePath dir FP.</> entry)

removeEmptyRun :: AbsDir -> IO ()
removeEmptyRun dir = D.removeFile (lockFile dir) >> P.removeDir dir

reap :: AbsDir -> IO ()
reap area = do
  names <- D.listDirectory (toFilePath area)
  forM_ (filter isRunName names) $ \name -> void $ tryIOError $ do
    dir <- (area </>) <$> parseRelDir name
    let path = FP.dropTrailingPathSeparator (toFilePath dir)
    linked <- D.pathIsSymbolicLink path
    unless linked $ do
      let marker = lockFile dir
      exists <- D.doesFileExist marker
      when exists $ do
        link <- D.pathIsSymbolicLink marker
        unless link $ do
          claimed <- bracket (openLockFile marker) hClose $ \handle -> do
            mine <- hTryLock handle ExclusiveLock
            when mine (clearRun dir)
            pure mine
          when claimed (removeEmptyRun dir)
  where
    isRunName ('r':digits) = length digits == 8 && all hex digits
    isRunName _ = False
    hex c = c >= '0' && c <= '9' || isAsciiLower c && c <= 'f'

newtype PreparationFailure = PreparationFailure FallbackReason deriving Show
instance Exception PreparationFailure

reject :: Text -> Text -> IO a
reject reason detail = throwIO (PreparationFailure (FallbackReason reason detail))

fromDecision :: ReuseDecision a -> IO a
fromDecision (Reusable x) = pure x
fromDecision (Fallback reason) = throwIO (PreparationFailure reason)

prepare :: IO a -> IO (ReuseDecision a)
prepare io = (Reusable <$> io) `catches`
  [ Handler $ \(PreparationFailure reason) -> pure (Fallback reason),
    Handler $ \(e :: IOException) -> pure (Fallback (FallbackReason "snapshot creation failed" (T.pack (displayException e))))
  ]

withSnapshot :: SnapshotRun -> Release -> SnapshotInput -> (PreparedSnapshot -> IO a) -> IO (ReuseDecision a)
withSnapshot run version input convert = mask $ \restore -> do
  allocation <- prepare (acquireTree run)
  case allocation of
    Fallback reason -> pure (Fallback reason)
    Reusable tree -> flip finally (P.removeDirRecur tree) $ restore $ do
      result <- prepare (copyInputs (ledger run) tree version input)
      case result of
        Fallback reason -> pure (Fallback reason)
        Reusable snapshot -> Reusable <$> convert snapshot

windowsSnapshotPathFits :: FilePath -> Bool
windowsSnapshotPathFits = (< 260) . sum . map units
  where units c = if fromEnum c > 0xffff then 2 :: Int else 1

copyInputs :: AbsDir -> AbsDir -> Release -> SnapshotInput -> IO PreparedSnapshot
copyInputs base tree version input = do
  manifest <- newIORef Map.empty
  paths <- newIORef Map.empty
  let mirror file = case stripProperPrefix base file of
        Nothing -> reject "snapshot input outside ledger" (T.pack (toFilePath file))
        Just rel -> pure (tree </> [reldir|l|] </> rel)
      destination source target = do
#ifdef mingw32_HOST_OS
        unless (windowsSnapshotPathFits (toFilePath target)) $
          reject "overlong Windows snapshot path" (T.pack (toFilePath target))
#endif
        P.createDirIfMissing True (parent target)
        -- Source and cache directories can have different case or Unicode
        -- equivalence rules. Never overwrite a copy made for another spelling.
        previous <- Map.lookup target <$> readIORef paths
        collision <- case previous of
          Just original -> pure (toFilePath original /= toFilePath source)
          Nothing -> D.doesPathExist (toFilePath target)
        when collision $ reject "snapshot filenames collide" (T.pack (toFilePath source))
      record source target digest = do
        previous <- Map.lookup source <$> readIORef manifest
        when (maybe False (/= digest) previous) $
          reject "dependency changed during snapshot" (T.pack (toFilePath source))
        modifyIORef' manifest (Map.insert source digest)
        modifyIORef' paths (Map.insert target source)
      copyBytes source target = do
        destination source target
        bytes <- BS.readFile (toFilePath source)
        BS.writeFile (toFilePath target) bytes
        record source target (digestBytes bytes)
        pure bytes
      validated check source target = do
        bytes <- copyBytes source target
        () <- fromDecision (check version source bytes)
        pure target
      copyConfig source = validated checkConfig source (tree </> [reldir|c|] </> filename source)
      copyTree kind root = do
        completed <- newIORef Set.empty
        copyIncluded kind completed Set.empty root
      copyIncluded kind completed ancestors source = do
        target <- mirror source
        identity <- D.canonicalizePath (toFilePath source)
        when (Set.member identity ancestors || Set.size ancestors >= 256) $ reject "cyclic or excessively deep include" (T.pack (toFilePath source))
        done <- Set.member (toFilePath source) <$> readIORef completed
        unless done $ do
          bytes <- copyBytes source target
          includes <- fromDecision (scanIncludes kind (Set.null ancestors) source bytes)
          forM_ includes $ \spelling -> do
            child <- resolveInclude kind base source spelling >>= fromDecision
            void $ copyIncluded kind completed (Set.insert identity ancestors) child
          modifyIORef' completed (Set.insert (toFilePath source))
        pure target
  copied <- case input of
    ConstructInput conf -> ConstructInput <$> traverse copyConfig conf
    RulesInput csv rules directives conf -> do
      dataCopy <- mirror csv
      destination csv dataCopy
      copyDigest csv dataCopy >>= record csv dataCopy
      rulesCopy <- copyTree (CsvRules version) rules
      directivesCopy <- traverse (copyTree JournalDirectives) directives
      RulesInput dataCopy rulesCopy directivesCopy <$> traverse copyConfig conf
  PreparedSnapshot copied <$> readIORef manifest <*> readIORef paths

-- hledger emits locations as "FILE:line:column:" (also "FILE:line:" and
-- "FILE: explanation"). Restrict replacement to its location fields.
-- Source excerpts following a Megaparsec gutter are user data.
translateDiagnostic :: PreparedSnapshot -> Text -> IO Text
translateDiagnostic snap diagnostic = T.intercalate "\n" <$> mapM translate (T.splitOn "\n" diagnostic)
  where
    translate line = do
      let (prefix, body) = splitHeader line
          candidates value = [ (T.take i value, T.drop i value)
                       | (i, ':') <- zip [0..] (T.unpack value)
                       , let next = T.take 1 (T.drop (i+1) value)
                       , T.null next || next == " " || T.all isDigit next ]
      if FP.isAbsolute (T.unpack body)
        then firstKnown prefix line (candidates body)
        else case T.stripPrefix "\"" body of
          Just quoted -> quotedPath prefix line (candidates quoted)
          Nothing -> translateRulePosition line
    -- Some config errors quote the entire location with Haskell string escaping.
    quotedPath _ line [] = pure line
    quotedPath prefix line ((token,suffix):rest) =
      case readMaybe (T.unpack ("\"" <> token <> "\"")) of
        Just name | FP.isAbsolute name -> do
          mapped <- lookupPath (T.pack name)
          case mapped of
            Just p -> pure (prefix <> T.dropEnd 1 (T.pack (show (toFilePath p))) <> suffix)
            Nothing -> quotedPath prefix line rest
        _ -> quotedPath prefix line rest
    -- 1.99.6 appends a second location to a field-assignment diagnostic.
    -- Keep arbitrary template/record text ahead of the final location intact.
    translateRulePosition line
      | any (`T.isPrefixOf` line) ["the ", "rule ", "    (overrides: "] || fieldLabel =
          let (before, body) = T.breakOnEnd "  (" line
              (fileColon, lineEnd) = T.breakOnEnd ":" body
              (digits, close) = T.span isDigit lineEnd
          in if T.null before || T.null fileColon || T.null digits || close `notElem` [")", "))"]
             then pure line
             else firstKnown before line [(T.dropEnd 1 fileColon, ":" <> lineEnd)]
      | otherwise = pure line
      where
        fieldLabel = case T.stripPrefix "  " line of
          Just field -> let (name, rest) = T.span isAlphaNum field
                        in not (T.null name) && ":" `T.isPrefixOf` rest
          Nothing -> False
    splitHeader line =
      let (spaces, rest) = T.span (== ' ') line
          (program, message) = splitPrefix ["hledger: ", "hledger.exe: "] rest
          (label, body) = splitPrefix ["Warning: ", "Error: ", "error: ", "in "] message
      in (spaces <> program <> label, body)
    splitPrefix prefixes value = case [(p,s) | p <- prefixes, Just s <- [T.stripPrefix p value]] of
      (parts:_) -> parts
      [] -> ("",value)
    firstKnown _ original [] = pure original
    firstKnown prefix original ((token, suffix):more) = do
      mapped <- lookupPath token
      case mapped of
        Just path -> pure (prefix <> T.pack (toFilePath path) <> suffix)
        Nothing -> firstKnown prefix original more
    lookupPath token = case parseAbsFile (T.unpack token) >>= (\p -> Map.lookup p (originalPaths snap)) of
      Just path -> pure (Just path)
      Nothing -> catchIOError (byIdentity (T.unpack token)) (const (pure Nothing))
    byIdentity name = do
      actual <- D.canonicalizePath name
      -- canonicalizePath resolves symlinked ancestors and native spellings.
      -- File IDs also identify a Windows short path when its spelling differs.
      source <- parseAbsFile actual
      targetId <- stampIdentity <$> statFile source
      matches <- filterM (\p -> (== targetId) . stampIdentity <$> statFile p) (Map.keys (originalPaths snap))
      pure $ case matches of
        [p] -> Map.lookup p (originalPaths snap)
        _ -> Nothing
