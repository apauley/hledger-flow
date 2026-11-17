{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Import.Dependencies
  ( Release (..), FallbackReason (..), ReuseDecision (..), Converter (..),
    IncludeFile (..), scanIncludes, resolveInclude, ruleIncludes, nestedRuleIncludes, discoverRules, selectRules, isSeparatedValues,
    checkedIO, atPath, checkArguments, discoverConfig, discoverDirectives, checkConfig, journalIncludes,
    newConverterProbe, converterProbeWith
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Monad (ap, foldM)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum, isAscii, isControl, isDigit, isSpace, toLower, GeneralCategory(CurrencySymbol), generalCategory)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Time (getCurrentTimeZone, timeZoneMinutes)
import GHC.IO.Encoding (getLocaleEncoding, getFileSystemEncoding, getForeignEncoding)
import Hledger.Flow.Common (firstConfirmedFile, lookupHledgerConf, probeHledgerVersion, hledgerVersionNumber)
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.ImportHelpers (rulesFileCandidates)
import Hledger.Flow.Import.Types (ImportDirs)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types (HledgerConf(..), hledgerConfPath)
import Path (Abs, parent, parseAbsFile, toFilePath)
import qualified System.Directory as D
import qualified System.FilePath as F
import System.Exit (ExitCode(..))
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Error (tryIOError, ioeGetFileName, ioeGetErrorString)
import qualified System.Info as Sys
#ifdef mingw32_HOST_OS
import System.Win32.Console (getConsoleCP, getConsoleOutputCP)
import System.Win32.NLS (getACP)
#endif

-- Exact releases have known reader quirks. Other eligible versions use syntax
-- shared by both readers, leaving ambiguous inputs to the ordinary converter.
data Release = V1524 | V1996 | OtherSupported deriving (Eq, Show)

data FallbackReason
  = FallbackReason {reasonScope :: T.Text, reasonDetail :: T.Text}
  | FileReason {reasonScope :: T.Text, reasonFile :: FilePath, reasonDetail :: T.Text}
  | OrdinaryInputError {reasonScope :: T.Text, reasonDetail :: T.Text}
  | CyclicRules {reasonScope :: T.Text, reasonDetail :: T.Text}
  deriving (Eq, Ord, Show)
data ReuseDecision a = Reusable a | Fallback FallbackReason deriving (Functor, Eq, Show)

instance Applicative ReuseDecision where
  pure = Reusable
  (<*>) = ap

instance Monad ReuseDecision where
  Reusable a >>= f = f a
  Fallback r >>= _ = Fallback r

atPath :: T.Text -> FilePath -> T.Text -> ReuseDecision a
atPath reason path detail = Fallback (FileReason reason path detail)

checkedIO :: T.Text -> FilePath -> IO (ReuseDecision a) -> IO (ReuseDecision a)
checkedIO reason path action = do
  result <- tryIOError action
  pure $ case result of
    Right a -> a
    Left e -> atPath reason (maybe path id (ioeGetFileName e)) (T.pack (ioeGetErrorString e))

physicalLines :: AbsFile -> B.ByteString -> ReuseDecision [T.Text]
physicalLines path bytes = case TE.decodeUtf8' bytes of
  Left _ -> atPath "unrecognised text encoding" (toFilePath path) "expected UTF-8"
  Right s -> pure (T.lines (T.replace "\r\n" "\n" s))

ruleIncludes :: Release -> AbsFile -> B.ByteString -> ReuseDecision [FilePath]
ruleIncludes release path bytes = do
  ls <- physicalLines path (dropUtf8Bom bytes)
  sequence (mapMaybe include (zip [1 :: Int ..] ls))
  where
    include (n,line)
      | Just raw <- T.stripPrefix "include " line = Just (checkPath n raw)
      | release == OtherSupported && looksLikeInclude line = Just (uncertain n)
      | otherwise = Nothing
    looksLikeInclude line = case T.words line of
      word:_ -> T.toCaseFold word == "include"
      [] -> False
    checkPath n raw
      | T.null name || T.any isControl name = uncertain n
      | release == OtherSupported && name /= T.stripEnd name = uncertain n
      | otherwise = pure (T.unpack name)
      where name = if release == V1996 then T.strip raw else T.stripStart raw
    uncertain n = atPath "uncertain rules include" (toFilePath path) ("line " <> T.pack (show n))

-- Older readers open nested rules through the native text handle. On POSIX a
-- CR at the end of an include line may be part of the filename.
nestedRuleIncludes :: Release -> AbsFile -> B.ByteString -> ReuseDecision [FilePath]
nestedRuleIncludes release path bytes
  | release /= V1996, any ambiguous (B.split 10 bytes) =
      atPath "uncertain nested rules include" (toFilePath path) "native line ending or byte-order mark"
  | otherwise = ruleIncludes release path bytes
  where
    ambiguous line = "include " `B.isPrefixOf` dropUtf8Bom line &&
                     (B.elem 13 line || dropUtf8Bom line /= line)

dropUtf8Bom :: B.ByteString -> B.ByteString
dropUtf8Bom b = maybe b id (B.stripPrefix (B.pack [0xef,0xbb,0xbf]) b)

data IncludeFile = CsvRules Release | JournalDirectives
  deriving (Eq, Show)

scanIncludes :: IncludeFile -> Bool -> AbsFile -> B.ByteString -> ReuseDecision [FilePath]
scanIncludes (CsvRules release) isRoot = if isRoot then ruleIncludes release else nestedRuleIncludes release
scanIncludes JournalDirectives _ = journalIncludes

fileKind :: IncludeFile -> T.Text
fileKind (CsvRules _) = "rules"
fileKind JournalDirectives = "journal"

-- Walk leading .. components relative to the including file's spelled path.
-- Verify the result against the filesystem's resolution, which can differ when a
-- symlink precedes .. . An internal symlink is mirrored under its original name.
resolveInclude :: IncludeFile -> AbsDir -> AbsFile -> FilePath -> IO (ReuseDecision AbsFile)
resolveInclude kind base including child = checkedIO ("uncertain " <> label) raw $
  if F.hasDrive child || not (F.isRelative child) || any F.isPathSeparator (take 1 child)
    then pure (atPath ("absolute " <> label) child (T.pack (toFilePath including)))
    else do
      let (ups, rest) = span (== "..") (filter (/= ".") (F.splitDirectories child))
          lexical = foldr (const F.takeDirectory) directory ups F.</> F.joinPath rest
      if ".." `elem` rest || null rest || not (inside (toFilePath base) lexical)
        then pure outside
        else do
          actual <- D.canonicalizePath raw
          mirror <- D.canonicalizePath lexical
          root <- D.canonicalizePath (toFilePath base)
          -- hledger resolves journal includes from the canonical including file.
          -- The snapshot preserves its spelled directory and journal reader.
          compatible <- case kind of
            CsvRules _ -> pure True
            JournalDirectives -> do
              source <- D.canonicalizePath (toFilePath including)
              dir <- D.canonicalizePath directory
              pure (F.equalFilePath dir (F.takeDirectory source) &&
                    F.equalFilePath actual (root F.</> F.makeRelative (toFilePath base) lexical))
          if compatible && F.equalFilePath actual mirror && inside root actual
            then pure (maybe outside Reusable (parseAbsFile lexical))
            else pure outside
  where
    directory = F.takeDirectory (toFilePath including)
    raw = directory F.</> child
    label = fileKind kind <> " include"
    outside = atPath (label <> " escapes the ledger or cannot be mirrored") raw (T.pack (toFilePath including))
    inside root path = F.splitDirectories (F.dropTrailingPathSeparator root) `isPrefixOf` F.splitDirectories path

discoverRules :: AbsDir -> Release -> AbsFile -> IO (ReuseDecision [(AbsFile,Digest)])
discoverRules base release = discoverTree base (CsvRules release)

discoverTree :: AbsDir -> IncludeFile -> AbsFile -> IO (ReuseDecision [(AbsFile,Digest)])
discoverTree base kind = walk Set.empty
  where
    nativeRules = case kind of
      CsvRules release -> release /= V1996
      JournalDirectives -> False
    walk ancestors path = checkedIO ("could not read " <> fileKind kind <> " file") (toFilePath path) $ do
      canonical <- D.canonicalizePath (toFilePath path)
      directory <- D.canonicalizePath (F.takeDirectory (toFilePath path))
      -- The same rules bytes can resolve includes in different directories.
      -- In 1.52.4 the first read also decodes line endings differently.
      let node = (canonical, directory, nativeRules && Set.null ancestors)
      if Set.member node ancestors && nativeRules
        then pure $ Fallback (CyclicRules "cyclic rules include" (T.pack (toFilePath path)))
        else if Set.member node ancestors || Set.size ancestors >= 256
          then pure (atPath ("cyclic or excessively deep " <> fileKind kind <> " include") (toFilePath path) "")
          else do
            bytes <- B.readFile (toFilePath path)
            case scanIncludes kind (Set.null ancestors) path bytes of
              Fallback r -> pure (Fallback r)
              Reusable children -> do
                nested <- descend (Set.insert node ancestors) path children
                pure (((path,digestBytes bytes):) <$> nested)
    descend _ _ [] = pure (pure [])
    descend ancestors path (c:cs) = do
      selected <- resolveInclude kind base path c
      tree <- case selected of
        Fallback r -> pure (Fallback r)
        Reusable file -> walk ancestors file
      case tree of
        Fallback r -> pure (Fallback r)
        Reusable files -> fmap (files ++) <$> descend ancestors path cs

selectRules :: AbsFile -> ImportDirs Abs -> IO (ReuseDecision (Maybe AbsFile))
selectRules csv dirs = checkedIO "could not select rules file" (toFilePath csv) $ do
  candidates <- rulesFileCandidates csv dirs
  selected <- firstConfirmedFile (map toFilePath candidates)
  Reusable <$> traverse parseAbsFile selected

isSeparatedValues :: AbsFile -> Bool
isSeparatedValues path = map toLower (F.takeExtension (toFilePath path)) `elem` [".csv",".tsv",".ssv"]

checkArguments :: [T.Text] -> ReuseDecision ()
checkArguments args = case filter ("@" `T.isPrefixOf`) args of
  a:_ -> Fallback (FallbackReason ("indirect hledger argument " <> a) "")
  [] -> pure ()

discoverConfig :: Release -> Maybe HledgerConf -> IO (ReuseDecision (Maybe (AbsFile,Digest)))
discoverConfig release configured = checkedIO "could not discover hledger config" "hledger.conf" $ do
  let given = case configured of
        Just (GivenConf p) -> Just p
        _ -> Nothing
  case checkArguments (maybe [] (pure . T.pack) given) of
    Fallback r -> pure (Fallback r)
    Reusable () -> do
      selected <- lookupHledgerConf given
      case hledgerConfPath <$> selected of
        Nothing -> pure (pure Nothing)
        Just path | "~" `isPrefixOf` path || ".." `elem` F.splitDirectories path ->
          pure (atPath "uncertain config path" path "")
        Just path -> do
          absolute <- D.makeAbsolute path
          case parseAbsFile absolute of
            Nothing -> pure (atPath "uncertain config path" path "")
            Just absPath -> fmap Just <$> readDependency (checkConfig release) absPath

discoverDirectives :: AbsFile -> IO (ReuseDecision [(AbsFile,Digest)])
discoverDirectives file = checkedIO "could not discover directives" (toFilePath file) $ do
  exists <- firstConfirmedFile [toFilePath file]
  case exists of
    Nothing -> pure (pure [])
    Just _ -> discoverTree (parent file) JournalDirectives file

readDependency :: (AbsFile -> B.ByteString -> ReuseDecision ()) -> AbsFile -> IO (ReuseDecision (AbsFile,Digest))
readDependency check path = checkedIO "could not read dependency" (toFilePath path) $ do
  bytes <- B.readFile (toFilePath path)
  pure ((path,digestBytes bytes) <$ check path bytes)

checkConfig :: Release -> AbsFile -> B.ByteString -> ReuseDecision ()
checkConfig release path bytes = do
  ls <- physicalLines path bytes
  () <$ foldM line "general" (zip [1 :: Int ..] ls)
  where
    -- Only canonical built-ins other than print have an inert section. Addons,
    -- aliases, abbreviations and names introduced in another release can have
    -- different scope and must be passed through to the ordinary converter.
    inactive = T.words $ "accounts activity add aregister balance balancesheet balancesheetequity cashflow check close codes commodities descriptions diff files help import incomestatement notes payees prices register repl rewrite roi run setup stats tags " <>
      case release of
        V1524 -> "commands demo test"
        V1996 -> "get holdings transactions"
        OtherSupported -> ""
    line section (n,raw)
      | T.null body || "#" `T.isPrefixOf` body = pure section
      | release /= V1996, "[" `T.isPrefixOf` body, T.stripStart raw /= raw = unsupported n body
      | Just start <- T.stripPrefix "[" body = case T.breakOn "]" start of
          (s,end) | not (T.null end), not ("#" `T.isInfixOf` s), let name = T.strip s,
                    name `elem` ("general":"print":inactive) -> option n name (T.drop 1 end)
          _ -> unsupported n body
      | otherwise = option n section body
      where body = T.strip raw
    option n section arg
      | section `elem` inactive = pure section
      | trackedPrintOptions (T.words setting) = pure section
      | otherwise = unsupported n setting
      where setting = T.strip (T.takeWhile (/= '#') arg)
    unsupported n value = atPath "unsupported hledger config" (toFilePath path) ("line " <> T.pack (show n) <> ": " <> value)

-- These options only change how print renders the tracked inputs. hledger
-- validates literal values on conversion. Unknown tokens require an ordinary run.
trackedPrintOptions :: [T.Text] -> Bool
trackedPrintOptions [] = True
trackedPrintOptions (flag:rest)
  | flag `elem` ["--explicit", "-x", "--invert"] = trackedPrintOptions rest
  | name `elem` ["--round", "--layout"] = case T.stripPrefix "=" suffix of
      Just value -> literal value && trackedPrintOptions rest
      Nothing -> case rest of
        value:more -> literal value && trackedPrintOptions more
        [] -> False
  | otherwise = False
  where
    (name,suffix) = T.breakOn "=" flag
    literal value = not (T.null value) && T.all (\c -> isAscii c && isAlphaNum c) value

journalIncludes :: AbsFile -> B.ByteString -> ReuseDecision [FilePath]
journalIncludes path bytes = do
  ls <- physicalLines path (dropUtf8Bom bytes)
  sequence (mapMaybe classify (zip [1 :: Int ..] ls))
  where
    classify (n,raw)
      | allowed raw = Nothing
      | ("include",rest) <- T.break horizontalSpace raw,
        let target = T.dropAround horizontalSpace (T.takeWhile (/= ';') rest)
            journalPath = T.stripPrefix "journal:" target
            name = fromMaybe target journalPath,
        not (T.null name),
        T.all (\c -> isAlphaNum c || c `elem` (" ./_-" :: String)) name =
          Just $ if journalPath == Nothing && map toLower (F.takeExtension (T.unpack name)) `elem` nonJournalExtensions
            then atPath "unsupported journal include reader" (toFilePath path) (lineDetail n raw)
            else pure (T.unpack name)
      | otherwise = Just (atPath "unsupported journal directive" (toFilePath path)
                          (lineDetail n raw))
    -- hledger defaults to the journal reader for unrecognized extensions.
    nonJournalExtensions = [".csv", ".tsv", ".ssv", ".rules", ".timeclock", ".timedot"]
    lineDetail n raw = "line " <> T.pack (show n) <> ": " <> T.strip raw
    horizontalSpace c = c == ' ' || c == '\t'
    allowed raw
      | T.null stripped || T.isPrefixOf ";" stripped || T.isPrefixOf "#" stripped || T.isPrefixOf "*" stripped = True
      | Just (c,_) <- T.uncons raw, isSpace c = False
      | otherwise = case T.words (T.takeWhile (/= ';') raw) of
          "account":xs -> not (null xs) && all (T.all accountChar) xs
          "commodity":xs -> amount xs
          "P":date:symbol:xs -> fullDate date && commodity symbol && amount xs
          _ -> False
      where stripped = T.strip raw
    accountChar c = isAlphaNum c || c `elem` (":_-/.'’" :: String)
    commodity s = not (T.null s) && T.all (\c -> isAlpha c || generalCategory c == CurrencySymbol) s
    amount [a,b] = (number a && commodity b) || (commodity a && number b)
    amount [a] =
      let (symbol,rest) = T.span (\c -> isAlpha c || generalCategory c == CurrencySymbol) a
          (value,suffix) = T.span (\c -> isDigit c || c `elem` (".,+-" :: String)) a
      in (commodity symbol && number rest) || (number value && commodity suffix)
    amount _ = False
    number s = case T.uncons (T.dropWhile (\c -> c == '+' || c == '-') s) of
      Just (c,rest) -> digit c && T.all (\x -> digit x || x == ',' || x == '.') rest
      _ -> False
    digit c = isAscii c && isDigit c
    fullDate s = case T.split (\c -> c == '-' || c == '/') s of
      [y,m,d] -> map T.length [y,m,d] == [4,2,2] && all (T.all digit) [y,m,d]
      _ -> False

data Converter = Converter
  { converterRelease :: Release
  , converterExecutable :: AbsFile
  , converterStamp :: FileStamp
  , converterKey :: [T.Text]
  } deriving (Eq, Show)

newConverterProbe :: RT.RuntimeOptions -> IO (IO (ReuseDecision Converter))
newConverterProbe opts = converterProbeWith (RT.hledgerExecutable opts) probeHledgerVersion

-- The expensive executable hash and version probe are shared by workers only
-- while the resolved executable and its handle-derived status still agree.
converterProbeWith :: AbsFile -> (AbsFile -> IO (ExitCode,T.Text)) -> IO (IO (ReuseDecision Converter))
converterProbeWith executable getVersion = do
  context <- checkedIO "could not identify the conversion context" original conversionContext
  cache <- newMVar Nothing
  pure $ case context of
    Fallback r -> pure (Fallback r)
    Reusable environment -> checkedIO "could not identify hledger executable" original $ do
      resolved <- D.canonicalizePath original >>= parseAbsFile
      let target = toFilePath resolved
      wrapped <- launcher original target
      if wrapped
        then pure (atPath "hledger launcher" original "select the real executable with --hledger-path")
        else modifyMVar cache $ \old -> do
          stamp <- statFile resolved
          case old of
            Just (p,s,c) | p == resolved && s == stamp -> pure (old, c)
            _ -> do
              observed <- observeFile resolved
              (status,version) <- getVersion resolved
              after <- statFile resolved
              live <- D.canonicalizePath original
              let key = [T.pack original,T.pack target,digestHex (observationDigest observed),version] ++ environment
                  result | after /= stamp || fileStamp observed /= stamp || not (F.equalFilePath live target) =
                             atPath "hledger executable changed" original ""
                         | status /= ExitSuccess = atPath "could not identify hledger version" original ""
                         | otherwise = case supportedRelease version of
                             Just release -> Reusable (Converter release resolved stamp key)
                             Nothing -> Fallback (FallbackReason ("unsupported hledger version " <> version) (T.pack original))
              pure (Just (resolved,stamp,result),result)
  where original = toFilePath executable

conversionContext :: IO (ReuseDecision [T.Text])
conversionContext = do
  offset <- timeZoneMinutes <$> getCurrentTimeZone
  locale <- show <$> getLocaleEncoding
  fs <- show <$> getFileSystemEncoding
  foreignEnc <- show <$> getForeignEncoding
#ifdef mingw32_HOST_OS
  pages <- sequence [show <$> getConsoleCP, show <$> getConsoleOutputCP, show <$> getACP]
#else
  let pages = []
#endif
  -- Rules and directives are inspected from their exact UTF-8 snapshot bytes.
  -- A different reader encoding could interpret a byte sequence as another path.
  pure $ if map toLower locale `elem` ["utf-8","utf8","cp65001"]
    then Reusable (map T.pack ([Sys.os,Sys.arch,show offset,locale,fs,foreignEnc] ++ pages))
    else Fallback (FallbackReason ("unsupported conversion encoding " <> T.pack locale) "")

supportedRelease :: T.Text -> Maybe Release
supportedRelease text = do
  let number = T.takeWhile (/= '-') (hledgerVersionNumber text)
      component part = case TR.decimal part of
        Right (n, rest) | T.null rest -> Just (n :: Integer)
        _ -> Nothing
  version <- traverse component (T.splitOn "." number)
  case version of
    [1,52,4] -> Just V1524
    [1,99,6] -> Just V1996
    _ | version >= [1,52,4] -> Just OtherSupported
      | otherwise -> Nothing

launcher :: FilePath -> FilePath -> IO Bool
launcher original target
  | any (\p -> map toLower (F.takeExtension p) `elem` [".cmd",".bat"]) [original,target] = pure True
  | otherwise = do
      shim <- firstConfirmedFile [F.replaceExtension p "shim" | p <- [original,target], map toLower (F.takeExtension p) == ".exe"]
      case shim of
        Just _ -> pure True
        Nothing -> do
          header <- withBinaryFile target ReadMode (flip B.hGet 4)
          pure $ not (B.take 2 header == "MZ" || header `elem` nativeMagic)
  where
    -- ELF, Mach-O (both byte orders) and universal Mach-O (32- and 64-bit).
    nativeMagic = map B.pack [[127,69,76,70],[0xfe,0xed,0xfa,0xce],[0xce,0xfa,0xed,0xfe],
      [0xfe,0xed,0xfa,0xcf],[0xcf,0xfa,0xed,0xfe],[0xca,0xfe,0xba,0xbe],[0xbe,0xba,0xfe,0xca],
      [0xca,0xfe,0xba,0xbf],[0xbf,0xba,0xfe,0xca]]
