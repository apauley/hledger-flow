{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Import.Dependencies
  ( Release (..), FallbackReason (..), ReuseDecision (..), Converter (..),
    IncludeFile (..), IncludeNode (..), readIncludeTree, scanIncludes, resolveInclude,
    ruleIncludes, nestedRuleIncludes, discoverRules, selectRules, isSeparatedValues,
    checkedIO, atPath, checkArguments, discoverConfig, discoverDirectives, checkConfig, journalIncludes,
    newConverterProbe, converterProbeWith
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Monad (ap, foldM)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum, isAscii, isControl, isDigit, isSpace, toLower, GeneralCategory(CurrencySymbol), generalCategory)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Time (getCurrentTimeZone, timeZoneMinutes)
import GHC.IO.Encoding (getLocaleEncoding, getFileSystemEncoding, getForeignEncoding)
import Hledger.Flow.Common (firstConfirmedFile, lookupHledgerConf, probeHledgerVersion, hledgerVersionNumber)
import Hledger.Flow.HledgerConfig (ConfigLine (..), parseConfigLines)
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.ImportHelpers (rulesFileCandidates)
import Hledger.Flow.Import.Types (ImportDirs)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types (HledgerConf(..), hledgerConfPath)
import Path (Abs, parent, parseAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
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
  -- The notice quotes the entry, so people see exactly what to change.
  | ConfigEntry {reasonScope :: T.Text, reasonFile :: FilePath, reasonDetail :: T.Text}
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

resolveInclude :: IncludeFile -> AbsDir -> AbsFile -> FilePath -> IO (ReuseDecision AbsFile)
resolveInclude kind base including child = checkedIO ("uncertain " <> label) raw $
  if F.hasDrive child || not (F.isRelative child) || any F.isPathSeparator (take 1 child)
    then pure (atPath ("absolute " <> label) child (T.pack (toFilePath including)))
    else case kind of
      CsvRules _ -> resolveRules
      JournalDirectives -> resolveJournal
  where
    -- Keep the filename as opened: a file symlink's nested includes are
    -- relative to its containing directory. A path without ".." is kept as
    -- spelled. Otherwise the OS resolves the parent, so a directory link
    -- before ".." leads where hledger's open would.
    resolveRules = case parseAbsFile raw of
      Just file -> pure (Reusable file)
      Nothing -> do
        exists <- D.doesFileExist raw
        if exists
          then do
            dir <- D.canonicalizePath (F.takeDirectory raw)
            pure (maybe outside Reusable (parseAbsFile (dir F.</> F.takeFileName raw)))
          else pure (atPath "could not read rules file" raw (T.pack (toFilePath including)))
    resolveJournal = do
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
          source <- D.canonicalizePath (toFilePath including)
          dir <- D.canonicalizePath directory
          let compatible = F.equalFilePath dir (F.takeDirectory source) &&
                F.equalFilePath actual (root F.</> F.makeRelative (toFilePath base) lexical)
          if compatible && F.equalFilePath actual mirror && inside root actual
            then pure (maybe outside Reusable (parseAbsFile lexical))
            else pure outside
    directory = F.takeDirectory (toFilePath including)
    raw = directory F.</> child
    label = fileKind kind <> " include"
    outside = atPath (label <> " escapes the ledger or cannot be mirrored") raw (T.pack (toFilePath including))
    inside root path = F.splitDirectories (F.dropTrailingPathSeparator root) `isPrefixOf` F.splitDirectories path

discoverRules :: AbsDir -> Release -> AbsFile -> IO (ReuseDecision [(AbsFile,Digest)])
discoverRules base release = discoverTree base (CsvRules release)

discoverTree :: AbsDir -> IncludeFile -> AbsFile -> IO (ReuseDecision [(AbsFile,Digest)])
discoverTree base kind root = fmap (map dependency) <$> readIncludeTree base kind root
  where dependency node = (includeSource node, digestBytes (includeBytes node))

-- A rules file can be read through a directory link followed by "..". Its
-- source and its position in a link-free copy can then be different.
data IncludeNode = IncludeNode
  { includeSource :: AbsFile
  , includePosition :: AbsFile
  , includeDirectories :: [AbsDir]
  , includeBytes :: B.ByteString
  }

-- Walk the spelling that hledger will traverse in the private copy. Retain
-- directories visited before "..", even when no included file lives there.
rulesPosition :: AbsFile -> FilePath -> Maybe (AbsFile, [AbsDir])
rulesPosition including spelling = do
  name <- parseRelFile (F.takeFileName spelling)
  (dir, visited) <- foldM step (parent including, []) (F.splitDirectories (F.takeDirectory spelling))
  pure (dir </> name, visited)
  where
    step acc "." = Just acc
    step (dir, visited) ".."
      | parent dir /= dir = Just (parent dir, dir : parent dir : visited)
      | otherwise = Nothing
    step (dir, visited) component = do
      child <- parseRelDir component
      pure (dir </> child, visited)

readIncludeTree :: AbsDir -> IncludeFile -> AbsFile -> IO (ReuseDecision [IncludeNode])
readIncludeTree base kind root = walk Set.empty root [] root
  where
    nativeRules = case kind of
      CsvRules release -> release /= V1996
      JournalDirectives -> False
    walk ancestors position dirs path = checkedIO ("could not read " <> fileKind kind <> " file") (toFilePath path) $ do
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
                nested <- descend (Set.insert node ancestors) path position children
                pure ((IncludeNode path position dirs bytes :) <$> nested)
    descend _ _ _ [] = pure (pure [])
    descend ancestors path position (c:cs) = do
      selected <- resolveInclude kind base path c
      tree <- case selected of
        Fallback r -> pure (Fallback r)
        Reusable file -> case kind of
          JournalDirectives -> walk ancestors file [] file
          CsvRules _ -> case rulesPosition position c of
            Just (child, dirs) -> walk ancestors child dirs file
            Nothing -> pure (atPath "rules include cannot be mirrored" c (T.pack (toFilePath path)))
      case tree of
        Fallback r -> pure (Fallback r)
        Reusable files -> fmap (files ++) <$> descend ancestors path position cs

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
  ls <- physicalLines path (dropUtf8Bom bytes)
  let settings =
        [ (configLineNumber row, word)
          | row <- parseConfigLines (release == V1996) ls,
            configSection row `elem` ["general", "print"],
            word <- configArguments row
        ]
  case configDependency settings of
    Nothing -> pure ()
    Just n -> Fallback $ ConfigEntry "non-deterministic hledger config entry" (toFilePath path)
      ("line " <> T.pack (show n) <> ": " <> T.strip (ls !! (n - 1)))

-- A denylist, not a config validator: hledger validates unfamiliar syntax and
-- options on conversion. Add only known dependencies outside the cache inputs.
configDependency :: [(Int,T.Text)] -> Maybe Int
configDependency settings = go settings
  where
    -- A fixed --today pins relative dates and valuation, but hledger ignores
    -- it for forecasting.
    pinned = or (zipWith fixedToday (map snd settings) (drop 1 (map snd settings) ++ [""]))
    fixedToday arg next = case T.stripPrefix "--today" arg of
      Just "" -> fixedDate next
      Just v | Just date <- T.stripPrefix "=" v -> fixedDate date
      _ -> False
    fixedDate date = not (T.null date || relativeDate date)
    go [] = Nothing
    go ((n,arg):rest)
      -- Untracked files, saved import state, extra output and temporary paths.
      | "@" `T.isPrefixOf` arg || flag ["--file", "--rules", "--rules-file", "--conf"] ["-f"] = found
      | flag ["--new", "--output-file", "--location", "--locations"] ["-o"] = found
      -- The rest depend on today's date.
      | flag ["--forecast"] [], T.null suffix || relativeDate value || openForecast value = found
      | pinned = go rest
      -- Without a report end date, hledger values amounts at today's prices.
      | flag ["--exchange"] ["-X", "-V"] = found
      | flag ["--value"] [], T.toLower (T.takeWhile (/= ',') value) `elem` ["end", "now"] = found
      | flag ["--begin", "--end", "--period", "--today"] ["-b", "-e", "-p"], relativeDate value = found
      | any relativeDate (queryDates arg) = found
      | otherwise = go rest
      where
        found = Just n
        (name,suffix) = T.breakOn "=" arg
        short = [s | s <- ["-f", "-o", "-X", "-b", "-e", "-p"], s `T.isPrefixOf` arg]
        flag longs shorts = name `elem` longs || any (`T.isPrefixOf` arg) shorts
        value | not (T.null suffix) = T.drop 1 suffix
              | s:_ <- short, T.length arg > T.length s = T.drop (T.length s) arg
              | (_,v):_ <- rest = v
              | otherwise = ""
    openForecast period = ".." `T.isSuffixOf` period
      || any (`T.isPrefixOf` period) ["from", "since"] && not ("to" `T.isInfixOf` period || ".." `T.isInfixOf` period)
    -- Dates in date: and date2: queries, including those inside expr:.
    queryDates token = case T.stripPrefix "expr:" (stripNot token) of
      Just expr -> mapMaybe queryDate (T.words (T.map (\c -> if c `elem` ("()" :: String) then ' ' else c) expr))
      Nothing -> maybeToList (queryDate token)
    queryDate token = case T.breakOn ":" (stripNot token) of
      (prefix,date) | prefix `elem` ["date", "date2"] -> Just (T.drop 1 date)
      _ -> Nothing
    stripNot token = maybe token stripNot (T.stripPrefix "not:" token)

-- Recognize relative words and dates with an implicit year. Unfamiliar date
-- syntax stays eligible, like unfamiliar options; this list can grow over time.
relativeDate :: T.Text -> Bool
relativeDate value = any relative parts
  where
    parts = T.words lowered ++ T.words (T.replace ".." " " (T.replace "to" " " lowered))
    lowered = T.toLower value
    relative part = any (`T.isPrefixOf` part) ["today", "tomorrow", "yesterday", "this", "last", "next"]
      || part `elem` ["ago", "ahead", "day", "days", "week", "weeks", "month", "months", "quarter", "quarters", "year", "years",
                     "jan", "january", "feb", "february", "mar", "march", "apr", "april", "may",
                     "jun", "june", "jul", "july", "aug", "august", "sep", "september",
                     "oct", "october", "nov", "november", "dec", "december", "q1", "q2", "q3", "q4"]
      || let digits = T.takeWhile isDigit (T.dropWhile (`elem` ("+-" :: String)) part) in not (T.null digits) && T.length digits < 4

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
