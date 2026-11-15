{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Import.Dependencies
  ( Release (..), FallbackReason (..), ReuseDecision (..), Converter (..),
    ruleIncludes, resolveRuleInclude, discoverRules, selectRules, isSeparatedValues,
    checkArguments, discoverConfig, discoverDirectives, checkConfig, checkDirectives,
    newConverterProbe, converterProbeWith
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Monad (ap, foldM)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum, isAscii, isControl, isDigit, isSpace, toLower, GeneralCategory(CurrencySymbol), generalCategory)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTimeZone, timeZoneMinutes)
import GHC.IO.Encoding (getLocaleEncoding, getFileSystemEncoding, getForeignEncoding)
import Hledger.Flow.Common (firstConfirmedFile, lookupHledgerConf, probeHledgerVersion, hledgerVersionNumber)
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.ImportHelpers (rulesFileCandidates)
import Hledger.Flow.Import.Types (ImportDirs)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types (HledgerConf(..), hledgerConfPath)
import Path (Abs, parseAbsFile, toFilePath)
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

data Release = V1524 | V1996 deriving (Eq, Show)

data FallbackReason = FallbackReason {reasonScope :: T.Text, reasonDetail :: T.Text} deriving (Eq, Ord, Show)
data ReuseDecision a = Reusable a | Fallback FallbackReason deriving (Functor, Eq, Show)

instance Applicative ReuseDecision where
  pure = Reusable
  (<*>) = ap

instance Monad ReuseDecision where
  Reusable a >>= f = f a
  Fallback r >>= _ = Fallback r

atPath :: T.Text -> FilePath -> T.Text -> ReuseDecision a
atPath reason path detail = Fallback (FallbackReason (reason <> ": " <> T.pack path) detail)

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
  let paths = [(n,clean s) | (n,l) <- zip [1 :: Int ..] ls, Just s <- [T.stripPrefix "include " l]]
      clean = if release == V1524 then T.stripStart else T.strip
      check (n,s)
        | T.null s || T.any isControl s = atPath "uncertain rules include" (toFilePath path) ("line " <> T.pack (show n))
        | otherwise = pure (T.unpack s)
  traverse check paths

dropUtf8Bom :: B.ByteString -> B.ByteString
dropUtf8Bom b = maybe b id (B.stripPrefix (B.pack [0xef,0xbb,0xbf]) b)

-- Walk leading .. components relative to the including file's spelled path.
-- Verify the result against the filesystem's resolution, which can differ when a
-- symlink precedes .. . An internal symlink is mirrored under its original name.
resolveRuleInclude :: AbsDir -> AbsFile -> FilePath -> IO (ReuseDecision AbsFile)
resolveRuleInclude base parent child = checkedIO "uncertain rules include" raw $
  if F.hasDrive child || not (F.isRelative child) || any F.isPathSeparator (take 1 child)
    then pure (atPath "absolute rules include" child (T.pack (toFilePath parent)))
    else do
      let (ups, rest) = span (== "..") (filter (/= ".") (F.splitDirectories child))
          lexical = foldr (const F.takeDirectory) (F.takeDirectory (toFilePath parent)) ups F.</> F.joinPath rest
      if ".." `elem` rest || null rest || not (inside (toFilePath base) lexical)
        then pure outside
        else do
          actual <- D.canonicalizePath raw
          mirror <- D.canonicalizePath lexical
          root <- D.canonicalizePath (toFilePath base)
          if F.equalFilePath actual mirror && inside root actual
            then pure (maybe outside Reusable (parseAbsFile lexical))
            else pure outside
  where
    raw = F.takeDirectory (toFilePath parent) F.</> child
    outside = atPath "rules include escapes the ledger or cannot be mirrored" raw (T.pack (toFilePath parent))
    inside root path = F.splitDirectories (F.dropTrailingPathSeparator root) `isPrefixOf` F.splitDirectories path

discoverRules :: AbsDir -> Release -> AbsFile -> IO (ReuseDecision [(AbsFile,Digest)])
discoverRules base release = walk Set.empty
  where
    walk ancestors path = checkedIO "could not read rules file" (toFilePath path) $ do
      canonical <- D.canonicalizePath (toFilePath path)
      if Set.member canonical ancestors || Set.size ancestors >= 256
        then pure (atPath "cyclic or excessively deep rules include" (toFilePath path) "")
        else do
          bytes <- B.readFile (toFilePath path)
          case ruleIncludes release path bytes of
            Fallback r -> pure (Fallback r)
            Reusable children -> do
              nested <- descend (Set.insert canonical ancestors) path children
              pure (((path,digestBytes bytes):) <$> nested)
    descend _ _ [] = pure (pure [])
    descend ancestors path (c:cs) = do
      selected <- resolveRuleInclude base path c
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

discoverDirectives :: AbsFile -> IO (ReuseDecision (Maybe (AbsFile,Digest)))
discoverDirectives file = checkedIO "could not discover directives" (toFilePath file) $ do
  exists <- firstConfirmedFile [toFilePath file]
  case exists of
    Nothing -> pure (pure Nothing)
    Just _ -> fmap Just <$> readDependency (checkDirectives V1996) file

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
    line section (n,raw)
      | T.null body || "#" `T.isPrefixOf` body = pure section
      | Just start <- T.stripPrefix "[" body = case T.breakOn "]" start of
          (s,end) | not (T.null end), not ("#" `T.isInfixOf` s), let name = T.strip s,
                    name `elem` ("general":"print":inactive) -> option n name (T.drop 1 end)
          _ -> unsupported n
      | otherwise = option n section body
      where body = T.strip raw
    option n section arg
      | section `elem` inactive = pure section
      | T.null setting = pure section
      | section == "print" && release == V1996 && setting == "--layout=hledger1" = pure section
      | otherwise = unsupported n
      where setting = T.strip (T.takeWhile (/= '#') arg)
    unsupported n = atPath "unsupported hledger config" (toFilePath path) ("line " <> T.pack (show n))

checkDirectives :: Release -> AbsFile -> B.ByteString -> ReuseDecision ()
checkDirectives _ path bytes = do
  ls <- physicalLines path (dropUtf8Bom bytes)
  case [n | (n,l) <- zip [1 :: Int ..] ls, not (allowed l)] of
    [] -> pure ()
    n:_ -> atPath "unsupported journal directive" (toFilePath path) ("line " <> T.pack (show n))
  where
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
                             Just release -> Reusable (Converter release resolved key)
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
supportedRelease text = case T.takeWhile (/= '-') (hledgerVersionNumber text) of
  "1.52.4" -> Just V1524
  "1.99.6" -> Just V1996
  _ -> Nothing

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
