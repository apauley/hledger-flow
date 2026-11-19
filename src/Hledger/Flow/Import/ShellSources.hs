{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Import.ShellSources
  ( SourceCache, newSourceCache, SourceInput, sourceKey, hookSources ) where

import Control.Monad (foldM, unless, when)
import Data.Char (isAlpha, isAlphaNum, isAscii, isControl, isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.PathHelpers (AbsFile)
import qualified Path as P
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralString)
import ShellCheck.Interface
import ShellCheck.Parser (parseScript)
import qualified System.Directory as D
import System.Environment (lookupEnv)
import qualified System.FilePath as F
import System.IO (IOMode(ReadMode), withBinaryFile)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as Posix
#endif

-- Cache only syntax, never path resolution or filesystem observations.
type SourceCache = IORef (Map.Map Digest [Step])
newSourceCache :: IO SourceCache
newSourceCache = newIORef Map.empty

data SourceInput = SourceInput FilePath FileObservation [(String, Digest)] deriving (Eq, Show)

sourceKey :: SourceInput -> [(T.Text, T.Text)]
sourceKey (SourceInput target obs variables) =
  (T.pack target, digestHex (observationDigest obs)) :
  [("source variable " <> T.pack name, digestHex digest) | (name,digest) <- variables]

data Piece = Literal String | Variable Bool String
data Step = Include [Piece] | ShellStateChanged | Uncertain

hookSources :: SourceCache -> AbsFile -> FileObservation -> IO [SourceInput]
hookSources cache hook observed = do
  let file = P.toFilePath hook
  header <- withBinaryFile file ReadMode (fmap (B8.takeWhile (/= '\n')) . flip BS.hGet 256)
  case interpreter file header of
    Other -> pure []
    UnrecognisedShell -> uncertain file
    ShellScript -> do
      startup <- lookupEnv "BASH_ENV"
      when (maybe False (not . null) startup) (uncertain file)
      files <- newIORef []
      visits <- newIORef (0 :: Int)
      let visit parents path observation = do
            canonical <- D.canonicalizePath path
            when (Set.member canonical parents) (uncertain path)
            n <- atomicModifyIORef' visits (\i -> (i+1,i))
            when (n >= 128) (uncertain path)
            plan <- scriptSteps cache path (observationDigest observation)
            foldM (step (Set.insert canonical parents) path) True plan
          step _ _ _ ShellStateChanged = pure False
          step _ path _ Uncertain = uncertain path
          step parents path pristine (Include parts) = do
            -- Earlier commands can change working directory, variable bindings
            -- or the meaning of the source builtin.
            unless pristine (uncertain path)
            expanded <- mapM (expand path) parts
            let name = concatMap fst expanded
                variables = concatMap snd expanded
            unless ('/' `elem` name && not ("-" `isPrefixOf` name) &&
                    not (any isControl name)) (uncertain path)
            -- Test the original spelling before canonicalizing missing/../
            -- and symlink/../ components.
#ifdef mingw32_HOST_OS
            regular <- D.doesFileExist name
#else
            regular <- Posix.isRegularFile <$> Posix.getFileStatus name
#endif
            unless regular (uncertain path)
            target <- D.canonicalizePath name
            dep <- P.parseAbsFile target >>= observeFile
            modifyIORef' files (SourceInput target dep variables :)
            visit parents target dep
      _ <- visit Set.empty file observed
      reverse <$> readIORef files
  where
    uncertain f = ioError (userError ("cannot resolve shell source safely in " ++ f))
    expand _ (Literal s) = pure (s, [])
    expand f (Variable quoted name) = do
      value <- lookupEnv name
      case value of
        Just s | not (null s), quoted || not (any (\c -> isSpace c || c `elem` ("*?[" :: String)) s) ->
          pure (s, [(name, digestBytes (TE.encodeUtf8 (T.pack s)))])
        _ -> uncertain f

data Interpreter = ShellScript | Other | UnrecognisedShell

interpreter :: FilePath -> BS.ByteString -> Interpreter
#ifdef mingw32_HOST_OS
interpreter file _ | F.takeExtension file == ".ps1" = Other
#endif
interpreter _ header = case B8.stripPrefix "#!" header of
  Nothing -> if BS.all (\c -> c == 9 || c == 13 || c >= 32) header then UnrecognisedShell else Other
  Just line -> case words (B8.unpack line) of
    exe:args | F.takeFileName exe == "env" -> case args of
                 [name] | shell name -> ShellScript
                 _ | any shell args -> UnrecognisedShell
                 _ -> Other
             | shell exe -> if null args then ShellScript else UnrecognisedShell
    _ -> Other
  where shell p = F.takeFileName p `elem` ["bash","sh","dash"]

scriptSteps :: SourceCache -> FilePath -> Digest -> IO [Step]
scriptSteps cache path digest = do
  existing <- Map.lookup digest <$> readIORef cache
  case existing of
    Just plan -> pure plan
    Nothing -> do
      bytes <- BS.readFile path
      unless (digestBytes bytes == digest) (ioError (userError ("script changed while reading " ++ path)))
      let plan = case TE.decodeUtf8' bytes of
            Left _ -> [Uncertain]
            Right body ->
              let result = runIdentity $ parseScript newSystemInterface newParseSpec
                    { psScript = T.unpack body, psFilename = path, psShellTypeOverride = Just Bash, psIgnoreRC = True }
              in case prRoot result of
                Just root | all ((/= ErrorC) . cSeverity . pcComment) (prComments result) -> steps root
                _ -> [Uncertain]
      atomicModifyIORef' cache (\m -> (Map.insert digest plan m, ()))
      pure plan

-- A function or loop can execute later or repeatedly with different bindings.
steps :: Token -> [Step]
steps token = case token of
  T_Annotation _ _ t -> steps t
  T_Script _ _ ts -> concatMap steps ts
  T_Pipeline _ [] [t] -> steps t
  T_Redirecting _ [] t -> steps t
  T_AndIf _ a b | not (containsSource b) -> steps a ++ steps b
  T_OrIf _ a b | not (containsSource b) -> steps a ++ steps b
  T_SimpleCommand _ [] (cmd:args)
    | isSource cmd -> case args of
        path:rest | all sourceArgument rest, Just parts <- pieces False path -> [Include parts]
        _ -> [Uncertain]
    | Just name <- getLiteralString cmd, name `elem` ["exit","true","false",":"],
      all (isJust . getLiteralString) args -> []
    | getLiteralString cmd == Just "set",
      all (\a -> getLiteralString a `elem` map Just ["-e","-u","-eu","-euo","-o","pipefail"]) args -> []
  T_Function _ _ _ name body | name `notElem` ["source",".","set","exit","true","false",":","[","test"], not (containsSource body) -> []
  T_Condition _ _ (TC_Unary _ _ op arg) | op `elem` ["-z","-n"], isJust (pieces True arg) || positionalArgument arg -> []
  _ | containsSource token -> [Uncertain]
    | otherwise -> [ShellStateChanged]
  where
    -- Forwarded hook arguments are already captured by the hook key.
    sourceArgument t = positionalArgument t || maybe False (all literal) (pieces False t)
    literal (Literal _) = True
    literal _ = False
    positionalArgument (T_NormalWord _ [T_DoubleQuoted _ [T_DollarBraced _ _ name]]) =
      case getLiteralString name of
        Just "@" -> True
        Just n -> not (null n) && all (\c -> c >= '0' && c <= '9') n
        Nothing -> False
    positionalArgument _ = False

isSource :: Token -> Bool
isSource t = getLiteralString t `elem` [Just ".", Just "source"]

containsSource :: Token -> Bool
containsSource t@(OuterToken _ children) = case t of
  T_SimpleCommand _ _ (cmd:args)
    | isSource cmd || getLiteralString cmd == Just "eval" -> True
    | getLiteralString cmd `elem` [Just "builtin", Just "command"], any isSource args -> True
  _ -> any containsSource (toList children)

-- Parameter operators, command substitution, globs and tilde expansion need Bash.
pieces :: Bool -> Token -> Maybe [Piece]
pieces quoted token = case token of
  T_NormalWord _ ts -> concat <$> mapM (pieces quoted) ts
  T_DoubleQuoted _ ts -> concat <$> mapM (pieces True) ts
  T_SingleQuoted _ s -> Just [Literal s]
  T_Literal _ s | quoted || not (any (\c -> c `elem` ("~*?[]{}" :: String)) s) -> Just [Literal s]
  T_DollarBraced _ _ t | Just name@(c:_) <- getLiteralString t,
    isAscii c && (isAlpha c || c == '_'), all (\x -> isAscii x && (isAlphaNum x || x == '_')) name,
    not ("BASH" `isPrefixOf` name || name `elem` ["_", "PWD", "OLDPWD", "IFS", "LINENO", "RANDOM", "SRANDOM",
          "SECONDS", "SHLVL", "UID", "EUID", "PPID", "OPTIND", "OPTARG", "SHELLOPTS", "FUNCNAME",
          "DIRSTACK", "GROUPS", "PIPESTATUS", "EPOCHSECONDS", "EPOCHREALTIME"]) ->
        Just [Variable quoted name]
  _ -> Nothing
