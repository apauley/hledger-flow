{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.HledgerConfig
  ( ConfigUse (..),
    ConfigConflict,
    ConfigLine (..),
    parseConfigLines,
    configConflicts,
    firstConfigConflict,
    findConfigConflict,
    findConfigConflicts,
    renderConfigConflict,
    requireSafeConfig,
  )
where

import qualified Data.ByteString as B
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Hledger.Flow.Common (hledgerVersionNumber)
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Types (HledgerConf, HledgerInfo (..), hledgerConfPath)

data ConfigLine = ConfigLine
  { configSection :: T.Text,
    configLineNumber :: Int,
    configSource :: T.Text,
    configArguments :: [T.Text],
    configStartsSection :: Bool
  }
  deriving (Eq, Show)

parseConfigLines :: Bool -> [T.Text] -> [ConfigLine]
parseConfigLines allowIndented = parseConfigLinesWith allowIndented configWords

parseConfigLinesWith :: Bool -> (T.Text -> [T.Text]) -> [T.Text] -> [ConfigLine]
parseConfigLinesWith allowIndented splitWords = go "general" . zip [1 ..]
  where
    go _ [] = []
    go section ((n, raw) : more)
      | T.null body || "#" `T.isPrefixOf` body = go section more
      | allowIndented || raw == T.stripStart raw,
        Just start <- T.stripPrefix "[" body,
        (name, end) <- T.breakOn "]" start, not (T.null end) =
          let next = T.strip name
           in ConfigLine next n body (splitWords (T.drop 1 end)) True : go next more
      | otherwise = ConfigLine section n body (splitWords body) False : go section more
      where body = T.strip raw

data ConfigUse = ImportConfig | ReportConfig | BalanceConfig
  deriving (Eq, Show)

data ConfigConflict = ConfigConflict FilePath ConfigLine Bool
  deriving (Eq, Show)

configConflicts :: FilePath -> [ConfigLine] -> [ConfigConflict]
configConflicts file rows = concatMap fromSection sections
  where
    sections = ["general", "print", "balancesheet", "incomestatement", "balance", "accounts"]
    fromSection section = scan
      [ (row, token)
        | row <- reverse (lastDefinition (reverse (filter ((== section) . configSection) rows))),
          token <- configArguments row
      ]
    lastDefinition [] = []
    lastDefinition (row : more) =
      row : if configStartsSection row then [] else lastDefinition more
    scan [] = []
    scan ((row, token) : remaining)
      | token == "--" = []
      | token == "--file" || "--file=" `T.isPrefixOf` token =
          ConfigConflict file row True : scan (if token == "--file" then drop 1 remaining else remaining)
      | Just (option, value) <- shortValue token =
          [ConfigConflict file row True | option == 'f'] ++ scan (if T.null value then drop 1 remaining else remaining)
      | "@" `T.isPrefixOf` token = ConfigConflict file row False : scan remaining
      | token `elem` takesValue = scan (drop 1 remaining)
      | otherwise = scan remaining
    takesValue =
      [ "--alias", "--pivot", "--begin", "--end", "--period", "--depth",
        "--output-file", "--output-format", "--rules", "--rules-file",
        "--today", "--value", "--conf"
      ]
    -- hledger accepts bundles such as -If. The first short option that takes
    -- a value consumes the rest of the token, or the following token.
    shortValue token
      | "--" `T.isPrefixOf` token = Nothing
      | otherwise = T.stripPrefix "-" token >>= T.uncons . T.dropWhile (`notElem` ("fbepoOXcmw" :: String))

firstConfigConflict :: ConfigUse -> [ConfigConflict] -> Maybe ConfigConflict
firstConfigConflict use = find (\(ConfigConflict _ row _) -> configSection row `elem` relevant)
  where
    relevant = "general" : case use of
      ImportConfig -> ["print"]
      ReportConfig -> ["balancesheet", "incomestatement", "balance", "print", "accounts"]
      BalanceConfig -> ["balance"]

readConfigConflicts :: HledgerInfo -> HledgerConf -> IO [ConfigConflict]
readConfigConflicts info conf = do
  let file = hledgerConfPath conf
      indented = case TR.decimal (hledgerVersionNumber (hlVersion info)) of
        Right (major, rest)
          | Just suffix <- T.stripPrefix "." rest, Right (minor, _) <- TR.decimal suffix ->
              (major, minor) >= ((1, 99) :: (Int, Int))
        _ -> True
      splitWords = if indented then configWords else legacyConfigWords
  bytes <- B.readFile file
  case TE.decodeUtf8' bytes of
    Left err -> ioError (userError ("Cannot read hledger config " ++ file ++ ": " ++ show err))
    Right contents -> return (configConflicts file
      (parseConfigLinesWith indented splitWords (T.lines (T.dropWhile (== '\xFEFF') contents))))

findConfigConflict :: HledgerInfo -> ConfigUse -> Maybe HledgerConf -> IO (Maybe ConfigConflict)
findConfigConflict info use = fmap (firstConfigConflict use) . findConfigConflicts info

findConfigConflicts :: HledgerInfo -> Maybe HledgerConf -> IO [ConfigConflict]
findConfigConflicts info = maybe (pure []) (readConfigConflicts info)

renderConfigConflict :: ConfigConflict -> T.Text
renderConfigConflict (ConfigConflict file row addsJournal) =
  T.pack file <> " line " <> T.pack (show (configLineNumber row)) <> " [" <> configSection row <> "] "
    <> (if addsJournal then "adds a journal: " else "reads arguments from another file: ")
    <> configSource row <> "\n"
    <> (if addsJournal then "hledger-flow picks its own journals, so this would mix another journal into the ledger."
        else "hledger-flow cannot check which journals that argument file would add.")
    <> "\nRemove that line, or pass --hledger-conf with a config that doesn't have it."

requireSafeConfig :: ConfigUse -> RT.RuntimeOptions -> IO ()
requireSafeConfig use opts = case RT.hledgerConf opts of
  Nothing -> return ()
  conf -> do
    info <- RT.hledgerInfo opts
    conflict <- findConfigConflict info use conf
    maybe (return ()) (ioError . userError . T.unpack . renderConfigConflict) conflict

-- Config arguments can quote spaces or a #; neither quotes nor comments are
-- arguments. Leave malformed quoting for hledger to diagnose.
configWords :: T.Text -> [T.Text]
configWords = map T.pack . wordsOutside Nothing Nothing . T.unpack
  where
    wordsOutside _ word [] = map reverse (maybeToList word)
    wordsOutside quote word (c:cs)
      | Just c == quote = wordsOutside Nothing word cs
      | quote == Nothing, c `elem` ['\'', '"'] = wordsOutside (Just c) (Just (maybe "" id word)) cs
      | quote == Nothing, c == '#' = map reverse (maybeToList word)
      | quote == Nothing, isSpace c =
          map reverse (maybeToList word) ++ wordsOutside Nothing Nothing cs
      | otherwise = wordsOutside quote (Just (c : maybe "" id word)) cs

-- Older hledger config files start comments at any # and only interpret a
-- quote at the start of a word. Quotes within an unquoted word stay literal.
legacyConfigWords :: T.Text -> [T.Text]
legacyConfigWords = go . T.stripStart . T.takeWhile (/= '#')
  where
    go input = case T.uncons input of
      Nothing -> []
      Just (quote, contents) | quote == '\'' || quote == '"' ->
        let (word, end) = T.break (== quote) contents
         in word : case T.uncons (T.drop 1 end) of
              Just (next, _) | isSpace next -> go (T.stripStart (T.drop 1 end))
              _ -> []
      _ -> let (word, rest) = T.break isSpace input in word : go (T.stripStart rest)
