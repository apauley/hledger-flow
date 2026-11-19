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
    configArguments :: [T.Text]
  }
  deriving (Eq, Show)

parseConfigLines :: Bool -> [T.Text] -> [ConfigLine]
parseConfigLines allowIndented = go "general" . zip [1 ..]
  where
    go _ [] = []
    go section ((n, raw) : more)
      | T.null body || "#" `T.isPrefixOf` body = go section more
      | allowIndented || raw == T.stripStart raw,
        Just start <- T.stripPrefix "[" body,
        (name, end) <- T.breakOn "]" start, not (T.null end) =
          let next = T.strip name
           in ConfigLine next n body (configWords (T.drop 1 end)) : go next more
      | otherwise = ConfigLine section n body (configWords body) : go section more
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
        | row <- rows, configSection row == section, token <- configArguments row
      ]
    scan [] = []
    scan ((row, token) : remaining)
      | token == "--" = []
      | token == "--file" || "--file=" `T.isPrefixOf` token || "-f" `T.isPrefixOf` token =
          ConfigConflict file row True : scan (if token `elem` ["--file", "-f"] then drop 1 remaining else remaining)
      | "@" `T.isPrefixOf` token = ConfigConflict file row False : scan remaining
      | token `elem` takesValue = scan (drop 1 remaining)
      | otherwise = scan remaining
    takesValue =
      [ "--alias", "--pivot", "-b", "--begin", "-e", "--end", "-p", "--period",
        "--depth", "-o", "--output-file", "-O", "--output-format", "--rules",
        "--rules-file", "--today", "--value", "--conf"
      ]

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
  bytes <- B.readFile file
  case TE.decodeUtf8' bytes of
    Left err -> ioError (userError ("Cannot read hledger config " ++ file ++ ": " ++ show err))
    Right contents -> return (configConflicts file
      (parseConfigLines indented (T.lines (T.dropWhile (== '\xFEFF') contents))))

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
configWords = map T.pack . wordsOutside Nothing "" . T.unpack
  where
    wordsOutside _ word [] = [reverse word | not (null word)]
    wordsOutside quote word (c:cs)
      | Just c == quote = wordsOutside Nothing word cs
      | quote == Nothing, c `elem` ['\'', '"'] = wordsOutside (Just c) word cs
      | quote == Nothing, c == '#' = [reverse word | not (null word)]
      | quote == Nothing, isSpace c =
          [reverse word | not (null word)] ++ wordsOutside Nothing "" cs
      | otherwise = wordsOutside quote (c:word) cs
