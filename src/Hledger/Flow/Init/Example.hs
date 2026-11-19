{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Init.Example
  ( exampleImportFiles,
    exampleBaseFiles,
    untouchedExample,
    rememberExampleImport,
    rememberExampleReports,
    writeExampleReport,
    cleanupExample,
  )
where

import Control.Monad (filterM, forM_, unless, void, when, zipWithM)
import Data.Aeson (decodeFileStrict', encode, encodeFile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Hledger.Flow.Common (includePreamble)
import Hledger.Flow.DocHelpers (DocLink (RulesFiles), docURL)
import Hledger.Flow.Import.Fingerprint (Digest, digestFile)
import Hledger.Flow.Import.IncludeFiles (toIncludeLine)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, pathOccupied, publishNewFile, withStagedOutput, writeNewUtf8File)
import Hledger.Flow.Types (BaseDir)
import Path
import qualified Path.IO as PathIO
import qualified System.FilePath as F
import System.IO (stderr)
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

exampleAccount :: RelDir
exampleAccount = [reldir|me/mybank/checking|]

sampleName :: RelFile
sampleName = [relfile|hledger-flow-example.csv|]

rulesName :: RelFile
rulesName = [relfile|mybank-checking.rules|]

sampleCsv :: Integer -> T.Text
sampleCsv year = T.unlines
  ["Date,Description,Amount", date <> ",Example purchase,-5.00", date <> ",Example refund,5.00"]
  where
    date = T.pack (show year) <> "-01-02"

sampleRules :: T.Text
sampleRules = T.unlines
  [ "# Rules for the example statement.",
    "# Docs: " <> docURL RulesFiles,
    "skip 1",
    "fields date, description, amount",
    "currency USD",
    "account1 assets:mybank:checking",
    "account2 expenses:unknown",
    "",
    "if Example",
    " account2 expenses:example"
  ]

exampleImportFiles :: Integer -> [(RelFile, T.Text)]
exampleImportFiles year =
  (exampleMarker, encodeExampleState (year, Nothing, []))
    : (exampleAccount </> rulesName, sampleRules)
    : paths
      [ (toFilePath exampleAccount F.</> "1-in" F.</> show year F.</> toFilePath sampleName, sampleCsv year),
        ("_manual_" F.</> show year F.</> "pre-import.journal", manual "before"),
        ("_manual_" F.</> show year F.</> "post-import.journal", manual "after")
      ]
  where
    manual place = T.unlines
      [ "; Put transactions here that should appear " <> place <> " imported statements.",
        "; " <> T.pack (show year) <> "-01-01 Opening balance",
        ";     assets:mybank:checking  100 USD",
        ";     equity:opening balances"
      ]

exampleBaseFiles :: Integer -> [(RelFile, T.Text)]
exampleBaseFiles year = paths
  [ ("directives.journal", "; Ledger-wide directives go here.\n; commodity 1,000.00 USD\n; account assets:mybank:checking\n"),
    ("prices" F.</> show year F.</> "prices.journal",
      "; Add market prices here.\n; P " <> T.pack (show year) <> "-01-01 EUR 1.10 USD\n")
  ]

paths :: [(FilePath, a)] -> [(RelFile, a)]
paths = mapMaybe (\(p, a) -> fmap (\path -> (path, a)) (parseRelFile p))

untouchedExample :: BaseDir -> [AbsFile] -> IO Bool
untouchedExample base [source] = case stripProperPrefix account source of
  Just rel
    | ["1-in", yearName, name] <- F.splitDirectories (toFilePath rel),
      name == toFilePath sampleName,
      Just year <- readMaybe yearName,
      Just generated <- parseRelFile ("3-journal" F.</> yearName F.</> "hledger-flow-example.journal"),
      Just yearIndex <- parseRelFile (yearName ++ "-include.journal") -> do
        result <- tryIOError $ do
          let rules = account </> rulesName
              journal = account </> generated
              bank = parent account
              owner = parent bank
              indexes dir = [dir </> yearIndex, dir </> [relfile|all-years.journal|]]
              allowed = [source, rules, journal] ++ concatMap indexes [owner, bank, account]
              dirs = [bank, account, parent source, parent (parent source), parent journal, parent (parent journal)]
          cleanPath <- allM plainDir [importRoot base, owner]
          cleanContents <- if cleanPath then containsOnly dirs allowed owner else return False
          if cleanContents
            then (&&) <$> matches source (sampleCsv year) <*> matches rules sampleRules
            else return False
        return (either (const False) id result)
  _ -> return False
  where
    account = sampleAccountDir base
    matches file expected = (== T.encodeUtf8 expected) <$> B.readFile (toFilePath file)
untouchedExample _ _ = return False

containsOnly :: [AbsDir] -> [AbsFile] -> AbsDir -> IO Bool
containsOnly allowedDirs allowedFiles dir = do
  (dirs, files) <- PathIO.listDir dir
  if all (`elem` allowedDirs) dirs && all (`elem` allowedFiles) files
    then do
      hasLinks <- (||) <$> (or <$> traverse PathIO.isSymlink dirs) <*> (or <$> traverse PathIO.isSymlink files)
      if hasLinks then return False else and <$> traverse (containsOnly allowedDirs allowedFiles) dirs
    else return False

-- The marker is staged by init with the import tree and expires when the
-- sample is removed. Checksums let cleanup distinguish its outputs from edits.
type ExampleState = (Integer, Maybe Digest, [(RelFile, Digest)])

encodeExampleState :: ExampleState -> T.Text
encodeExampleState = T.decodeUtf8 . BL.toStrict . encode

exampleMarker :: RelFile
exampleMarker = [relfile|.hledger-flow-example.json|]

withExampleState :: BaseDir -> (AbsFile -> ExampleState -> IO ()) -> IO ()
withExampleState base action = void . tryIOError $ do
  let marker = importRoot base </> exampleMarker
  whenM (plainDir (importRoot base)) $ whenM (regularFile marker) $ do
    state <- decodeFileStrict' (toFilePath marker)
    forM_ state (action marker)

saveExampleState :: AbsFile -> ExampleState -> IO ()
saveExampleState marker state =
  withStagedOutput marker (\stage -> encodeFile (toFilePath stage) state)

importRoot :: BaseDir -> AbsDir
importRoot base = base </> [reldir|import|]

sampleAccountDir :: BaseDir -> AbsDir
sampleAccountDir base = importRoot base </> exampleAccount

sampleInputAndOutput :: BaseDir -> Integer -> IO (AbsFile, AbsFile)
sampleInputAndOutput base year = do
  yearDir <- parseRelDir (show year)
  let account = sampleAccountDir base
  return
    ( account </> [reldir|1-in|] </> yearDir </> sampleName,
      account </> [reldir|3-journal|] </> yearDir </> [relfile|hledger-flow-example.journal|]
    )

rememberExampleImport :: BaseDir -> [AbsFile] -> IO ()
rememberExampleImport base written = void . tryIOError $ do
  let root = importRoot base
      marker = root </> exampleMarker
  whenM (plainDir root) $ do
    marked <- pathOccupied marker
    if marked
      then withExampleState base $ \file (year, _, reports) -> do
        (_, journal) <- sampleInputAndOutput base year
        when (journal `elem` written) $ do
          digest <- importedExampleDigest year journal
          saveExampleState file (year, digest, reports)
      else whenM (plainDir (sampleAccountDir base)) $
        -- A fresh conversion can establish ownership for an earlier init.
        forM_ (take 1 (mapMaybe exampleYear written)) $ \(year, journal) -> do
          digest <- importedExampleDigest year journal
          forM_ digest $ \observed ->
            writeNewUtf8File marker (encodeExampleState (year, Just observed, []))
  where
    importedExampleDigest year journal = do
      (source, _) <- sampleInputAndOutput base year
      example <- untouchedExample base [source]
      isolated <- if example then directivesAreComments base else pure False
      if isolated then Just <$> digestFile journal else pure Nothing
    exampleYear journal = do
      relative <- stripProperPrefix (sampleAccountDir base </> [reldir|3-journal|]) journal
      case F.splitDirectories (toFilePath relative) of
        [year, "hledger-flow-example.journal"] -> fmap (\n -> (n, journal)) (readMaybe year)
        _ -> Nothing

rememberExampleReports :: BaseDir -> IO Bool -> [(AbsFile, Maybe Digest)] -> IO ()
rememberExampleReports base runStillCurrent written = withExampleState base $ \marker (year, digest, previous) -> do
  dir <- exampleReportYear base year
  let relativeWrites = [(file, hash) | (output, hash) <- written, Just file <- [stripProperPrefix dir output]]
      retained = filter ((`notElem` map fst relativeWrites) . fst) previous
  unless (null relativeWrites) $ do
    (source, journal) <- sampleInputAndOutput base year
    sampleOnly <- case digest of
      Nothing -> pure False
      Just expected -> do
        example <- untouchedExample base [source]
        if example
          then and <$> sequence
            [ (== expected) <$> digestFile journal,
              sampleIndexesUnchanged base year journal,
              directivesAreComments base
            ]
          else pure False
    eligible <- if sampleOnly then runStillCurrent else pure False
    let owned = [(file, hash) | eligible, (file, Just hash) <- relativeWrites]
    saveExampleState marker (year, digest, owned ++ retained)

-- Capture the actual generated bytes before publishing any report that can
-- belong to the example. The normal writer remains in use for other paths.
writeExampleReport :: BaseDir -> AbsFile -> T.Text -> IO (Maybe Digest)
writeExampleReport base file contents = do
  let root = base </> [reldir|reports|]
      owner = root </> [reldir|me|]
      marker = importRoot base </> exampleMarker
      write path = TIO.writeFile (toFilePath path) contents
  candidate <- if isProperPrefixOf owner file
    then do
      tagged <- regularFile marker
      occupied <- pathOccupied file
      replaceable <- if occupied then regularFile file else pure True
      return (tagged && replaceable)
    else pure False
  if candidate
    then Just <$> withStagedOutput file (\stage -> write stage >> digestFile stage)
    else write file >> pure Nothing

-- Owner reports read directives too. Any real directive or included journal
-- makes those reports ineligible for sample cleanup.
directivesAreComments :: BaseDir -> IO Bool
directivesAreComments base = do
  let file = base </> [relfile|directives.journal|]
  exists <- pathOccupied file
  if not exists
    then pure True
    else do
      regular <- regularFile file
      if regular then all comment . T.lines <$> TIO.readFile (toFilePath file) else pure False
  where
    comment line = let text = T.strip line in T.null text || ";" `T.isPrefixOf` text

sampleIndexesUnchanged :: BaseDir -> Integer -> AbsFile -> IO Bool
sampleIndexesUnchanged base year journal = do
  yearFile <- parseRelFile (show year ++ "-include.journal")
  let account = sampleAccountDir base
      bank = parent account
      owner = parent bank
      dirs = [owner, bank, account]
      yearPaths = map (</> yearFile) dirs
      allYears = map (</> [relfile|all-years.journal|]) dirs
      check file target = do
        actual <- TIO.readFile (toFilePath file)
        return (actual == includePreamble <> "\n" <> toIncludeLine (toFilePath (parent file)) (toFilePath target) <> "\n")
  and <$> zipWithM check (allYears ++ yearPaths) (yearPaths ++ drop 1 yearPaths ++ [journal])

cleanupExample :: BaseDir -> IO ()
cleanupExample base = withExampleState base $ \marker (year, _, reports) -> do
  let account = sampleAccountDir base
      bank = parent account
      owner = parent bank
  exists <- pathOccupied account
  unless exists $ do
    whenM (plainDir owner) $ do
      pruneEmptyExampleDir year bank
      pruneEmptyExampleDir year owner
    ownerExists <- pathOccupied owner
    unless ownerExists $ do
      dir <- exampleReportYear base year
      removed <- filterM (removeExampleReport base dir) reports
      unless (null removed) $
        whenM (allM plainDir [base </> [reldir|reports|], parent dir]) $ do
          whenM (plainDir dir) $ do
            let monthly = [reldir|monthly|]
            when (any ((== monthly) . parent . fst) removed) (removeEmptyDir (dir </> monthly))
            removeEmptyDir dir
          removeEmptyDir (parent dir)
    PathIO.removeFile marker

-- Move first, then verify again. A file replaced after the first checksum
-- stays recoverable even if a new file has appeared at its original path.
removeExampleReport :: BaseDir -> AbsDir -> (RelFile, Digest) -> IO Bool
removeExampleReport base dir (name, expected) = do
  safe <- safeExampleReport base dir name
  let file = dir </> name
  matches <- if safe then (== expected) <$> digestFile file else pure False
  if not matches then pure False else do
    let root = base </> [reldir|reports|]
    recovery <- PathIO.createTempDir root ".example-cleanup-"
    let moved = recovery </> filename file
    PathIO.renameFile file moved
    regular <- regularFile moved
    stillMatches <- if regular then (== expected) <$> digestFile moved else pure False
    if stillMatches
      then do
        PathIO.removeFile moved
        removeEmptyDir recovery
        return True
      else do
        restored <- if regular then tryIOError (publishNewFile moved file) else pure (Left (userError "not a regular file"))
        case restored of
          Right () -> do
            whenM (regularFile moved) (PathIO.removeFile moved)
            removeEmptyDir recovery
          Left _ -> TIO.hPutStrLn stderr ("A report changed during example cleanup. Its previous entry is in " <> T.pack (toFilePath moved))
        return False

pruneEmptyExampleDir :: Integer -> AbsDir -> IO ()
pruneEmptyExampleDir year dir = do
  safe <- plainDir dir
  when safe $ do
    (dirs, files) <- PathIO.listDir dir
    yearFile <- parseRelFile (show year ++ "-include.journal")
    let expected = map (dir </>) [[relfile|all-years.journal|], yearFile]
        blankIndex file = do
          safeFile <- regularFile file
          if safeFile then (== includePreamble <> "\n") <$> TIO.readFile (toFilePath file) else pure False
    when (null dirs && all (`elem` expected) files) $ do
      blank <- allM blankIndex files
      when blank $ do
        mapM_ PathIO.removeFile files
        removeEmptyDir dir

exampleReportYear :: BaseDir -> Integer -> IO AbsDir
exampleReportYear base year = ((base </> [reldir|reports/me|]) </>) <$> parseRelDir (show year)

safeExampleReport :: BaseDir -> AbsDir -> RelFile -> IO Bool
safeExampleReport base dir name = do
  let file = dir </> name
      allowed = [[relfile|accounts.txt|], [relfile|balance-sheet.txt|], [relfile|income-expenses.txt|],
                 [relfile|monthly/income-expenses.txt|], [relfile|unknown-transactions.txt|]]
  if name `elem` allowed
    then allM plainDir [base </> [reldir|reports|], parent dir, dir, parent file] >>= \safe ->
      if safe then regularFile file else pure False
    else pure False

plainDir :: AbsDir -> IO Bool
plainDir dir = do
  exists <- PathIO.doesDirExist dir
  if exists then not <$> PathIO.isSymlink dir else pure False

regularFile :: AbsFile -> IO Bool
regularFile file = do
  exists <- PathIO.doesFileExist file
  if exists then not <$> PathIO.isSymlink file else pure False

removeEmptyDir :: AbsDir -> IO ()
removeEmptyDir dir = whenM (plainDir dir) (void (tryIOError (PathIO.removeDir dir)))

whenM :: IO Bool -> IO () -> IO ()
whenM check action = check >>= \ok -> when ok action

allM :: (a -> IO Bool) -> [a] -> IO Bool
allM check = fmap and . traverse check
