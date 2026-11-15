{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Hledger.Flow.Import.Incremental
  ( IncrementalRun, withIncrementalRun, importStatement ) where

import Control.Concurrent.STM (TChan)
import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hledger.Flow.Common (directivesFile, writeStagedText)
import Hledger.Flow.DocHelpers
import Hledger.Flow.Import.Cache
import Hledger.Flow.Import.Conversion
import Hledger.Flow.Import.Dependencies
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.Inventory (generatedOutput)
import Hledger.Flow.Import.Snapshot
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, withStagedOutput)
import Hledger.Flow.RuntimeOptions
import Hledger.Flow.Types (LogMessage)
import qualified Path as P
import qualified Path.IO as P
import qualified System.Directory as D
import System.IO.Error (tryIOError)
import qualified System.Info as Sys

data IncrementalRun = IncrementalRun SnapshotRun (IO (ReuseDecision Converter)) (IORef (Map.Map AbsFile (Set.Set T.Text)))

withIncrementalRun :: RuntimeOptions -> TChan LogMessage -> Int -> (IncrementalRun -> IO a) -> IO a
withIncrementalRun opts ch count action = withSnapshotRun (baseDir opts) $ \snapshots -> do
  reasons <- newIORef Map.empty
  probe <- newConverterProbe opts
  when (count > 0) $ do
    initialized <- tryIOError $ initializeCache (baseDir opts)
    either (channelErrLn ch . ("Could not initialize incremental cache: " <>) . T.pack . show) pure initialized
  finally (action (IncrementalRun snapshots probe reasons)) $ do
    warnings <- readIORef reasons
    unless (Map.null warnings) $ do
      let scope | Map.size warnings == count = "all selected statements"
                | otherwise = T.pack (show (Map.size warnings)) <> " of " <> T.pack (show count) <> " selected statements"
      channelErrLn ch $ "Incremental reuse unavailable for " <> scope <> ": "
        <> T.intercalate ", " (Set.toAscList (Set.unions (Map.elems warnings))) <> ". See " <> docURL IncrementalImportFallback

data HookInputs = HookInputs AbsFile FileObservation AbsFile FileObservation
  deriving (Eq, Show)

observeHook :: AbsFile -> AbsFile -> IO (ReuseDecision HookInputs)
observeHook script source = checkedIO "could not observe hook inputs" (P.toFilePath script) $ do
  runnable <- if Sys.os == "mingw32" then pure True else D.executable <$> D.getPermissions (P.toFilePath script)
  if runnable
    then Reusable <$> (HookInputs script <$> observeFile script <*> pure source <*> observeFile source)
    else pure $ atPath "hook is not executable" (P.toFilePath script) ""

hookInputsKey :: ImportDirs P.Abs -> FilePath -> T.Text -> HookInputs -> StageInputs
hookInputsKey dirs cwd stage (HookInputs script s input i) =
  [("stage",stage),("working directory",T.pack cwd),("hook",pathText script),
   ("hook digest",digestHex (observationDigest s)),("input",pathText input),
   ("input digest",digestHex (observationDigest i))] ++ map ("argument",) (hookDirArgs dirs)

pathText :: AbsFile -> T.Text
pathText = T.pack . P.toFilePath

data JournalInputs = JournalInputs
  { engine :: Converter
  , copiedInputs :: SnapshotInput
  , recordedInputs :: StageInputs
  , copiedDigests :: Map.Map AbsFile Digest
  , liveConstruct :: Maybe HookInputs
  } deriving (Eq, Show)

importStatement :: IncrementalRun -> RuntimeOptions -> TChan LogMessage -> ImportDirs P.Abs -> AbsFile -> IO (AbsFile,Bool,Bool)
importStatement (IncrementalRun snapshots probe reasons) opts ch dirs source = do
  cwd <- D.getCurrentDirectory
  record <- recordPath (baseDir opts) source
  old <- readRecord record
  intermediate <- generatedOutput [P.reldir|2-preprocessed|] ".csv" source
  let hook name = findHook opts ch name (accountDir dirs P.</> (if name == "preprocess" then [P.relfile|preprocess|] else [P.relfile|construct|]))
      report problem = do
        let reason = reasonScope problem
            detail = reasonDetail problem
        atomicModifyIORef' reasons $ \m -> (Map.insertWith Set.union source (Set.singleton reason) m,())
        logVerbose opts ch $ reason <> " " <> detail
      changed = FallbackReason "dependency changed during conversion" (pathText source)
      trusted = importPolicy opts == TrustScripts
  pre <- hook "preprocess"
  (csv,preRecord,preRan) <- case pre of
    Nothing -> do
      input <- savedInput opts ch source intermediate
      pure (input,Nothing,False)
    Just script -> do
      before <- observeHook script source
      let key = hookInputsKey dirs cwd "preprocess" <$> before
          prior = old >>= preprocessRecord
      hit <- case key of
        Reusable k | trusted -> stageMatches k intermediate prior
        _ -> pure False
      if hit then pure (intermediate,prior,False) else do
        previous <- digestIfPresent intermediate
        digest <- installOutput True intermediate (preprocess opts ch script dirs source intermediate)
        when (Just digest == previous) $ logVerbose opts ch "Preprocessing ran; the CSV is unchanged."
        after <- observeHook script source
        selected <- hook "preprocess"
        usable <- case key of
          Fallback reason -> report reason >> pure Nothing
          Reusable k | selected == pre && after == before -> pure (Just (StageRecord k digest))
                     | otherwise -> report changed >> pure Nothing
        pure (intermediate,usable,True)
  output <- generatedOutput [P.reldir|3-journal|] ".journal" csv
  let discover = do
        candidate <- hook "construct"
        context <- probe
        case context of
          Fallback r -> pure (Fallback r)
          Reusable converter -> checkedIO "could not verify journal inputs" (P.toFilePath csv) $ do
            config <- discoverConfig (converterRelease converter) (hledgerConf opts)
            case config of
              Fallback r -> pure (Fallback r)
              Reusable conf -> do
                let common = [("source",pathText source),("effective CSV",pathText csv),("preprocess",maybe "" pathText pre)]
                      ++ map ("converter",) (converterKey converter)
                    mk inputs files extra construct =
                      JournalInputs converter inputs (common ++ extra ++ [(pathText p,digestHex d) | (p,d) <- files]) (Map.fromList files) construct
                case candidate of
                  Just script -> do
                    observation <- observeHook script csv
                    pure $ (\h -> mk (ConstructInput (fst <$> conf)) (maybeToList conf)
                                        (hookInputsKey dirs cwd "construct" h) (Just h)) <$> observation
                  Nothing | not (isSeparatedValues csv) -> pure $ atPath "unsupported input reader" (P.toFilePath csv) ""
                  Nothing -> do
                    rules <- selectRules csv dirs
                    case rules of
                      Fallback r -> pure (Fallback r)
                      Reusable Nothing -> pure $ atPath "missing rules file" (P.toFilePath csv) ""
                      Reusable (Just rf) -> do
                        tree <- discoverRules (baseDir opts) (converterRelease converter) rf
                        directives <- discoverDirectives (directivesFile opts)
                        digest <- digestFile csv
                        pure $ do
                          rulesTree <- tree
                          ds <- directives
                          let files = (csv,digest) : rulesTree ++ maybeToList ds ++ maybeToList conf
                          pure $ mk (RulesInput csv rf (fst <$> ds) (fst <$> conf)) files [("rules",pathText rf)] Nothing
      ordinary = do
        script <- hook "construct"
        case script of
          Nothing -> hledgerImport opts ch dirs csv output
          Just s -> withStagedOutput output (customConstruct opts ch s dirs csv output)
      prior = old >>= journalRecord
      fallback reason = do
        report reason
        case reason of
          CyclicRules _ file -> ioError $ userError ("Cyclic CSV rules include at " ++ T.unpack file)
          _ -> ordinary >> pure (Nothing,True)
  rawDecision <- discover
  let agrees inputs = case preRecord of
        Nothing -> True
        Just entry -> Just (stageOutput entry) == case liveConstruct inputs of
          Nothing -> Map.lookup csv (copiedDigests inputs)
          Just (HookInputs _ _ _ observed) -> Just (observationDigest observed)
      decision = rawDecision >>= \inputs -> if agrees inputs then pure inputs else Fallback changed
  (journal,generated) <- case decision of
    Fallback r -> fallback r
    Reusable inputs -> do
      hit <- if trusted || liveConstruct inputs == Nothing
               then stageMatches (recordedInputs inputs) output prior else pure False
      if hit then logVerbose opts ch "Reused the verified journal." >> pure (prior,False) else do
        result <- withSnapshot snapshots (converterRelease (engine inputs)) (copiedInputs inputs) $ \snapshot -> do
          let pin = maybe ["--no-conf"] (\f -> ["--conf",pathText f])
              translate = translateDiagnostic snapshot
              actualOpts = opts {hledgerExecutable = converterExecutable (engine inputs)}
          outDigest <- case (copiedInputs inputs,snapshotInputs snapshot,liveConstruct inputs) of
            (RulesInput src rf ds _,RulesInput src' rf' ds' conf,_) -> do
              contents <- convertRules actualOpts ch (RulesPaths src rf ds) (RulesPaths src' rf' ds') (pin conf) translate
              target <- D.canonicalizePath (P.toFilePath output) >>= P.parseAbsFile
              installOutput False target (writeStagedText target contents)
            (ConstructInput _,ConstructInput conf,Just (HookInputs s _ _ _)) ->
              installOutput False output (constructWith actualOpts ch (pin conf) translate s dirs csv output)
            _ -> ioError (userError "Inconsistent incremental conversion inputs")
          again <- discover
          preNow <- hook "preprocess"
          effective <- case preNow of
            Nothing -> P.doesFileExist intermediate >>= \exists -> pure (if exists then intermediate else source)
            Just _ -> pure intermediate
          let stable = again == Reusable inputs && snapshotManifest snapshot == copiedDigests inputs && preNow == pre && effective == csv
          if stable then pure $ Just (StageRecord (recordedInputs inputs) outDigest)
                    else report changed >> pure Nothing
        case result of
          Fallback r -> fallback r
          Reusable entry -> pure (entry,True)
  -- A concurrent writer can replace the intermediate after the hook returned.
  -- A later run must verify its bytes before trusting the preprocessing entry.
  currentPre <- case preRecord of
    Just entry -> do
      actual <- digestIfPresent intermediate
      pure $ if actual == Just (stageOutput entry) then Just entry else Nothing
    Nothing -> pure Nothing
  let next = StatementRecord currentPre journal
  when (Just next /= old && (old /= Nothing || next /= StatementRecord Nothing Nothing)) $ do
    saved <- tryIOError $ saveRecord record next
    either (channelErrLn ch . ("Could not save incremental import record: " <>) . T.pack . show) pure saved
  pure (output,generated,preRan || generated)
