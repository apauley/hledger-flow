# Add mtime-based regeneration to --new-files-only flag

## Overview
- Enhance the `--new-files-only` flag to compare modification times between source and target files
- Currently the flag only checks if target exists; this improvement reprocesses when source is newer than target
- Enables incremental rebuilds when source files are updated without requiring full reimport

## Context (from discovery)
- Files involved:
  - `src/Hledger/Flow/Common.hs` - add helper function
  - `src/Hledger/Flow/Import/CSVImport.hs` - integrate helper at two points
  - `app/Main.hs` - update help text
- Related patterns: `verboseTestFile` exists in Common.hs for file existence checks
- Dependencies: `Turtle.modtime` returns `UTCTime`, directly comparable with `>`

## Development Approach
- **Testing approach**: Regular (code first, then tests)
- Complete each task fully before moving to the next
- Make small, focused changes
- **CRITICAL: every task MUST include new/updated tests** for code changes in that task
- **CRITICAL: all tests must pass before starting next task**
- Run tests after each change with `stack test`
- Maintain backward compatibility

## Testing Strategy
- **Unit tests**: Test `needsRegeneration` helper function
- Test cases:
  - Target doesn't exist → returns True
  - Target exists, source newer → returns True
  - Target exists, target newer → returns False
  - Target exists, same mtime → returns False

## Progress Tracking
- Mark completed items with `[x]` immediately when done
- Add newly discovered tasks with ➕ prefix
- Document issues/blockers with ⚠️ prefix

## Implementation Steps

### Task 1: Add needsRegeneration helper function with tests

**Files:**
- Modify: `src/Hledger/Flow/Common.hs`
- Modify: `test/Common/Integration.hs`

- [x] Add `needsRegeneration :: TurtlePath -> TurtlePath -> IO Bool` function to Common.hs
- [x] Implement logic: return True if target doesn't exist OR source mtime > target mtime
- [x] Use `Turtle.testfile` for existence and `Turtle.modtime` for mtime comparison
- [x] Export the function from the module
- [x] Add `testNeedsRegeneration` test in `test/Common/Integration.hs` following `testFirstExistingFile` pattern
- [x] Write test case: target doesn't exist → returns True
- [x] Write test case: target exists, source newer → returns True (use sleep or touch with delay)
- [x] Write test case: target exists, target newer → returns False
- [x] Add test to the `tests` list in Integration.hs
- [x] Run `stack test` - all tests must pass before next task

### Task 2: Integrate needsRegeneration in preprocessIfNeeded

**Files:**
- Modify: `src/Hledger/Flow/Import/CSVImport.hs`

- [x] Import `needsRegeneration` from Common module (if not already re-exported)
- [x] Replace `not targetExists` check with `needsRegeneration src csvOut` call
- [x] Update the `shouldProceed` logic to use the new helper
- [x] Run `stack test` - all tests must pass before next task

### Task 3: Integrate needsRegeneration in importCSV

**Files:**
- Modify: `src/Hledger/Flow/Import/CSVImport.hs`

- [x] Replace `not <$> verboseTestFile opts ch journalOut` with `needsRegeneration csvFile journalOut`
- [x] Verify the `preprocessHappened` check is preserved (if we just preprocessed, always import)
- [x] Run `stack test` - all tests must pass before next task

### Task 4: Update help text

**Files:**
- Modify: `app/Main.hs`

- [ ] Update `--new-files-only` help text from existence-based to mtime-based description
- [ ] New text: "Skip regenerating output files that are newer than their source. This applies to preprocessed files, hledger journal files, and construct script outputs."
- [ ] Run `stack test` - all tests must pass before next task

### Task 5: Verify acceptance criteria

- [ ] Verify `--new-files-only` skips files when target is newer than source
- [ ] Verify `--new-files-only` reprocesses when source is newer than target
- [ ] Verify preprocessing and import stages both respect mtime
- [ ] Run full test suite: `stack test`

### Task 6: [Final] Cleanup

- [ ] Update CLAUDE.md if new patterns discovered
- [ ] Move this plan to `docs/plans/completed/`

## Technical Details

**needsRegeneration function**:
```haskell
needsRegeneration :: TurtlePath -> TurtlePath -> IO Bool
needsRegeneration src target = do
  targetExists <- Turtle.testfile target
  if not targetExists
    then return True
    else do
      srcMtime <- Turtle.modtime src
      targetMtime <- Turtle.modtime target
      return (srcMtime > targetMtime)
```

**Integration in preprocessIfNeeded** (lines 112-114):
```haskell
shouldProceed <-
  if onlyNewFiles opts
    then do
      needsRegen <- needsRegeneration src csvOut
      return $ scriptExists && needsRegen
    else return scriptExists
```

**Integration in importCSV** (lines 85-88):
```haskell
shouldImport <-
  if onlyNewFiles opts && not preprocessHappened
    then needsRegeneration csvFile journalOut
    else return True
```

## Post-Completion

**Manual verification**:
- Test with actual statement files to verify mtime comparison works correctly
- Test touching a source file and re-running import with `--new-files-only`

**Edge cases to verify manually**:
- Rules file changes are NOT tracked (documented limitation)
- Construct/preprocess script changes are NOT tracked (documented limitation)
