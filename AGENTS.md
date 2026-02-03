# AGENTS.md - hledger-flow

**Project summary**
- `hledger-flow` is a Haskell CLI that orchestrates an hledger-based workflow for importing electronic statements (mostly CSV) into generated journals and then producing reports.
- Most accounting logic comes from upstream `hledger`; `hledger-flow` focuses on directory conventions, preprocessing/construct hooks, include-file generation, and report automation.

**Primary docs**
- `README.org` for project overview and status.
- `docs/README.org` for the feature reference and directory layout details.
- `step-by-step/README.org` for a tutorial-style walkthrough.
- `CONTRIBUTING.org` for build/test instructions and contributor workflow.

**Core commands**
- Build + test (from `CONTRIBUTING.org`):
  - `./bin/build-and-test` (runs `stack test` then `stack install`).
- Run locally without installing:
  - `stack exec hledger-flow -- import [DIR]`
  - `stack exec hledger-flow -- report [BASEDIR]`
- Show CLI help:
  - `stack exec hledger-flow -- --help`
- Requirements:
  - `stack` is required for building and tests.
  - A recent `hledger` binary must be available on `PATH`, or pass `--hledger-path`.
  - Submodules are required for full docs/examples:
    - `git submodule init`
    - `git submodule update`

**Runtime expectations (base dir and layout)**
- Base directory is detected by walking upward until an `import/` directory is found. You can also pass a directory explicitly on the `import` and `report` subcommands.
- Expected input layout (examples):
  - `import/<owner>/<bank>/<account>/1-in/<year>/<statement>.csv`
  - Output from preprocessing goes to `2-preprocessed/`.
  - Output journals go to `3-journal/`.
- Include files are generated per year at multiple levels, plus top-level `import/all-years.journal`. The recommended `LEDGER_FILE` is `all-years.journal` at the base directory.
- Optional files:
  - Rules files at `import/<bank>.rules` or `import/<owner>/<bank>/<account>/<bank>-<account>.rules`, plus statement-specific overrides like `*_rfo-<name>.csv` that look for `rfo-<name>.rules`.
  - `preprocess` script at the account directory, invoked with input path, output path, bank, account, owner.
  - `construct` script at the account directory, invoked with input path, `-` (stdout), bank, account, owner.
  - Manual journals via `import/<owner>/_manual_/<year>/{pre-import,post-import}.journal`.
  - `directives.journal` at base dir (auto-included in `all-years.journal`).
  - Price files under `prices/<year>/prices.journal` (auto-included in year includes).

**Repo layout (high-signal paths)**
- `app/Main.hs` and `app/Parsing.hs` define the CLI, subcommands, and argument parsing.
- `src/Hledger/Flow/Import/` implements the import pipeline, include generation, and preprocess/construct hooks.
- `src/Hledger/Flow/Reports.hs` generates reports using hledger and writes them to `reports/`.
- `src/Hledger/Flow/BaseDir.hs` handles base-directory discovery and run-dir scoping.
- `src/Hledger/Flow/Common.hs` and `src/Hledger/Flow/Logging.hs` provide shared utilities and logging.
- `test/Spec.hs` is the HUnit test entry point; tests live under `test/`.
- `bin/` contains helper scripts for build/test and releases.

**Implementation notes and conventions**
- Paths are handled using `path` + `turtle` with helpers in `Hledger.Flow.PathHelpers`. Prefer those helpers for new filesystem code.
- `RuntimeOptions` is the primary config type; new CLI flags typically require adding fields to `RuntimeOptions` and wiring them through `app/Main.hs`.
- Import flow is orchestrated in `Hledger.Flow.Import.CSVImport`:
  - Find input files.
  - Run `preprocess` script if present.
  - Run `construct` script if present, otherwise call `hledger` with rules.
  - Write include files.
- Report flow is orchestrated in `Hledger.Flow.Reports` and depends on `LEDGER_FILE` (defaults to `all-years.journal` if not set).

**Testing**
- `stack test` (or `./bin/build-and-test`) runs HUnit tests.
- Tests create temporary directories under `test/tmp` and do not require network access.

**Recommended tooling (not yet enforced)**
- `hlint` for linting (suggested).
- `fourmolu` or `ormolu` for formatting (pick one and document when adopted).

**Release scripts (maintainers only)**
- `bin/release-tarball`, `bin/tag-release`, `bin/show-latest-release-info`, etc. are used for release tasks.
