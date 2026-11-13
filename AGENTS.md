# AGENTS.md - hledger-flow

**Project summary**
- `hledger-flow` is a Haskell CLI that orchestrates an hledger-based workflow for importing electronic statements (mostly CSV) into generated journals and then producing reports.
- Most accounting logic comes from upstream `hledger`; `hledger-flow` focuses on directory conventions, preprocessing/construct hooks, include-file generation, and report automation.

**Primary docs**
- The project site, https://hledger-flow.github.io/, built from `site/` (Astro Starlight). It holds the landing page, tutorial, reference and guides.
- `QUICKSTART.md` is the quick start. It ships in every release archive, and `site/scripts/sync-repo-docs.mjs` copies it into the site at build time.
- `README.md` is the GitHub and Hackage front page: overview, install summary and links to the site.
- `CONTRIBUTING.md` for build/test instructions and contributor workflow.
- `docs/README.md` and `step-by-step/` are stubs that keep old URLs working. Don't add content there.

**Keeping the docs in step with the code**
- Every user-visible change updates the site in the same commit as the code: the reference page, `QUICKSTART.md`, the tutorial or the landing page (`site/src/pages/index.astro`), whichever describes the changed behaviour.
- Doc links printed by the CLI go through `DocLink` in `src/Hledger/Flow/DocHelpers.hs`. Each constructor's slug is a permanent `https://hledger-flow.github.io/go/<slug>/` URL. `site/go-links.json` maps each slug to its current page and section.
  - When a message should point at docs, link the exact section: add a `DocLink` constructor and a `go-links.json` entry. Never hard-code a docs URL.
  - When you move or rename a page or heading, update its targets in `go-links.json`.
  - Never remove or rename a slug. Released binaries keep printing it.
- Within the site, link pages with root-relative paths such as `/reference/rules-files/#stability`. Use full `https://hledger-flow.github.io/...` URLs in `QUICKSTART.md`, `README.md` and the ChangeLog.
- The landing page's examples came from a real run of hledger-flow and hledger. If the output format changes, rerun the example and update them.
- The tutorial's code blocks come from a real run. Never edit its output by hand: change `site/scripts/tutorial/` (each part's commands and the files the reader saves) and run `site/scripts/tutorial/update.sh`, with `HLEDGER` set to the version in `site/scripts/tutorial/hledger-version`. CI runs it with `--check`. See `CONTRIBUTING.md`, section "Update the tutorial".
- Example account names follow hledger's lowercase style everywhere: `assets:mybank:checking`, `expenses:bank-fees`, `equity:opening/closing balances`.
- `cd site && npm run check` builds the site and fails on broken internal links, missing anchors and `/go/` links that don't resolve. HUnit fails if a `DocLink` has no `go-links.json` entry.
- `cd site && npm run dev` serves the site locally on port 4321.

**Core commands**
- Build + test (from `CONTRIBUTING.md`):
  - `./bin/build-and-test` (HUnit, site checks, platform CLI scenarios, offline release checks, then installation; requires Stack, hledger, Git, jq, Python 3 and Node.js with npm).
- Run locally without installing:
  - `stack exec hledger-flow -- import [DIR]`
  - `stack exec hledger-flow -- report [BASEDIR]`
- Show CLI help:
  - `stack exec hledger-flow -- --help`
- Requirements:
  - `stack` is required for building and tests.
  - A recent `hledger` binary must be available on `PATH`, or pass `--hledger-path`.
  
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
- `site/` is the project site. Docs pages live in `site/src/content/docs/`. `.github/workflows/site.yaml` checks the site on every push and publishes it when a release is published.

**Implementation notes and conventions**
- Follow the writing guidelines at the top of [ChangeLog.md](ChangeLog.md?plain=1).
- When describing changes across platforms, list Linux first, then macOS, then Windows. Use the same order for subsets, table rows and columns, and corresponding figures. This applies to documentation, release notes, benchmark reports, and replies.
- Paths are handled using `path` and `path-io` with helpers in `Hledger.Flow.PathHelpers`. Prefer those helpers for new filesystem code.
- `RuntimeOptions` is the primary config type; new CLI flags typically require adding fields to `RuntimeOptions` and wiring them through `app/Main.hs`.
- Import flow is orchestrated in `Hledger.Flow.Import.CSVImport`:
  - Find input files.
  - Run `preprocess` script if present.
  - Run `construct` script if present, otherwise call `hledger` with rules.
  - Write include files.
- Report flow is orchestrated in `Hledger.Flow.Reports` and depends on `LEDGER_FILE` (defaults to `all-years.journal` if not set).

**Testing**
- `stack test` runs HUnit tests. `./bin/build-and-test` runs the full suite from Bash (Git Bash on Windows).
- HUnit covers active inventory, filesystem, include rendering and concurrency behavior; shelltestrunner covers the CLI with real hledger and a 120-second scenario timeout.
- Tests create temporary directories under `test/tmp` and do not require network access.

**Recommended tooling (not yet enforced)**
- `hlint` for linting (suggested).
- `fourmolu` or `ormolu` for formatting (pick one and document when adopted).

**Releases (maintainers only)**
- Follow `release-procedure.md`. Every step is a `bin/release` command; commands never prompt, and their errors name the fix.
- Stop at each **Review** point in the procedure and wait for the maintainer's approval before continuing: the ChangeLog wording and the proposed version number before `bump`, the local tag before `push-tag`, and both the GitHub draft and the Hackage candidate before `publish` (or `hackage-publish` for recovery).
- Run the permanent commands (`push-tag`, `hackage-publish`, `publish`) only when the maintainer explicitly asks.
- `ci` and `push-tag` wait for GitHub Actions runs that can take over 30 minutes. Run them in the background; re-running either one resumes waiting.
