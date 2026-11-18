---
title: Incremental import
description: Reuse journals when the statement and its conversion inputs have the same content.
---

Run `hledger-flow import --incremental` to import new statements and rebuild journals whose inputs have changed. The first run builds a record for each eligible statement. Subsequent runs can reuse those journals.

Suppose you have twelve monthly statements. Adding April's download for the next year imports one statement. Correcting March rebuilds March, even if the bank preserved the download's timestamp. Touching March's file or downloading the same bytes again permits reuse. Editing a category in an included rules file rebuilds every selected statement that uses it. Adding or deleting an account-specific rules file can also change which rules apply.

## Choose a mode

| Command | When a statement is processed |
| --- | --- |
| `hledger-flow import` | Regenerate all selected statements. |
| `hledger-flow import --new-files-only` | Compare timestamps at each stage. [Timestamp-only imports](/reference/input-files/#timestamp-only-imports) describes recovery and limitations. |
| `hledger-flow import --incremental` or `--incremental=run-scripts` | Always run each selected statement’s `preprocess` and `construct` scripts, even when the script and statement are unchanged. |
| `hledger-flow import --incremental=skip-unchanged-scripts` | Skip a script when its code, known inputs and saved output still match the previous run. |

Use an equals sign to select a mode. A separate argument after `--incremental` is the directory to import.

With no scripts, both incremental modes do the same work. Use `skip-unchanged-scripts` when each script depends only on its input and arguments, or clear the import cache after a file the script reads changes. For example, if a `preprocess` script reads a merchant lookup table that you edit regularly, plain `--incremental` picks up those edits on every run.

In `run-scripts` mode, if preprocessing strips a download header and produces the same CSV as last time, its timestamp is preserved. If the rules and other conversion inputs also match, hledger-flow reuses the journal. A `construct` script always runs its hledger conversion in this mode. If a running script updates its own input, hledger-flow leaves its result in place and runs the script again the next time. A subsequent timestamp-only import can rebuild once if the original is newer than the saved CSV.

The selected mode applies to every account in the import. Run from an account directory to limit its scope. Changing working directory can rerun scripts because their output may depend on that directory. When a script is skipped, changes to an imported Python module or a file included by its output can leave the journal stale. Checks made while scripts run can also miss a directory temporarily replaced and restored, or a same-byte rewrite on a filesystem with coarse timestamps.

## What gets checked

Both modes verify the generated output, the effective CSV, TSV or SSV, and the hledger arguments used for conversion. An upgrade that changes those arguments rebuilds the affected journals. Rules-based conversion follows nested rules includes and checks the effective config and applicable `directives.journal`.

Each incremental import hashes the hledger executable once while it remains unchanged. On macOS it uses the system SHA-256 implementation. Replacing hledger with different bytes triggers a rebuild even if the version, file size and modification time stay the same. For a small selection, a full import can still finish sooner. Incremental import supports **hledger 1.52.4 or later**, including development builds such as `1.99-g01a4b4866`. Package-manager and locally compiled builds use the same checks.

Incremental import hashes the whole active hledger config file. Editing it triggers a rebuild even when the changed setting applies to another command. Run `hledger-flow status` from the same directory to [find the active config](/reference/status/#hledger-config-file).

Config settings permit reuse by default, including static queries, formatting options, and unfamiliar options. hledger still validates the config during conversion. Settings under `[print]` also apply when hledger-flow converts statements, because conversion uses `hledger print`.

Only entries known to be non-deterministic prevent reuse in general settings and `[print]`, because their output can change while the tracked inputs stay the same:

- Relative dates in periods, `--begin`/`--end` and `date:` queries (also inside `expr:`), dates without a year, forecasts without a fixed end date, and valuation with `-V`, `-X`, `--value=end` or `--value=now` depend on today's date. Fixed dates and `--value=then` permit reuse. Setting `--today` to a fixed date also permits reuse, except with forecasts, because hledger ignores `--today` there.
- `--file`/`-f`, `--rules`, `--rules-file`, `--conf`, and `@FILE` can read files whose contents are not tracked.
- `--new` depends on saved import state.
- `--output-file`/`-o` writes a separate output that reusing a journal would not recreate.
- `--location`/`--locations` exposes paths to temporary input copies.

Settings in other command sections do not prevent reuse. The list above can grow as more dependencies are identified; unknown settings are assumed deterministic. If a setting depends on changing inputs that are not tracked, use an ordinary import to force conversion.

In `directives.journal` you can use single-line literal account declarations, commodity formats, prices with full numeric dates, and relative includes such as the ones below. Blank lines, comments, inline tags and commodity precisions are supported.

```journal
account assets:bank:current  ; type: C
commodity 1,000.00000000 BTC
P 2000-01-01 EUR €1
include accounts.journal
```

Paths are relative to the file containing the `include`. Incremental import checks the contents of each included file, including nested includes. Editing one rebuilds the selected statements that use it.

Use literal paths inside the ledger, with letters, numbers, spaces, dots, hyphens, underscores and forward slashes. Journal filenames can have a custom extension or no extension at all. A name such as `vat.csv` selects hledger's CSV reader and triggers ordinary conversion. If the file contains journal syntax, `include journal:vat.csv` selects the journal reader and permits reuse.

Globs, absolute paths, paths outside the ledger, and symlinks that redirect an include trigger ordinary conversion. If an included file has an unsupported directive, run `hledger-flow --verbose import --incremental` to see the file and line.

## When reuse is unavailable

A notice on standard error lists each reason once and counts the affected statements. When a reason points to one file, the notice names it. Use `--verbose` for individual paths. For a non-deterministic config entry, the notice quotes the config file, line and entry. Verbose output also identifies a changed rules file, a missing journal, or the line and setting that prevented reuse. Full conversion still runs. Missing rules and non-executable scripts produce the ordinary import error. The displayed command uses the configured hledger path, including when it is a symlink.

For example, hledger versions differ in how they read trailing spaces in an `include` filename. With 1.52.4 and 1.99.6, hledger-flow follows the known behavior. On other eligible versions, a statement with that ambiguity goes through ordinary conversion each time. Statements using other, unambiguous rules can still reuse their journals. A shared ambiguous input affects every statement that uses it.

Common reasons include a hledger version below 1.52.4, an unreadable version, an unsupported input reader, a config setting with a known dependency outside the tracked inputs, a multiline or unknown directive, an unresolved rules include, and an absolute rules include or one outside the ledger. Use `--hledger-path` to select the real hledger executable if the configured path is a launcher. An unrecognized native executable that forwards to another program can hide changes to its target. Keep the installed converter stable during an import. Temporarily replacing its parent directory and restoring it during conversion can escape the executable identity checks.

An eligible rebuild uses private copies so the record describes the bytes hledger read. If hledger reports an error in a copied file, the diagnostic names the original file so you can open it at the reported line. Reuse can also be unavailable if private staging fails, a planned Windows snapshot path reaches 260 UTF-16 code units, or inputs change during conversion. A warm rules-based hit creates no snapshot.

If two include filenames map to the same snapshot file (for example, `A.rules` and `a.rules` in a case-insensitive cache), hledger-flow converts that statement using the originals.

On a shared ledger, other users can have read access to the ledger root while the temporary statement copies remain private. Give permission to rename or replace directories along the cache path only to people you trust with the statements. A user with that permission could redirect writes while an import is running.

## Clear the cache

The records are under `.hledger-flow/cache/import/` at the ledger root. Delete that directory to rebuild the records on your next incremental import. The cache creates its own Git ignore file. Ordinary and timestamp-only imports do not need cache records. If saving a record fails, the successful journal remains available and the import reports a warning.
