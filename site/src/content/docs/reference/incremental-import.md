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

With no scripts, both incremental modes do the same work. Use `skip-unchanged-scripts` when each script depends only on its input and arguments, or clear the import cache after a file the script reads changes. For example, if a `preprocess` script reads a merchant lookup table that you edit regularly, plain `--incremental` picks up those edits on every run.

In `run-scripts` mode, if preprocessing strips a download header and produces the same CSV as last time, its timestamp is preserved. If the rules and other conversion inputs also match, hledger-flow reuses the journal. A `construct` script always runs its hledger conversion in this mode. A subsequent timestamp-only import can rebuild once if the original is newer than the saved CSV.

The selected mode applies to every account in the import. Run from an account directory to limit its scope. Changing working directory can rerun scripts because their output may depend on that directory. When a script is skipped, changes to an imported Python module or a file included by its output can leave the journal stale. Checks made while scripts run can also miss a directory temporarily replaced and restored, or a same-byte rewrite on a filesystem with coarse timestamps.

## What gets checked

Both modes verify the generated output and the effective CSV, TSV or SSV. Rules-based conversion follows nested rules includes and checks the effective config and applicable `directives.journal`.

Each incremental import hashes the hledger executable once while it remains unchanged. On macOS it uses the system SHA-256 implementation. Replacing hledger with different bytes triggers a rebuild even if the version, file size and modification time stay the same. For a small selection, a full import can still finish sooner. Incremental import supports **hledger 1.52.4 or later**, including development builds such as `1.99-g01a4b4866`. Package-manager and locally compiled builds use the same checks.

A missing, empty or comment-only config permits reuse. Settings in recognized sections for other commands such as `[balance]` also permit reuse. The whole config is hashed, so editing one of those sections triggers one rebuild. In general settings and `[print]`, the following fixed layout permits reuse on versions that accept it.

```ini
[print]
--layout=hledger1
```

For `directives.journal`, the supported forms are single-line literal account declarations, commodity formats and prices with full numeric dates. Blank lines, comments, inline tags and commodity precisions are supported. For example:

```journal
account assets:bank:current  ; type: C
commodity 1,000.00000000 BTC
P 2000-01-01 EUR €1
```

## When reuse is unavailable

A notice on standard error names the reason and the affected scope. Use `--verbose` for statement, stage and file details. Full conversion still runs. A conversion error has the usual exit status and diagnostics.

For example, hledger versions differ in how they read trailing spaces in an `include` filename. With 1.52.4 and 1.99.6, hledger-flow follows the known behavior. On other eligible versions, a statement with that ambiguity goes through ordinary conversion each time. Statements using other, unambiguous rules can still reuse their journals. A shared ambiguous input affects every statement that uses it.

Common reasons include a hledger version below 1.52.4, an unreadable version, an unsupported input reader, an unrecognized config heading or active setting, a multiline or unknown directive, an unresolved rules include, and an absolute rules include or one outside the ledger. Use `--hledger-path` to select the real hledger executable if the configured path is a launcher. An unrecognized native executable that forwards to another program can hide changes to its target. Keep the installed converter stable during an import. Temporarily replacing its parent directory and restoring it during conversion can escape the executable identity checks.

An eligible rebuild uses private copies so the record describes the bytes hledger read. If hledger reports an error in a copied file, the diagnostic names the original file so you can open it at the reported line. Reuse can also be unavailable if private staging fails, a planned Windows snapshot path reaches 260 UTF-16 code units, or inputs change during conversion. A warm rules-based hit creates no snapshot.

## Clear the cache

The records are under `.hledger-flow/cache/import/` at the ledger root. Delete that directory to rebuild the records on your next incremental import. The cache creates its own Git ignore file. Ordinary and timestamp-only imports do not need cache records. If saving a record fails, the successful journal remains available and the import reports a warning.
