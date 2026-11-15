---
title: Incremental import
description: Reuse journals when the statement and its conversion inputs have the same content.
---

Run `hledger-flow import --incremental=checked` to import new statements and rebuild journals whose inputs have changed. The first run builds a record for each eligible statement. Subsequent runs can reuse those journals.

Suppose you have twelve monthly statements. Adding April's download for the next year imports one statement. Correcting March rebuilds March, even if the bank preserved the download's timestamp. Touching March's file or downloading the same bytes again permits reuse. Editing a category in an included rules file rebuilds every selected statement that uses it. Adding or deleting an account-specific rules file can also change which rules apply.

## Choose a mode

| Command | When a statement is processed |
| --- | --- |
| `hledger-flow import` | Regenerate all selected statements. |
| `hledger-flow import --new-files-only` | Compare timestamps at each stage. [Timestamp-only imports](/reference/input-files/#timestamp-only-imports) describes recovery and limitations. |
| `hledger-flow import --incremental=checked` | Check content and dependencies. Always run selected preprocess and construct hooks. |
| `hledger-flow import --incremental=trust-scripts` | Also permit skipping hooks when their known inputs and saved outputs match. |

Choose one mode per import. The usual directory and `--start-year` selection applies. Under `checked`, if a preprocess hook removes a changing download header and leaves the same CSV bytes, the existing CSV keeps its timestamp. The rules-based journal can then be reused. A construct hook always regenerates its journal in this mode. A later timestamp-only import can rebuild once if the original is newer than the saved CSV.

`trust-scripts` applies to all selected accounts. Run from an account directory to limit its scope. Changing from the ledger root to an account directory can rerun that account's hooks because a script can read its working directory. An edit to an imported Python module, a lookup table, or a journal referenced by `include extra.journal` in construct output can leave a trusted journal stale. Use `checked` when those inputs should be read on every import. Live hook input checks can miss a directory temporarily replaced and restored while a hook runs, or a same-byte rewrite on a filesystem with coarse timestamps.

## What gets checked

Both modes verify the generated output and the effective CSV, TSV or SSV. Rules-based conversion follows nested rules includes. It fingerprints the selected hledger executable, effective config and applicable `directives.journal`. A replacement executable can invalidate records even if its version is unchanged. The supported releases are **1.52.4** and **1.99.6**, including package-manager and locally compiled builds.

A missing, empty or comment-only config permits reuse. Settings in recognized sections for other commands such as `[balance]` also permit reuse. The whole config is hashed, so editing one of those sections triggers one rebuild. In general settings and `[print]`, the supported exception is this setting on 1.99.6.

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

Common reasons include an unsupported hledger version or input reader, an unrecognized config heading or active setting, a multiline or unknown directive, an unresolved rules include, and an absolute rules include or one outside the ledger. Use `--hledger-path` to select the real hledger executable if the configured path is a launcher. An unrecognized native executable that forwards to another program can hide changes to its target.

An eligible rebuild uses private copies so the record describes the bytes hledger read. Reuse can also be unavailable if private staging fails, a planned Windows snapshot path reaches 260 UTF-16 code units, or inputs change during conversion. A warm rules-based hit creates no snapshot.

## Clear the cache

The records are under `.hledger-flow/cache/import/` at the ledger root. Delete that directory to rebuild the records on your next incremental import. The cache creates its own Git ignore file. Ordinary and timestamp-only imports do not need cache records. If saving a record fails, the successful journal remains available and the import reports a warning.
