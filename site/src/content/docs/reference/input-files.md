---
title: Input files
description: Where to put your statements, and what happens to generated files when a statement goes away.
---

Save each statement in `import/<owner>/<bank>/<account>/1-in/<year>/`.
For an empty ledger, `hledger-flow status` shows a command to create those
directories. Substitute your owner, bank, account and year.
For example, John’s June savings statement might be
`import/john/mybank/savings/1-in/2018/123456789_2018-06-30.csv`.
Import reads every visible file directly in that year folder as a statement.
For a format hledger’s CSV rules can’t read, supply a
[preprocess](/reference/preprocess/) or [construct](/reference/construct/) script.

Use non-overlapping exports for each account. If the same purchase appears in
two statements, it gets two journal entries. When a bank issues a corrected
statement, replace the original at the same path and run `hledger-flow import`.
For a transfer between two accounts, keep both statements and
[classify the two sides of the transfer](/guides/transfers/).

An owner, bank, or account can itself be named `import`. For example,
`import/john/mybank/import/1-in/2018/april.csv` belongs to John's account
named `import` at `mybank`. Run `hledger-flow import` and `hledger-flow status`
from the ledger root to use that layout.

Some people may want to include accounts belonging to their spouse as
part of the household finances:
`import/spouse/otherbank/checking/1-in/2018/987654321_2018-06-30.csv`.

Parallel import converts one statement per core at a time, and starts another
as soon as a worker finishes. Use `--batch-size N` to set how many run at once
or `--sequential` to run one at a time.

Run `hledger-flow import --verbose` to see where import looks for each
statement's scripts, rules file and include files, and the exact commands it
runs. You can paste a command into a shell at the ledger root. If you started
import elsewhere, the command first changes to that working directory so scripts
can resolve relative paths and hledger can find the same config. Paths below the
working directory start with `./`. Each statement's lines appear together once
it's done. With `--sequential`, they appear as import works.

## Year folders

Use the year of the transactions in the statement. If a statement contains
transactions from two years, split it and put each file in its own year
folder. When status finds a misplaced delimited statement, it can suggest
a year from its dates and show a move command. Confirm the year yourself
when dates are missing or transaction and posting dates disagree.

## More About Input Files

The owner folder groups accounts for reporting. For example, filing statements
under `import/mycompany/` and `import/personal/` gives you reports for the
company, for your personal accounts and for both together. You can create an
owner folder in advance. Reporting skips that owner until import creates its
`all-years.journal`.

Within each account, `1-in/` holds your originals, `2-preprocessed/` holds
any CSVs written by a preprocess script, and `3-journal/` holds the journals.
All three use the same year and filename stem.

Keep the filenames your bank provides. For example, `April [1].csv`
and `April 1.csv` can sit in the same year directory. Import treats the
brackets literally and includes each statement's transactions once. If an
earlier import duplicated one statement and omitted the other, run
`hledger-flow import` to rebuild the journals from both files.

<span id="incremental-imports"></span>

## Timestamp-only imports

`hledger-flow import --new-files-only` compares each generated file's
modification time with its source. It rebuilds a preprocessed CSV or journal
when its source is newer, even if the two timestamps differ by only a
fraction of a second. Equal timestamps count as up to date. If you replace
a statement and preserve its timestamp, run `hledger-flow import` to force
a rebuild. Also use a full import after changing rules or import scripts. This flag
compares only the statement, cached CSV and journal timestamps. To track
content changes in statements, rules and nested includes, see
[Incremental import](/reference/incremental-import/).

## Generated Files and Recovery

When an import or report fails, the command finishes writing its queued
diagnostics before it exits, even if a pipe consumer reads slowly. To save
the failure message for inspection, capture standard error, for example
with `hledger-flow import 2>import-errors.log`.

On Windows, you can paste the displayed command into PowerShell at the ledger
root. If `USERPROFILE` differs from the Windows profile directory, the
command uses absolute paths for files outside the ledger so they still resolve.

`1-in` is the source of truth. Both `2-preprocessed` and `3-journal`
contain generated output; user-supplied files in those stages are not
supported. Use [manually managed journals](/reference/manual-journals/)
for transactions without an input statement.

Import writes each rules-generated journal and each changed include file to
a temporary file before replacing the destination. If a write fails, the
previous complete copy of that file stays in place. Correct the cause of
the failure and rerun the same import. Files written successfully earlier
in the run may already contain the new data.

After all selected inputs have been converted successfully, import moves
orphaned `2-preprocessed/YYYY/*.csv` and `3-journal/YYYY/*.journal` files
into the account's `.trash` directory. A corresponding input must be in
`1-in` for the same account and year, with a filename that produces that
output when its extension is replaced. An old preprocessed CSV does not
keep an orphaned journal active. If two inputs would produce the same
output, import stops before converting or moving files and names the
conflicting inputs. If a symlink points to a source-backed journal, import keeps
the link and includes the journal once, preferring the generated filename.

For example, removing or renaming `1-in/2026/march.csv` retires its old
outputs to:

```text
.trash/2026/2-preprocessed/march.csv
.trash/2026/3-journal/march.journal
```

Each move is reported. Existing trash entries are preserved, with `.1`,
`.2`, and so on appended to subsequent filenames. The trash is never
imported or emptied automatically. Restore the input and run import again
to regenerate its outputs, or recover handwritten transactions from a
trashed journal into `_manual_`. Restoring just the generated file without
its input will cause it to be retired again.

If you keep your base directory in Git, add `.trash/` to your `.gitignore`
so retired outputs stay out of version control.

Trash moves use an account-local `.trash/.move-lock` directory to prevent
concurrent imports from overwriting recovery files. If an interrupted
process leaves this lock behind, the error explains how to remove the
empty lock directory once no import is running and retry. A failed run may
have moved some outputs already; they remain recoverable in `.trash`.

Running `hledger-flow import` from anywhere below an account selects the
whole account for statement conversion and orphan cleanup. For example,
running it from `import/john/mybank/savings/1-in/2018/archive/` selects
`savings` across its statement years and leaves sibling accounts' statements
and generated journals alone. The same scope applies when you pass that
directory to `import`. Use `--start-year 2024` to limit conversion and cleanup
to 2024 and later years, including years whose last input was removed. Supply
exactly four ASCII digits or use `--start-year current` for the current year.
An invalid value such as `24` stops the command before import begins.

Include files are rebuilt globally
from source-backed journals and supported manual files, so orphaned
journals outside the cleanup scope are also excluded from the ledger.
Obsolete generated year and account indexes are rewritten as empty ledgers.
Manual-only and empty imports succeed without dummy input files.

Import ignores hidden files and directories. Keep supporting documents such
as PDF receipts outside `1-in/<year>/` unless a preprocess or construct script converts them.
Deleting an input can therefore remove its transactions from the ledger,
but does not permanently delete the generated files.

## Stability

The basic owner/bank/account/year structure has been used and tested
fairly extensively, I don't expect a need for it to change.

I'm open to suggestions for improvement though.
