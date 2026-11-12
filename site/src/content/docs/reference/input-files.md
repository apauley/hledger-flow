---
title: Input files
description: Where to put your statements, and what happens to generated files when a statement goes away.
---

Your input files will probably be CSV files with a line for each
transaction, although other file types will work fine if you use a
`preprocess` or a `construct` script that can read them. These scripts
are explained later.

We mostly use conventions based on a predefined directory structure for
your input statements.

For example, assuming you have a `savings` account at `mybank`, you'll
put your first CSV statement here:
`import/john/mybank/savings/1-in/2018/123456789_2018-06-30.csv`.

Some people may want to include accounts belonging to their spouse as
part of the household finances:
`import/spouse/otherbank/checking/1-in/2018/987654321_2018-06-30.csv`.

## More About Input Files

All files and directories under the `import` directory are related to the
automatic importing and classification of transactions.

The directory directly under `import` is meant to indicate the owner or
custodian of the accounts below it. It mostly has an impact on
reporting. You may want to have separate reports for `import/mycompany`
and `import/personal`.

Below the directory for the owner we can indicate where an account is
held. For a bank account you may choose to name it `import/john/mybank`.

If your underground bunker filled with gold has CSV statements linked to
it, then you can absolutely create `import/john/secret-treasure-room`.

Under the directory for the financial institution, you'll have a
directory for each account at that institution, e.g.
`import/mycompany/bigbankinc/customer-deposits` and
`import/mycompany/bigbankinc/expense-account`.

Next you'll create a directory named `1-in`. This is to distinguish it
from `2-preprocessed` and `3-journal` which will be auto-generated
later.

Under `1-in` you'll create a directory for the year, e.g. `2018`, and
within that you can copy the statements for that year:
`import/john/mybank/savings/1-in/2018/123456789_2018-06-30.csv`

Keep the filenames your bank provides. For example, `April [1].csv`
and `April 1.csv` can sit in the same year directory. Import treats the
brackets literally and includes each statement's transactions once. If an
earlier import duplicated one statement and omitted the other, run
`hledger-flow import` to rebuild the journals from both files.

## Generated Files and Recovery

`1-in` is the source of truth. Both `2-preprocessed` and `3-journal`
contain generated output; user-supplied files in those stages are not
supported. Use [manually managed journals](/reference/manual-journals/)
for transactions without an input statement.

After all selected inputs have been converted successfully, import moves
orphaned `2-preprocessed/YYYY/*.csv` and `3-journal/YYYY/*.journal` files
into the account's `.trash` directory. A corresponding input must be in
`1-in` for the same account and year, with a filename that produces that
output when its extension is replaced. An old preprocessed CSV does not
keep an orphaned journal active. If two inputs would produce the same
output, import stops before converting or moving files and names the
conflicting inputs.

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

Moves respect the selected import directory and `--start-year`, including
when the last input has been removed. Include files are rebuilt globally
from source-backed journals and supported manual files, so orphaned
journals outside the cleanup scope are also excluded from the ledger.
Obsolete generated year and account indexes are rewritten as empty ledgers.
Manual-only and empty imports succeed without dummy input files.

Only the documented stage/year layout is processed. Unrelated files such
as PDFs are left untouched; hidden files and directories are ignored.
Deleting an input can therefore remove its transactions from the ledger,
but does not permanently delete the generated files.

## Stability

The basic owner/bank/account/year structure has been used and tested
fairly extensively, I don't expect a need for it to change.

I'm open to suggestions for improvement though.
