---
title: Rules files
description: Where hledger-flow looks for the hledger CSV rules that convert each statement.
---

For most downloaded bank CSVs, an hledger rules file handles the whole
conversion. Use `fields` to map columns, `skip` to ignore headers, `if` to
classify matching transactions, and `include` to share rules across accounts.
See [hledger’s CSV rules cheatsheet](https://hledger.org/hledger.html#csv-rules-cheatsheet).

The upcoming hledger 2 release also has experimental rules functions,
available since preview 1.99.6. For example, if you named a CSV column
`memo`, you can use `description %{replace(%memo, "^POS +", "")}` to strip a
bank's prefix, or `%{substr(%memo, 1, 4)}` to extract the first four characters.
See [hledger's rules functions](https://hledger.org/dev/hledger.html#functions).

`hledger-flow` will try to find a rules file for each statement in a few
places. The same rules file is typically used for all statements of a
specific account, or even for all accounts of the same specific bank.

- A global rules file for any `mybank` statement can be saved here:
  `import/mybank.rules`
- A rules file for all statements of a specific account:
  `import/spouse/bigbankinc/savings/bigbankinc-savings.rules`

To generate a starter `fields` line from a statement's header, run
`hledger-flow status --rules-template john/mybank/savings` with your account
path. Each column gets a unique field name in header order. For example,
`Reference_2,Reference,Reference` becomes `reference_2, reference, reference_3`
because the first column already uses `reference_2`.

## Statement-specific Rules Files

If your bank changes its CSV columns, keep the rules that still read the
old statements. Give files in the new format a filename suffix to select a
different rules file. A full rebuild can then read both formats.

Add a final underscore-separated suffix beginning with `rfo` (rules file
override) before the filename extensions. For example, both
`2024.04_rfo-card.csv` and `march_rfo-card.bank.csv` select
`rfo-card.rules`. Dots in the date and additional file extensions leave
the override name intact.

For `john/mybank/savings`, import checks these paths in order and uses
the first match.

1. `import/john/mybank/savings/rfo-card.rules`
2. `import/john/mybank/rfo-card.rules`
3. `import/rfo-card.rules`

If none exists, import looks for the usual account or bank rules.

## Example rules file usage

A common scenario is multiple accounts that share the same file format,
but have different `account1` directives.

One possible approach would be to include a shared rules file in your
account-specific rules file.

If you are lucky enough that all statements at `mybank` share a common
format across all accounts, then you can `include` a rules file that
just defines the parts that are shared across accounts.

Two accounts at `mybank` may have rules files similar to these.

A checking account at mybank:

    # Saved as: import/john/mybank/checking/mybank-checking.rules
    include ../../../mybank-shared.rules
    account1 assets:john:mybank:checking

Another account at mybank:

    # Saved as: import/alice/mybank/savings/mybank-savings.rules
    include ../../../mybank-shared.rules
    account1 assets:alice:mybank:savings

Where `import/mybank-shared.rules` may define some shared attributes:

    skip 1

    fields date, description, amount, balance

    date-format %Y-%m-%d
    currency $

## Stability

Rules files are a stable feature within
[hledger](https://hledger.org/), and we're just using the normal
hledger rules files. The account, bank and statement-specific rules
files have been used and tested fairly extensively, I don't expect this
to change.

Let me know if you think it should change.

## Starter rules for exports without a header

Run `hledger-flow rules OWNER/BANK/ACCOUNT` to create a starter in the
account folder. It writes UTF-8 and refuses to replace an existing file.
Use `hledger-flow status --rules-template OWNER/BANK/ACCOUNT` to preview the
same starter on standard output. For a headerless export it shows the first
row by column and uses
up to 50 rows to suggest the date, amount and description fields. Check the
guesses, account names and date format before importing. A neutral name such
as `column3` leaves that field for you to identify.

The starter preserves the export's date separator and accepts day and month
numbers with or without leading zeroes. When the dates leave the day and
month ambiguous or mix formats, set `date-format` yourself using the
example in the starter.

## Reuse existing rules

Put your existing rules and any files they include in the account folder.
If status identifies one main rules file under a different name, it suggests
a safe rename to the expected account filename. Keep the names of files the
main rules include. If status finds several possible main files, choose the
one that matches the statement and rename it yourself.
