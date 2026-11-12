---
title: Rules files
description: Where hledger-flow looks for the hledger CSV rules that convert each statement.
---

For most downloaded bank CSVs, an hledger rules file handles the whole
conversion. Use `fields` to map columns, `skip` to ignore headers, `if` to
classify matching transactions, and `include` to share rules across accounts.
See [hledger's CSV rules reference](https://hledger.org/1.52/hledger.html#csv).

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

## Statement-specific Rules Files

What happens if some of the statements for an account has a different
format than the others?

This can happen if you normally get your statements directly from your
bank, but some statements you had to download from somewhere else, like
Mint, because your bank is being daft with older statements.

In order to tell `hledger-flow` that you want to override the rules file
for a specific statement, you need to add a suffix, separated by an
underscore (`_`) and starting with the letters `rfo` (rules file
override) to the filename of that statement.

For example: assuming you've named your statement
`99966633_20171223_1844_rfo-mint.csv`.

`hledger-flow` will look for a rules file named `rfo-mint.rules` in the
following places:

- in the import directory, e.g. `import/rfo-mint.rules`
- in the bank directory, e.g. `import/john/mybank/rfo-mint.rules`
- in the account directory, e.g.
  `import/john/mybank/savings/rfo-mint.rules`

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
    account1 Assets:Current:John:MyBank:Checking

Another account at mybank:

    # Saved as: import/alice/mybank/savings/mybank-savings.rules
    include ../../../mybank-shared.rules
    account1 Assets:Current:Alice:MyBank:Savings

Where `import/mybank-shared.rules` may define some shared attributes:

    skip 1

    fields date, description, amount, balance

    date-format %Y-%m-%d
    currency $

## Stability

Rules files are a stable feature within
[hledger](http://hledger.org/), and we're just using the normal
hledger rules files. The account, bank and statement-specific rules
files have been used and tested fairly extensively, I don't expect this
to change.

Let me know if you think it should change.
