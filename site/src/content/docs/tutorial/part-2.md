---
title: "Part 2: The first statement"
description: Draft a rules file for a tidy credit card CSV, import it and ask hledger about it.
---

This is the second part of a [six-part tutorial](/tutorial/). It continues
in `my-finances` from [part 1](/tutorial/part-1/), where Gawie filed his
first Kudu Bank statement.

## A rules file

hledger converts a CSV statement with a
[rules file](/reference/rules-files/): a few lines saying which column is
which and which accounts to use. `hledger-flow` can draft one from the
statement’s header row:

```bash
hledger-flow rules gawie/kudu/creditcard
cat import/gawie/kudu/creditcard/kudu-creditcard.rules
```

```text
Created import/gawie/kudu/creditcard/kudu-creditcard.rules
Edit it: check the fields line and the account names. Then run: hledger-flow import
# Starter rules for kudu/creditcard, made by hledger-flow from the header row of
# import/gawie/kudu/creditcard/1-in/2016/creditcard_2016-03-31.csv. Check every line, then run: hledger-flow import
# hledger's CSV rules reference: https://hledger.org/hledger.html#csv

skip 1
fields date, description, amount

# Make the date format match your statement, e.g. %d/%m/%Y
# date-format %Y-%m-%d

# Separate debit and credit columns? Name them amount-out and amount-in
# in the fields line instead of amount.

account1 assets:kudu:creditcard
account2 expenses:unknown
```

The draft is nearly right. `skip 1` skips the header row, and the `fields`
line already names the date, description and amount columns. The dates are
in hledger’s own format, so there’s no `date-format` to set.

Gawie makes two changes. A credit card is money he owes, so `account1` is a
liability. And the amounts are in rand, so he adds `currency R`. He saves
the result as `import/gawie/kudu/creditcard/kudu-creditcard.rules`:

```hledger title="import/gawie/kudu/creditcard/kudu-creditcard.rules"
skip 1
fields date, description, amount

currency R
account1 liabilities:kudu:creditcard
account2 expenses:unknown
```

## Import

```bash
hledger-flow import
```

```text
Moved orphaned output import/gawie/kudu/creditcard/3-journal/2026/hledger-flow-example.journal to import/gawie/kudu/creditcard/.trash/2026/3-journal/hledger-flow-example.journal
Imported 1 statement for gawie/kudu/creditcard.
```

The import also found the example’s journal, whose statement Gawie
deleted in part 1. hledger-flow never deletes your files, so it moved the
journal to a `.trash/` folder in the account. Gawie can look at it there
and delete it when he likes. He tells Git to ignore those folders, then
looks at what the import changed:

```bash
echo '.trash/' > .gitignore
git add -A
git status --short
```

```text
A  .gitignore
A  import/2016-include.journal
M  import/2026-include.journal
M  import/all-years.journal
A  import/gawie/2016-include.journal
M  import/gawie/2026-include.journal
M  import/gawie/all-years.journal
A  import/gawie/kudu/2016-include.journal
M  import/gawie/kudu/2026-include.journal
M  import/gawie/kudu/all-years.journal
A  import/gawie/kudu/creditcard/2016-include.journal
M  import/gawie/kudu/creditcard/2026-include.journal
A  import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
D  import/gawie/kudu/creditcard/3-journal/2026/hledger-flow-example.journal
M  import/gawie/kudu/creditcard/all-years.journal
A  import/gawie/kudu/creditcard/kudu-creditcard.rules
```

The statement became an hledger journal in `3-journal/2016/`, with a
`2016-include.journal` beside it and at every level above it. The include
files for the current year are now empty, since Gawie removed that year’s
starters. hledger-flow leaves them in place.

```bash
head -n 7 import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
```

```hledger
2016-03-02 PUNDA MARIA TRADING STORE
    liabilities:kudu:creditcard                R-842.50
    expenses:unknown                            R842.50

2016-03-04 LETABA FUEL STOP
    liabilities:kudu:creditcard                R-910.00
    expenses:unknown                            R910.00
```

## Ask hledger

Status warned that hledger won’t find this ledger by default. Gawie sets
`LEDGER_FILE`, as the note suggested, so hledger reads the whole ledger.
He also adds the export line to his shell profile for future sessions.

```bash
export LEDGER_FILE="$HOME/my-finances/all-years.journal"
hledger balance
```

```text
           R-1745.60  liabilities:kudu:creditcard
            R1745.60  expenses:unknown
--------------------
                   0  
```

Gawie owes Kudu Bank R1745.60, and every rand of it went to
`expenses:unknown`. He commits his first real import:

```bash
git commit -q -m "Import my first Kudu statement"
```

Sorting the spending into categories is the job for
[part 3](/tutorial/part-3/).
