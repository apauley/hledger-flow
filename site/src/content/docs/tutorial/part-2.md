---
title: "Part 2: The first statement"
description: Draft a rules file for a tidy credit card CSV, import it and ask hledger about it.
---

This is the second part of a [six-part tutorial](/tutorial/). It continues
in `finances` from [part 1](/tutorial/part-1/), where Gawie filed his
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
liability. And the amounts are in rand, so he adds `currency ZAR`, the
rand’s three-letter code. He saves the result as
`import/gawie/kudu/creditcard/kudu-creditcard.rules`:

```hledger title="import/gawie/kudu/creditcard/kudu-creditcard.rules"
skip 1
fields date, description, amount

currency ZAR
account1 liabilities:kudu:creditcard
account2 expenses:unknown
```

`directives.journal`, which `init` made, declares how hledger shows common
currencies. Rand isn’t one of them, so Gawie adds it. The alias lets him
type the familiar `R` in journals he writes by hand, and hledger reads it as
ZAR:

```bash
echo 'commodity 1,000.00 ZAR  ; alias: R' >> directives.journal
```

## Import

```bash
hledger-flow import
```

```text
Moved orphaned output import/gawie/kudu/creditcard/3-journal/2026/hledger-flow-example.journal to .hledger-flow/trash/import/gawie/kudu/creditcard/3-journal/2026/hledger-flow-example.journal
Imported 1 statement for gawie/kudu/creditcard.
```

The import also found the example’s journal, whose statement Gawie
deleted in part 1. hledger-flow never deletes your files, so it moved the
journal to its trash in `.hledger-flow/trash/`, which Git ignores. Gawie
looks at what changed:

```bash
git add -A
git status --short
git diff --staged -- directives.journal
```

```ansi
␛[32mM␛[0m  directives.journal
␛[32mA␛[0m  import/2016-include.journal
␛[32mM␛[0m  import/2026-include.journal
␛[32mM␛[0m  import/all-years.journal
␛[32mA␛[0m  import/gawie/2016-include.journal
␛[32mM␛[0m  import/gawie/2026-include.journal
␛[32mM␛[0m  import/gawie/all-years.journal
␛[32mA␛[0m  import/gawie/kudu/2016-include.journal
␛[32mM␛[0m  import/gawie/kudu/2026-include.journal
␛[32mM␛[0m  import/gawie/kudu/all-years.journal
␛[32mA␛[0m  import/gawie/kudu/creditcard/2016-include.journal
␛[32mM␛[0m  import/gawie/kudu/creditcard/2026-include.journal
␛[32mA␛[0m  import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
␛[32mD␛[0m  import/gawie/kudu/creditcard/3-journal/2026/hledger-flow-example.journal
␛[32mM␛[0m  import/gawie/kudu/creditcard/all-years.journal
␛[32mA␛[0m  import/gawie/kudu/creditcard/kudu-creditcard.rules
␛[1mdiff --git a/directives.journal b/directives.journal␛[0m
␛[1mindex f3d4e10..1806cb5 100644␛[0m
␛[1m--- a/directives.journal␛[0m
␛[1m+++ b/directives.journal␛[0m
␛[36m@@ -29,3 +29,4 @@␛[0m ␛[0maccount assets:cash             ; type: C␛[0m
 account equity:conversion       ; type: V␛[0m
 account equity:unrealised-gain  ; type: U␛[0m
 account revenues:gain           ; type: G␛[0m
␛[32m+␛[0m␛[32mcommodity 1,000.00 ZAR  ; alias: R␛[0m
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
    liabilities:kudu:creditcard                 -842.50 ZAR
    expenses:unknown                             842.50 ZAR

2016-03-04 LETABA FUEL STOP
    liabilities:kudu:creditcard                 -910.00 ZAR
    expenses:unknown                             910.00 ZAR
```

## Ask hledger

Status warned that hledger won’t find this ledger by default. Gawie sets
`LEDGER_FILE`, as the note suggested, so hledger reads the whole ledger.
He also adds the export line to his shell profile for future sessions.

```bash
export LEDGER_FILE="$HOME/finances/all-years.journal"
hledger balance
```

```text
       -1,745.60 ZAR  liabilities:kudu:creditcard
        1,745.60 ZAR  expenses:unknown
--------------------
                   0  
```

Gawie owes Kudu Bank 1,745.60 ZAR, and every rand of it went to
`expenses:unknown`. He commits his first real import:

```bash
git commit -q -m "Import my first Kudu statement"
```

Sorting the spending into categories is the job for
[part 3](/tutorial/part-3/).
