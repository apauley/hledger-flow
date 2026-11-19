---
title: "Part 6: The last resort"
description: Write your own transactions with a construct script when the input isn't a table.
---

This is the last part of a [six-part tutorial](/tutorial/). It continues
in `finances` from [part 5](/tutorial/part-5/).

## The tip jar

Guests at the Grillerige Groen Goggatjie tip generously, in whatever money
they brought: rand, US dollars from Zimbabwe, meticais from Mozambique. The
restaurant’s tip app exports each chef’s month as JSON:

```json
{
  "restaurant": "Grillerige Groen Goggatjie",
  "chef": "Gawie de Groot",
  "month": "2016-05",
  "shifts": [
    {"date": "2016-05-06", "service": "dinner",
     "tips": [{"currency": "ZAR", "amount": "420.00"}, {"currency": "USD", "amount": "20.00"}]},
    {"date": "2016-05-07", "service": "lunch",
     "tips": [{"currency": "ZAR", "amount": "180.00"}, {"currency": "MZN", "amount": "500.00"}]},
```

Gawie treats the tips as another account, owned by him, at the restaurant:

```bash
mkdir -p import/gawie/goggatjie/tips/1-in/2016
cp ../Downloads/Goggatjie/tips_2016-05.json import/gawie/goggatjie/tips/1-in/2016/
hledger-flow status
```

```text
Ledger: ~/finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✓ 3 journals
  gawie/bogart/savings    1 statement (2017)    ✓ 1 journal
  gawie/goggatjie/tips    1 statement (2016)    ✗ no rules file
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

  To review:
    1 transfer account to check  reports/all/2017/transfer-balance.txt
    1 transfer account to check  reports/all/2016/transfer-balance.txt

Next: Create import/gawie/goggatjie/tips/goggatjie-tips.rules (rules for gawie/goggatjie/tips)
      Run: hledger-flow rules gawie/goggatjie/tips
      It creates that file from the statement's columns, for you to edit.
      Docs: https://hledger-flow.github.io/go/rules-files/

Environment:
  hledger-flow  0.20.1 (0d49906), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

```bash
hledger-flow rules gawie/goggatjie/tips
```

```text
hledger-flow: import/gawie/goggatjie/tips/1-in/2016/tips_2016-05.json isn't a CSV file, so there's no header row to build rules from. A preprocess script can convert it to CSV first: https://hledger-flow.github.io/go/preprocess-script/
```

## Choosing the tool

`hledger-flow` suggests the usual way forward: a preprocess script that
turns the JSON into CSV, with one row per tip, and a rules file for the
rest. For most JSON exports, that’s the right choice, because the rules
still do the classifying.

But hledger’s rules turn each CSV row into its own transaction. Gawie wants
one transaction per shift, holding all of that shift’s tips in their
different currencies. That’s a transaction shape no rules file can build
from separate rows.

When you need to write the transactions yourself, use a
[construct script](/reference/construct/). If an account directory has an
executable called `construct`, `hledger-flow` runs it instead of using a
rules file. It gets the statement’s path, a `-`, and the bank, account and
owner names, and writes hledger journal entries to standard output.
hledger formats the entries and hledger-flow saves the result in
`3-journal/`.

Gawie writes his in Python and saves it as
`import/gawie/goggatjie/tips/construct`:

```python title="import/gawie/goggatjie/tips/construct"
#!/usr/bin/env python3
# Turn the restaurant's monthly tip export (JSON) into hledger
# transactions: one per shift, one posting per currency.
import json
import sys

with open(sys.argv[1]) as statement:
    export = json.load(statement)

for shift in export["shifts"]:
    print(f"{shift['date']} * Tips, {shift['service']} shift")
    for tip in shift["tips"]:
        print(f"    assets:goggatjie:tips    {tip['amount']} {tip['currency']}")
    print("    income:tips")
    print()
```

The export already uses ZAR, like the rest of Gawie’s ledger, so the
script copies each currency code as it is. Each transaction ends with an
`income:tips` posting without an amount.
hledger fills it in, with one amount per currency. A construct script is
easy to try on its own:

```bash
chmod +x import/gawie/goggatjie/tips/construct
import/gawie/goggatjie/tips/construct import/gawie/goggatjie/tips/1-in/2016/tips_2016-05.json -
```

```hledger
2016-05-06 * Tips, dinner shift
    assets:goggatjie:tips    420.00 ZAR
    assets:goggatjie:tips    20.00 USD
    income:tips

2016-05-07 * Tips, lunch shift
    assets:goggatjie:tips    180.00 ZAR
    assets:goggatjie:tips    500.00 MZN
    income:tips

2016-05-07 * Tips, dinner shift
    assets:goggatjie:tips    610.00 ZAR
    assets:goggatjie:tips    10.00 USD
    assets:goggatjie:tips    250.00 MZN
    income:tips

2016-05-13 * Tips, dinner shift
    assets:goggatjie:tips    385.00 ZAR
    income:tips

2016-05-14 * Tips, dinner shift
    assets:goggatjie:tips    540.00 ZAR
    assets:goggatjie:tips    15.00 USD
    income:tips
```

```bash
hledger-flow import
hledger balance tips
```

```text
Imported 8 statements for 4 accounts.
          750.00 MZN
           45.00 USD
        2,135.00 ZAR  assets:goggatjie:tips
         -750.00 MZN
          -45.00 USD
       -2,135.00 ZAR  income:tips
--------------------
                   0  
```

## What the tips are worth

hledger keeps each currency separate. To see the tips in rand, it needs
exchange rates, and status says so:

```bash
hledger-flow status
```

```text
Ledger: ~/finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✓ 3 journals
  gawie/bogart/savings    1 statement (2017)    ✓ 1 journal
  gawie/goggatjie/tips    1 statement (2016)    ✓ 1 journal
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

Next: Add market prices for your commodities
      Save each pair's P directives in prices/<year>/<commodity>-<quote>.prices, such as XAU-USD.prices.
      Include each pair file from prices/<year>/prices.journal, then run hledger-flow import.
      Docs: https://hledger-flow.github.io/go/price-files/

Environment:
  hledger-flow  0.20.1 (0d49906), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

`hledger-flow` includes `prices/<year>/prices.journal` in the ledger for
that year. The starter that `init` made in part 1 kept one file per
currency pair, such as `EUR-USD.prices`, with prices in US dollars, and
`prices.journal` only included them. Status suggests the same pattern, so
Gawie follows it for 2016, with rand as his base currency. He saves the rates he looked up in
`prices/2016/USD-ZAR.prices`:

```hledger title="prices/2016/USD-ZAR.prices"
; Rand per US dollar, for Gawie's tips.
P 2016-05-01 USD 15.40 ZAR
```

and `prices/2016/MZN-ZAR.prices`:

```hledger title="prices/2016/MZN-ZAR.prices"
; Rand per Mozambican metical, for Gawie's tips.
P 2016-05-01 MZN 0.29 ZAR
```

and includes both from `prices/2016/prices.journal`:

```hledger title="prices/2016/prices.journal"
include USD-ZAR.prices
include MZN-ZAR.prices
```

`--value=then,ZAR` converts each amount to rand at the rate on its date:

```bash
hledger-flow import
hledger balance tips --value=then,ZAR
```

```text
Imported 8 statements for 4 accounts.
        3,045.50 ZAR  assets:goggatjie:tips
       -3,045.50 ZAR  income:tips
--------------------
                   0  
```

```bash
hledger incomestatement --pretty --period 2016-05 --value=then,ZAR
```

```text
Income Statement 2016-05, valued at posting date

┌───────────────────────╥───────────────┐
│                       ║           May │
╞═══════════════════════╬═══════════════╡
│ Revenues              ║               │
├───────────────────────╫───────────────┤
│ income:salary         ║ 37,256.28 ZAR │
│ income:tips           ║  3,045.50 ZAR │
├───────────────────────╫───────────────┤
│                       ║ 40,301.78 ZAR │
╞═══════════════════════╬═══════════════╡
│ Expenses              ║               │
├───────────────────────╫───────────────┤
│ expenses:car          ║  1,815.00 ZAR │
│ expenses:cash         ║  4,000.00 ZAR │
│ expenses:charity      ║  4,000.00 ZAR │
│ expenses:fun          ║  2,905.00 ZAR │
│ expenses:groceries    ║  2,163.25 ZAR │
│ expenses:health       ║    189.50 ZAR │
│ expenses:insurance    ║  5,234.43 ZAR │
│ expenses:mopane-worms ║  6,510.00 ZAR │
│ expenses:phone        ║    250.00 ZAR │
│ expenses:rent         ║  5,000.00 ZAR │
├───────────────────────╫───────────────┤
│                       ║ 32,067.18 ZAR │
╞═══════════════════════╬═══════════════╡
│ Net:                  ║  8,234.60 ZAR │
└───────────────────────╨───────────────┘
```

Gawie typed two rates by hand. For daily rates over many months,
[fetch them with pricehist](/reference/price-files/#fetch-prices-with-pricehist) into each pair’s file.

The tips pay for nearly half of the month’s mopane worms. Gawie is ready for a quiet
season.

```bash
hledger-flow status
```

```text
Ledger: ~/finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✓ 3 journals
  gawie/bogart/savings    1 statement (2017)    ✓ 1 journal
  gawie/goggatjie/tips    1 statement (2016)    ✓ 1 journal
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

Next: Generate reports, or explore with hledger
      Run: hledger-flow report
      Or try: hledger -f "$HOME/finances/all-years.journal" balance

Environment:
  hledger-flow  0.20.1 (0d49906), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

Gawie commits his tips, and his ledger is up to date:

```bash
git add -A
git commit -q -m "Add the tip jar"
```

## Which tool when

| The statement… | Use | As in |
| --- | --- | --- |
| is a CSV with a header row | the rules file `hledger-flow rules` drafts | [Part 2](/tutorial/part-2/) |
| needs categories | `if` blocks, shared with `include` | [Part 3](/tutorial/part-3/) |
| has extra rows, odd dates or split columns | `skip`, if tables and rules functions | [Part 4](/tutorial/part-4/) |
| needs information from another row, or isn’t CSV | a `preprocess` script that fixes only that, plus rules | [Part 5](/tutorial/part-5/) |
| needs transactions no rules file can build | a `construct` script | Part 6 |

Start at the top of the table, and move down only when the row above can’t
do the job. The further down you go, the more of the conversion is code
you maintain yourself.
