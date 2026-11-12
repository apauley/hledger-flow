---
title: "Part 5: The last resort"
description: Write your own transactions with a construct script when the input isn't a table.
---

This is the last part of a [five-part tutorial](/tutorial/). It continues
in `my-finances` from [part 4](/tutorial/part-4/).

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
Ledger: ~/my-finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✓ 3 journals
  gawie/bogart/savings    1 statement (2017)    ✓ 1 journal
  gawie/goggatjie/tips    1 statement (2016)    ✗ no rules file
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

Next: Create import/gawie/goggatjie/tips/goggatjie-tips.rules (rules for gawie/goggatjie/tips)
      Run: hledger-flow status --rules-template gawie/goggatjie/tips
      Save its output as that file, then edit it.
      Docs: https://hledger-flow.github.io/go/rules-files/

Environment:
  hledger-flow  0.19.0 (57a13c8), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Ledger        ~/my-finances
  Cores         18, using 18
```

```bash
hledger-flow status --rules-template gawie/goggatjie/tips
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
`hledger-flow` checks them with hledger and saves the result in
`3-journal/`.

Gawie writes his in Python and saves it as
`import/gawie/goggatjie/tips/construct`:

```python title="import/gawie/goggatjie/tips/construct"
#!/usr/bin/env python3
# Turn the restaurant's monthly tip export (JSON) into hledger
# transactions: one per shift, one posting per currency.
import json
import sys

symbols = {"ZAR": "R"}

with open(sys.argv[1]) as statement:
    export = json.load(statement)

for shift in export["shifts"]:
    print(f"{shift['date']} * Tips, {shift['service']} shift")
    for tip in shift["tips"]:
        currency, amount = tip["currency"], tip["amount"]
        money = symbols[currency] + amount if currency in symbols else f"{amount} {currency}"
        print(f"    assets:goggatjie:tips    {money}")
    print("    income:tips")
    print()
```

Each transaction ends with an `income:tips` posting without an amount.
hledger fills it in, with one amount per currency. A construct script is
easy to try on its own:

```bash
chmod +x import/gawie/goggatjie/tips/construct
import/gawie/goggatjie/tips/construct import/gawie/goggatjie/tips/1-in/2016/tips_2016-05.json -
```

```hledger
2016-05-06 * Tips, dinner shift
    assets:goggatjie:tips    R420.00
    assets:goggatjie:tips    20.00 USD
    income:tips

2016-05-07 * Tips, lunch shift
    assets:goggatjie:tips    R180.00
    assets:goggatjie:tips    500.00 MZN
    income:tips

2016-05-07 * Tips, dinner shift
    assets:goggatjie:tips    R610.00
    assets:goggatjie:tips    10.00 USD
    assets:goggatjie:tips    250.00 MZN
    income:tips

2016-05-13 * Tips, dinner shift
    assets:goggatjie:tips    R385.00
    income:tips

2016-05-14 * Tips, dinner shift
    assets:goggatjie:tips    R540.00
    assets:goggatjie:tips    15.00 USD
    income:tips
```

```bash
hledger-flow import
hledger balance tips
```

```text
Imported 8 statements for 4 accounts.

Next: Generate reports, or explore with hledger
      Run: hledger-flow report
      Or try: hledger balance
          750.00 MZN
            R2135.00
           45.00 USD  assets:goggatjie:tips
         -750.00 MZN
           R-2135.00
          -45.00 USD  income:tips
--------------------
                   0  
```

## What the tips are worth

hledger keeps each currency separate. To see the tips in rand, it needs
exchange rates. `hledger-flow` includes `prices/<year>/prices.journal` in
the ledger for that year, so Gawie saves the rates he looked up as
`prices/2016/prices.journal`:

```hledger title="prices/2016/prices.journal"
; Exchange rates for Gawie's tips, May 2016
P 2016-05-01 USD R15.40
P 2016-05-01 MZN R0.29
```

`--value=then,R` converts each amount to rand at the rate on its date:

```bash
hledger-flow import
hledger balance tips --value=then,R
```

```text
Imported 8 statements for 4 accounts.

Next: Generate reports, or explore with hledger
      Run: hledger-flow report
      Or try: hledger balance
            R3045.50  assets:goggatjie:tips
           R-3045.50  income:tips
--------------------
                   0  
```

```bash
hledger incomestatement --pretty --period 2016-05 --value=then,R
```

```text
Income Statement 2016-05, valued at posting date

┌───────────────────────╥───────────┐
│                       ║       May │
╞═══════════════════════╬═══════════╡
│ Revenues              ║           │
├───────────────────────╫───────────┤
│ income:salary         ║ R37256.28 │
│ income:tips           ║  R3045.50 │
├───────────────────────╫───────────┤
│                       ║ R40301.78 │
╞═══════════════════════╬═══════════╡
│ Expenses              ║           │
├───────────────────────╫───────────┤
│ expenses:car          ║  R1815.00 │
│ expenses:cash         ║  R4000.00 │
│ expenses:charity      ║  R4000.00 │
│ expenses:fun          ║  R2905.00 │
│ expenses:groceries    ║  R2163.25 │
│ expenses:health       ║   R189.50 │
│ expenses:insurance    ║  R5234.43 │
│ expenses:mopane-worms ║  R6510.00 │
│ expenses:phone        ║   R250.00 │
│ expenses:rent         ║  R5000.00 │
├───────────────────────╫───────────┤
│                       ║ R32067.18 │
╞═══════════════════════╬═══════════╡
│ Net:                  ║  R8234.60 │
└───────────────────────╨───────────┘
```

The tips pay for nearly half of the month’s mopane worms. Gawie is ready for a quiet
season.

```bash
hledger-flow status
```

```text
Ledger: ~/my-finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✓ 3 journals
  gawie/bogart/savings    1 statement (2017)    ✓ 1 journal
  gawie/goggatjie/tips    1 statement (2016)    ✓ 1 journal
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

Next: Generate reports, or explore with hledger
      Run: hledger-flow report
      Or try: hledger balance

Environment:
  hledger-flow  0.19.0 (57a13c8), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Ledger        ~/my-finances
  Cores         18, using 18
```

## Which tool when

| The statement… | Use | As in |
| --- | --- | --- |
| is a CSV with a header row | the rules file `status --rules-template` drafts | [Part 1](/tutorial/part-1/) |
| needs categories | `if` blocks, shared with `include` | [Part 2](/tutorial/part-2/) |
| has extra rows, odd dates or split columns | `skip`, if tables and rules functions | [Part 3](/tutorial/part-3/) |
| needs information from another row, or isn’t CSV | a `preprocess` script that fixes only that, plus rules | [Part 4](/tutorial/part-4/) |
| needs transactions no rules file can build | a `construct` script | Part 5 |

Start at the top of the table, and move down only when the row above can’t
do the job. The further down you go, the more of the conversion is code
you maintain yourself.
