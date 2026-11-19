---
title: Price files
description: Yearly market price journals that are included automatically, and ways to fetch the prices.
---

Save the year’s [market prices as hledger `P` directives](https://hledger.org/hledger.html#p-directive)
under `prices/<year>/` alongside `import/`. A new ledger’s
`prices/<year>/prices.journal` includes `EUR-USD.prices` and
`BTC-USD.prices`. Both starter files quote prices in USD. Name pair files
`<priced-commodity>-<quote-currency>.prices` and add an `include` line for each one. The **priced commodity** is the unit
being priced. The **quote currency** is what you express its price in.
For example, `EUR-USD.prices` contains the price of one EUR in USD.

Existing ledgers can also put `P` directives directly in `prices.journal`.
When import builds that year’s include file under `import/`, it includes
`prices.journal` if present.

After running an import with available price files you'll see a line has been added to `import/2020-include.journal`:

    include ../prices/2020/prices.journal

Each year’s include file then has the prices it needs, so a report on a
single year values amounts correctly:

```bash
hledger -f import/2020-include.journal balance --value=then,USD
```

Keep prices out of `directives.journal`. hledger-flow passes that file to
every statement conversion, so with [incremental import](/reference/incremental-import/)
each new price would rebuild every statement. Price files are only read by
reports.

## Commodity names

A price only applies to amounts that use the same commodity name. Price tools
such as pricehist and hledger’s `getprices` write ISO codes like `USD`. If your
statements write US dollars as `$`, declare `$` as an
[alias](https://hledger.org/dev/hledger.html#commodity-aliases) of `USD` in
a file included by the year’s price index. For example, add
`include aliases.journal` to `prices/2025/prices.journal` and save:

```hledger title="prices/2025/aliases.journal"
commodity USD  ; alias: $
```

hledger then reads amounts written as `$` as `USD`, so prices quoted in `USD`
value them, and reports show `USD`.

:::note
Commodity aliases are only supported from hledger 1.99.5 onwards. hledger
1.99.3 introduced them, but amounts written with an alias are converted to
the commodity only from 1.99.5. hledger 1.52 and earlier don’t have them. Use
the [linking price](#link-names-with-a-price) instead.
:::

Include the alias file from every year’s price index. An alias only converts amounts
in the files read together with it, and a year’s include file, such as
`import/2025-include.journal`, doesn’t include `directives.journal`.

### Link names with a price

With hledger older than 1.99.5, add a price that links the two names instead:

```hledger
P 2000-01-01 USD $1
```

hledger chains prices, so `--value=then,$` can then use prices quoted in `USD`.

## Fetch prices with pricehist

[pricehist](https://gitlab.com/chrisberkhout/pricehist) downloads currency,
cryptocurrency and stock prices and prints them as `P` directives. Install it
with `pipx install pricehist` or `uv tool install pricehist`, then list its
sources with `pricehist sources`. Some sources, such as Alpha Vantage and
CoinMarketCap, need a free API key.

Fetch a year of euro exchange rates from the European Central Bank and append
them to that year’s pair file. Add `include EUR-USD.prices` to
`prices/2025/prices.journal` if it is new:

```bash
mkdir -p prices/2025
pricehist fetch ecb EUR/USD -s 2025-01-01 -e 2025-12-31 -o ledger --fmt-time '' >> prices/2025/EUR-USD.prices
```

Each line looks like this:

```hledger
P 2025-12-19 EUR 1.1712 USD
```

Yahoo Finance names exchange rates like `GBPUSD=X`. Use `--fmt-base` to name
the commodity being priced. Add `include GBP-USD.prices` to the index:

```bash
pricehist fetch yahoo GBPUSD=X -s 2025-01-01 -e 2025-12-31 -o ledger --fmt-time '' \
  --fmt-base GBP >> prices/2025/GBP-USD.prices
```

To add only prices newer than those already saved, use `-sx` with the date of
the last saved price instead of `-s`.

## Fetch a year’s prices with hledger’s getprices

hledger’s
[`getprices`](https://github.com/hledgerorg/hledger/blob/main/bin/getprices)
script picks a pricehist source for each commodity: currencies, cryptocurrencies
or stocks. It takes the quote currency, the priced commodity and a date
range, so it can fetch one year at a time. Save it as `prices/getprices` and make it
executable:

```bash
curl -o prices/getprices https://raw.githubusercontent.com/hledgerorg/hledger/main/bin/getprices
chmod +x prices/getprices
```

For a ledger that keeps prices directly in `prices.journal`, this fetches
prices for every commodity used in 2025. It replaces the
`P` lines in that year’s price file, keeps its other lines, such as a
[commodity alias](#commodity-names), and changes nothing if it fetched no
prices:

```bash
year=2025
mkdir -p prices/$year
{
  grep -v '^P ' prices/$year/prices.journal 2>/dev/null
  for c in $(hledger -f import/$year-include.journal commodities --used); do
    prices/getprices USD "$c" $year-01-01 $year-12-31
  done
} > prices/$year/prices.new
grep -q '^P ' prices/$year/prices.new && mv prices/$year/prices.new prices/$year/prices.journal
```

Replace `USD` with the quote currency you want to use for valuation.
`getprices` skips that currency and expects ISO codes such as `EUR`
and `GBP`. If your statements use symbols, declare them as
[aliases](#commodity-names) first, so hledger lists the ISO codes. For the
current year, use today’s date as the
end date.

The loop only finds commodities that appear in the year’s transactions. A
holding carried over from an earlier year, with no transaction that year,
gets no prices. Add its opening balance to the year’s
[manual journal](/reference/manual-journals/), or add the commodity to the
list in the loop. Then run `hledger-flow import`, which adds the price file to the
year’s include file if it is new.

hledger’s [`get` command](https://hledger.org/dev/hledger.html#get) runs the
same script, but always fetches from each commodity’s first transaction until
today and saves one `prices/<COMM>.prices` file per commodity. hledger-flow
does not include those files, because each one spans every year.
