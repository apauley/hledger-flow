---
title: Price files
description: Yearly market price journals that are included automatically.
---

`hledger-flow` looks for [price files](https://hledger.org/journal.html#market-prices) to include in each yearly include file.

For example, the presence of a file named `${BASE}/prices/2020/prices.journal` will result in some extra include file magic.

The rest of this section assumes you'll have a file named `prices/2020/prices.journal` which contains price data for the year 2020.
The `prices` directory should be right at the top of your `hledger-flow` base directory, next to the `import` directory.

`hledger-flow` does not care how the price files got there, it only cares that you should have a separate file per year,
and that it follows the above naming convention.

Here is an example script which downloads prices and follows the naming convention:
<https://gist.github.com/apauley/398fa031c202733959af76b3b8ce8197>

After running an import with available price files you'll see a line has been added to `import/2020-include.journal`:

    include ../prices/2020/prices.journal
