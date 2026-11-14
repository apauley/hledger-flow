---
title: Price files
description: Yearly market price journals that are included automatically.
---

Save the year’s [market prices as hledger `P` directives](https://hledger.org/hledger.html#p-directive)
in `prices/<year>/prices.journal`, under the ledger root alongside `import/`.
When import builds that year’s include file under `import/`, it adds the price
file if present. You supply the prices hledger uses to value your holdings.

Here is an example script which downloads prices and follows the naming convention:
<https://gist.github.com/apauley/398fa031c202733959af76b3b8ce8197>

After running an import with available price files you'll see a line has been added to `import/2020-include.journal`:

    include ../prices/2020/prices.journal
