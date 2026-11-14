---
title: Using Ledger
description: Use ledger-cli alongside hledger on the journals hledger-flow generates.
---

hledger-flow uses hledger for imports and reports. To query the generated
journals with Ledger, try `ledger -f all-years.journal balance` from the
ledger root. Ledger reads the journal entries and include directives.
Keep any handwritten directives compatible with both programs.
[hledger and Ledger compatibility notes](https://hledger.org/ledger.html)

hledger-flow includes an account’s statement journals in filename order.
Ledger checks a balance assertion at the point it reads the posting, so
out-of-order statements can cause a valid bank balance to fail the check.
Names starting with `YYYY-MM` put monthly statements in chronological order.
Ledger’s `--permissive` option suppresses assertion errors, including ones
caused by a real discrepancy. [Ledger balance verification](https://ledger-cli.org/doc/ledger3.html#Balance-verification)
