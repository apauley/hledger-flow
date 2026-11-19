---
title: The construct script
description: Generate journal entries yourself instead of using CSV rules.
---

For most downloaded CSVs, [hledger's CSV rules](/reference/rules-files/) can generate
the postings directly. Reserve `construct` for cases where you need to
generate journal entries yourself, for example when you already have a
converter that produces a journal for a specialised statement format.
With a construct script you maintain the logic for generating and balancing the
postings. You can use it on its own or after `preprocess` if both steps
are needed.

Save your `construct` script in the account directory, e.g.
`import/john/mybank/savings/construct`.

`hledger-flow` will call your `construct` script with 5 positional
parameters:

1.  The path to the preprocessed CSV if one is in use, otherwise the original
    statement in `1-in/<year>/`
2.  A "-" (indicating that output should be sent to `stdout`)
3.  The name of the bank, e.g. `mybank`
4.  The name of the account, e.g. `savings`
5.  The name of the owner, e.g. `john`

Your `construct` script is expected to:

- read the input file
- generate your own `hledger` journal transactions
- be idempotent. Running `construct` multiple times on the same files
  should produce the same result.
- send all journals to `stdout`. `hledger-flow` will pipe your standard output into
  `hledger` which will format it and save it to an output file.

hledger-flow replaces the journal in `3-journal/<year>/` only after both
the construct script and hledger finish successfully. If either fails, the
previous journal stays in place. This conversion runs hledger with
`--ignore-assertions`. To check balance assertions with the opening balances
and other statements present, run `hledger -f all-years.journal check` after import.

You can still use `stderr` in your construct script for any other output that you may want to see.
hledger-flow shows it once the script finishes.

`hledger-flow import --verbose` shows each run as one pipeline, ready to paste
into a shell at the base directory: your script, then the hledger command that reads its output.
The pipeline names the final journal in `3-journal/<year>/`, so a pasted command
writes that journal directly. If the script or hledger exits with an error,
import stops with that program's exit code and shows its error output under its name.

## Stability

Stable and tested.
