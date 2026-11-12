---
title: The preprocess script
description: Clean up or convert a statement before hledger reads it.
---

Most bank CSVs can go straight through [hledger's CSV rules](/reference/rules-files/).
Add a `preprocess` script only when the input needs work those rules cannot
do, such as converting a non-CSV statement or repairing malformed rows
before hledger can read them. The script becomes part of every rebuild for
that account, including imports of older statements.

If you put a script called `preprocess` in the account directory, e.g.
`import/john/mybank/savings/preprocess`, then `hledger-flow` will call
that script for each input statement.

The `preprocess` script will be called with 5 positional parameters:

1.  The path to the input statement, e.g.
    `import/john/mybank/savings/1-in/2018/123456789_2018-06-30.csv`
2.  A temporary output path. Its filename and extension match the final CSV,
    e.g. `123456789_2018-06-30.csv`. Write to the supplied path directly.
    Its parent is a temporary directory, so don't derive the year or any
    other information from the output path.
3.  The name of the bank, e.g. `mybank`
4.  The name of the account, e.g. `savings`
5.  The name of the owner, e.g. `john`

Your `preprocess` script is expected to:

- read the input file
- write a new output file at the supplied path that works with your
  rules file
- be idempotent. Running `preprocess` multiple times on the same files
  will produce the same result.

After the script succeeds, hledger-flow moves its output into
`2-preprocessed/<year>/`. If the script fails, any previous CSV stays in
place. A failed first run leaves no cached CSV.

If you remove the hook, import can still use its cached CSV while that CSV
is at least as new as the original statement. If you then correct the
statement and it is newer than the cache, both a full import and
`import --new-files-only` stop and name the two files.

Restore the hook if the statement still needs conversion. If the original
statement is ready to import directly, move the named CSV out of
`2-preprocessed/<year>/`. In either case, run `hledger-flow import` to
rebuild the journal.

## Stability

Stable and tested.
