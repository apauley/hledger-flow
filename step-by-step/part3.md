# Part 3

This is the third part in a a series of step-by-step instructions.

They are intended to be read in sequence. Head over to the [docs README](README.md) to see all parts.

# About This Document

This document started out as an Emacs org-mode literate program: the output shown
below each command was produced by running it. It is also available
[as a slide show](https://pauley.org.za/hledger-flow/part3.html).

# Adding More Statements

Now that the all the boilerplate for the first statement has been done,
adding some more should be easy:

``` bash
cp -f Downloads/Bogart/123456789_2016*.csv \
  my-finances/import/gawie/bogart/cheque/1-in/2016/
hledger-flow import ./my-finances
hledger -f my-finances/all-years.journal incomestatement \
  --pretty-tables --monthly --average --begin 2016-03-01
```

```
Found 3 input files in 0.002017s. Proceeding with import...
Wrote include files for 3 journals in 0.004568s
Imported 3/3 journals in 0.109403s
Income Statement 2016-03-01..2016-05-31

                  ║       Mar        Apr        May    Average 
══════════════════╬════════════════════════════════════════════
 Revenues         ║                                            
──────────────────╫────────────────────────────────────────────
 income:unknown   ║ R37256.28  R37256.28  R37256.28  R37256.28 
──────────────────╫────────────────────────────────────────────
                  ║ R37256.28  R37256.28  R37256.28  R37256.28 
══════════════════╬════════════════════════════════════════════
 Expenses         ║                                            
──────────────────╫────────────────────────────────────────────
 expenses:unknown ║ R37284.66  R40333.49  R37234.43  R38284.19 
──────────────────╫────────────────────────────────────────────
                  ║ R37284.66  R40333.49  R37234.43  R38284.19 
══════════════════╬════════════════════════════════════════════
 Net:             ║   R-28.38  R-3077.21     R21.85  R-1027.91 
```

Actually this doesn't look so good.
In March and April, Gawie spent more than he earned.

It is time to classify each transaction so that he can have a better view into
what is going on.

And the new statements gets added to the repository.

``` bash
cd my-finances/
git add .
git commit -m 'Added a few more statements'
cd ..
```
