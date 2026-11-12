---
title: "Part 1: The first CSV statement"
description: Set up the directories, a preprocess script and a rules file for a first statement.
---

This is the first part in a a series of step-by-step instructions.

They are intended to be read in sequence. See [all parts of the tutorial](/tutorial/).

## About This Document

This document started out as an Emacs org-mode literate program: the output shown
below each command was produced by running it. It is also available
[as a slide show](https://pauley.org.za/hledger-flow/).

## The Story of an African Chef

Meet Gawie de Groot. Gawie is a successful [mopane worm](https://en.wikipedia.org/wiki/Gonimbrasia_belina#As_food) chef, based in the northernmost section of the [Kruger National Park](https://en.wikipedia.org/wiki/Kruger_National_Park).

![](./img/mopane-worm-meal.jpg)

In times gone by he used to work in a big city as an IT professional.
But after a few years he decided to choose a quieter life in the bushveld.

He now works at a renowned bush restaurant aptly named the "`Grillerige Groen Goggatjie`".

Demand for his traditional African cuisine has sky-rocketed, with visitors from all over Zimbabwe, Mozambique and South Africa
coming to enjoy his dishes.

Business has been good the last few years, but Gawie wants to make sure that he'll also be OK when business is down.

It is time for Gawie to take charge of his finances.

## The First CSV Statement

Gawie followed the [build instructions](/quickstart/#build-from-source) and ended up with his very own `hledger-flow` executable.

Let's just run this `hledger-flow` command and see what happens…

``` bash
hledger-flow
```

    An hledger workflow focusing on automated statement import and classification:
    https://github.com/apauley/hledger-flow#readme

    Usage: hledger-flow ([-v|--verbose] [-H|--hledger-path HLEDGER-PATH] 
                          [--show-options] [--batch-size SIZE] [--sequential] 
                          (import | report) |
                          (-V|--version))

    Available options:
      -h,--help                Show this help text
      -v,--verbose             Print more verbose output
      -H,--hledger-path HLEDGER-PATH
                               The full path to an hledger executable
      --show-options           Print the options this program will run with
      --batch-size SIZE        Parallel processing of files are done in batches of
                               the specified size. Default: 200. Ignored during
                               sequential processing.
      --sequential             Disable parallel processing
      -V,--version             Display version information

    Available commands:
      import                   Uses hledger with your own rules and/or scripts to convert electronic statements into categorised journal files
      report                   Generate Reports

Well OK, at least it prints some helpful output.

Gawie wants to import his CSV statements.

Luckily the `import` subcommand has a bit of help text as well:

``` bash
hledger-flow import --help
```

```
Usage: hledger-flow import [DIR] [--start-year YEAR] [--new-files-only]
  Uses hledger with your own rules and/or scripts to convert electronic statements into categorised journal files

Available options:
  DIR                      The directory to import. Use the base directory for a
                           full import or a sub-directory for a partial import.
                           Defaults to the current directory. This behaviour is
                           changing: see --enable-future-rundir
  --start-year YEAR        Import only from the specified year and onwards,
                           ignoring previous years. By default all available
                           years are imported. Valid values include a 4-digit
                           year or 'current' for the current year
  --new-files-only         Skip regenerating output files that are at least as
                           new as their source. This applies to preprocessed files
                           and hledger journal files.
  -h,--help                Show this help text
```

Gawie starts by creating a new directory specifically for his hledger journals:

``` bash
mkdir my-finances
```

Now he can point the import to his new finances directory:

``` bash
hledger-flow import ./my-finances
```

Hmmm, an error:

```
hledger-flow: Unable to find an import directory at "./my-finances/" (or in any of its parent directories).

Have a look at the documentation for more information:
https://github.com/apauley/hledger-flow#getting-started
```

Gawie carefully interprets the error message using the skills he obtained during his years as an IT professional.

He concludes that `hledger-flow` expects to find its input files in specifically named directories.

Looking at the [documentation](/reference/input-files/) he sees there should be several account and bank-specific directories
under the `import` directory.

Gawie's salary is deposited into his cheque account at `Bogart Bank`, so this seems like a good account to start with:

``` bash
mkdir -p my-finances/import/gawie/bogart/cheque/1-in/2016/
cp Downloads/Bogart/123456789_2016-03-30.csv \
  my-finances/import/gawie/bogart/cheque/1-in/2016/
```

Let's see what our tree structure looks like now:

``` bash
tree my-finances/
```

```
my-finances/
`-- import
    `-- gawie
        `-- bogart
            `-- cheque
                `-- 1-in
                    `-- 2016
                        `-- 123456789_2016-03-30.csv

6 directories, 1 file
```

It is time to add what we have to source control.

``` bash
cd my-finances/
git init .
git add .
git commit -m 'Initial commit'
cd ..
```

Let's try the import again:

``` bash
hledger-flow import ./my-finances
```

```
I couldn't find an hledger rules file while trying to import
import/gawie/bogart/cheque/1-in/2016/123456789_2016-03-30.csv

I will happily use the first rules file I can find from any one of these 2 files:
import/gawie/bogart/cheque/bogart-cheque.rules
import/bogart.rules

Here is a bit of documentation about rules files that you may find helpful:
https://github.com/apauley/hledger-flow#rules-files
```

Another cryptic error.

This one is caused by a missing [rules file](/reference/rules-files/).

After looking through the [hledger documentation on CSV rules files](http://hledger.org/csv.html),
Gawie concludes that the dates in Bogart Bank's CSV statement is incompatible with basic logic, reason and decency.

Luckily he isn't the only one suffering at the hands of bureaucratic incompetence: someone else has already written [a script](https://github.com/apauley/fnb-csv-demoronizer) to
fix stupid dates like those used by Bogart Bank.

This looks like a job for a [preprocess script](/reference/preprocess/).

Gawie adds the CSV transformation script as a submodule to his repository:

``` bash
cd my-finances/
git submodule add https://github.com/apauley/fnb-csv-demoronizer.git
git commit -m 'Added submodule: fnb-csv-demoronizer'
cd ..
```

`hledger-flow` looks for a file named [preprocess](/reference/preprocess/) in the account directory.

Gawie just creates a symbolic link named `preprocess`.
This works because the downloaded script takes an input file and an output file as the first two positional arguments,
very much as the `preprocess` script would expect.
And luckily it ignores the other parameters that `hledger-flow` sends through.

``` bash
cd my-finances/import/gawie/bogart/cheque
ln -s ../../../../fnb-csv-demoronizer/fnb-csv-demoronizer preprocess
```

Now when we try the import again, it still displays an error due to our missing rules file:

``` bash
hledger-flow import ./my-finances
```

```
I couldn't find an hledger rules file while trying to import
import/gawie/bogart/cheque/2-preprocessed/2016/123456789_2016-03-30.csv

I will happily use the first rules file I can find from any one of these 2 files:
import/gawie/bogart/cheque/bogart-cheque.rules
import/bogart.rules

Here is a bit of documentation about rules files that you may find helpful:
https://github.com/apauley/hledger-flow#rules-files
```

This time we can see that our statement was preprocessed despite the rules file error:

``` bash
head -n 2 my-finances/import/gawie/bogart/cheque/2-preprocessed/2016/123456789_2016-03-30.csv
```

```
"5","'Nommer'","'Datum'","'Beskrywing1'","'Beskrywing2'","'Beskrywing3'","'Bedrag'","'Saldo'","'Opgeloopte Koste'"
"5","1","2016-03-01","#Monthly Bank Fee","","","-500.00","40000.00",""
```

Time for another git checkpoint.

``` bash
cd my-finances/
git add .
git commit -m 'The preprocessed CSV now has dates we can work with!'
cd ..
```

Now that we have sane dates in a CSV file, let's try to create a [rules file](http://hledger.org/manual.html#csv-rules):

``` hledger
skip 1

fields _, _, date, desc1, desc2, desc3, amount, balance, _

currency R
status *

account1 Assets:Current:Gawie:Bogart:Cheque
description %desc1/%desc2/%desc3
```

Gawie saves this file as `my-finances/import/gawie/bogart/cheque/bogart-cheque.rules`.

Time for another git checkpoint.

``` bash
cd my-finances/
git add .
git commit -m 'A CSV rules file'
cd ..
```

This time the import is successful, and we see a number of newly generated files:

``` bash
hledger-flow import ./my-finances
tree my-finances
```

```
Wrote include files for 1 journals in 0.003716s
Imported 1/1 journals in 0.101596s
my-finances
|-- all-years.journal
|-- fnb-csv-demoronizer
|   |-- README.org
|   `-- fnb-csv-demoronizer
`-- import
    |-- 2016-include.journal
    |-- all-years.journal
    `-- gawie
        |-- 2016-include.journal
        |-- all-years.journal
        `-- bogart
            |-- 2016-include.journal
            |-- all-years.journal
            `-- cheque
                |-- 1-in
                |   `-- 2016
                |       `-- 123456789_2016-03-30.csv
                |-- 2-preprocessed
                |   `-- 2016
                |       `-- 123456789_2016-03-30.csv
                |-- 2016-include.journal
                |-- 3-journal
                |   `-- 2016
                |       `-- 123456789_2016-03-30.journal
                |-- all-years.journal
                |-- bogart-cheque.rules
                `-- preprocess -> ../../../../fnb-csv-demoronizer/fnb-csv-demoronizer

11 directories, 16 files
```

Bogart Bank's CSV file has been transformed into an `hledger` journal file.

This is the first transaction in the file:

``` bash
head -n 3 my-finances/import/gawie/bogart/cheque/3-journal/2016/123456789_2016-03-30.journal
```

```
2016-03-01 * #Monthly Bank Fee//
    Assets:Current:Gawie:Bogart:Cheque        R-500.00 = R40000.00
    expenses:unknown                           R500.00
```

A final checkpoint and we're done with part 1.

``` bash
cd my-finances/
git add .
git commit -m 'My first imported journal'
cd ..
```

The story continues with [part 2](/tutorial/part-2/).
