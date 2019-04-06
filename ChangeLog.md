# Changelog for [hledger-makeitso](https://github.com/apauley/hledger-makeitso)

## 0.9.0.1

First hackage release. Minor changes to fix `stack sdist` warnings and errors, in preperation of
the hackage upload.

1bf817c "Merge pull request #9 from apauley/hackage-upload" Mar 31 21:51:38 2019 +0200

## 0.9

Process all statements in parallel.

This has a significant speed improvement on multi-processor machines when dealing with lots of input files.
a906bb5 "Merge pull request #8 from apauley/parallel-import" 2019-03-27 22:45:23 +0200

## 0.8

Generate an all-years.journal on each level which includes all the available years for that level. Replace the old makeitso.journal with the top-level version of this:

06f2127 "Merge pull request #5 from apauley/all-years-includes" 2019-03-22 00:09:27 +0200

## 0.7

Change the way include files are aggregated.

It used to be by owner/bank/account, now each of those levels (owner/bank/account) are aggregated per year:
eb17fed "Merge pull request #3 from apauley/annual-includes" 2019-03-12 23:09:17 +0200

## 0.6

61c71d6 "Upgrade to lts-13.9 (GHC 8.6.3)" 2019-03-01 11:31:23 +0200

## 0.5

3a7a39e "Upgrade to lts-13.6 (GHC 8.6.3)" 2019-02-16 09:54:49 +0200

## 0.4

213552d "Upgrade to lts-12.16 (GHC 8.4.4)" 2018-11-03 20:00:21 +0200

## 0.3

5e2d45f "Update from lts-12.1 to lts-12.11" 2018-10-01 23:07:21 +0200

## 0.2

First support for the construct script, when it was confusingly named an import script:

24ac4c7 "Support a fully custom import script" 2018-09-16 16:11:53 +0200

## 0.1

The first semi-useful version, replacing a previous bash script:

131f8af "Write journal" 2018-07-23 16:08:44 +0200
