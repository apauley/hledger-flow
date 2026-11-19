# Part 1: try the example. Commands as shown in part-1.md.
c hledger-flow
c mkdir my-finances
c cd my-finances
c git init -q
c hledger-flow init
c git add -A
c git status --short
c 'git commit -q -m "Start a ledger"'
c hledger-flow import
c git add -A
c git status --short
c git diff --staged -- all-years.journal import/gawie/mybank/checking/3-journal
c 'git commit -q -m "Import the example statement"'
c hledger-flow report
c git add -A
c git status --short
c 'cat reports/gawie/*/accounts.txt'
c 'git commit -q -m "Report on the example"'
c hledger-flow status
c git status --short
c mv import/gawie/mybank import/gawie/kudu
c mv import/gawie/kudu/checking import/gawie/kudu/creditcard
c 'rm import/gawie/kudu/creditcard/1-in/*/hledger-flow-example.csv'
c rm import/gawie/kudu/creditcard/mybank-checking.rules
c rm -r import/_manual_ import/gawie/_manual_ import/gawie/kudu/_manual_ import/gawie/kudu/creditcard/_manual_
c 'rm import/gawie/kudu/creditcard/*-opening.journal import/gawie/kudu/creditcard/*-closing.journal'
c rm -r prices reports
c mkdir -p import/gawie/kudu/creditcard/1-in/2016
c cp ../Downloads/Kudu/creditcard_2016-03-31.csv import/gawie/kudu/creditcard/1-in/2016/
c hledger-flow status
c git add -A
c git status --short
c 'git commit -q -m "Replace the example with my Kudu statement"'
