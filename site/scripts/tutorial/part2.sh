# Part 2: the first statement. Commands as shown in part-2.md.
c hledger-flow rules gawie/kudu/creditcard
c cat import/gawie/kudu/creditcard/kudu-creditcard.rules
put import/gawie/kudu/creditcard/kudu-creditcard.rules
c "echo 'commodity 1,000.00 ZAR  ; alias: R' >> directives.journal"
c hledger-flow import
c git add -A
c git status --short
c git diff --staged -- directives.journal
c head -n 7 import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
c 'export LEDGER_FILE="$HOME/finances/all-years.journal"'
c hledger balance
c 'git commit -q -m "Import my first Kudu statement"'
