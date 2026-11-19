# Part 2: the first statement. Commands as shown in part-2.md.
c hledger-flow rules gawie/kudu/creditcard
c cat import/gawie/kudu/creditcard/kudu-creditcard.rules
put import/gawie/kudu/creditcard/kudu-creditcard.rules
c hledger-flow import
c "echo '.trash/' > .gitignore"
c git add -A
c git status --short
c head -n 7 import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
c 'export LEDGER_FILE="$HOME/my-finances/all-years.journal"'
c hledger balance
c 'git commit -q -m "Import my first Kudu statement"'
