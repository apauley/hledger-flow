# Part 1: the first statement. Commands as shown in part-1.md.
c hledger-flow
c mkdir my-finances
c cd my-finances
c hledger-flow init
q git init -q .
c mkdir -p import/gawie/kudu/creditcard/1-in/2016
c cp ../Downloads/Kudu/creditcard_2016-03-31.csv import/gawie/kudu/creditcard/1-in/2016/
c hledger-flow status
c hledger-flow status --rules-template gawie/kudu/creditcard
put import/gawie/kudu/creditcard/kudu-creditcard.rules
c hledger-flow import
c tree
c head -n 7 import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
c 'export LEDGER_FILE="$HOME/my-finances/all-years.journal"'
c hledger balance
commit 'My first statement'
