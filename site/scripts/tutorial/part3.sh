# Part 3: categories and reports. Commands as shown in part-3.md.
c "cp ../Downloads/Kudu/creditcard_2016-0[45]*.csv import/gawie/kudu/creditcard/1-in/2016/"
c hledger-flow import
c hledger balance
c git add -A
c 'git commit -q -m "Import April and May"'
put import/gawie/categories.rules
put import/gawie/kudu/creditcard/kudu-creditcard.rules
c hledger-flow import
c hledger balance
c git add -A
c git diff --staged --stat
c git diff --staged -- import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
c 'git commit -q -m "Sort spending into categories"'
c hledger-flow report
c tree reports
c hledger-flow status
c cat reports/gawie/2016/unknown-transactions.txt
c git add -A
c 'git commit -q -m "Generate reports"'
