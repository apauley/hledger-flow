# Part 2: categories and reports. Commands as shown in part-2.md.
c "cp ../Downloads/Kudu/creditcard_2016-0[45]*.csv import/gawie/kudu/creditcard/1-in/2016/"
c hledger-flow import
c hledger balance
put import/gawie/categories.rules
put import/gawie/kudu/creditcard/kudu-creditcard.rules
c hledger-flow import
c hledger balance
c hledger-flow report
c tree reports
c cat reports/gawie/2016/unknown-transactions.txt
commit 'Categories'
