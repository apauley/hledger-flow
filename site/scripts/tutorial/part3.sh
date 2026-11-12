# Part 3: a difficult statement. Commands as shown in part-3.md.
c mkdir -p import/gawie/bogart/cheque/1-in/2016
c "cp ../Downloads/Bogart/123456789_2016-0*.csv import/gawie/bogart/cheque/1-in/2016/"
c hledger-flow status
c hledger-flow status --rules-template gawie/bogart/cheque
c head -n 9 import/gawie/bogart/cheque/1-in/2016/123456789_2016-03-30.csv
put import/gawie/bogart/cheque/bogart-cheque.rules
put import/gawie/categories.rules
c hledger-flow import
c head -n 7 import/gawie/bogart/cheque/3-journal/2016/123456789_2016-03-30.journal
c hledger incomestatement
put import/gawie/bogart/cheque/2016-opening.journal
c hledger-flow import
c hledger incomestatement --monthly --pretty
c hledger balancesheet --pretty
commit 'Bogart cheque account'
# The exercise's answer, so it's tested too.
put import/gawie/kudu/creditcard/kudu-creditcard.rules part3-exercise
q hledger-flow import
q "grep -q '^2016-05-03 Punda Maria Trading Store$' import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-05-31.journal"
commit 'Exercise'
