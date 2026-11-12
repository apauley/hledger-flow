# Part 4: when rules aren't enough. Commands as shown in part-4.md.
c mkdir -p import/gawie/bogart/savings/1-in/2017
c cp ../Downloads/Bogart/123456790_2017-01-27.csv import/gawie/bogart/savings/1-in/2017/
put import/gawie/bogart/bogart-statements.rules part4-before-preprocess
put import/gawie/bogart/cheque/bogart-cheque.rules
put import/gawie/bogart/savings/bogart-savings.rules
put import/gawie/categories.rules
c hledger-flow import
c hledger incomestatement
put import/gawie/bogart/add-year
c chmod +x import/gawie/bogart/add-year
c ln -s ../add-year import/gawie/bogart/cheque/preprocess
c ln -s ../add-year import/gawie/bogart/savings/preprocess
put import/gawie/bogart/bogart-statements.rules
c hledger-flow import
c head -n 9 import/gawie/bogart/savings/2-preprocessed/2017/123456790_2017-01-27.csv
c hledger register bogart:savings
c hledger-flow report
c cat reports/all/2016/transfer-balance.txt
commit 'Bogart savings account'
