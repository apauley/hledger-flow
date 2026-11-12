# Part 5: the last resort. Commands as shown in part-5.md.
c mkdir -p import/gawie/goggatjie/tips/1-in/2016
c cp ../Downloads/Goggatjie/tips_2016-05.json import/gawie/goggatjie/tips/1-in/2016/
c hledger-flow status
c hledger-flow status --rules-template gawie/goggatjie/tips
put import/gawie/goggatjie/tips/construct
c chmod +x import/gawie/goggatjie/tips/construct
c import/gawie/goggatjie/tips/construct import/gawie/goggatjie/tips/1-in/2016/tips_2016-05.json -
c hledger-flow import
c hledger balance tips
put prices/2016/prices.journal
c hledger-flow import
c hledger balance tips --value=then,R
c hledger incomestatement --pretty --period 2016-05 --value=then,R
c hledger-flow status
commit 'Tips'
