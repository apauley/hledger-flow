# Part 6: the last resort. Commands as shown in part-6.md.
c mkdir -p import/gawie/goggatjie/tips/1-in/2016
c cp ../Downloads/Goggatjie/tips_2016-05.json import/gawie/goggatjie/tips/1-in/2016/
c hledger-flow status
c hledger-flow rules gawie/goggatjie/tips
put import/gawie/goggatjie/tips/construct
c chmod +x import/gawie/goggatjie/tips/construct
c import/gawie/goggatjie/tips/construct import/gawie/goggatjie/tips/1-in/2016/tips_2016-05.json -
c hledger-flow import
c hledger balance tips
c hledger-flow status
put prices/2016/USD-ZAR.prices
put prices/2016/MZN-ZAR.prices
put prices/2016/prices.journal
c hledger-flow import
c hledger balance tips --value=then,ZAR
c hledger incomestatement --pretty --period 2016-05 --value=then,ZAR
c hledger-flow status
c git add -A
c 'git commit -q -m "Add the tip jar"'
