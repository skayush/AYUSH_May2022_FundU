library(quantmod)
library(dygraphs)

getSymbols("AAPL")
head(AAPL)

getSymbols("SPY")
head(SPY)

price<-OHLC(AAPL)

dygraph(price)

graph<-dygraph(Cl(SPY), main = "SPY") 
dyShading(graph, from="2007-08-09", 
          to="2017-07-27", color="#FFE6E6")


AAPL <- tail(AAPL, n=30)
graph<-dygraph(OHLC(AAPL))
dyCandlestick(graph)