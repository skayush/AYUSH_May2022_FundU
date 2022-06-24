library(dygraphs)
library(tidyverse)
library(xts)
library(tidyquant)
library(lubridate)
library(highcharter)

dataset<- read.csv("June13_June19/Stocks.csv")


# We have to convert TIMESTAMP column to date format

data1 <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% filter(SYMBOL == "63MOONS") %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))
data2 <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% filter(SYMBOL == "ACE") %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))

data<- bind_rows(data1,data2)

#Converted to xts based object for using dygraphs
finaldata <- xts(data[,2:5], order.by= data[,6])

#Plotting using dygraphs

#Simple
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = c('blue','brown','green','red')) %>%
  dyRangeSelector()

#Step Plot
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(stepPlot = TRUE, colors = c('blue','brown','green','red')) %>%
  dyRangeSelector()

# Fill Graph
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.1,colors = c('blue','brown','green','red')) %>%
  dyRangeSelector()

#Point Graph
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,colors = c('blue','brown','green','red')) %>%
  dyRangeSelector(height = 20)






