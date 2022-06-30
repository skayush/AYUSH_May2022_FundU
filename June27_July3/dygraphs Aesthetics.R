library(dygraphs)
library(tidyverse)
library(xts)
library(tidyquant)
library(lubridate)
library(RColorBrewer)
library(plotly)

dataset<- read.csv("https://raw.githubusercontent.com/skayush/AYUSH_May2022_FundU/main/June27_July3/Stocks.csv")


# We have to convert TIMESTAMP column to date format

data_large <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>%
  filter(SYMBOL %in% SYMBOL[1:1000]) %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))

data_large_corr <- data_large %>% select(OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% 
  group_by(TIMESTAMP) %>% mutate(OPEN = mean(OPEN), HIGH = mean(HIGH), LOW = mean(LOW), CLOSE = mean(CLOSE) ) %>% distinct()

data_large_df <- as.data.frame(data_large_corr)

#Converting our dataset to xts 
finaldata<- xts(data_large_df[,1:4], order.by = data_large_df[,5])



#Simple
#Colours according to available RColorBrewer templates

#With Grid Lines
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyOptions(colors = brewer.pal(8,"Paired")) %>%
  dyRangeSelector()

#Without Grid Lines
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyOptions(colors = brewer.pal(8,"Dark2"), drawGrid = FALSE) %>%
  dyRangeSelector()


#Along with custom colours, series highlighting, MobileDisableYTouch etc.

#RCOLORBREWER
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyHighlight(highlightCircleSize = 1, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = brewer.pal(8,"Set1"), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
  dyRangeSelector(fillColor = '#984EA3')




dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyHighlight(highlightCircleSize = 1, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = brewer.pal(8,"Set2"), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
  dyRangeSelector(fillColor = '#FC8D62')


dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyHighlight(highlightCircleSize = 1, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = brewer.pal(8,"Dark2"), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
  dyRangeSelector(fillColor = '#E7298A')



#CUSTOM COlours
dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyHighlight(highlightCircleSize = 1, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = c('#1F78B4','#33A02C','#FF7F00','#E31A1C'), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
  dyRangeSelector(fillColor = '#E31A1C')



dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyHighlight(highlightCircleSize = 1, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = c('#ffea4d','#248aff','#24ffba','#E26D5C'), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
  dyRangeSelector(fillColor = '#E26D5C')



dygraph(finaldata, main = "Stock Prices") %>%
  dyHighlight(highlightCircleSize = 1, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyAxis("x", label = "Date", drawGrid = FALSE) %>%
  dyAxis("y", label = "Price") %>%
  dyOptions(colors = c('#FFBC42','#1F78B4','#D81159','#33A02C'), strokeWidth = 1,mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
  dyRangeSelector(fillColor = '#33A02C')


