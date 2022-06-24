library(dygraphs)
library(highcharter)
library(tidyverse)
library(xts)
library(tidyquant)
library(lubridate)

dataset<- read.csv("June20_June26/Stocks.csv")


#Plotting on small dataset
#------------------------------------------------------------------------------------------------------------------------------
#Filtering out the required data.
data_small <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% 
  filter(SYMBOL %in% SYMBOL[1:2]) %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))

#Now since we are considering more than one stocks, so we have to take the average value of the  OHLC over all of the selected stocks
#otherwise the cumulated price on each day for every stock will appear on the plot.
#So, we take the mean values over all the stocks on the selected day and choose distinct rows only.
data_small_corr = data_small %>% select(OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% 
  group_by(TIMESTAMP) %>% mutate(OPEN = mean(OPEN), HIGH = mean(HIGH), LOW = mean(LOW), CLOSE = mean(CLOSE) ) %>% distinct()

# Applying the above functions changes the type to tibble. So, we convert it back to data frame.
data_small_df <- as.data.frame(data_small_corr)
  
#Converting our dataset to xts 
finaldata_small<- xts(data_small_df[,1:4], order.by = data_small_df[,5])

#Plotting the data using dygraphs -
dygraph(finaldata_small, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = c('blue','brown','green','red')) %>%
  dyRangeSelector()


#Plotting using highcharts
#Basic 
highchart(type = "stock") %>% 
  hc_add_series(finaldata_small)

#For OHLC data
highchart(type = "stock") %>% 
  hc_add_series(finaldata_small, type ="ohlc", name = "OHLC")

#Line graph - Major disadvantage is that we have to add the lines as individual series unlike dygraphs
highchart(type = "stock") %>% 
  hc_title(text ="Stocks", align ="center")%>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Price")) %>%
  hc_add_series(finaldata_small$OPEN, type = "line", name = "OPEN") %>%
  hc_add_series(finaldata_small$HIGH, type = "line", name = "HIGH") %>%
  hc_add_series(finaldata_small$LOW, type = "line", name = "LOW") %>%
  hc_add_series(finaldata_small$CLOSE, type = "line", name = "CLOSE")

#Column graph 
highchart(type = "stock") %>% 
  hc_title(text ="Stocks", align ="center")%>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Price")) %>%
  hc_add_series(finaldata_small$OPEN, type = "column", name = "OPEN") %>%
  hc_add_series(finaldata_small$HIGH, type = "column", name = "HIGH") %>%
  hc_add_series(finaldata_small$LOW, type = "column", name = "LOW") %>%
  hc_add_series(finaldata_small$CLOSE, type = "column", name = "CLOSE")

#Scatter Graph
highchart(type = "stock") %>% 
  hc_title(text ="Stocks", align ="center")%>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Price")) %>%
  hc_add_series(finaldata_small$OPEN, type = "scatter", name = "OPEN") %>%
  hc_add_series(finaldata_small$HIGH, type = "scatter", name = "HIGH") %>%
  hc_add_series(finaldata_small$LOW, type = "scatter", name = "LOW") %>%
  hc_add_series(finaldata_small$CLOSE, type = "scatter", name = "CLOSE")



#Plotting on large dataset
#----------------------------------------------------------------------------------------------------------------------------
#Filtering out the required data.
data_large <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>%
  filter(SYMBOL %in% SYMBOL[1:100]) %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))

data_large_corr <- data_large %>% select(OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% 
  group_by(TIMESTAMP) %>% mutate(OPEN = mean(OPEN), HIGH = mean(HIGH), LOW = mean(LOW), CLOSE = mean(CLOSE) ) %>% distinct()

data_large_df <- as.data.frame(data_large_corr)

#Converting our dataset to xts 
finaldata_large<- xts(data_large_df[,1:4], order.by = data_large_df[,5])


#Plotting the data using dygraphs -
dygraph(finaldata_large, main = "Stock Prices",ylab ="Price",xlab = "Date")%>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(colors = c('blue','brown','green','red')) %>%
  dyRangeSelector()


#Plotting using highcharts
highchart(type = "stock") %>% 
  hc_title(text ="Stocks", align ="center")%>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Price")) %>%
  hc_add_series(finaldata_large$OPEN, type = "line", name = "OPEN") %>%
  hc_add_series(finaldata_large$HIGH, type = "line", name = "HIGH") %>%
  hc_add_series(finaldata_large$LOW, type = "line", name = "LOW") %>%
  hc_add_series(finaldata_large$CLOSE, type = "line", name = "CLOSE")


