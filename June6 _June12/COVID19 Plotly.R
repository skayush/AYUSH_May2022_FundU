library(tidyverse)
library(plotly)

dataset = read.csv("June6 _June12/COVID19_line_list_data.csv")
View(dataset)

# Filtering out dataset for no. of males and females countrywise
male<- dataset%>% group_by(country,gender) %>% filter(gender == "male") %>% summarize(count=n()) 
female<- dataset%>% group_by(country,gender) %>% filter(gender == "female") %>% summarize(count=n())  


fig1 <- male %>% plot_ly(x = ~country, y = ~count, type = 'bar',color = I("blue"))
fig1

fig2 <- female %>% plot_ly(x = ~country, y = ~count, type = 'bar', fill = I("red"))
fig2


fig3<- plot_ly(data = male,x = ~country, y = ~count, type = 'bar', name = "Male") %>% add_trace(data = female,y = ~count, name ="Female") %>%layout(barmode = 'group')
fig3

