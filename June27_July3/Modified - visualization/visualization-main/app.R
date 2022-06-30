library(bs4Dash)
library(quantmod)
library(dygraphs)
library(shiny)
library(highcharter)
library(dplyr)
library(gapminder)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(plotly)
library(dygraphs)
library(tidyverse)
library(xts)
library(tidyquant)
library(lubridate)


shinyApp(
  ui = dashboardPage(
    title = "Basic Dashboard",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "bs4Dash",
        color = "primary",
        href = "https://www.google.fr",
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE

    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      
      sidebarMenu(
        sidebarHeader("Visualizations"),
        menuItem(
          "Highcharts",
          tabName = "highcharts",
          icon = icon("sliders")
        ),
        menuItem(
          "Dygraphs",
          tabName = "dygraphs",
          icon = icon("id-card")
        ),
        menuItem(
          "Ayush_plots",
          tabName = "ayush_plots",
          icon = icon("id-card")
        ),
        menuItem(
          "dygraphs_Aesthetics",
          tabName = "dygraphs_Aesthetics",
          icon = icon("id-card")
        )
      )
    ),
  
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "highcharts",
          fluidRow(
            column(
              width = 6,
              highchartOutput("pie_chart_2")
              
            ),
            tags$hr(),
            column(width = 6,
                   highchartOutput("pie_chart_3")
                   
            )
            
          ),
          tags$hr(),
          fluidRow(
            column(
              width = 6,
              highchartOutput("pie_chart_4")
            ),
            column(width = 6,
                   highchartOutput("pie_chart_1"))),
          tags$hr(),
          
          fluidRow(
            column(
              width = 6,
              highchartOutput("time_series_1")
            ),
            column(width = 6,
                   highchartOutput("time_series_2"))),
          fluidRow(
            highchartOutput("time_series_3")
            
          )

        ),
        tabItem(
          tabName = "dygraphs",
          
          fluidRow(
            column(
              width = 4,
              dygraphOutput("dygraph_1")
              
            ),
            tags$hr(),
            column(width = 4,
                   dygraphOutput("dygraph_2")
                   
            ),
            column(width = 4,
                   dygraphOutput("dygraph_3"))
            
          ),
          fluidRow(
            column(width = 3,
                   dygraphOutput("dygraph_4")),
            column(width = 3,
                   dygraphOutput("dygraph_5")),
            
            
            column(width = 3,
                   dygraphOutput("dygraph_6")),
            
            column(width = 3,
                   dygraphOutput("dygraph_7"))
            
          )
    
        ),
        tabItem(
          tabName = "ayush_plots",
          fluidRow(
            column(width = 4,
                   dygraphOutput("dygraph_8"))),
          fluidRow(
            column(width = 3,
                   highchartOutput("highchart_9")),
            
            
            column(width = 3,
                   highchartOutput("highchart_10")),
            
            column(width = 3,
                   highchartOutput("highchart_11")),
            column(width = 3,
                   highchartOutput("highchart_12"))
            
          ),
          fluidRow(
            column(width = 4,
                   highchartOutput("highchart_13")
                   ),
            column(width = 4,
                   dygraphOutput("dygraph_9")
                   ),
            column(width = 4,
                   highchartOutput("highchart_14"))
          )
          
        ),
        tabItem(
            tabName = "dygraphs_Aesthetics",
            
            fluidRow(
              column(
                width = 4,
                dygraphOutput("dygraph_10")
                
              ),
              tags$hr(),
              column(width = 4,
                     dygraphOutput("dygraph_11")
                     
              ),
              column(width = 4,
                     dygraphOutput("dygraph_12"))
              
            ),
            fluidRow(
              column(width = 4,
                     dygraphOutput("dygraph_13")),
              column(width = 4,
                     dygraphOutput("dygraph_14")),
              
              
              column(width = 4,
                     dygraphOutput("dygraph_15"))
            ),
            fluidRow(
              column(width = 4,
                     dygraphOutput("dygraph_16")),
              
              column(width = 4,
                     dygraphOutput("dygraph_17"))
            )
          )
        )
      )
  ),
  server = function(input, output) {

    dataset<- read.csv("./Stocks.csv")
    
# Start of the Ayush plots
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
    
    
    
   output$dygraph_8<-renderDygraph({
     #Plotting the data using dygraphs -
     dygraph(finaldata_small, main = "dygraph plot",ylab ="Price",xlab = "Date") %>%
       dyAxis("x", drawGrid = FALSE) %>%
       dyOptions(colors = c('blue','brown','green','red')) %>%
       dyRangeSelector()
     
   }) 
   output$highchart_9<-renderHighchart({
     #Basic 
     highchart(type = "stock") %>% 
       hc_add_series(finaldata_small)
     
     })
   
   output$highchart_10<-renderHighchart({
     highchart(type = "stock") %>% 
       hc_add_series(finaldata_small, type ="ohlc", name = "OHLC")
     
     })
   
   output$highchart_11<-renderHighchart({
     #Line graph - Major disadvantage is that we have to add the lines as individual series unlike dygraphs
     highchart(type = "stock") %>% 
       hc_title(text ="Stocks", align ="center")%>%
       hc_xAxis(title = list(text = "Date")) %>%
       hc_yAxis(title = list(text = "Price")) %>%
       hc_add_series(finaldata_small$OPEN, type = "line", name = "OPEN") %>%
       hc_add_series(finaldata_small$HIGH, type = "line", name = "HIGH") %>%
       hc_add_series(finaldata_small$LOW, type = "line", name = "LOW") %>%
       hc_add_series(finaldata_small$CLOSE, type = "line", name = "CLOSE")
     
     })
   
   output$highchart_12<-renderHighchart({
     
     #Column graph 
     highchart(type = "stock") %>% 
       hc_title(text ="Stocks", align ="center")%>%
       hc_xAxis(title = list(text = "Date")) %>%
       hc_yAxis(title = list(text = "Price")) %>%
       hc_add_series(finaldata_small$OPEN, type = "column", name = "OPEN") %>%
       hc_add_series(finaldata_small$HIGH, type = "column", name = "HIGH") %>%
       hc_add_series(finaldata_small$LOW, type = "column", name = "LOW") %>%
       hc_add_series(finaldata_small$CLOSE, type = "column", name = "CLOSE")
     
     
     })
   
   output$highchart_13<-renderHighchart({
     #Scatter Graph
     highchart(type = "stock") %>% 
       hc_title(text ="Stocks", align ="center")%>%
       hc_xAxis(title = list(text = "Date")) %>%
       hc_yAxis(title = list(text = "Price")) %>%
       hc_add_series(finaldata_small$OPEN, type = "scatter", name = "OPEN") %>%
       hc_add_series(finaldata_small$HIGH, type = "scatter", name = "HIGH") %>%
       hc_add_series(finaldata_small$LOW, type = "scatter", name = "LOW") %>%
       hc_add_series(finaldata_small$CLOSE, type = "scatter", name = "CLOSE")
     
   })

   output$dygraph_9<-renderDygraph({
     
     #Plotting the data using dygraphs -
     dygraph(finaldata_large, main = "Stock Prices",ylab ="Price",xlab = "Date")%>%
       dyAxis("x", drawGrid = FALSE) %>%
       dyOptions(colors = c('blue','brown','green','red')) %>%
       dyRangeSelector()
   })
   
   output$highchart_14<-renderHighchart({
     
     #Plotting using highcharts
     highchart(type = "stock") %>% 
       hc_title(text ="Stocks", align ="center")%>%
       hc_xAxis(title = list(text = "Date")) %>%
       hc_yAxis(title = list(text = "Price")) %>%
       hc_add_series(finaldata_large$OPEN, type = "line", name = "OPEN") %>%
       hc_add_series(finaldata_large$HIGH, type = "line", name = "HIGH") %>%
       hc_add_series(finaldata_large$LOW, type = "line", name = "LOW") %>%
       hc_add_series(finaldata_large$CLOSE, type = "line", name = "CLOSE")
     
   })
   
   
   # End of the Ayush plots
    
 ##############################################################################################################################   
     
# Start to the Dygraphs output render functions     
    getSymbols("SPY")
    getSymbols("AAPL")
  
  
    data1 <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% filter(SYMBOL == "63MOONS") %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))
    data2 <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% filter(SYMBOL == "ACE") %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))
    
    data<- bind_rows(data1,data2)
    
    #Converted to xts based object for using dygraphs
    finaldata <- xts(data[,2:5], order.by= data[,6])
    
output$dygraph_1<-renderDygraph({
  price<-OHLC(AAPL)
  dygraph(price)
  
})

output$dygraph_2<-renderDygraph({

  graph<-dygraph(Cl(SPY), main = "SPY") 
  
  dyShading(graph, from="2007-08-09", 
            to="2017-07-27", color="#FFE6E6")
  
})
    
output$dygraph_3<-renderDygraph({
  AAPL <- tail(AAPL, n=30)
  graph<-dygraph(OHLC(AAPL))
  dyCandlestick(graph)
})

output$dygraph_4<-renderDygraph({
  
  #Simple
  dygraph(finaldata, main = "Simple plot",ylab ="Price",xlab = "Date") %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(colors = c('blue','brown','green','red')) %>%
    dyRangeSelector()
})

output$dygraph_5<-renderDygraph({
  #Step Plot
  dygraph(finaldata, main = "Step Plot",ylab ="Price",xlab = "Date") %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(stepPlot = TRUE, colors = c('blue','brown','green','red')) %>%
    dyRangeSelector()
})
output$dygraph_6<-renderDygraph({
  # Fill Graph
  dygraph(finaldata, main = " Fill Graph",ylab ="Price",xlab = "Date") %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.1,colors = c('blue','brown','green','red')) %>%
    dyRangeSelector()
})
output$dygraph_7<-renderDygraph({
  
  #Point Graph
  dygraph(finaldata, main = "Point Graph",ylab ="Price",xlab = "Date") %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(drawPoints = TRUE, pointSize = 2,colors = c('blue','brown','green','red')) %>%
    dyRangeSelector(height = 20) 
})

# End of the dygraphs output render functions    
##################################################################################################################################
#Start of dygraphs Aesthetics

# We have to convert TIMESTAMP column to date format

data_large <- dataset %>% select(SYMBOL, OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>%
  filter(SYMBOL %in% SYMBOL[1:1000]) %>% mutate(TIMESTAMP = as.Date(TIMESTAMP,'%d-%m-%Y'))

data_large_corr <- data_large %>% select(OPEN, HIGH, LOW, CLOSE, TIMESTAMP) %>% 
  group_by(TIMESTAMP) %>% mutate(OPEN = mean(OPEN), HIGH = mean(HIGH), LOW = mean(LOW), CLOSE = mean(CLOSE) ) %>% distinct()

data_large_df <- as.data.frame(data_large_corr)

#Converting our dataset to xts 
finaldata<- xts(data_large_df[,1:4], order.by = data_large_df[,5])

output$dygraph_10<-renderDygraph({
  #With Grid Lines
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyOptions(colors = brewer.pal(8,"Paired"),  axisLineWidth = 3) %>%
    dyRangeSelector()
  
})

output$dygraph_11<-renderDygraph({
  #Without Grid Lines
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyOptions(colors = brewer.pal(8,"Dark2"), drawGrid = FALSE,  axisLineWidth = 3) %>%
    dyRangeSelector()
  
})

output$dygraph_12<-renderDygraph({
  #RCOLORBREWER
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(colors = brewer.pal(8,"Set1"), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
    dyRangeSelector(fillColor = '#984EA3')
  
  
})

output$dygraph_13<-renderDygraph({
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(colors = brewer.pal(8,"Set2"), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
    dyRangeSelector(fillColor = '#FC8D62')
  
  
})

output$dygraph_14<-renderDygraph({
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(colors = brewer.pal(8,"Dark2"), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
    dyRangeSelector(fillColor = '#E7298A')
  
  
})

output$dygraph_15<-renderDygraph({
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(colors = c('#1F78B4','#33A02C','#FF7F00','#E31A1C'), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
    dyRangeSelector(fillColor = '#E31A1C')
  
  
})

output$dygraph_16<-renderDygraph({
  dygraph(finaldata, main = "Stock Prices",ylab ="Price",xlab = "Date") %>%
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyOptions(colors = c('#ffea4d','#248aff','#24ffba','#E26D5C'), mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
    dyRangeSelector(fillColor = '#E26D5C')
  
})

output$dygraph_17<-renderDygraph({
  dygraph(finaldata, main = "Stock Prices") %>%
    dyHighlight(highlightCircleSize = 1, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
    dyAxis("x", label = "Date", drawGrid = FALSE) %>%
    dyAxis("y", label = "Price") %>%
    dyOptions(colors = c('#FFBC42','#1F78B4','#D81159','#33A02C'), strokeWidth = 1,mobileDisableYTouch = TRUE, axisLineWidth = 3) %>%
    dyRangeSelector(fillColor = '#33A02C')
  
})



#End of dygraphs Aesthetics

######################################################################################################################
    ds <- gapminder%>%
      dplyr::filter(year == max(year))%>%
      dplyr::arrange(-pop)%>%
      head(30)
    
    output$time_series_3<-renderHighchart({
      # Load a demo data
      options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
      
      data("mpg", package = "ggplot2")
      
      # Summary table
      summary.table <- mpg %>% 
        group_by(manufacturer) %>% 
        summarise(
          nb_cars = n(), 
          nb_model = length(unique(model))
        ) %>% 
        arrange(-nb_cars, -nb_model)
      hc <- summary.table %>%
        hchart(
          "treemap", 
          hcaes(x = manufacturer, value = nb_cars, color = nb_model)
        ) %>%
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))%>% hc_title(
          text = "This is a title for Tree Map Graph with different colours <i>margin</i> and <b>Using Highchart</b>",
          
          margin = 20,
          align = "left",
          style = list(color = "#22A884", useHTML = TRUE)
          
        ) %>%
        hc_subtitle(text = "Top 10 countries by population <br>techanswers88",
                    margin = 20,
                    align = "left",
                    style = list(color = "#22A884", useHTML = TRUE)
        )
    })  
    output$time_series_2<-renderHighchart({
      # Load a demo data
      data("mpg", package = "ggplot2")
      
      # Summary table
      summary.table <- mpg %>% 
        group_by(manufacturer) %>% 
        summarise(
          nb_cars = n(), 
          nb_model = length(unique(model))
        ) %>% 
        arrange(-nb_cars, -nb_model)
      
      hc <- summary.table %>%
        hchart(
          "treemap", 
          hcaes(x = manufacturer, value = nb_cars, color = nb_model)
        )%>% hc_title(
          text = "This is a title for Tree Map Graph <i>margin</i> and <b>Using Highchart</b>",
          
          margin = 20,
          align = "left",
          style = list(color = "#22A884", useHTML = TRUE)
          
        ) %>%
        hc_subtitle(text = "Top 10 countries by population <br>techanswers88",
                    margin = 20,
                    align = "left",
                    style = list(color = "#22A884", useHTML = TRUE)
        )
    })
    output$time_series_1<-renderHighchart({
      options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
      data("economics_long", package = "ggplot2")
      # Check automatically if the x column is date class
      economics_long2 <- economics_long %>%
        filter(variable %in% c("pop", "uempmed", "unemploy"))
      economics_long2 %>%
        hchart(
          "line", 
          hcaes(x = date, y = value01, group = variable)
        )%>% hc_title(
          text = "This is a title for Time Series Graph <i>margin</i> and <b>Using Highchart</b>",
          
          margin = 20,
          align = "left",
          style = list(color = "#22A884", useHTML = TRUE)
          
        ) %>%
        hc_subtitle(text = "Top 10 countries by population <br>techanswers88",
                    margin = 20,
                    align = "left",
                    style = list(color = "#22A884", useHTML = TRUE)
        )
      
      
    })
    
    
    output$pie_chart_4<-renderHighchart({
      
      
      highchart() %>%
        hc_chart(type = "column", polar = TRUE) %>% 
        hc_xAxis(categories = ds$country) %>% 
        hc_series(list(
          name = "life exp",
          data = ds$lifeExp,
          colorByPoint = TRUE,
          type = "column",
          colors = c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50"),
          showInLegend = TRUE
        ),
        list(
          name = "life exp",
          data = ds$lifeExp,
          pointPlacement = "on",
          type = "line",
          color = "steelblue",
          showInLegend = TRUE
        )
        )
    })  
    output$pie_chart_3<-renderHighchart({
      
      hc <- ds %>%
        hchart("pie", hcaes(x = country, y = pop),
               name = "Label"
               
        )%>%
        hc_title(
          text = "This is a title for Pie -Chart <i>margin</i> and <b>Strong or bold text</b>",
          
          margin = 20,
          align = "left",
          style = list(color = "#22A884", useHTML = TRUE)
          
        ) %>%
        hc_subtitle(text = "Top 10 countries by population <br>techanswers88",
                    margin = 20,
                    align = "left",
                    style = list(color = "#22A884", useHTML = TRUE)
        ) %>%
        hc_add_theme(hc_theme_ggplot2())
      
      hc
    })
    
    output$pie_chart_1 <- renderHighchart({
      df <- tibble(
        name = c("Animals", "Fruits"),
        y = c(5, 2),
        drilldown = tolower(name)
      )
      
      df
      #> # A tibble: 2 x 3
      #>   name        y drilldown
      #>   <chr>   <dbl> <chr>
      #> 1 Animals     5 animals
      #> 2 Fruits      2 fruits
      
      hc <- highchart() %>%
        hc_title(text = "Basic drilldown") %>%
        hc_xAxis(type = "category") %>%
        hc_legend(enabled = FALSE) %>%
        hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE)
          )
        ) %>%
        hc_add_series(
          data = df,
          type = "pie",
          hcaes(name = name, y = y),
          name = "Things",
          colorByPoint = TRUE
        )
      
      dfan <- data.frame(
        name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
        value = c(4, 3, 1, 2, 1)
      )
      
      dffru <- data.frame(
        name = c("Apple", "Organes"),
        value = c(4, 2)
      )
      
      
      dsan <- list_parse2(dfan)
      
      dsfru <- list_parse2(dffru)
      
      hc <- hc %>%
        hc_drilldown(
          allowPointDrilldown = TRUE,
          series = list(
            list(
              id = "animals",
              type = "pie",
              data = dsan
            ),
            list(
              id = "fruits",
              type = "pie",
              data = dsfru
            )
          )
        )
      hc
      
      
    })
    
    output$pie_chart_2 <- renderHighchart({  
      
      highchart() %>% 
        #     hc_add_series(type = "pie", data = ds, hcaes(ds$country, ds$pop), 
        #                   size = "60%", name = "Browers", center = c(50, 50), 
        #                   dataLabels = list(distance = -50, 
        #                                     formatter = JS("function () {
        #                                                 return this.y > 5 ? this.point.name : null;
        # }"))) %>%
        hc_add_series(type = "pie", data = ds, hcaes(country, pop), size = "80%", name = "Versions", 
                      innerSize = "60%", dataLabels = list(formatter = JS("function () {
      // display only if larger than 1
      return this.y > 1 ? '<b>' + this.point.name + ':</b> ' +
        this.y + '%' : null;
    }"), id = 'versions')) %>% 
        hc_title(text = "Donut chart, January, 2018") %>% 
        hc_subtitle(text = 'Source: <a href="http://statcounter.com" target="_blank">statcounter.com</a>')
      
    })
    
    
    
    
    
  }
)
