#Different Plots with Navigation Bar on Iris Dataset

library(shiny)
library(shinythemes)
library(ggplot2)

iris <- iris



ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Shiny Page",
                  tabPanel("Main Tab",
                           
                           sidebarPanel(
                             selectInput(
                               inputId = "SelectPlot",
                               label = "TypeofPlot",
                               choices = c("Scatter","Jitter","Line"),
                               selected = "Scatter"
                             ),
                             
                             sliderInput(
                               inputId="Shape", 
                               label="Shape of Plot", 
                               min=1,
                               max=20, 
                               step=1,
                               value=1
                             ),
                             
                           ),
                           
                           
                           mainPanel(
                             h1("Output"),
                             h4("Plot"),
                             plotOutput("distPlot"),
                           ),
                           
                           
                  ),
                  tabPanel("Tab2","Arriving Soon"),
                  tabPanel("Tab3","Arriving Soon")
                )
)


server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$SelectPlot =="Scatter"){
      a <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) + geom_point(shape=input$Shape)
    }
    
    if(input$SelectPlot =="Jitter"){
      a <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) + geom_jitter(shape=input$Shape)
    }
    
    
    
    if(input$SelectPlot =="Line"){
      a <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) + geom_line(col=input$Shape)
    }
    
    a
  })
}




shinyApp(ui, server)
