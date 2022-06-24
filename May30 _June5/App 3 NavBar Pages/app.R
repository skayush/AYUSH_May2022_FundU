#Using NavBarPages

library(shiny)         

ui <- navbarPage("My Application",
                 tabPanel("Pane 1",
                          selectInput("Distribution", "Please Select Distribution Type",
                                      choices = c("Normal", "Exponential")),
                          sliderInput("sampleSize", "Please Select Sample Size",
                                      min = 100, max = 5000, value = 1000, step = 100),
                          conditionalPanel(condition = "input.Distribution == 'Normal'",
                                           textInput("Mean", "Please Select the mean", 10),
                                           textInput("SD", "Please Select Standard Deviation", 3)),
                          conditionalPanel(condition = "input.Distribution == 'Exponential'",
                                           textInput("Lambda", "Please Select Lambda:",1)),
                          
                          
                          mainPanel(
                            plotOutput("plot")
                          )
                          
                 ),
                 
                 tabPanel("Pane 2"),
                 navbarMenu("More",
                            tabPanel("Pane 2A"),
                            tabPanel("Pane 2B")),
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    distType <- input$Distribution
    size <- input$sampleSize
    
    if(distType == "Normal"){
      randomVec <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$SD))
    }
    else{
      randomVec <- rexp(size, rate = 1/as.numeric(input$Lambda))
    }
    
    hist(randomVec, col = "green")
  })
  
}

shinyApp(ui, server)