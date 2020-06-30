# 01-kmeans-app
library(shiny)
setwd("/Users/wangqi/Desktop/Github/dashboard-Rshiny/")
df <- read.csv("sample_data.csv")

ui <- fluidPage(
  headerPanel('Dashborad with R-shiny'),
  selectInput('selectedK', 'K Variable', unique(df$K)),
  sidebarPanel(
    radioButtons('ycol', 'Y Variable', choices = c("Y", "Z"), 
                 selected = "Y")
    
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    df[df$K == input$selectedK, c("X", input$ycol)]
  })
  
  output$plot1 <- renderPlot({
    plot(selectedData(), main = input$ycol)
  })
  
}

shinyApp(ui = ui, server = server)
