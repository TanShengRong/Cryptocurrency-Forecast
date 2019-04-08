#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#libraries 
library(shiny)
library(tidyverse)
library(dygraphs)
library(xts)

#read and process file
crypto <- read.csv("crypto-markets.csv", stringsAsFactors = FALSE)
#make it slimmer
crypto <- crypto %>% 
  select(symbol, date, open, close)
#make date as date format
crypto$date <- as.Date(crypto$date)

#troubleshoot
print(min(crypto$date))

# Define UI for application 
ui <- fluidPage(
  titlePanel('Cryptocurrency dashboard'),
  sidebarLayout(
    sidebarPanel(
      selectInput('selectCoins', 'Select crypto(s)', choices = c("Bitcoin", "Ethereum", "XRP", "Bitcoin Cash", "Litecoin")#, multiple = T
                  ),
      selectInput('selectOutput', 'Select output', choices = c("open", "close")),
      dateRangeInput('selectDate', 'Select date', start = min(crypto$date), end = max(crypto$date))
    ),
    mainPanel(dygraphOutput("priceGraph", width = "100%", height = "800px")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  getData <- reactive({
    # get inputs 
    selectedCoins <- input$selectCoins
    selectedOutput <- input$selectOutput
    dates <- input$selectDate
    startDate <- dates[1]
    endDate <- dates[2]
    if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
    else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
    else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
    else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
    #filter by coin
    tmpData <- crypto[which(crypto$symbol==selectedCoins), ]
    #filter by output
    
    #filter by date
    tmpData <- tmpData[which(tmpData$date >= startDate & tmpData$date <= endDate), ]
    tmpData
  })
    output$priceGraph <- renderDygraph({
      data <- getData()
      time_series <- xts(data, order.by = data$date)
      dygraph(time_series)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
# no more code after this