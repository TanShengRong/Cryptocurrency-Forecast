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
#print(min(crypto$date))

# Define UI for application 
ui <- navbarPage(
  strong("Cryptography"),
  tabPanel("Forecast using BTS", 
   sidebarLayout(
     sidebarPanel(
      "We will use the best ETS model to forecast 2 months, display the cross-validated accuracy, parameters, confidence intervals.",
      selectInput('selectCryptoETS', 'Select crypto(s)', choices = c("Bitcoin", "Ethereum", "XRP", "Bitcoin Cash", "Litecoin")),
      selectInput('selectPriceETS', 'Select Start/End price', choices = c("Open", "Close")), 
      sliderInput("selectHorizonETS",label="How many months to predict?",min=1,max=12,value=2)
     ),
     mainPanel(plotOutput("bestETSForecast", width = "100%", height = "800px"),
               textOutput("explainETX", container = pre))
   )
  ),
  tabPanel("Interactive Graph", 
   sidebarLayout(
    sidebarPanel(
      "Choose the crypto and the period you're interested in",
      selectInput('selectCoins', 'Select crypto(s)', choices = c("Bitcoin", "Ethereum", "XRP", "Bitcoin Cash", "Litecoin")),
      dateRangeInput('selectDate', 'Select date', start = min(crypto$date), end = max(crypto$date))),
    mainPanel(dygraphOutput("priceGraph", width = "100%", height = "800px"))
   )
  ),
  tabPanel("Other Forecast"), 
  tabPanel("Read Me",
           h3(strong("Assumptions")),
           p("Each month has 30 days."),
           br(),
           h3(strong("Assignment 4, Members:")),
           p("Yani"),
           p("Fiqah"),
           p("Sr")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ##for interactive open close price
  getDataset <- reactive({
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
      cryptoData <- getDataset()
      time_series <- xts(cryptoData, order.by = cryptoData$date)
      dygraph(time_series)
    })
    
    ##for forecasting
    getDatasetETS <- reactive({
      # get inputs
      selectedCoins <- input$selectCryptoETS
      selectedHorizon <- input$selectHorizonETS
      if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      #filter by coin
      cryptoData <- crypto[which(crypto$symbol==selectedCoins), ]
      #filter by output
      if (input$selectPriceETS == "Open") { cryptoData <- cryptoData %>% select(open, date) }
      else { cryptoData <- cryptoData %>% select(close, date) }
      
      #generating TS
      minDate <- ((min(cryptoData$date)))
      maxDate <- ((max(cryptoData$date)))
      cryptoData <- cryptoData %>% select(-c(date)) #drops the date column for TS
      cryptoTS <- ts(cryptoData, frequency = 365, 
                      start = c(year(ymd(minDate)), yday(minDate), 
                                end = c(year(ymd(maxDate)),yday(maxDate))))
      minDate <- ymd(minDate) #set the date in ymd format
      maxDate <- ymd(maxDate)
      
      smallestMAPEinW <- 100
      optimalW <- 0
      for (i in 0:5) { #windows 3 for test, window 12-i for train
        for (j in 0:12) { #13 times, slides for 1 year including current month
          Train <- window(cryptoTS, start = decimal_date(maxDate %m-% months(12+selectedHorizon+j+i)), end = decimal_date((maxDate %m-% months(selectedHorizon+j))))
          Test <- window(cryptoTS, start = decimal_date(maxDate %m-% months(selectedHorizon+j)))
          #forecast with best window
          etsFit <- ets(Train, model = 'ZZZ', damped = FALSE)
          #getting MAPE
          tmp <- accuracy(forecast(etsFit, h = selectedHorizon * 30), Test)
          tmp <- (tmp["Training set", "MAPE"]+tmp["Test set", "MAPE"])/2 #get average of MAPE btwn test and train
          if (tmp < smallestMAPEinW) {
            smallestMAPEinW <- tmp
            optimalW <- i
          }
        }
      }
      smallestMAPEinW
      optimalW <- 12-optimalW
      print(paste("The optimal window for predicting", selectedHorizon, "months is", optimalW, "months"))
      minDate <- maxDate %m-% months(optimalW)
      cryptoTS <- ts(cryptoData, frequency = 365, 
                      start = c(year(minDate)), yday(minDate), 
                      end = c(year(maxDate),yday(maxDate)))
      cryptoTS
      })
    
    output$explainETX <- renderPrint({
      dataTS <- getDatasetETS()
      etsFit <- ets(dataTS, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
      summary(etsFit)
    })
    
    output$bestETSForecast <- renderPlot({
      dataTS <- getDatasetETS()
      etsFit <- ets(dataTS, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
      tsDataFC <- forecast(etsFit, h = input$selectHorizonETS * 30)
      autoplot(tsDataFC)
    })  
    
}
# Run the application 
shinyApp(ui = ui, server = server)
# no more code after this