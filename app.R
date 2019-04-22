#libraries 
library(shiny)
library(tidyverse)
library(dygraphs)
library(xts)
library(rvest)
library(lubridate)
library(forecast)
library(shinythemes)
library(DT)

#read and process file
crypto <- read.csv("crypto-markets.csv", stringsAsFactors = FALSE)
#make it slimmer
crypto <- crypto %>% 
  select(symbol, date, open, close)
#make date as date format
crypto$date <- as.Date(crypto$date)

# Define UI for application 
ui <- navbarPage(
  theme = shinytheme("readable"),
  "DSBee's Cryptocurrency",
  tabPanel("Interactive Graph", 
   sidebarLayout(
    sidebarPanel(
      h5(strong("Choose the crypto and the period you're interested in")),
      uiOutput("selectCoinsUI"),
      dateRangeInput('selectDate', 'Select date', start = min(crypto$date), end = max(crypto$date)),
      h5(strong("Explaination")),
      p("In this project we made use of the library dygraph."),
      p("You can hover over the graph to see the individual open and close values for that specific day."),
      p("The green represent open prices whereas the purple line represent close prices for the selected cryptocurrency."), 
      p("Please change the range of the dates to see a more obvious difference between open and close prices. You can do so by setting the range explicitly or by dragging on the plotted area.")
      ),
    mainPanel(dygraphOutput("priceGraph", width = "100%", height = "800px"))
   )
  ),
  tabPanel("Optimal ETS Forecast", 
           sidebarLayout(
             sidebarPanel(
               h5(strong("Choose the crypto and the number of months to forecast")),
               uiOutput("selectCryptoWebETS"),
               selectInput('selectPriceETS', 'Select Start/End price', choices = c("Open", "Close")), 
               sliderInput("selectHorizonETS",label="How many months to predict?",min=1,max=12,value=2),
               h5(strong("Explaination")),
               p("We will use the best ETS model(ZZZ) to forecast, display the cross-validated accuracy, parameters, confidence intervals. "),
               p("The optimal training window size is determined between 12-7 months by determining the lowest MAPE obtained per window."),
               p("It might take a few extra seconds to generate the graph.")
             ),
             mainPanel(plotOutput("bestETSForecast", width = "100%", height = "800px"),
                       textOutput("explainETX", container = pre))
           )
  ),
  
  tabPanel("Other Forecast", 
           sidebarLayout(
             sidebarPanel(
               h5(strong("Choose the crypto and the number of months to forecast")),
               uiOutput("selectCryptoWebOther"),
               selectInput('selectPriceOther', 'Select Start/End price', choices = c("Open", "Close")), 
               sliderInput("selectHorizonOther",label="How many months to predict?",min=1,max=12,value=2),

               h5(strong("Explaination")),
               p("We will be performing forecasting using 4 commonly used smoothing models : Simple Exponential, Holt's Linear, Holt's Winter Additive and Multiplicative on the same cryptocurrency."),
               p("We will also be performing forecasing using the simple models: Drift, Mean, Naive, Seasonal Naive. They are then plotted and compared in a single graph."),
               p("The output being printed summarizes the soothing parameters and training set error measures for each model."),
               p("The data table can be searched according to the smoothing model or sorted accordingly for a particular error measurement.")
             ),
             mainPanel(               
               h5(strong("Comparison between the Smoothing Models:")),
               div(DT::dataTableOutput("table"), style = "font-size: 80%; width: 70%"),
               h5(strong("Plot Smoothing Models:")),
               p("Simple Exponential"),
               plotOutput("simpleExpo", width = "100%", height = "400px"),
               #textOutput("explainsimpleExpo", container = pre),
               p("Holt’s Linear"),
               plotOutput("holtLinear", width = "100%", height = "400px"),
               #textOutput("explainholtLinear", container = pre),
               p("Holt’s Winter's Addition"),
               plotOutput("holtWinterAdd", width = "100%", height = "400px"),
               #textOutput("explainholtWinterAdd", container = pre),
               p("Holt-Winter’s Multiplicative"),
               plotOutput("holtWinterMulti", width = "100%", height = "400px"),
               #textOutput("explainholtWinterMulti", container = pre),
               p("Simple Models (Not Reliable)"),
               plotOutput("simpleModels", width = "100%", height = "400px")
               #textOutput("explainsimpleModels", container = pre)
             )
           )
           ),
  tabPanel("Read Me",
           h3(strong("Libraries Used")),
           HTML("
                <ul>
                <li><strong>shiny -</strong> An open source R package that provides an elegant and powerful web framework for building web applications.</li>
                <li><strong>tidyverse -</strong> The tidyverse is a coherent system of packages for data manipulation, exploration and visualization</li>
                <li><strong>dygraphs -</strong> Dygraphs provides facilities for charting time-series data in R and automatically plots xts time series objects</li>
                <li><strong>xts -</strong> xts commonly referred to as eXtensible Time Series (xts) provides time series class and enables uniform handling by extending zoo</li>
                <li><strong>rvest -</strong> Rvest is a package that makes it easy to scrape (or harvest) data from html web pages</li>
                <li><strong>lubridate -</strong> Lubridate simplifies the data that contains dates and times which can be tricky and made use of parsing functions that automatically handle a wide variety of formats and separators</li>
                <li><strong>forecast -</strong> Forecast contains methods and tools for displaying and analyzing univariate time series forecasts</li>
                <li><strong>shinythemes -</strong> Acts like a bootstrap in shiny</li>
                <li><strong>DT -</strong> DT provides an R interface to the JavaScript library 'DataTables'</li>
                </ul>
                "),
           br(),
           h3(strong("Reasonings")),
           # ul(
           #   li("hi")
           # ),
           HTML("
                <h5>Interactive Graph</h5>
                  <ul>
                    <li>The dropdown menu for the cryptocurrencies is dynamically generated by scrapping using rvest on coinmarketcap.</li>
                    <li>The initial start and end date range in the Interactive Graph changes dynamically for every cryptocurrency as they each have unique min and max date in the dataset provided.</li>
                    <li>The following details are relevant to the top 10 currency as of 17 April 2019</li>
                    <li>The valid time period for Bitcoin dataset is between 2013-04-28 to 2018-11-29</li>
                    <li>The valid time period for Ethereum dataset is between 2015-08-07 to 2018-11-29</li>
                    <li>The valid time period for XRP dataset is between 2013-08-04 to 2018-11-29</li>
                    <li>The valid time period for Bitcoin Cash dataset is between 2017-07-23 to 2018-11-29</li>
                    <li>The valid time period for EOS dataset is between 2017-07-01 to 2018-11-29</li>
                    <li>The valid time period for Litecoin dataset is between 2013-04-28 to 2018-11-29</li>
                    <li>The valid time period for Binance Coin dataset is between 2017-07-25 to 2018-11-29</li>
                    <li>The valid time period for Tether dataset is between 2015-02-25 to 2018-11-29</li>
                    <li>The valid time period for Stellar dataset is between 2014-08-05 to 2018-11-29</li>
                    <li>The valid time period for Cardano dataset is between 2017-10-01 to 2018-11-29</li>
                    <li>The valid time period for Stellar dataset is between 2014-08-05 to 2018-11-29</li>
                </ul>
                <h5>Optimal ETS Forecast</h5>
                  <ul>
                    <li>We assume each month has 30 days as we use the userinput of horizon is in months. We would need days as our dataset has daily records.</li>
                    <li>Seasonality is ignored as the frequency of the time series is >24. The dataset is daily. Thus the frequency is 365, which represents the number of days in a year.</li>
                    <li>We vary the training window from 12-7 months and slides the window by 1-12 months to determine the optimal window size for the forecasting.</li>
                    <li>The lowest MAPE was obtained by taking the average MAPE of the training and testing set. Since MAPE is a measure of error for forecast accuracy, the lower the MAPE, the better the forecast.</li>
                  </ul>
                <h5>Other Forecast</h5>
                  <ul>
                    <li>The training window size is fixed at 7 months.</li>
                    <li>The optimal training window size would otherwise be different for each crypt/method.</li>
                  </ul>
                "),
           br(),
           h3(strong("Plans for future features and updates")),
           h5("Working on Dynamic Data"),
           p("In our project, we made use of the csv being provided by Kaggle hence it is based on static data. Future work might include the use of dynamic data hence all information will be scraped according to real tim.
             Although it may take a while to scrape the data, users will benefit from a collection of updated information."),
           h5("Plotly"),
           p("Another library that can be used to beautify the data visualization is by using a library called plotly. As plotly graphs are rendered locally through the htmlwidgets framework, this will provide a better readibility and improve the aesthetics of the widgets being displayed onto the interface."),
           h5("Comparing across cryptocurrencies"),
           p("Comparing across cryptocurrencies could be added to improve the app. This allows the users to see how different cryptos perform intuitively. In this project, comparing cryptocurrencies side by side may be hard to compare as each crypto will have different optimal window size."),
           h5("Selection of every cryptocurrency"),
           p("Currenctly, only the top 10 currencies from the coinmarketcap (as of 17 April 2019) can be chosed to be forecasted. 
             As we are working with static data, it makes sense to expand the cryptocurrency selection only when dynamic data is enabled.
             This is because the top 10 currencies do not change in the Kaggle csv that we are using in our shiny app."),
           br(),
           h3(strong("Assignment 4, DSBee Members:")),
           p("Tan Sheng Rong"),
           p("Nurul Syafiqah"),
           p("Siti Heryani"),
           br()
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ##for forecasting
    getDatasetETS <- reactive({
      # get inputs
      selectedCoins <- input$selectCryptoETS
      selectedHorizon <- input$selectHorizonETS
      if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      else if (selectedCoins == "XRP") { }
      else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      else if (selectedCoins == "EOS") { }
      else if (selectedCoins == "Binance Coin") { selectedCoins = "BNB" }
      else if (selectedCoins == "Tether") { selectedCoins = "USDT" }
      else if (selectedCoins == "Stellar") { selectedCoins = "XLM" }
      else if (selectedCoins == "Cardano") { selectedCoins = "ADA" }

      #filter by coin
      cryptoData <- crypto[which(crypto$symbol==selectedCoins), ]
      
      #filter by output
      if (input$selectPriceETS == "Open") { cryptoData <- cryptoData %>% select(open, date) }
      else { cryptoData <- cryptoData %>% select(close, date) }

      #generating TS
      minDate <- ((min(cryptoData$date)))
      maxDate <- ((max(cryptoData$date)))
      
      tmp <- cryptoData[which(cryptoData$date==max(cryptoData$date)), ]
      
      subset(cryptoData, start="2018-04-28", end=maxDate)
      
#      cryptoData <- cryptoData %>% select(-c(date)) #drops the date column for TS
      minDate <- ymd(minDate) #set the date in ymd format
      maxDate <- ymd(maxDate)
      
      optimalW <- getOptW()
      
      minDateOp <- maxDate %m-% months(optimalW)
      minDateOp <- year(minDateOp) + yday(minDateOp)/365
      maxDateOp <- year(maxDate) + yday(maxDate)/365
      
      minDate <- maxDate %m-% months(optimalW)
      
      tmp <- cryptoData[which(cryptoData$date<=maxDate & cryptoData$date>=minDate), ]
      rownames(tmp) <- NULL
      tmp <- tmp %>% select(-c(date)) #drops the date column for TS
      
      cryptoTS <- ts(tmp, frequency = 365,
                     start = c(year(minDate), yday(minDate)),
                     end = c(year(maxDate),yday(maxDate))
                     )
      
      # cryptoTS <- ts(cryptoData, frequency = 365, 
      #                start = c(2018, yday("2018-04-28")), 
      #                end = c(2018,yday("2018-11-29"))
      #                )
      #cryptoTS <- window(cryptoTS, start = minDateOp, end = maxDateOp, frequency = 365)
      
      autoplot(cryptoTS)
      cryptoTS
      })
    
    getOptW <- reactive({
      # get inputs
      selectedCoins <- input$selectCryptoETS
      selectedHorizon <- input$selectHorizonETS
      if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      else if (selectedCoins == "XRP") { }
      else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      else if (selectedCoins == "EOS") { }
      else if (selectedCoins == "Binance Coin") { selectedCoins = "BNB" }
      else if (selectedCoins == "Tether") { selectedCoins = "USDT" }
      else if (selectedCoins == "Stellar") { selectedCoins = "XLM" }
      else if (selectedCoins == "Cardano") { selectedCoins = "ADA" }
      
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
                     start = c(year(ymd(minDate)), yday(minDate)), 
                               end = c(year(ymd(maxDate)),yday(maxDate)))
      minDate <- ymd(minDate) #set the date in ymd format
      maxDate <- ymd(maxDate)
      
      smallestMAPEinW <- 100
      optimalW <- 0
      for (i in 0:5) { #windows 3 for test, window 12-i for train
        for (j in 0:12) { #13 times, slides for 1 year including current month
          tryCatch({
            Train <- window(cryptoTS, start = decimal_date(maxDate %m-% months(12+selectedHorizon+j-i)), end = decimal_date((maxDate %m-% months(selectedHorizon+j))))
            Test <- window(cryptoTS, start = decimal_date(maxDate %m-% months(selectedHorizon+j)))
            #forecast with best window
            etsFit <- ets(Train, model = 'ZZZ', damped = FALSE)
            #getting MAPE
            tmp <- accuracy(forecast(etsFit, h = selectedHorizon * 30), Test)
            tmp <- (tmp["Training set", "MAPE"]+tmp["Test set", "MAPE"])/2 #get average of MAPE btwn test and train
            if (tmp < smallestMAPEinW) {
              #print(paste("changed MAPE")) #debug
              smallestMAPEinW <- tmp
              optimalW <- i
            }
          }, error = function(e) { 
            #cat("ERROR :",conditionMessage(e), "\n") #debug
          })
        }
      }
      optimalW <- 12-optimalW
      optimalW
    })
    output$explainETX <- renderPrint({
      dataTS <- getDatasetETS()
      print(paste("The optimal window for predicting", input$selectHorizonETS, "months is", getOptW(), "months and the parameters are as follows: "))
      etsFit <- ets(dataTS, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
      summary(etsFit)
    })
    
    output$bestETSForecast <- renderPlot({
      dataTS <- getDatasetETS()
      etsFit <- ets(dataTS, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
      tsDataFC <- forecast(etsFit, h = input$selectHorizonETS * 30)

      wikiDataTS <- getActual()
      
      autoplot(tsDataFC, xlab = "Time", ylab = "Price") +
        autolayer(wikiDataTS, series = "Actual")
      # dataTS <- data.frame(Y=as.matrix(dataTS), date=time(dataTS))
      # wikiDataTS <- data.frame(Y=as.matrix(wikiDataTS), date=time(wikiDataTS))
      
      #plot_ly(data = tmp, x = ~date, y=~open, type = "scatter", mode = "lines+text")
      
      # p <- plot_ly() %>%
      #   add_lines(x = time(cryptoTS), y = cryptoTS,
      #             color = I("black"), name = "observed") %>%
      #   add_ribbons(x = time(tsDataFC$mean), ymin = tsDataFC$lower[, 2], ymax = tsDataFC$upper[, 2],
      #               color = I("gray95"), name = "95% confidence") %>%
      #   add_ribbons(x = time(tsDataFC$mean), ymin = tsDataFC$lower[, 1], ymax = tsDataFC$upper[, 1],
      #               color = I("gray80"), name = "80% confidence") %>%
      #   add_lines(x = time(tsDataFC$mean), y = tsDataFC$mean, color = I("blue"), name = "prediction") %>%
      #   add_lines(data = wikiDataTS, x = ~date, y = ~open, color = I("red"), name = "actual") 
      # p
    })  
    getActual <- reactive({
      #==========#
      selectedCoins <- input$selectCryptoETS
      selectedPrice <- input$selectPriceETS
      # if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      # else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      # else if (selectedCoins == "XRP") { }
      # else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      # else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      # else if (selectedCoins == "EOS") { }
      # else if (selectedCoins == "Binance Coin") { selectedCoins = "BNB" }
      # else if (selectedCoins == "Tether") { selectedCoins = "USDT" }
      # else if (selectedCoins == "Stellar") { selectedCoins = "XLM" }
      # else if (selectedCoins == "Cardano") { selectedCoins = "ADA" }
      
      switch (selectedCoins,
              "Bitcoin" = { link <- "https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20181130&end=20191130" },
              "Ethereum" = { link <- "https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20181130&end=20191130" },
              "XRP" = { link <- "https://coinmarketcap.com/currencies/ripple/historical-data/?start=20181130&end=20191130" },
              "Bitcoin Cash" = { link <- "https://coinmarketcap.com/currencies/bitcoin-cash/historical-data/?start=20181130&end=20191130" },
              "Litecoin" = { link <- "https://coinmarketcap.com/currencies/litecoin/historical-data/?start=20181130&end=20191130" },
              "EOS" = { link <- "https://coinmarketcap.com/currencies/eos/historical-data/?start=20181130&end=20191130" },
              "Binance Coin" = { link <- "https://coinmarketcap.com/currencies/binance-coin/historical-data/?start=20181130&end=20191130" },
              "Tether" = { link <- "https://coinmarketcap.com/currencies/tether/historical-data/?start=20181130&end=20191130" },
              "Stellar" = { link <- "https://coinmarketcap.com/currencies/stellar/historical-data/?start=20181130&end=20191130" },
              "Cardano" = { link <- "https://coinmarketcap.com/currencies/cardano/historical-data/?start=20181130&end=20191130" }
      )
      
      wikiPage <- read_html(link)
      wikiTable <- wikiPage %>% html_nodes("table.table") %>% .[[1]]
      wikiData <- as.data.frame(html_table(wikiTable, header = TRUE))
      
      wikiData <- wikiData %>% 
        select(Date, `Open*`, `Close**`)
      wikiData$Date <- as.Date(mdy(wikiData$Date))
      
      minDate <- ((min(wikiData$Date)))
      maxDate <- ((max(wikiData$Date)))
      
      wikiData <- rename(wikiData, 
                         "date" = "Date",
                         "open" = "Open*",
                         "close" = "Close**")
      wikiData <- arrange(wikiData, date)
      
      if (selectedPrice == "Open") { wikiData <- wikiData %>% select(open, date) }
      else { wikiData <- wikiData %>% select(close, date) }
      
      selectedHorizon <- input$selectHorizonETS
      maxDate <- minDate %m+% months(selectedHorizon)
      
      wikiData <- wikiData[which(wikiData$date<=maxDate & wikiData$date>=minDate), ]
      rownames(wikiData) <- NULL
      
      wikiData <- wikiData %>% select(-c(date)) #drops the date column for TS

      wikiDataTS <- ts(wikiData, frequency = 365, 
                       start = c(year(ymd(minDate)), yday(minDate)), 
                       end = c(year(ymd(maxDate)),yday(maxDate)))
      wikiDataTS
      #==========#
    })
    
    ##dynamic input from webscrapping 
    getCryptoListDataFrame <- reactive({
      myurl <- read_html("https://coinmarketcap.com") # read our webpage as html
      myurl
      myurlTable <- myurl %>% html_nodes("table#currencies.table") %>% .[[1]]
      myurlTable
      myurlData <- as.data.frame(html_table(myurlTable, header = TRUE)[2])
      myurlData <- myurlData[1:10,]
      myurlData <- gsub("\\n", ",", myurlData)
      myurlData
      myurlData <- strsplit(myurlData, ",")
      myurlData
    })
    
    output$selectCryptoWebETS <- renderUI({
      tmp <- getCryptoListDataFrame()
      selectInput('selectCryptoETS', 'Select crypto(s)', choices = c(
        tmp[[1]][2], tmp[[2]][2], tmp[[3]][2], tmp[[4]][2], tmp[[5]][2], 
        tmp[[6]][2], tmp[[7]][2], tmp[[8]][2], tmp[[9]][2], tmp[[10]][2]))
    })
    
    output$selectCoinsUI <- renderUI({
      tmp <- getCryptoListDataFrame()
      selectInput('selectCoins', 'Select crypto(s)', choices = c(
        tmp[[1]][2], tmp[[2]][2], tmp[[3]][2], tmp[[4]][2], tmp[[5]][2], 
        tmp[[6]][2], tmp[[7]][2], tmp[[8]][2], tmp[[9]][2], tmp[[10]][2]))
    })
    
    getCryptoFilteredDF <- reactive({
      selectedCoins <- input$selectCoins
      
      if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      else if (selectedCoins == "XRP") { }
      else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      else if (selectedCoins == "EOS") { }
      else if (selectedCoins == "Binance Coin") { selectedCoins = "BNB" }
      else if (selectedCoins == "Tether") { selectedCoins = "USDT" }
      else if (selectedCoins == "Stellar") { selectedCoins = "XLM" }
      else if (selectedCoins == "Cardano") { selectedCoins = "ADA" }
      #filter by coin
      tmpData <- crypto[which(crypto$symbol==selectedCoins), ]
      tmpData
    })
  
    ##for interactive open close price
    ###get dataset based on dates
    getDataset <- reactive({
      # # get inputs 
      dates <- input$selectDate
      startDate <- dates[1]
      endDate <- dates[2]
      tmpData <- getCryptoFilteredDF()
      tmpData <- tmpData[which(tmpData$date >= startDate & tmpData$date <= endDate), ]
      tmpData
    })
    ###generate interactive graph
    output$priceGraph <- renderDygraph({
      cryptoData <- getDataset()
      time_series <- xts(cryptoData, order.by = cryptoData$date)
      dygraph(time_series)
    })
    ###get min max and update daterangeinput for user input
    getMinDate <- reactive({
      tmp <- getCryptoFilteredDF()
      tmp <- min(tmp$date)
      tmp
    })
    getMaxDate <- reactive({
      tmp <- getCryptoFilteredDF()
      tmp <- max(tmp$date)
      tmp
    })
    observeEvent(input$selectCoins, {
      print(paste("observed", getMinDate(), getMaxDate()))
      updateDateRangeInput(session, "selectDate", start = getMinDate(), end = getMaxDate())
    })
    
    ##other forecasts
    output$selectCryptoWebOther <- renderUI({
      tmp <- getCryptoListDataFrame()
      selectInput('selectCoinsOther', 'Select crypto(s)', choices = c(
        tmp[[1]][2], tmp[[2]][2], tmp[[3]][2], tmp[[4]][2], tmp[[5]][2], 
        tmp[[6]][2], tmp[[7]][2], tmp[[8]][2], tmp[[9]][2], tmp[[10]][2]))
    })
    
    getCoinOther <- reactive({
      tmp <- input$selectCoinsOther
      tmp
    })
    getPriceOther <- reactive({
      tmp <- input$selectPriceOther
      tmp
    })
    getHorizonOther <- reactive({
      tmp <- input$selectHorizonOther
      tmp
    })

    getsimpleExpo <- reactive({
      cryptoData <- getDatasetOtherTrain()
      sesFit <- ets(cryptoData, model = 'ANN', damped = FALSE)
      sesFit
    })
    output$simpleExpo <- renderPlot({
      cryptoData <- getDatasetOtherTrain()
      sesFit <- getsimpleExpo()
      cryptoDataSes <- forecast(sesFit, h = getHorizonOther() * 30)
      autoplot(cryptoDataSes, series = "Predicted", 
               xlab = "Time", ylab = "Price") + 
        autolayer(cryptoData, series = "Original")
    })

    
    getholtLinear <- reactive({
      cryptoData <- getDatasetOtherTrain()
      hlmFit <- ets(cryptoData, model = 'AAN', damped = FALSE)
      hlmFit
    })
    output$holtLinear <- renderPlot({
      cryptoData <- getDatasetOtherTrain()
      hlmFit <- getholtLinear()
      cryptoDataHlm <- forecast(hlmFit, h = getHorizonOther() * 30)
      autoplot(cryptoDataHlm, series = "Predicted", 
               xlab = "Time", ylab = "Price") + 
        autolayer(cryptoData, series = "Original")
    })
    
    getholtWinterAdd <- reactive({
      cryptoData <- getDatasetOtherTrain()
      hwaFit <- ets(cryptoData, model = 'AAA', damped = FALSE)
    })
    output$holtWinterAdd <- renderPlot({
      cryptoData <- getDatasetOtherTrain()
      hwaFit <- getholtWinterAdd()
      cryptoDataHlm <- forecast(hwaFit, h = getHorizonOther() * 30)
      autoplot(cryptoDataHlm, series = "Predicted", 
               xlab = "Time", ylab = "Price") + 
        autolayer(cryptoData, series = "Original")
    })
    
    getholtWinterMulti <- reactive({
      cryptoData <- getDatasetOtherTrain()
      hwmFit <- ets(cryptoData, model = 'MAM', damped = FALSE)
      hwmFit
    })
    output$holtWinterMulti <- renderPlot({
      cryptoData <- getDatasetOtherTrain()
      hwmFit <- getholtWinterMulti()
      cryptoDataHlm <- forecast(hwmFit, h = getHorizonOther() * 30)
      autoplot(cryptoDataHlm, series = "Predicted", 
               xlab = "Time", ylab = "Price") 
      # + 
      #   autolayer(cryptoData, series = "Original")
      # tmp <- accuracy(cryptoDataHlm, getDatasetOtherTest())
      # print(tmp)
    })
    
    # output$explainsimpleExpo <- renderPrint({
    #   # sesFit <- getsimpleExpo()
    #   # summary(sesFit)
    # })
    # output$explainholtLinear <- renderPrint({
    #   hlmFit <- getholtLinear()
    #   summary(hlmFit)
    # })
    # output$explainholtWinterAdd <- renderPrint({
    #   hwaFit <- getholtWinterAdd()
    #   summary(hwaFit)
    # })
    # output$explainholtWinterMulti <- renderPrint({
    #   hwmFit <- getholtWinterAdd()
    #   summary(hwmFit)
    # })
    # output$explainsimpleModels <- renderPrint({
    #   sesFit <- getsimpleExpo()
    #   hlmFit <- getholtLinear()
    #   hwaFit <- getholtWinterAdd()
    #   hwmFit <- getholtWinterAdd()
    #   print("Simple Exponential")
    #   summary(sesFit)
    #   print("Holt’s Linear")
    #   summary(hlmFit)
    #   print("Holt’s Winter's Addition")
    #   summary(hwaFit)
    #   print("Holt-Winter’s Multiplicative")
    #   summary(hwmFit)
    # })
    
    output$table <- DT::renderDataTable({
      #ME     RMSE      MAE       MPE     MAPE MASE       ACF1
      
      # tmpColnames = c("sesFit", "hlmFit", "hwaFit", "hwmFit")
      tmpColnames = c("Simple Exponential", "Holt’s Linear", "Holt’s Winter's Addition", "Holt-Winter’s Multiplicative")
      tmpRownames = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1")
      # sesFit <- summary(getsimpleExpo())
      # hlmFit <- summary(getholtLinear())
      # hwaFit <- summary(getholtWinterAdd())
      # hwmFit <- summary(getholtWinterAdd())
      #create matrix by concaternating the rows. However, matrix is matrix[i] and not matrix[i][j]
      tmp <- rbind(summary(getsimpleExpo()), 
                   summary(getholtLinear()),
                   summary(getholtWinterAdd()),
                   summary(getholtWinterAdd()))
      
      #check for NaN, otherwise, it would be empty in datatable
      for (i in 1:28) {
        if (is.na(tmp[[i]])){
          tmp[[i]] <- "NaN"
          print("ok")
        }
      }
      
      tmpMatrix <- matrix(c(
        tmp[[1]], tmp[[2]], tmp[[3]], tmp[[4]], tmp[[5]], tmp[[6]], tmp[[7]],
        tmp[[8]], tmp[[9]], tmp[[10]], tmp[[11]], tmp[[12]], tmp[[13]], tmp[[14]],
        tmp[[15]], tmp[[16]], tmp[[17]], tmp[[18]], tmp[[19]], tmp[[20]], tmp[[21]],
        tmp[[22]], tmp[[23]], tmp[[24]], tmp[[25]], tmp[[26]], tmp[[27]], tmp[[28]]
      ), nrow = 4, ncol = 7, byrow = FALSE, 
      dimnames = list(tmpColnames,
                      tmpRownames))
      
      myDT <- tmpMatrix %>% datatable %>% 
        formatStyle(
          columns = 1:6,
          backgroundColor = styleInterval( 
            cuts = c(-10, -.01, 0, 10), 
            values = c("orange", "yellow", "white", "yellow", "orange") # c(a,b) c(colA, colB, colC) if(<a) colA if (a<x<b) colB if (>b) colC
          )
        )
    }, server = FALSE)
    
    output$simpleModels <- renderPlot({
      cryptoData <- getDatasetOtherTrain()
      selectedHorizon <- getHorizonOther()
      autoplot(cryptoData, 
               main = "", xlab = "Time", ylab = "Price") +
        autolayer(meanf(cryptoData, h = selectedHorizon * 30),
                  series="Mean", PI=FALSE) +
        autolayer(naive(cryptoData, h = selectedHorizon * 30),
                  series="Naïve", PI=FALSE) +
        autolayer(rwf(cryptoData, h = selectedHorizon * 30, drift = TRUE),
                  series="Drift", PI=FALSE) +
        autolayer(snaive(cryptoData, h = selectedHorizon * 30),
                  series="SNaive", PI=FALSE)
    })
    
    getDatasetOtherTrain <- reactive({
      # get inputs
      selectedCoins <- getCoinOther()
      selectedPrice <- getPriceOther()
      selectedHorizon <- getHorizonOther()
      if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      else if (selectedCoins == "XRP") { }
      else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      else if (selectedCoins == "EOS") { }
      else if (selectedCoins == "Binance Coin") { selectedCoins = "BNB" }
      else if (selectedCoins == "Tether") { selectedCoins = "USDT" }
      else if (selectedCoins == "Stellar") { selectedCoins = "XLM" }
      else if (selectedCoins == "Cardano") { selectedCoins = "ADA" }
      
      #filter by coin
      cryptoData <- crypto[which(crypto$symbol==selectedCoins), ]
      #filter by output
      if (selectedPrice == "Open") { cryptoData <- cryptoData %>% select(open, date) }
      else { cryptoData <- cryptoData %>% select(close, date) }
      
      # #generating TS
      # minDate <- ((min(cryptoData$date)))
      # maxDate <- ((max(cryptoData$date)))
      cryptoData <- cryptoData %>% select(-c(date)) #drops the date column for TS
      #defauly, 7 months test window
      minDate <- ymd("2018-04-29") #set the date in ymd format
      maxDate <- ymd("2018-11-29")
      
      minDate <- minDate %m-% months(selectedHorizon)
      maxDate <- maxDate %m-% months(selectedHorizon)
      
      # optimalW <- getOptW()
      # minDate <- maxDate %m-% months(optimalW)
      cryptoTS <- ts(cryptoData, frequency = 365, 
                     start = c(year(minDate), yday(minDate)), 
                     end = c(year(maxDate),yday(maxDate)))
      cryptoTS
    })
    getDatasetOtherTest <- reactive({
      # get inputs
      selectedCoins <- getCoinOther()
      selectedPrice <- getPriceOther()
      selectedHorizon <- getHorizonOther()
      if (selectedCoins == "Bitcoin") { selectedCoins = "BTC" }
      else if (selectedCoins == "Ethereum") { selectedCoins = "ETH" }
      else if (selectedCoins == "XRP") { }
      else if (selectedCoins == "Bitcoin Cash") { selectedCoins = "BCH" }
      else if (selectedCoins == "Litecoin") { selectedCoins = "LTC" }
      else if (selectedCoins == "EOS") { }
      else if (selectedCoins == "Binance Coin") { selectedCoins = "BNB" }
      else if (selectedCoins == "Tether") { selectedCoins = "USDT" }
      else if (selectedCoins == "Stellar") { selectedCoins = "XLM" }
      else if (selectedCoins == "Cardano") { selectedCoins = "ADA" }
      
      #filter by coin
      cryptoData <- crypto[which(crypto$symbol==selectedCoins), ]
      #filter by output
      if (selectedPrice == "Open") { cryptoData <- cryptoData %>% select(open, date) }
      else { cryptoData <- cryptoData %>% select(close, date) }
      
      # #generating TS
      # minDate <- ((min(cryptoData$date)))
      # maxDate <- ((max(cryptoData$date)))
      cryptoData <- cryptoData %>% select(-c(date)) #drops the date column for TS
      minDate <- ymd("2018-11-29")
      
      minDate <- minDate %m-% months(selectedHorizon)

      # optimalW <- getOptW()
      # minDate <- maxDate %m-% months(optimalW)
      cryptoTS <- ts(cryptoData, frequency = 365, 
                     start = c(year(minDate), yday(minDate)))
      cryptoTS
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
# no more code after this