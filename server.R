
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(xts)
library(caret)
library(texreg)
library(ggplot2)
library(forecast)

shinyServer(function(input, output) {
  
  
  SSNSource.text <- eventReactive(input$SSNSource, {
    input$SSNSource
  })
  
  SSN.Number <- eventReactive(input$SSNSource, {
    input$SSNSource
  })
  
  # show table with data
  data.visual <- reactive({
    #a <- as.matrix(sim.ts)
    small <- c(0:9)
    x.min <- start(main.data())[1] # 1700
    x.min.small <- trunc(x.min/10)*10
    x.max <- end(main.data())[1] # 1988
    x.max.small <- trunc(x.max/10)*10
    
    Data <- matrix(ncol = length(small)+1, nrow = trunc((x.max-x.min)/10+1))
    Data <- as.data.frame(Data)
    names(Data) <- c("Year", small)
    for (i in 1:trunc((x.max-x.min)/10+1)) {
      Data$Year[i] <- x.min.small+(i-1)*10
    }
    for (i in 2:11) {
      Data[,i] <- 0
    }
    for (i in 1:length(main.data())) {
      # i <- 1
      year.number <- x.min+i-1
      year.number.main <- trunc(year.number/10)*10
      year.number.column <- year.number - year.number.main
      Data[which(Data$Year==year.number.main), year.number.column+2] <- main.data()[i]
    }
    return(Data)
    #return(mtcars)
    # data.visual <- Data
    # http://shiny.rstudio.com/reference/shiny/latest/renderDataTable.html
  })  

  output$data.visual.table =renderDataTable({
    #mtcars
    data.visual()
  })

  output$SSNSourceNumber <- renderText({
    SSNSource.text()
  })
  
  output$plot1 <- renderPlot({

    #sunspot.arima <- ar(main.data())
    sunspot.arima <- arima(
      main.data(), 
      order = c(
        input$aure, 
        input$diff, 
        input$moav))
  
    sunspot.forecast <- predict(sunspot.arima, n.ahead = 40)
    plot(main.data(),xlim=c(1700, 2030),col="grey",lwd=1.5, 
         ylab="Sunspots Number (SSN)", xlab="Years")
    lines(sunspot.forecast$pred, col="green", lwd=1.5)
  })
  
  output$plot2 <- renderPlot({
    if(input$SSNSource==1){
      # Internal RStudio SSN Data Set
      data(sunspot.year)
    } else {
      # Belgium World Data Center
      # http://sidc.be/silso/DATA/SN_y_tot_V2.0.txt      
      url <- "http://sidc.be/silso/DATA/SN_y_tot_V2.0.txt"
      sunspot.year <- read.csv(url, sep="", dec=".", header=F)
      sunspot.year$V1 <- trunc(sunspot.year$V1, prec = 0)
      sunspot.year <- sunspot.year[-c(3:4)]
      year.min <- min(sunspot.year$V1)
      year.max <- max(sunspot.year$V1)
      sunspot.year <- sunspot.year[-1]
      #sunspot.year$V1 <- paste0(as.character(sunspot.year$V1), "/06/01")  
      #sunspot.year <- ts(sunspot.year$V2, as.Date(sunspot.year$V1, format='%Y/%m/%d'))      
      sunspot.year <- ts(sunspot.year,start=year.min,end=year.max,frequency=1)
    }
    
    #sunspot.ar <- ar(main.data())
    sunspot.ar <- arima(
      main.data(), 
      order = c(
        input$aure, 
        input$diff, 
        input$moav))
    sunspot.forecast <- predict(sunspot.ar, n.ahead = 40)
    
    upper <- sunspot.forecast$pred + sunspot.forecast$se
    lower <- sunspot.forecast$pred - sunspot.forecast$se
    xx <- c(time(sunspot.forecast$pred), rev(time(sunspot.forecast$pred)))
    yy <- c(upper,rev(lower))
    plot(main.data(),xlim=c(1970, 2030),col="grey",lwd=1.5, type="b", pch=20,
         ylab="Sunspots (n)")
    polygon(xx,yy,col="lightgreen",border = NA)
    points(sunspot.forecast$pred,col="darkgreen",lwd=1.5,pch=20)
    lines(sunspot.forecast$pred,col="darkgreen",lwd=1.5)
    
  })
  
  output$plot.Compare <- renderPlot({

      #0 Data preparing
      DataShare <- as.numeric(input$DataShare)/100 # share of data for testing
      #DataShare <- 0.15
      
      #if different main set choosed
      if(input$SSNSource==1){
        legend.text <- c("Internal RStudio SSN Data Set (main set)", "Belgium Data Center") 
      } else {
        legend.text <- c("Belgium Data Center (main set)", "Internal RStudio SSN Data Set") 
      }  

    x.min <- start(main.data())[1]
    #x.min <- 1700
    x.max <- end(main.data())[1]
    #x.max <- 1988
    x.share <- trunc((x.max - x.min)*DataShare)
    
    # 3
    # place a plot
    plot(main.data(), xlim=c(x.min, x.max), ylim=c(0, 300), 
         col="red", lwd=1.5, type="b",
         ylab="Sunspots Number (SSN)", xlab="Years")
    lines(second.data(), col="blue", type="b", lwd=1.5)
    #abline(v =(x.max-x.share), untf = FALSE, col="green") # vertical line for test part
    
    legend(x.min+trunc(0.05*(x.max-x.min)), 230, pt.cex = 0.5, cex=0.7, # places a legend at the appropriate place 
    legend.text, xjust=0, yjust=0, y.intersp=1.5,
    lty=c(1, 1, 1), # gives the legend appropriate symbols (lines)
    lwd=c(2,2, 2), col=c("red", "blue", "green")) # gives the legend lines the correct color and width
    
    
    # prepare model for prediction
    #B <- ts(main.data[1:trunc(length(main.data)*(1-DataShare))], start =x.min, frequency = 1 )
    # prepare text describing the model
    #main.model <- ar(main.data)
    #A <- "list(main.model)"
    
    #output$model.description <- renderText({
      #print(tagList(main.model))
    #})  
    
      
  })
  
  main.data <-reactive({
    if(input$SSNSource==1){
        data(sunspot.year)
        data <- sunspot.year
    } else {
      url <- "http://sidc.be/silso/DATA/SN_y_tot_V2.0.txt"
      data <- read.csv(url, sep="", dec=".", header=F)
      data$V1 <- trunc(data$V1, prec = 0)
      data <- data[-c(3:4)]
      year.min <- min(data$V1)
      year.max <- max(data$V1)
      data <- data[-1]
      data <- ts(data, start=year.min, end=year.max, frequency=1)
    }
    return(data)
    # main.data <- data
  })
  
  second.data <- reactive({
    if(input$SSNSource==1){
      url <- "http://sidc.be/silso/DATA/SN_y_tot_V2.0.txt"
      data <- read.csv(url, sep="", dec=".", header=F)
      data$V1 <- trunc(data$V1, prec = 0)
      data <- data[-c(3:4)]
      year.min <- min(data$V1)
      year.max <- max(data$V1)
      data <- data[-1]
      data <- ts(data, start=year.min, end=year.max, frequency=1)
    }  else {
      data(sunspot.year)
      data <- sunspot.year
    }
    return(data)
    # second.data<- data
  })
  

  
  
  
})

