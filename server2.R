library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$Plot <- renderPlot({
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    dat2 <- read.csv(paste(input$List2,".csv", sep = ''))
    z <- dat2[[input$Data]]
    y <- NULL
    a <- NULL
    
    for(i in 2:length(x)){
      y <- c(y, x[i]/x[i-1])
    }
    for(i in 2:length(z)){
      a <- c(a, z[i]/z[i-1])
    }
    
    y <- log(y)
    m <- mean(y)
    s <- sd(y)
    
    a <- log(a)

    bins <- seq(min(y), max(y), length.out = input$Slider + 1)
    
    hist(y, breaks = bins, col = "yellow", border = "red", main = paste("Histogram of Log returns of Stock data of",input$List), 
         ylab = "Frequency", xlab = "Log Returns")

    xaxis <- seq(min(y), max(y), length.out = 1000)
    
    lines(xaxis, dnorm(xaxis, mean=m, sd=s), 
          col="darkblue", lwd=2)
    
  })

  output$Confid <- renderText({
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    y <- NULL
    
    for(i in 2:length(x)){
      y[i-1] <-  x[i]/x[i-1]
    }
    y <- log(y)
    m <- mean(y)
    s <- sd(y)
    
    error = qnorm(input$Conf/200 + 0.5)*s/sqrt(length(y))
    
    paste(input$Conf, "% Confidence Interval: (", round(m-error,6), ",", round(m + error,6), ")")
  })
  output$Regression <- renderPlot({
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    dat2 <- read.csv(paste(input$List2,".csv", sep = ''))
    z <- dat2[[input$Data]]
    y <- NULL
    a <- NULL
    
    for(i in 2:length(x)){
      y <- c(y, x[i]/x[i-1])
    }
    for(i in 2:length(z)){
      a <- c(a, z[i]/z[i-1])
    }
    
    y <- log(y)
    m <- mean(y)
    s <- sd(y)
    
    a <- log(a)
    Reg <- lm(y~a)
  plot(Reg)
    })
  
  
})

