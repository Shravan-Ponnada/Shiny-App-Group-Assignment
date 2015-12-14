library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$Plot <- renderPlot({
    dat <<- read.csv(paste(input$List,".csv", sep = ''))
    x <<- dat[[input$Data]]
    dat2 <<- read.csv(paste(input$List2,".csv", sep = ''))
    z <<- dat2[[input$Data]]
    y <- NULL
    a <- NULL
    
    for(i in 2:length(x)){
      y <- c(y, x[i]/x[i-1])
    }
    for(i in 2:length(z)){
      a <- c(a, z[i]/z[i-1])
    }
    
    y <<- log(y)
    m <<- mean(y)
    s <<- sd(y)
    
    a <<- log(a)

    bins <- seq(min(y), max(y), length.out = input$Slider + 1)
    
    hist(y, breaks = bins, col = "yellow", border = "red", main = paste("Histogram of Log returns of Stock data of",input$List), 
         ylab = "Frequency", xlab = "Log Returns")

    xaxis <- seq(min(y), max(y), length.out = 1000)
    
    lines(xaxis, dnorm(xaxis, mean=m, sd=s), 
          col="darkblue", lwd=2)
    
  })

  output$Confid1 <- renderText({
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
    
    paste(input$Conf, "% Confidence Interval of Mean: (", round(m-error,6), ",", round(m + error,6), ")")
  })
  
  output$Confid2 <- renderText({
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    y <- NULL
    
    for(i in 2:length(x)){
      y[i-1] <-  x[i]/x[i-1]
    }
    y <- log(y)
    m <- mean(y)
    s <- sd(y)
    v <- s*s
    
    
    deg <- length(x)-1
    up <- qchisq((1-(100-input$Conf)/200),deg)
    lo <- qchisq((100-input$Conf)/200,deg)
    
    paste(input$Conf, "% Confidence Interval of Variance : (", round((deg*v)/up,6), ",", round((deg*v)/lo,6), ")")
  
    })
  
  
  output$Regression <- renderPlot({
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    dat2 <- read.csv(paste(input$List2,".csv", sep = ''))
    z <- dat2[[input$Data]]
    y <- NULL
    a <- NULL
    k <- NULL
    for(i in 2:length(x)){
      y <- c(y, x[i]/x[i-1])
    }
    for(i in 2:length(z)){
      a <- c(a, z[i]/z[i-1])
    }
    k <- 1:length(y)
    
    y <- log(y)
    
    
    a <- log(a)
    Reg <- lm(y~a)
    par(mfrow=c(1,3))
   plot(a,y, main = "Regression plot - Two Stocks", xlab = input$List, ylab = input$List2)
 abline(Reg)
 #plot(Reg)
    plot(resid(Reg), main = "Residual Plot", xlab = "Time", ylab = "Residuals")
  plot(k,y, main = "Regression plot with Time", xlab = "Time", ylab = input$List)
    Reg2 <- lm(y~k)
    abline(Reg2)
    })
  
  output$Means <- renderText({
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
    m1 <- mean(y)
    
    
    a <- log(a)
    m2 <- mean(a)
    
    
    
    
    if (m1==m2)
      paste(" The means of", input$List, "and", input$List2, " have been found to be equal")
    else
      paste(" The means of", input$List, "and", input$List2, " have been been found to be inequal")
    
    
  })
  
  output$RegressionSummary <- renderTable({
    
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    dat2 <- read.csv(paste(input$List2,".csv", sep = ''))
    z <- dat2[[input$Data]]
    y <- NULL
    a <- NULL
    k <- NULL
    
    for(i in 2:length(x)){
      y <- c(y, x[i]/x[i-1])
    }
    for(i in 2:length(z)){
      a <- c(a, z[i]/z[i-1])
    }
    k <- 1:length(y)
    
    y <- log(y)
    a <- log(a)
    Reg <- data.frame()
    Reg <- lm(y~a)
    print(Reg)
    
    #par(mfrow=c(1,3))
    #plot(a,y, main = "Regression plot - Two Stocks", xlab = input$List, ylab = input$List2)
    #abline(Reg)
    #plot(Reg)
    #plot(resid(Reg), main = "Residual Plot", xlab = "Time", ylab = "Residuals")
    #plot(k,y, main = "Regression plot with Time", xlab = "Time", ylab = input$List)
    #Reg2 <- lm(y~k)
    #abline(Reg2)
    
  })
  
  output$Rsquared <- renderText({
    dat <- read.csv(paste(input$List,".csv", sep = ''))
    x <- dat[[input$Data]]
    dat2 <- read.csv(paste(input$List2,".csv", sep = ''))
    z <- dat2[[input$Data]]
    y <- NULL
    a <- NULL
    k <- NULL
    for(i in 2:length(x)){
      y <- c(y, x[i]/x[i-1])
    }
    for(i in 2:length(z)){
      a <- c(a, z[i]/z[i-1])
    }
    
    k <- 1:length(y)
    
    y <- log(y)
    a <- log(a)
    
    Reg <- data.frame()
    Reg <- lm(y~a)
    paste("The value of Rsquared of", input$List, "and", input$List2, " have been found to be", summary(Reg)$r.squared)
    
    
  })
  
})

