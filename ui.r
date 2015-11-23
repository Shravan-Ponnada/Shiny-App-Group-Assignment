library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Web App for Stock Data"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("Slider",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),

      selectInput("List", "Stock Label", 
                  c("Apple - AAPL",
                    "Bank of America - BAC",
                    "CISCO - CSCO",
                    "Citigroup - C",
                    "Coca Cola - KO",
                    "Facebook - FB",
                    "General Electric - GE",
                    "Tesla - TSLA",
                    "Yahoo - YHOO",
                    "McDonalds - MCD")),
      selectInput("Data", "Data", 
                  c("Open",
                    "High",
                    "Low",
                    "Close"
                    )),
      sliderInput("Conf", "Confidence Level", min = 0, max = 100, value = 95)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      textOutput("Confid")
    )
  )
))