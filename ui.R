library(shiny)
library(plotly)
library(ggplot2)


ui <- fluidPage(
  
  headerPanel("Yield Curves"),
  sidebarPanel(
    # data_set()$Date is now a date 
    
    sliderInput('Curve_Date', 'Date', min = as.Date("2016-01-01","%Y-%m-%d"),
                max = as.Date("2016-12-01","%Y-%m-%d"),
                value=as.Date("2016-12-01"),
                timeFormat="%Y-%m-%d"),
    checkboxInput("show model1","Muni/TR Ratio",value=TRUE)
  ),
  
  
  mainPanel(
    plotlyOutput('trendPlot', height = "300px",width="600px"),
    h5("Muni Yield/Treasury Yield"),
  
    textOutput("RATIO_CALC")
  )
)
