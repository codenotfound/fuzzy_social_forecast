library(shiny)
library(rCharts)
options(RCHART_LIB = 'polycharts')
shinyUI(pageWithSidebar(
  headerPanel("Forecasting number of drug users in Saint-Petersburg using fuzzy logic"),
  sidebarPanel(
    sliderInput('horizon', 'Set the desirable forecast horizon',value = 3, min = 1, max = 5, step = 1,)
  ),
  mainPanel(
    showOutput("fuzzyPlot", "polycharts")
  )
))
