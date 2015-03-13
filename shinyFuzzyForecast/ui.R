library(shiny)
library(rCharts)
library(ggplot2)
# options(RCHART_LIB = 'polycharts')
shinyUI(pageWithSidebar(
    headerPanel(paste("Forecasting number of drug users in ", 
                      "Saint-Petersburg using fuzzy logic")), 
    sidebarPanel(
        checkboxGroupInput(inputId = "factors", label = "Choose predictors",
                           choices = unlist(filenames)),
        strong("Predictand"),
        div(paste("Состоит на учете больных с диагнозом «наркомания»,", 
                                 " на 100 тыс. населения")),
        sliderInput('horizon', 'Set the desirable forecast horizon', 
                    value = 3, min = 1, max = 5, step = 1,)
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("fuzzyPlot")),
            tabPanel("Summary", tableOutput("valCmp"), tableOutput("errTable")),
            tabPanel("Fuzzy logic", plotOutput("mfPlot"), verbatimTextOutput("fuzzySummary"))

        )
    )
))
