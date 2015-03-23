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
                    value = 3, min = 1, max = 5, step = 1,),
        selectInput("method.type", "Select learning algorithm",
                    c("Wang and Mendel’s technique (regression)"="WM", 
                      "subtractive clustering method (regression)"="SBC",
                      "hybrid neural fuzzy inference systems (regression)"="HYFIS",
                      "adaptive neuro-fuzzy inference systems (regression)"="ANFIS",
                      "Ishibuchi’s weighted FRBCS (classification)"="FRBCS.W",
                      "Chi's method FRBCS (classification)"="FRBCS.CHI",
                      "dynamic evolving neuro-fuzzy IS (regression)"="DENFIS",
                      "FS using heuristic and gradient descent (regression)"="FS.HGD",
                      "fuzzy inference rules by descent method (regression)"="FIR.DM",
                      "genetic FS using MOGUL methodology (regression)"="GFS.FR.MOGUL",
                      "Thrift’s genetic algorithms (regression)"="GFS.THRIFT",
                      "Ishibuchi’s genetic cooperative-competitive learning (class.)"=
                          "GFS.GCCL",
                      "Ishibuchi’s hybridization of GCCL and Pittsburgh (classification)"=
                          "FH.GBML",
                      "structural learning algorithm on vague environment (classification)"=
                          "SLAVE",
                      "genetic algorithm for lateral tuning and rule selection"="GFS.LT.RS"))

    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("fuzzyPlot")),
            tabPanel("Summary", dataTableOutput("valCmp"), tableOutput("errTable")),
            tabPanel("Fuzzy logic", plotOutput("mfPlot"), verbatimTextOutput("fuzzySummary"))

        )
    )
))
