library(shiny)
library(grDevices)

shinyServer(
  function(input, output) {
      source("../helper_functions.R")
      filenames <- list("Численность безработных всего.xlsx", 
                        "Преступления связанные с незаконным оборотом наркотиков  зарегистрировано.xlsx", 
                        "Состоит на учете больных с диагнозом «наркомания», на 100 тыс. населения.xlsx")
      factors <- lapply(filenames, load_xlsx)
      factors <- lapply(factors, clean_xlsx_df)
      filenames <- lapply(filenames, str_replace_all, "[^[:alnum:]]", ".")
      names(factors) <- sub("\\.[[:alnum:]]+$", "", filenames)
      zdf <- join_factors(factors)

      predictand <- "Состоит.на.учете.больных.с.диагнозом..наркомания...на.100.тыс..населения"
      output$fuzzyPlot <- renderPlot({
          horizon <- input$horizon
          model_data <- fuzzy_forecast(zdf, predictand, horizon = horizon)
          plot_model_data(model_data)
      })
    
  }
)
