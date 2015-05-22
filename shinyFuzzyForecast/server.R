library(shiny)
library(grDevices)
#library(rCharts)
#options(RCHART_WIDTH = 800)
source("code/helper_functions.R")

shinyServer( function(input, output) {
      factors <- lapply(filenames, load_xlsx)
      factors <- lapply(factors, clean_xlsx_df)
      filenames <- lapply(filenames, str_replace_all, "[^[:alnum:]]", ".")
      names(factors) <- sub("\\.[[:alnum:]]+$", "", filenames)
      zdf <- join_factors(factors)
      predictand <- "Состоит.на.учете.больных.с.диагнозом..наркомания...на.100.тыс..населения"

      ## Interactive dataset generation
      model_data <- reactive({
          horizon <- input$horizon
          method.type <- input$method.type
          model_data <- fuzzy_forecast(zdf, predictand, horizon = horizon,
                                       method.type = method.type)
      })
      ## Plotting
      output$fuzzyPlot <- renderPlot({ 
          ## Comparing between simulation and real data. Preparing dataset.
          model_data  <- model_data()
          sink("sink-examp.txt")
          summary(model_data$object)  
          sink()
          unlink("sink-examp.txt")
          predicted <- rbind(model_data$res.fit, model_data$res.test)
          df <- data.frame(year=index(model_data$zoo_predictand),
                         real=coredata(model_data$zoo_predictand), predicted=predicted) 
          dataset <- melt(df, 'year')

          real <- subset(dataset, variable=="real", select = c(year,value))
          print("REAL#####")
          print(head(real))
          predicted <- subset(dataset, variable=="predicted", select = c(year,value))

          # get the range for the x and y axis
          xrange <- range(real$year)
          yrange <- range(real$value)

          # set up the plot
          plot(xrange, yrange, type="n", xlab="Year", ylab="Value") 
          lines(real, col = "red")
          lines(predicted, col = "blue")
          abline(v=df$year[length(model_data$res.fit)])
      })
      output$mfPlot <- renderPlot({plotMF(model_data()$object)})
      output$fuzzySummary <- renderPrint({summary(model_data()$object)})

      ## Real vs predicted values
      output$valCmp <- renderDataTable({
      errors <- err_calc(model_data())
      errors$bench
      })

      ## Error table
      output$errTable <- renderTable({
      errors <- err_calc(model_data())
      #pander(errors$bench, style="rmarkdown", 
      #caption="Значения модели и эмпирические значения")
      errors$err#, style='rmarkdown', caption="Оценки ошибок прогноза") 
  })
    
  }
)
