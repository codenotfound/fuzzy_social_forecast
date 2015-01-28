library(shiny)
library(grDevices)
library(rCharts)
options(RCHART_WIDTH = 800)
source("code/helper_functions.R")

shinyServer( function(input, output) {
      filenames <- list("Численность безработных всего.xlsx", 
                        "Преступления связанные с незаконным оборотом наркотиков  зарегистрировано.xlsx", 
                        "Состоит на учете больных с диагнозом «наркомания», на 100 тыс. населения.xlsx")
      factors <- lapply(filenames, load_xlsx)
      factors <- lapply(factors, clean_xlsx_df)
      filenames <- lapply(filenames, str_replace_all, "[^[:alnum:]]", ".")
      names(factors) <- sub("\\.[[:alnum:]]+$", "", filenames)
      zdf <- join_factors(factors)
      predictand <- "Состоит.на.учете.больных.с.диагнозом..наркомания...на.100.тыс..населения"

      dataset <- reactive({
          horizon <- input$horizon
          model_data <- fuzzy_forecast(zdf, predictand, horizon = horizon)
          ## Comparing between simulation and real data
          predicted <- rbind(model_data$res.fit, model_data$res.test)
          df <- data.frame(year=index(model_data$zoo_predictand),
                         real=coredata(model_data$zoo_predictand), predicted=predicted) 
          melt(df, 'year')

      })
      output$fuzzyPlot <- renderPlot({ #renderChart2({
         # cat(str(melt(df,'year')))
          #p <- qplot(year, value, shape = variable, data = dataset(), geom = 'point') #+
          p <- ggplot(dataset(), aes(x=year, y=value, colour=variable)) + geom_line(aes(group=variable))
         # geom_vline(xintercept=df$year[length(model_data$res.fit)]) + theme_bw()
          #plot(melt(df, 'year')) 

         # p <- rPlot(value ~ year, color = 'variable', type = 'line', data = melt(df,'year'))
          #p$layer(data=data.frame(x=df$year[length(model_data$res.fit)],y=180), copy_layer=T, type='line', color=list(const="blue"))
          #p$layer(x=df$year[length(model_data$res.fit)],y='year', data = melt(df,'year'), type='line')
          #return(p)
          print(p)
          #plot(dataset())
      })
    
  }
)
