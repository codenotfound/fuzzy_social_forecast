# Модель нечеткого прогнозирования распространения наркомании
# (Fuzzy rule-based system based on space partition. 
# Wang and Mendel’s technique (WM), 1992.)

source("helper_functions.R")
filenames <- list("В состоянии наркотического опьянения.xlsx", 
                  "Преступления связанные с незаконным оборотом наркотиков  зарегистрировано.xlsx", 
                  "Состоит на учете больных с диагнозом «наркомания», на 100 тыс. населения.xlsx")
indicators <- sapply(filenames, load_xlsx)
indicators <- lapply(indicators, clean_xlsx_df)
filenames <- lapply(filenames, str_replace_all, "[^[:alnum:]]", ".")
names(indicators) <- sub("\\.[[:alnum:]]+$", "", filenames)
zdf <- join_factors(indicators)
predictand = "Состоит.на.учете.больных.с.диагнозом..наркомания...на.100.тыс..населения"
horizon=3
model_data <- fuzzy_forecast(zdf, predictand)
plot_model_data(model_data)

#Сравнение модели WM (predicted) с эмпирическими данными (real) с горизонтом прогнозирования = `r horizon`
## Error calculation
##y.pred <- res.test
##y.real <- real.val
##bench <- cbind(y.pred, y.real)
##colnames(bench) <- c("predicted", "real")
##pander(bench, style="rmarkdown", caption="Значения модели и эмпирические значения")
##residuals <- (y.real - y.pred)
##MSE <- mean(residuals^2)
##RMSE <- sqrt(mean(residuals^2))
##SMAPE <- mean(abs(residuals)/(abs(y.real) + abs(y.pred))/2)*100
##err <- c(MSE, RMSE, SMAPE)
##names(err) <- c("MSE", "RMSE", "SMAPE")
##pander(as.matrix(err), type='rmarkdown', caption="Оценки ошибок прогноза") 
