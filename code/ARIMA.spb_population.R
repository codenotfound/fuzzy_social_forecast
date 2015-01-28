library(forecast)
## load file
path <- normalizePath(file.path(".", "data", "spb_population.csv")) #we are in root folder, so '.'
spb_population <- read.csv(path, sep = ";")

## forecast

fit <- auto.arima(as.ts(spb_population$population[1:22]))
forecast <- forecast(fit,h=9)
plot(forecast(fit,h=9))
lines(spb_population$population, col="red", type = "l")
## error calculation
y.pred <- forecast$mean
y.real <- spb_population$population[23:31]
bench <- cbind(y.pred, y.real)
colnames(bench) <- c("pred. val.", "real. val.")
print("Comparison ARIMA Vs Real Value on Spb Population Data Set")
print(bench)

residuals <- (y.real - y.pred)
MSE <- mean(residuals^2)
RMSE <- sqrt(mean(residuals^2))
SMAPE <- mean(abs(residuals)/(abs(y.real) + abs(y.pred))/2)*100
err <- c(MSE, RMSE, SMAPE)
names(err) <- c("MSE", "RMSE", "SMAPE")
print("Error Measurement: ")
print(err) 
