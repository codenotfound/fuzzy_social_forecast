library(forecast)
## load file
path <- normalizePath(file.path(".", "imputed_registered_drug_users.csv")) #we are in root folder, so '.'
imputed_registered_drug_users <- read.csv(path)

## forecast
fit <- holt(as.ts(imputed_registered_drug_users$number[1:20]),h=3)
# forecast <- forecast(fit,h=3)
png(filename="holt.png")
plot(forecast(fit,h=3))
lines(imputed_registered_drug_users$number, col="red", type = "l")
dev.off()
## error calculation
y.pred <- forecast$mean
y.real <- imputed_registered_drug_users$number[21:23]
bench <- cbind(y.pred, y.real)
colnames(bench) <- c("pred. val.", "real. val.")
print("Comparison ets Vs Real Value on Spb Registered Drug Users Data Set")
print(bench)

residuals <- (y.real - y.pred)
MSE <- mean(residuals^2)
RMSE <- sqrt(mean(residuals^2))
SMAPE <- mean(abs(residuals)/(abs(y.real) + abs(y.pred))/2)*100
err <- c(MSE, RMSE, SMAPE)
names(err) <- c("MSE", "RMSE", "SMAPE")
print("Error Measurement: ")
print(err) 
