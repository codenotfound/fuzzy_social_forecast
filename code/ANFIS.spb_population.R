library(frbs)

## load file
path <- normalizePath(file.path(".", "data", "spb_population.csv")) #we are in root folder, so '.'
tmp <- read.csv(path, sep = ";")

##create dataset
##input: population(t-2), population(t-1)
##output: population(t)
## ? - should we use lag()
t <- tmp$population[3:31]
tminus1 <- tmp$population[2:30]
tminus2 <- tmp$population[1:29]
spb_population <- data.frame(tminus2, tminus1 , t)


## split the data to the training and testing datasets
data.train <- spb_population[1 : 20, ]
data.fit <- data.train[, 1 : 2]
data.tst <- spb_population[21 : 29, 1 : 2]
real.val <- matrix(spb_population[21 : 29, 3], ncol = 1)
range.data <- range.data <-apply(data.train,2,range)

## Define interval of data
#range.data<-c(1:6000,0)

## set the method and its parameters
method.type <- "ANFIS"
control <- list(num.labels = 7, max.iter = 100, step.size = 0.01, type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH", name = "SPbPop")
object <- frbs.learn(data.train, range.data, method.type, control)

## This process is a part of fitting the model using data training. 
res.fit <- predict(object, data.fit)

## predicting step
res.test <- predict(object, data.tst)

## error calculation
y.pred <- res.test
y.real <- real.val
bench <- cbind(y.pred, y.real)
colnames(bench) <- c("pred. val.", "real. val.")
print("Comparison ANFIS Vs Real Value on Spb Population Data Set")
print(bench)

residuals <- (y.real - y.pred)
MSE <- mean(residuals^2)
RMSE <- sqrt(mean(residuals^2))
SMAPE <- mean(abs(residuals)/(abs(y.real) + abs(y.pred))/2)*100
err <- c(MSE, RMSE, SMAPE)
names(err) <- c("MSE", "RMSE", "SMAPE")
print("Error Measurement: ")
print(err) 

## Comparing between simulation and real data
op <- par(mfrow = c(2, 1))
x1 <- seq(from = 1, to = nrow(res.fit))
result.fit <- cbind(data.train[, 3], res.fit)
plot(x1, result.fit[, 1], col="red", main = "Spb Population: Fitting phase (the training data(red) Vs Sim. result(blue))", type = "l", ylab = "population")
lines(x1, result.fit[, 2], col="blue")

result.test <- cbind(real.val, res.test)
x2 <- seq(from = 1, to = nrow(result.test))
plot(x2, result.test[, 1], col="red", main = "Spb Population: Predicting phase (the Real Data(red) Vs Sim. result(blue))", type = "l", ylab = "population")
lines(x2, result.test[, 2], col="blue", type = "l")
par(op)