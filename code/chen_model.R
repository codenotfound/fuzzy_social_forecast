## Fuzzy time series model as proposed by Chen in 1996.
## Programmed in R language. Package "sets" is loaded.

## Load historical data of UA enrollments 1971-1992

UA_data <- read.csv("UA_enrollments.csv")

## Step 1: Define the universe of discourse and 
## partition it into equally lengthy intervals.

## The universe of discourse U is defined as [Dmin???D1,Dmax???D2] 
## where Dmin and Dmax are the minimum and maximum historical enrollment.

Dmin <- min(UA_data$enrollments)
Dmax <- max(UA_data$enrollments)

## The variables D1 and D2 are just two positive numbers, properly chosen 
## by the user. If we let D1 = 55 and D2 = 663, we get U=[13000, 20000].

D1 <- 55
D2 <- 663
sets_options("universe", seq(from = Dmin - D1, to = Dmax - D2, by = 1))

## Chen used seven intervals which is the same number used in most cases 
## observed in literature. Dividing U into seven evenly lengthy intervals 
## u1, u2, ..., u7, we get u1 = [13000, 14000], u2 = [14000, 15000], ...,  
## ..., u7 = [19000, 20000].
## ^later...


## multi set
s <- set(gset(1,1),gset(2,2),gset(3,3))
(cl <- closure(s))
(re <- reduction(cl))
stopifnot(s == re)



