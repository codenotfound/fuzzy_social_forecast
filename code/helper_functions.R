library(frbs)
library(knitr)
library(stringi)
library(xlsx)
library(pander)
library(reshape)
library(zoo)

## load file
load_xlsx <- function(fname){
    path <- normalizePath(file.path("..", "data", fname)) 
    df <- read.xlsx(path, sheetIndex=1,header=T)
    return(df)
}
# cleaning
clean_xlsx_df <- function(df){
    #Sys.setlocale("LC_TIME", "ru_RU.utf8") 
    #clean header
    names(df) <- stri_trans_general(names(df), 
                                    "Any-Latin; nfd; [:nonspacing mark:] remove; nfc")
    names(df) <- gsub("X\\.", "",names(df))
    names(df)[which(names(df) == "NA.")] <- "value" #awful workaround
    names(df) <- tolower(names(df))
    names(df) <- gsub("\\.", "_", names(df))
    #clean variables
    df$vrema <- as.integer(df$vrema <- substr(df$vrema, start=0, stop=4))
    #keep only relevant columns
    keeps <- c("vrema", "value") 
    return(df[keeps])
}

join_factors <- function(factors, impute = median){
    # gets list of factors (created by "factors<-list(f1=f1,f2=f2...")
    # returns zoo object with filled in NAs
    z <- zoo()
    for(factor in factors){
        z <- merge(z, read.zoo(factor))

    }
    names(z) <- names(factors)
    #impute missing valiues
    z <- na.aggregate(z, FUN=impute)
    return(z)
}

fuzzy_modelling(df, predictand, horizon = 3, method.type = "WM",
                control = list(num.labels = 15, type.mf = "GAUSSIAN", 
                               type.defuz = "WAM", type.tnorm = "MIN", 
                               type.snorm = "MAX", type.implication.func = "ZADEH", 
                               name="sim-0")) 
{
    ## Create dataset
    ## 22 records on UA enrollments
    ## input: enrollments(t-2), enrollments(t-1), 
    ## output: enrollments(t)
    ## horizon - number of points to forecast
    start <- 1
    finish <- nrow(df)
    predictand <- tmp$enrollments[(start+2):finish]
    tminus1 <- tmp$enrollments[(start+1):(finish-1)]
    tminus2 <- tmp$enrollments[start:(finish-2)]
    UA_enrollments <- data.frame(tminus2, tminus1 , t)

    ## Split the data to the training and testing datasets
    finish <- nrow(UA_enrollments)

    data.train <- UA_enrollments[start : (finish-horizon), ]
    data.fit <- data.train[, 1 : 2]
    data.tst <- UA_enrollments[(finish-horizon+1) : finish, 1 : 2]
    real.val <- matrix(UA_enrollments[(finish-horizon+1) : finish, 3], ncol = 1)
    range.data <- apply(data.train,2,range)

    ## Generate fuzzy model
    object <- frbs.learn(data.train, range.data, method.type, control)
    ## Fitting step
    res.fit <- predict(object, data.fit)
    ## Predicting step
    res.test <- predict(object, data.tst)

    model_data <- list(object, res.fit, res.test)
    return(model_data)
}

