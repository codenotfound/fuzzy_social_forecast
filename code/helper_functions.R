library(frbs)
library(knitr)
library(stringi)
library(xlsx)
library(pander)
library(reshape)
library(zoo)
library(ggplot2)
library(stringr)

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

fuzzy_forecast <- function(zdf, predictand, include = TRUE, horizon = 3, 
                           method.type = "WM", 
                           control = list(num.labels = 15, type.mf = "GAUSSIAN", 
                                          type.defuz = "WAM", type.tnorm = "MIN", 
                                          type.snorm = "MAX", 
                                          type.implication.func = "ZADEH",
                                          name="sim-0")) 
{
    ## zdf is a zoo object that contains 1..n predictors and exactly one predictand
    ## include - whether to include predicand as one of the predictors or not
    ## horizon - number of points to forecast
    start <- 1
    finish <- length(zdf[,1])

    ## Split the data to the training and testing datasets

    df <- data.frame(zdf)
    drops <- c(predictand)
    print(!(names(df) %in% drops))
    data.train <- df[start : (finish-horizon), 
                     if(!include) !(names(df) %in% drops) else 1:length(names(df))] 
    data.fit <- data.train[,!(names(data.train) %in% drops)]
    data.tst <- df[(finish-horizon+1) : finish, !(names(df) %in% drops)]
    range.data <- apply(df,2,range)

    ## Generate fuzzy model
    object <- frbs.learn(data.train, range.data, method.type, control)
    ## Fitting step
    res.fit <- predict(object, data.fit)
    ## Predicting step
    res.test <- predict(object, data.tst)

    model_data <- list(object=object, res.fit=res.fit, res.test=res.test, 
                       zoo_predictand = zdf[,predictand], 
                       start=start, finish=finish,horizon=horizon)
    return(model_data)
}

plot_model_data <- function(model_data)
{
    ## Comparing between simulation and real data
    predicted <- rbind(model_data$res.fit, model_data$res.test)
    df <- data.frame(year=index(model_data$zoo_predictand),
                     real=coredata(model_data$zoo_predictand), predicted=predicted) 
    qplot(year, value, colour = variable, data = melt(df, 'year'), geom = 'line') +
    geom_vline(xintercept=df$year[length(model_data$res.fit)]) + theme_bw()
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
