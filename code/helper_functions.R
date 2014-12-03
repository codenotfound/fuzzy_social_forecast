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
    return(df)
}

join_factors <- function(factors){
    # gets list of factors (created by "factors<-list(f1=f1,f2=f2..."
    # returns zoo onject with filled in NAs
    z <- zoo()
    for(factor in factors){
        z <- merge(z, read.zoo(factor))

    }
    names(z) <- names(factors)
    return(z)

}
