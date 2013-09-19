sampledata <- read.csv("sampledata.csv",
    header = TRUE)
sampledata$Location <- NULL
sampledata <- rename(sampledata,
  c(csacode = "Location"))




source("functions.R")



    # becomes:
    firstAppYear <- 1975
    lastAppYear <- 2010
    locale <- 408
    category <- "Drugs & Medical"



    localdata <- NarrowData(sampledata, sampledata$Location, locale)
    localdata <- NarrowData(localdata, localdata$Category, category)
    localdata <- NarrowData(localdata, localdata$AppYearStr, firstAppYear:lastAppYear)

str(localdata)
range(localdata$AppYearStr)