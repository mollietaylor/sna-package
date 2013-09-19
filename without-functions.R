# library(plyr)

sampledata <- read.csv("sampledata.csv",
    header = TRUE)
sampledata$Location <- NULL
sampledata <- rename(sampledata,
  c(csacode = "Location"))








    firstAppYear <- 1975
    lastAppYear <- 2010
    locale <- 408
    category <- "Drugs & Medical"


    # limit large file to locale, years, and category:
    localdata <- sampledata[which(sampledata$Location == locale),]
str(localdata) # 71
range(localdata$AppYearStr) # 1967 2007
    if(category != "All") {
      localdata <- localdata[which(localdata$Category == category),]
    }
str(localdata) # 20 obs
range(localdata$AppYearStr) # 1981 2006
    localdata <- localdata[which(localdata$AppYearStr >= firstAppYear & localdata$AppYearStr <= lastAppYear),]


str(localdata) # 20 obs
range(localdata$AppYearStr) # 1981 2006
