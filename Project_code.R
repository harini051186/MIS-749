rm(list=ls())
dir <- "C:/Users/Nick Ozanich/Desktop/Class Materials/749MIS/Project"
setwd(dir)

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("readxl", "data.table", "psych", "car", "corrgram", "scatterplot3d", "ISLR", "ggplot2", 
              "ggthemes", "gridExtra", "MASS", "ISLR", "caret", "e1071", "corrplot", "pROC", "leaps",
              "elasticnet", "pls", "gam", "visreg", "lubridate")
lapply(packages, pkgTest)

flights.data <- fread('flights.csv')

#routes <- subset(flights.data[,8:9])
#nrow(unique(routes[,c('ORIGIN_AIRPORT','DESTINATION_AIRPORT')])) #unique flight paths

#flights.cancelled <- subset(flights.data, flights.data[25]>0)
#missing.data <- is.na(flights.data[,1:25])
#flights.delayed <- subset(flights.data, !is.na(flights.data[,27:31]))
