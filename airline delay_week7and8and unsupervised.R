rm(list=ls())
dir <- "C:/Users/harin/Desktop/MSIS/MIS 749/project/flight-delays"
setwd(dir)

library(leaps)
library(ISLR)

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
summary(flights.data)
str(flights.data)




# Week 7 - variable selection 

library(leaps)
library(ISLR)
str(flights.data)

# delete unwanted columns 
flights.data = subset(flights.data , select = -c(AIR_SYSTEM_DELAY:WEATHER_DELAY) )
flights.data = subset(flights.data , select = -c(DIVERTED,CANCELLED) )

flights.data <-  na.omit(flights.data)
str(flights.data) 



# create delayed_arrival column

delayed_arrival <- as.numeric(flights.data$ARRIVAL_DELAY > 0)
flights.data <- cbind(flights.data,delayed_arrival)
str(flights.data)

#data wrangling from date column

library(lubridate)


form.dates<-ymd(paste(flights.data$YEAR, flights.data$MONTH, flights.data$DAY, sep="-"))
form.dates
flights.data$date<-form.dates

# dropping separate year, month columns
flights.data = subset(flights.data , select = -c(YEAR:DAY_OF_WEEK) )
str(flights.data)

# correlation graphs
flights.data <-  na.omit(flights.data)
flights.numeric = subset(flights.data , select = -c(AIRLINE,TAIL_NUMBER,ORIGIN_AIRPORT,DESTINATION_AIRPORT,CANCELLATION_REASON) ) 
str(flights.numeric)

res <- cor(flights.numeric)
res
library(corrplot)
par(mar=rep(2,4))
plot(flights.numeric)

install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

library(corrgram)
corrgram(res, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="WINE QUALITY")

library("PerformanceAnalytics")
chart.Correlation(res, histogram=TRUE, pch=19)




#variable selection

#best subset, look at up to 19 models
# not including YEAR,MONTH, DAY,DAY_OF_WEEK,ORIGIN_AIRPORT,DESTINATION_AIRPORT,DISTANCE,ARRIVAL_TIME
h.subset <- regsubsets(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE + DIVERTED + CANCELLED + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL   , data=flights.data, nvmax=10)

h.subset <- regsubsets(delayed_arrival ~ ORIGIN_AIRPORT+DESTINATION_AIRPORT+DISTANCE+ARRIVAL_TIME, data=flights.data, nvmax=5)
#Error: cannot allocate vector of size 53.6 Gb
memory.limit(50000)

h.subset.summary <- summary(h.subset)

#review models and predictors included across M0 through M19
h.subset.summary

#retrieve model fitness measures
names(h.subset.summary)
h.subset.summary$rsq

#lets use built in plotting of function
graphics.off()
par("mar")
par(mar=c(15000,15000,15000,15000))

plot(h.subset) #top has lowest bic
#Error in plot.new() : figure margins too large

coef(h.subset, 6) #retrieve M6 coeficients
plot(h.subset, scale="adjr2")

#forward and stepwise regression
h.fwd <- regsubsets(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE + DIVERTED + CANCELLED + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL  , data=flights.data, nvmax=19, method="forward")
summary(h.fwd)

h.bwd <- regsubsets(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE + DIVERTED + CANCELLED + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL, data=flights.data, nvmax=19, method="backward")
#got Error: cannot allocate vector of size 1.1 Gb 
summary(h.bwd)

#lets use caret to tune the best value for nvmax (number of models)
library(caret)
ctrl <- trainControl(method = "cv", number=5)

#lcv on forward
set.seed(195) #SEED
h.tfwd <- train(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE + DIVERTED + CANCELLED + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL , data=flights.data, method = "leapForward", tuneLength=10, trControl=ctrl)

#Error in na.fail.default(list(delayed_arrival = c(0, 0, 1, 0, 0, 1, 0,  : missing values in object
     
h.tfwd

#lcv on forward
set.seed(195) #SEED
h.tbwd <- train(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE + DIVERTED + CANCELLED + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL , data= flights.data, method = "leapBackward", tuneLength=10, trControl=ctrl)

#Error: Stopping
#In addition: There were 12 warnings (use warnings() to see them)
h.tbwd

#lasso an ridge regression
library(elasticnet)

set.seed(195) #SEED
h.ridge <- train(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE + DIVERTED + CANCELLED + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL , 
                 preProcess=c("scale"),
                 data= flights.data, method = "ridge", tuneLength=10, trControl=ctrl)

#Error in na.fail.default(list(delayed_arrival = c(0, 0, 1, 0, 0, 1, 0,  : missing values in object
h.ridge

plot(h.ridge)

set.seed(195) #SEED
h.lasso <- train(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE, 
                 data= flights.data, 
                 method = "lasso", tuneLength=30, trControl=ctrl)
#Warning message:
#  In train.default(x, y, weights = w, ...) :
#  You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.
h.lasso
plot(h.lasso)

getTrainPerf(h.lasso)

getTrainPerf(h.ridge)

library(pls)
set.seed(195) #SEED
h.pcr <- train(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE, data= flights.data, method = "pcr", tuneLength=10, trControl=ctrl)
# gives a long warning message
h.pcr
plot(h.pcr)

set.seed(195) #SEED
h.pls <- train(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE, data=flights.data, method = "pls", tuneLength=10, trControl=ctrl)
# long error and warning msg
h.pls
plot(h.pls)

#lets gather the models
#first lets put all trained models in a list object
models<- list("Fwd"=h.tfwd, "Bwd" = h.tbwd,
              "Ridge" = h.ridge, "Lasso"=h.lasso,
              "PCR" = h.pcr,
              "pls" = h.pls)


hitter.resamples<- resamples(models)
summary(hitter.resamples)

#plot performances
bwplot(hitter.resamples, metric="RMSE")
bwplot(hitter.resamples, metric="Rsquared")


##week 8

library(ISLR)
library(gam)
library(visreg)
 
set.seed(1)

pairs(flights.data)
#Error in pairs.default(flights.data) : non-numeric argument to 'pairs'

#linear regression
str(flights.data)
basic.lm <- lm(delayed_arrival ~ DEPARTURE_DELAY +AIRLINE  + DEPARTURE_TIME + SCHEDULED_DEPARTURE+TAXI_OUT + WHEELS_OFF  + SCHEDULED_TIME + ELAPSED_TIME + AIR_TIME +WHEELS_ON + TAXI_IN + SCHEDULED_ARRIVAL, data=flights.data)
memory(50000)
summary(basic.lm)



rmse(basic.lm$residuals)

#polynomial regression
flights.data$DEPARTURE_DELAY
rss = rep(NA, 10)
fits = list()
for (d in 1:10) {
 fits[[d]] = lm(delayed_arrival ~ poly(DEPARTURE_DELAY, d), data =flights.data)
 rss[d] = deviance(fits[[d]])
 } 
rss


anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]])

fits[[4]] = lm(delayed_arrival ~ poly(DEPARTURE_DELAY, d), data =flights.data)
#Error: cannot allocate vector of size 305.2 Mb

library(glmnet)
library(boot)


#step function regression
cv.errs = rep(NA, 10)
for (c in 2:10) {
 flights.data$dis.cut = cut(flights.data$DEPARTURE_DELAY, c)
   step.lm <- lm(delayed_arrival ~ dis.cut, flights.data) 
   summary(step.lm)
   #rmse(step.lm$residuals)
   cv.errs[c] = cv.glm(Auto, fit, K = 10)$delta[2]
   }
 which.min(cv.errs)
 
# Error: cannot allocate vector of size 43.6 Mb

cv.errs



#visualization of models
visreg(basic.lm, main="simple regression")
visreg(fits[[4]], main="tenth-degree poly")
visreg(step.lm, main="step regression")

#rmse function to calculate training error on residuals
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# cubic spline with knots at displacements 100,300,400
flights.data$DEPARTURE_DELAY
cspline.lm <- lm(delayed_arrival ~ bs(DEPARTURE_DELAY, knots=c(10,50,100)), data=flights.data)
rmse(cspline.lm$residuals)

visreg(cspline.lm, main="cubic spline")


#natural spline fit 
 cv.errs = rep(NA, 10)
 for (df in 3:10) {
  nspline.lm <- lm(delayed_arrival ~ ns(DEPARTURE_DELAY, df=df), data=flights.data)
   rmse(nspline.lm$residuals)
   cv.errs[df] = cv.glm(flights.data, fit, K = 10)$delta[2]
  
    #visreg(cspline.lm, main="natural spline")
  
}
which.min(cv.errs)
rmse(nspline.lm$residuals)

 #smooth spline using cross validation to determine smoothness
 sspline <- smooth.spline(x=flights.data$DEPARTURE_DELAY, y=flights.data$delayed_arrival, cv=T)
 
 #Warning message:
 #In smooth.spline(x = flights.data$DEPARTURE_DELAY, y = flights.data$delayed_arrival,  :
#                    cross-validation with non-unique 'x' values seems doubtful
 sspline$df


#visualize spline
plot(delayed_arrival ~ DEPARTURE_DELAY, data=flights.data)
#Error in plot.new() : figure margins too large

lines(sspline, col="blue", lwd=3)

#generalized additive models
memory.limit(50000)
gam.train <- train(delayed_arrival ~ s(AIRLINE, 4) , data=flights.data, 
                   method="gamSpline",tuneLength=10,
                   trControl=ctrl)

#Error: Stopping
#In addition: There were 50 or more warnings (use warnings() to see the first 50)

summary(gam.train)

visreg(gam.fit)

rmse(gam.fit$residuals)

par(mfrow = c(2,2));
plot(gam.fit)


#unsupervised learning

#remove missing data and convert factors to dummy code
flights.data <-  na.omit(flights.data)
str(flights.data)



flights.dmodel <- dummyVars(~ ., data=flights.numeric, fullRank=T)

flights.dmodel

#apply  the dummy code model to data frame
flights.d <- as.data.frame(predict(flights.dmodel, flights.data))

str(flights.d)

#lets remove the reponse variable
flights.d$delayed_arrival <- NULL

#lets fit PCA making sure to scale
flights.pca <- prcomp(flights.d, scale=TRUE) 

#view scaling centers and SD
flights.pca$center
flights.pca$scale

#view the pca loadings one PC for each 19 variables
flights.pca$rotation

#view biplot of first two components
biplot(flights.pca, scale=0)

#variance explained by each component, squaring standard deviation 
pca.var<- flights.pca$sdev^2

#proportion of variance explained
pve<- pca.var/ sum(pca.var)

#scree plot, variance explained by component
plot(pve, xlab="PCA", ylab="Prop of Variance Explained", ylim=c(0,1), type='b')

#cumulative variance explained
#scree plot, variance explained by component
plot(cumsum(pve), xlab="PCA", ylab="Cumulative Prop of Variance Explained", ylim=c(0,1), type='b')

#grabe the first five PCs
flights.pca$x[,1:5]

#shorter version of summarizing variance
summary(flights.pca)
plot(flights.pca)


#clustering k-means
set.seed(22)
#k = 3 with 20 random initilizations 
flights.kmeans.2 <- kmeans(flights.d, 2, nstart=20)
flights.kmeans.2
#seeking to reduce within sum of squares error
flights.kmeans.2$tot.withinss

#lets try k 3
flights.kmeans.3 <- kmeans(flights.d, 3, nstart=20)
flights.kmeans.3$tot.withinss

#finding optimal K by wss, try k 1 through 10
wss <- numeric(10) 
for (k in 1:10) 
{
  clust <- kmeans(flights.d, centers=k, nstart=25)
  
  wss[k] <- sum(clust$withinss)
}
#vew plot of wss by cluster size
#3 or 4 seem ideal
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 

library(cluster)
#plot the clusters by first two PCAs to reduced dimensons
clusplot(flights.d, flights.kmeans.3$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

#hierarchical clusters, scaling before calculating distance and clustering)
flights.hc.complete <- hclust(dist(scale(flights.d)), method="complete")

#plot the dendograms
plot(flights.hc.complete)
#select vertical cut to create clusters
cutree(flights.hc.complete, 4) #4 ggroups
cutree(flights.hc.complete, h=1500) #or by height




