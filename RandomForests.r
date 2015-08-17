#
# Project: Predict Bike Sharing Demand using R
# Author : Gopinath Munisifreddy
# Date   : Aug-18-2014
#


#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
trainOrig <- read.csv("..//train.csv")
#test <- read.csv("..\\test.csv")

#install required libraries
#install.packages("lubridate") 
#install.packages('randomForest')
#install.packages('Metrics')
library(Metrics)
library(caret)
library(lubridate)
library(randomForest)

set.seed(8796)
inTraining <- createDataPartition(trainOrig$count, p = .5, list = FALSE)
train <- trainOrig[ inTraining,]
test  <- trainOrig[-inTraining,]

#Compute hour of the day
train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)

#Compute day of the week
train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)

saved <- test$count

test$count<-0
#Create a random forest
fit <- randomForest(as.factor(count) ~ season + holiday + weather + dow+ hour + temp + atemp+humidity+windspeed , data=train, ntree = 700, importance=TRUE)
#Uncomment the following line if you want to see how your model plot looks like
#varImpPlot(fit)

#Predict values and save output
Prediction <- predict(fit, test)
submit <- data.frame(datetime = test$datetime, count = Prediction)


rmsle(saved,as.numeric(submit$count))
#0.70745

warnings()
