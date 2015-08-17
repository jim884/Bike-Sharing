setwd('~/repos/Bike-Sharing/especializacion')
library(party)
library(rpart)
library(pROC)
library(caret)
library(mboost)
library(ipred)
library(earth)
library(gbm)
library(plyr)
library(randomForest)
library(Metrics)
require(doMC)
registerDoMC(cores=4)
source('~/Repos/Bike-Sharing/especializacion/R/caret/mymetric.R')
source('~/Repos/Bike-Sharing/especializacion/R/caret/tools.R')


#read in train/test
test_classes = c(
  "character", # datetime
  "factor", # season
  "factor", # holiday
  "factor", # workingday
  "factor", # weather
  "numeric", # temp
  "numeric", # atemp
  "integer", # humidity
  "numeric", # windspeed
  "numeric", #registered
  "integer" #count
)
set_up_features <- function(df) {
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %H:%M:%S")
  df$hour <- as.factor(df$datetime$hour)
  df$wday <- as.factor(df$datetime$wday)
  df$month <- as.factor(df$datetime$mon)
  df$year <- as.factor(df$datetime$year + 1900)
  df
}


# data loading 
# split the datetime to hour, month, year, and wday using strptime
# calculate log(count+1) to replace count, which will be used for prediction.
#Advantage: 
#  1. the evalutation RMSLE metrics will be transofrmed to RMSE, one of the default metrics in *caret::train*. 
#  2. The prediction of regression won't be negative. 
#- removing the unnecesary features
# 1. casual, register: Useless -- not appear in the test set.
# 2. datetime, count: Redaudant -- replaced by new features

train <- read.csv("train.csv", colClasses=test_classes)
test <- read.csv("test.csv", colClasses=test_classes[1:9])
train$count <- as.integer(train$count)

train_factor <- set_up_features(train)
test_factor <- set_up_features(test)

train_factor$lgcount <- train_factor$count #log(train_factor$count+1)
train_factor<- train_factor[,-12]
train_factor<- train_factor[,-11]
train_factor<- train_factor[,-10]
train_factor<- train_factor[,-1]
test_factor<- test_factor[,-1]


### Sample split and CV optimization -- use *caret*

#Split the sample into 20% testing subsample and 80% trainning subsample.
set.seed(212312)
trainIndex <- createDataPartition(train_factor$lgcount, p = 0.8, list=FALSE, times=1)
subTrain <- train_factor[trainIndex,]
subTest <- train_factor[-trainIndex,]


#MODEL 1
### A simple tree model
#- a simple test using Conditional Inference Tree (*party::ctree*)
#- Use the entire dataset with no sample splitting 
#create our formula
formula <- lgcount ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + month + year
#build our model
fit.ctree.party <- ctree(formula, data=subTrain,controls=ctree_control(mincriterion=0.95,savesplitstats=FALSE))
ctree.prediction <- predict(fit.ctree.party, subTest)
rmsle(subTest$lgcount,ctree.prediction)
#-- The RMSLE of left-out testing subsample is 0.418. Not bad, actually. 
#-- The tree is quite large (~46MBs), and I don't know how to examine the result easily. Let's try *caret* instead. 

### Modeling with *caret*
#### Try to reproduce the *party::ctree* result with trainning subsample 
# all features 
formula <- lgcount ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + month + year
fit.ctree <- train(formula, data=subTrain,method='ctree',tuneGrid=expand.grid(mincriterion=0.95))
ctree.prediction <- predict(fit.ctree, subTest)
rmsle(subTest$lgcount,ctree.prediction)
#0.7233769


#### Model1: Try *ctree2* with 6-fold CV optimization with *caret*
#here I use ctree2, since the parameter (maxdepth) is easier to understand.
#use RMSE to select the best model 
##ctree2 with CV
fitControl <- trainControl(method = 'cv', number=6,summaryFunction=defaultSummary)
set.seed(123)
Grid <- expand.grid(maxdepth = seq(15, 50,5))
formula <- lgcount ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + month + year
fit.ctree2CV <- train(formula, data=subTrain, method = 'ctree2', trControl=fitControl,tuneGrid=Grid,metric='RMSE')
ctree.prediction <- predict(fit.ctree2CV, subTest)
rmsle(subTest$lgcount,ctree.prediction)
#0.7073847
plot(fit.ctree2CV)



#### Model2: Try *caret::rpart* with 6-fold CV optimization
# try another tree-like model: CART using *caret::rpart* 
##model2a: CART using rpart with CV
set.seed(123)
fitControl <- trainControl(method = 'cv', number=6)
Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
formula <- lgcount ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + month + year
fit.rpartCV <- train(formula, data=subTrain, method = 'rpart', trControl=fitControl, metric='RMSE',maximize=FALSE, tuneGrid = Grid)
ctree.prediction <- predict(fit.rpartCV, subTest)
rmsle(subTest$lgcount,ctree.prediction)
#0.6456003
##model2b: rpart2 with CV
set.seed(123)
formula <- lgcount ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + month + year
fitControl <- trainControl(method = 'cv', number=6)
Grid<-expand.grid(.maxdepth=seq(5,20,5))
fit.rpart2CV <- train(formula, data=subTrain, method = 'rpart2', trControl=fitControl, metric = 'RMSE', maximize=FALSE, tuneGrid=Grid)
ctree.prediction <- predict(fit.rpart2CV, subTest)
rmsle(subTest$lgcount,ctree.prediction)
#1.302553
plot(fit.rpartCV)
plot(fit.rpart2CV)



