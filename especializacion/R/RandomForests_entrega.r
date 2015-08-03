#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
train <- read.csv("..//train.csv")
test <- read.csv("..//test.csv")

library(caret)

#inTraining <- createDataPartition(trainOrig$count, p = .7, list = FALSE)
#train <- trainOrig[ inTraining,]
#test  <- trainOrig[-inTraining,]


#install required libraries
#install.packages("lubridate") 
#install.packages('randomForest')
library(lubridate)
library(randomForest)

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
write.csv(submit, file = "random-forest.csv", row.names = FALSE)
#0.70745