library(lubridate)
library(neuralnet)

#Lubridate will help us break the datetime string down into something that the machine likes more.  We’ll also use the neuralnet package in R, although in general neural networks are generally done in Python or Torch due to R’s limitations.

# import train and test

train <- read.csv("train.csv")
 test <- read.csv("test.csv")

#make hour a separate variable
train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)

#make day a separate variable
train$day <- wday(train$datetime)
test$day <- wday(test$datetime)

#make year a separate variable
train$year <- year(train$datetime)
test$year <- year(test$datetime)

#write code to look for weather 4
train$weather[train$weather == 4]
test$weather[test$weather == 4]

#get rid of weather 4
train$weather[train$weather==4] <- 3
test$weather[test$weather==4] <- 3

#After you run this, you’ll find that the train data only has one hour with a label of “4” while the test data has two.  
#This is far from ideal, so let’s just go ahead and assign these to 3.

# make factor
train$season <- as.factor(train$season)
test$season <- as.factor(test$season)
train$workingday <- as.factor(train$workingday)
test$workingday <- as.factor(test$workingday)
train$weather <- as.factor(train$weather)
test$weather <- as.factor(test$weather)
train$year <- as.factor(train$year)
test$year <- as.factor(test$year)
train$day <- as.factor(train$day)
test$day <- as.factor(test$day)
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)

#The Neuralnet package is a bit picky, so this was the way that I got the data into something that made it happy,
#where I could also change my model fairly easily.

# turn train and test into matrices for dummy variables 
trainmat <- model.matrix(count~season+workingday+weather+year+hour+day,data=train)
testmat <- model.matrix(~season+workingday+weather+year+hour+day,data=test)

#turn matrices back into data frames because that seems to be what the neuralnet package likes
trainmat <- as.data.frame(trainmat)
testmat <- as.data.frame(testmat)

#scale count
count <- train$count/1000

#add count to trainmat
trainmat <- cbind(trainmat,count)

#Write formula
formula <- count ~ season2+season3+season4+workingday1+weather2+weather3+year2012+hour1+hour2+hour3+hour4+hour5+hour6+hour7+hour8+hour9+hour10+hour11+hour12+hour13+hour14+hour15+hour16+hour17+hour18+hour19+hour20+hour21+hour22+hour23+day2+day3+day4+day5+day6+day7

#train your data.  note that this is a neural network with 5 hidden layers of 7, 8, 9, 8, and 7 respectively.

fit <- neuralnet(formula,data=trainmat,hidden=c(7,8,9,8,7),threshold=.04,stepmax=1e+06,learningrate=.001,algorithm="rprop+",lifesign="full",likelihood=T)

#Threshold controls how close you want to get to convergence.  In other words, “early stopping.
#Increase this number for faster processing and to decrease overfitting. 
#Decrease this number for more precision but at the risk of possible overfitting.  
#I’m using the rprop+ algorithm.  I set the lifesign to full” because I like to know that the sucker is still running. 
#That’s particularly helpful when your algorithm takes a few hours to run. 

#Your mileage may vary, but this particular neural network only took a few minutes to run on my MacBook Air.

#Now for some quick housecleaning, because the neuralnet package is finicky. 
#Then we’ll use our model to make predictions using the test data, and assign them to a variable and re-scale them.

## Delete the first column.  With the neural net package, you have to be careful to always make sure that the covariate
#matrix matches your test set.  Columns must be in the same order.

str(testmat)
str(fit)
aux <- testmat

testmat <- testmat[,2:37]

#Get predictions
predict <- compute(fit,testmat)

#Assign predictions to variable because compute produces more than we need
predict<- predict$net.result

#Rescale
predict<- predict*1000

#Since this model is a bit overfitted, we’ll check to make sure we don’t have any values that are really low. 
#Since we will, we’ll set the minimum prediction to an arbitrary number, 3.8

#Check for any negative variables
predict[predict<3]
# We’ll set the minimum prediction here to 3.8
predict[predict<3] <- 3.8

submit <- data.frame(datetime = test$datetime, count=predict)
write.csv(submit, file="neuralNet.csv",row.names=FALSE)
