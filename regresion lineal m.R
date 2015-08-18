#Bike Share Kaggle Competition
#12/11/2014

library('lubridate')

# This doesn't work.  You must login to dowload the file from Kaggle
# trainingURL <- "file:///account/login?ReturnUrl=/c/bike-sharing-demand/download/train.csv"
# download.file(trainingURL, destfile = "train.csv",  method="curl")

train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv")
sampleSubmission <- read.csv("sampleSubmission.csv")


#turn categorical vars into factors
train$weather_factor = factor(train$weather)
train$season_factor = factor(train$season)
train$holiday_factor = factor(train$holiday)
train$workingday_factor = factor(train$workingday)

test$weather_factor = factor(test$weather)
test$season_factor = factor(test$season)
test$holiday_factor = factor(test$holiday)
test$workingday_factor = factor(test$workingday)


#make hour a separate variable
train$hour <- hour(train$datetime)
train$hour_factor <- factor(train$hour)
test$hour <- hour(test$datetime)
test$hour_factor <- factor(test$hour)

#make day a separate variable
train$day <- wday(train$datetime)
train$day_factor <- factor(train$day)
test$day <- wday(test$datetime)
test$day_factor <- factor(test$day)

#make year a separate variable
train$year <- year(train$datetime)
train$year_factor <- factor(train$year)
test$year <- year(test$datetime)
test$year_factor <- factor(test$year)



#remove weather 4; it only has one
train$weather[train$weather==4] <- 3
test$weather[test$weather==4] <- 3

#take a small subset
smalltrain <- train[sample(1:nrow(train),500, replace=FALSE),]
smalltrain$as = smalltrain$atemp^2
smalltrain$ac = smalltrain$atemp^3
smalltrain$aq = smalltrain$atemp^4




##########################################################################
##########################################################################
# Exploration
attach(train[,])

plot(casual)
plot(registered)

#bimodal behavior based on year




##########################################################################
##########################################################################






##########################################################################
##########################################################################
#Multiple Linear Regression
attach(train)


fit1 <- lm(registered ~ 1 + temp  + atemp + windspeed + day_factor + year_factor + hour_factor + season_factor + workingday_factor + holiday_factor + weather_factor, data=train)
summary(fit1) # show results

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit1)


##########################################################################
##########################################################################





#It doesn't make sense to be linear with temperature
# There is an optimal temperature (75 F?) and the deviations either way are negative




##########################################################################
##########################################################################
#kernel regression


library(np) # non parametric library
attach(smalltrain)


npfitcas <- npreg(casual ~ 1 + atemp + weather_factor + workingday_factor + season_factor + holiday_factor + hour_factor)
npfitreg <- npreg(registered ~ 1 + atemp + weather_factor + workingday_factor + season_factor + holiday_factor + hour_factor)

summary(npfitcas)
summary(npfitreg)


########################################################################
########################################################################





##########################################################################
##########################################################################
#spline regression


attach(train)
library(earth)


marsfit <- earth(registered ~ 1 + atemp + temp + windspeed + humidity + weather_factor + workingday_factor + day_factor + season_factor + holiday_factor + hour_factor + day_factor + year_factor, thresh = .0005, degree = 2, nk = 200, trace=1, nfold = 10)
marsfitcas <- earth(casual ~ 1 + atemp + temp + windspeed + humidity + weather_factor + workingday_factor + day_factor + season_factor + holiday_factor + hour_factor + day_factor + year_factor, thresh = .001, degree = 2, nk = 200, trace=1, nfold = 10)
summary(marsfit)
summary(marsfitcas)



#predictions
mars_reg <- predict(marsfit, train)
mars_cas <- predict(marsfitcas, train)

mars_predicted_reg <- predict(marsfit, test)
mars_predicted_cas <- predict(marsfitcas, test)

#remove negative predictions
mars_predicted_reg <- mars_predicted_reg * (mars_predicted_reg > 0)
mars_predicted_cas <- mars_predicted_cas * (mars_predicted_cas > 0)

mars_predicted_count <- mars_predicted_cas + mars_predicted_reg

submission = data.frame(test$datetime,mars_predicted_count)
colnames(submission)[1] <- "datetime"
colnames(submission)[2] <- "count"


write.csv(submission, file = "spline submission1.csv", row.names=FALSE)
##########################################################################
##########################################################################



#we don't need to be quite this agnostic though.  Let's try to make a model for optimality of temp
#and transform our data