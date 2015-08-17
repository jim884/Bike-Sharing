library(caret)
library(gbm)
library(Cubist)
library(lubridate)
library(Metrics)
input_orig <- read.csv('train.csv')

# Cleaning Data
#As a firt step, we load the input data:
input_orig <- read.csv('train.csv', colClasses = c('character', rep('integer', 4), rep('numeric', 4), rep('integer', 3)))

input <- input_orig[, -c(6)]
#input <- input_orig[, -c(9, 10)] #trabajo con count

#Compute hour & day 
input$hour <-hour(input$datetime)
input$dow <- day(input$datetime)
#delete date
input <- input[, -c(1)]
str(input)


my.data.frame <- subset(input, count<10 & hour <22 & hour>6  )
nrow(my.data.frame)
head(my.data.frame,1000)

input$dow <- month(input$datetime)


#partition test/train
set.seed(1143)
inTrain <- createDataPartition(y=input$count, p=0.7, list=F)
training <- input[inTrain, ]
validation <- input[-inTrain, ]

#Linear Regression
#method = 'lm'
#Type: Regression
#No Tuning Parameters
set.seed(1143)
mdl<- train(count ~ ., data=training, method='lm')
p <- predict(mdl, validation)
p[p<0] <- 0
rmsle(validation$count,p)
#1.261737

#We can leverage the quadratic relationship of count with atemp and hour to improve the model:
set.seed(1153)
mdl2 <- train(count ~ . + I(atemp^2) + I(hour^2), data=training, method='lm')
p2 <- predict(mdl2, validation)
p2[p2<0] <- 0
rmsle(validation$count,p2)
#1.350259


#stochastic gradient boosting
set.seed(1143)
str(input)
mdl3 <- train(count ~ . + I(atemp^2) + I(hour^2),, data=training, method='gbm', verbose=F)
p3 <- predict(mdl3, validation)
p3[p3<0] <- 0
rmsle(validation$count,p3)
#1.350259

#The boosting has reduced the error on the validation set. We can try out another boosting algorithm, Cubist.
set.seed(1143)
mdl4 <- train(count ~ . + I(atemp^2) + I(hour^2), data=training, method='cubist')
p4 <- predict(mdl4, validation)
p4[p4<0] <- 0
res <- rmsle(validation$count,p4)
paste("p4: " , res)
#0.4430753


set.seed(1143)
mdl5.1.casual<- train(casual ~ . + I(atemp^2) + I(hour^2) - registered - count, data=training, method='cubist')
p5.1.casual <- predict(mdl5.1.casual, validation)
p5.1.casual[p5.1.casual<0] <- 0

mdl5.1.registered<- train(registered ~ . + I(atemp^2) + I(hour^2) - casual - count, data=training, method='cubist')
p5.1.registered <- predict(mdl5.1.registered, validation)
p5.1.registered[p5.1.registered<0] <- 0

mdl5.1.total <- round(p5.1.casual + p5.1.registered, 0)

rmsle(validation$count,mdl5.1.total)

