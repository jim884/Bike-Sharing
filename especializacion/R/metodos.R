library(caret)
library(gbm)
library(Cubist)
input_orig <- read.csv('train.csv')
str(input)

# Cleaning Data
#As a firt step, we load the input data:
input_orig <- read.csv('train.csv', colClasses = c('character', rep('integer', 4), rep('numeric', 4), rep('integer', 3)))

#partition test/train
set.seed(1143)
input <- input_orig[, -c(6, 10, 11)]
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

#We can leverage the quadratic relationship of count with atemp and hour to improve the model:
set.seed(1143)
mdl2 <- train(count ~ . + I(atemp^2) + I(hour^2), data=training, method='lm')
p2 <- predict(mdl2, validation)
p2[p2<0] <- 0
rmsle(validation$count,p)



