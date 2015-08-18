library(ggplot2)
library(caret)

# Need to set wd to project root
trainData <- read.csv("train.csv", header=TRUE, stringsAsFactors = FALSE)
trainData$dataType <- "train"
testData <- read.csv("test.csv", header=TRUE, stringsAsFactors = FALSE)
testData$dataType <- "test"
testData$casual <- NA
testData$registered <- NA
testData$count <- NA

data <- rbind(trainData, testData)

data$season = as.factor(data$season)
data$weather = as.factor(data$weather)

# Extract hour, weekday, month, and year from datetime
datetime <- as.POSIXlt(data$datetime)
hour <- as.factor(datetime$hour)
weekday <- as.factor(datetime$wday)
month <- as.factor(datetime$mon)
year <- as.factor(1900 + datetime$year)
data$datetime <- datetime
dayType <- as.factor(ifelse(data$workingday == 1, 0, ifelse(data$holiday == 1, 2, 1)))
# Compute the approximate temperature (floored to the nearest multiple of 5), since gbm needs
# to take in factors
approxTemp <- floor(data$temp / 5)
# approxTemp has a single value in the test set with approxTemp == 8
# this causes issues with gbm
approxTemp <- ifelse(approxTemp > 8, 7, approxTemp)
approxTemp <- as.factor(as.numeric(approxTemp))

data <- cbind(data, hour, weekday, month, year, dayType, approxTemp)

training <- data[data$dataType == "train",]
testing <- subset(data[data$dataType == "test", ], select = -c(casual, registered, count))

## Now we build the GBM model

# Define the range of values over which we would want to cross-validate our model
cfg.grid <- expand.grid(n.trees = c(100), interaction.depth = c(22), shrinkage = 0.2)

# Define parameters for cross validation
cfg.fit <- caret::trainControl(method = "repeatedcv")

# Initialize randomization seed
set.seed(1805)

# List of independent variables
x.vars <- c("hour", "weather", "approxTemp", "dayType", "weekday", "month", "year", "windspeed", "humidity")

GBMmodel <- caret::train(count ~ hour + weather + approxTemp + dayType + weekday + month + year + windspeed + humidity,
                  data = training,
                  method = "gbm",
                  trControl = cfg.fit,
                  verbose = TRUE
                  #,
                  #tuneGrid = cfg.grid
                  )
GBMmodel
summary(GBMmodel)

outcome <- predict(GBMmodel, newdata = training)

# calculate RMSE on training data
sqrt(sum((outcome - training$count)^2, na.rm = T)) / nrow(training)

datetime <- testing$datetime
count <- predict(GBMmodel, newdata = testing[, x.vars])
count <- ifelse(count < 0, 0, count)

rf.sub <- data.frame(datetime, count)
write.csv(rf.sub, file = "rf_gbm.csv", row.names = FALSE)
