delays.df <- read.csv("FlightDelays.csv", stringsAsFactors = FALSE)

# change numerical variables to categorical first
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
delays.df$DEP_TIME <- factor(delays.df$DEP_TIME)
# create hourly bins departure time
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# Create training and validation sets.
train.df <- delays.df[1:1320, c(10, 1, 8, 4, 2)]
test.df <- delays.df[1321:2201,c(10, 1, 8, 4, 2)]
str(train.df)
train_label.df <- delays.df[1:1320,]$Flight.Status
test_label.df <- delays.df[1321:2201,]$Flight.Status

# run naive bayes
library(e1071)
delays.nb <- naiveBayes(train.df, train_label.df)
delays.nb

## predict class membership
pred.class <- predict(delays.nb, newdata = test.df)
library(caret)
confusionMatrix(factor(pred.class), factor(test_label.df))
