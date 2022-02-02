#entropy
#P(red) = 0.6 P(white) = 0.4
H = -(P(red)*log2(P(red)) + P(white)*log2(P(white)))

curve( -x*log2(x) - (1-x)*log2(1-x), col = "red", xlab = "x", ylab = "Entropy", lwd = 4)

credit <- read.csv("credit.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
credit$default <- factor(credit$default)
str(credit)

table(credit$savings_balance)
table(credit$checking_balance)

summary(credit$months_loan_duration, credit$amount)

table(credit$default)

set.seed(123)
train_sample <- sample(1000,900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

table(credit_train$default)
table(credit_test$default)

install.packages("C50")
library

# 17th var is default which is target, so exclude
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

summary(credit_model)

credit_pred <- predict( credit_model, credit_test)
library(gmodels)
CrossTable( credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dm = c('actual default', 'predicted default'))

credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable( credit_test$default, credit_boost_pred10, prop.chisp = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

error_cost <- matrix(c(0,1,4,0), nrow = 2, dimnames = matrix_dimensions)
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable( credit_test$default, credit_cost_pred, prop.chisp = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))


