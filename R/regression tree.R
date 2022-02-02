wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
summary(wine)

wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

install.packages("rpart")
library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual-predicted))
}

MAE(wine_test$quality, p.rpart)
mean(wine_train$quality)
MAE(mean(wine_train$quality), wine_test$quality)

install.packages("Cubist")
library(Cubist)
m.cubist <- cubist(x=wine_train[-12], y = wine_train$quality)
m.cubist
summary(m.cubist)

p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)
cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)
