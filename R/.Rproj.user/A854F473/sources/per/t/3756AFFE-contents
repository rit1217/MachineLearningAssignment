mushrooms <- read.csv( "mushrooms.csv", stringsAsFactors = TRUE)

# veil_type : all the data has the same value -> drop it out
mushrooms$veil_type <- NULL
str(mushrooms)

table(mushrooms$type)

install.packages("OneR")
library(OneR)

mushroom_1R <- OneR(type ~., data = mushrooms)
mushroom_1R

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

install.packages("RWeka")
library(RWeka)

mushroom_JRip <- JRip(type ~., data = mushrooms)
mushroom_JRip
