insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE, fileEncoding = "UTF-8-BOM")
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)

cor(insurance[c("age","bmi","children","expenses")])

pairs(insurance[c("age","bmi","children","expenses")])
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","expenses")])

ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model
ins_model1 <- lm(expenses ~ ., data = insurance)
ins_model1

insurance$age2 <- insurance$age^2
str(insurance$age2)

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)

insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a=0, b=1, col = "red", lwd =3, lty = 2)

male_exp <- predict(ins_model2,
                    data.frame(age = 30, age2 = 30^2, children = 2,
                               
                               bmi = 30, sex = "male", bmi30 = 1,
                               smoker = "no", region = "northeast"))
male_exp

female_exp <- predict(ins_model2,
                    data.frame(age = 30, age2 = 30^2, children = 2,
                               
                               bmi = 30, sex = "female", bmi30 = 1,
                               smoker = "no", region = "northeast"))
  female_exp 