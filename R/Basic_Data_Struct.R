#Ex 2.1 Construct a vector "subject_name"
# patients' name: "John Doe", "Jane Doe", "Steve Graves"
# "temperature" --> patients' temperature: 98.1, 98.6, 101.4
# "flu_status" --> patients' flu_status: FALSE, FALSE, TRUE

subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

#Ex 2.2 get access to the second element in the temperature vector
temperature[2]

#Ex 2.3 Extract temperature of Jane Doe and Steve Graves
temperature[2:3]

#Ex 2.4 Get the temperature of patients EXCEPT "Jane Doe"
temperature[-2] #drop out element index 2

#Ex 2.5 Get the element with logical values
temperature[c(TRUE, FALSE, TRUE)]

#FACTORS
#Ex 2.6 Create a factor from a character vector gender
gender <- factor(c("MALE","FEMALE","MALE"))

#Ex 2.7 Add additional levels that may not appear in data
blood <- factor(c("O", "AB", "A"), levels = c("A","B","AB","O"))

#Ex 2.8 Adding order to the factor
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"), levels = c("MILD","MODERATE","SEVERE"), ordered = TRUE)

#Ex 2.9
symptoms > "MODERATE"

#LIST
#Ex 2.11 Create a list with named components for all first patient's data 
subject1 <- list( fullname = subject_name[1],
                  temperature = temperature[1],
                  flu_status = flu_status[1],
                  gender = gender[1],
                  blood = blood[1],
                  symptoms = symptoms[1])

#Ex 2.12 Extract the temperature of subject1
subject1[2]

#Ex 2.13 Use double brackets when attempting to select the list component
# GET FROM INDEX
subject1[[2]]

#Ex 2.14 Access list components directly by index name
subject1$temperature

#Ex 2.15 Access multiple value of the list using c()
subject1[c("temperature", "flu_status")]

#DATA FRAMES
#Ex 2.16 Create data frame
pt_data <- data.frame(subject_name, temperature, flu_status, gender, symptoms, stringsAsFactors = FALSE)

#Ex 2.17 Extract subject_name from the data fram
pt_data$subject_name

#Ex 2.18 Extract several columns from data frame
pt_data[c("temperature", "flu_status")]

#Ex 2.19
pt_data[2:3]

#Ex 2.20
pt_data[2,1]
pt_data$temperature[1]

#Ex 2.21
pt_data[1,2]
pt_data[c(1,3),c(2,4)]
pt_data[2,]

#Ex 2.2...
pt_data[c(1,3), c("temperature", "gender")]
pt_data[c(-2), c(-1,-3,-5)]

#MATRICES
m <- matrix(c(1,2,3,4), nrow=2)
m <- matrix(c(1,2,3,4), ncol=2)

#Ex 2.27
m1 <- matrix(c(1,2,3,4,5,6), nrow = 2)
m2 <- matrix(c(1,2,3,4,5,6), ncol = 2)
m3 <- matrix(c(1,2,3,4,5,6), nrow = 2, byrow = TRUE)
m4 <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE)

#Ex 2.28 access to [row,column]
m2[1,]

#Ex 2.29
m2[1,]

#Ex 2.30
m1[2,c(2,3)]

#Ex 2.31
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

#Ex 2.32
str(usedcars)

#Ex 2.33
summary(usedcars[c("price","mileage")])

#Ex 2.35
mean(c(36000,44000,56000))
median(c(36000,44000,56000))

#Ex 2.36
mean(usedcars$price)
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price)
quantile(usedcars$price, prob = c(0.01,0.99))
quantile(usedcars$price, seq(from=0, to=1, by=0.20))
summary(usedcars$price)

#Boxplot
boxplot(usedcars$price, main = "Boxplot of Used Car Prices", ylab = "Price ($)")

#Histogram
hist(usedcars$mileage, main ="Histogram of Used Car Prices", xlab = "Price ($)")
hist(usedcars$mileage, main ="Histogram of Used Car Mileage", xlab = "Odometer (mi.)")

#Table
table(usedcars$color)
#One-way table
model_table <- table(usedcars$model)
prop.table(model_table)

color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

#Find the mode of used car variable color.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
color_table = usedcars$color
result <- getmode(color_table)
result
table(usedcars$color)

plot( x)

#Two-way Cross-Tabulations
usedcars$conservative <- usedcars$color %in%
c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
CrossTable(x=usedcars$model, y = usedcars$conservative, chisq = TRUE)


#k-NN
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1] #without id
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), 
                         labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}
normalize(c(1,2,3,4,5))
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize)) #[2:31] except diagnosis ( first feature )
summary(wbcd_n$area_mean)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[ 470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21) #class library
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

