launch <- read.csv("challenger.csv", fileEncoding="UTF-8-BOM")
str(launch)

b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
a <- mean(launch$distress_ct) - b*mean(launch$temperature)

#correlation
r <- cov(launch$temperature, launch$distress_ct) / (sd(launch$temperature)*sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

#regression
reg <- function( y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  # %*% = matrix multiplication
  # t(x) = X transpose
  b <- solve(t(x) %*% x) %*% t(x) %*% y #(XtransposeX)invert XtransposeY
  colnames(b) <- "estimate"
  print(b)
}

str(launch)
reg(y = launch$distress_ct, x = launch[2]) #simple linear regression x = temperature
# beta0 = 3.70
# beta1 = -0.048

reg(y = launch$distress_ct, x = launch[2:4]) #multiple linear regression
