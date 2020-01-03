# Read in the data
Data <- read.table(file="C:/Users/hji403/Hongmei/Teaching/STAT350/DataSet/CH06FI05.txt", header=FALSE)

# Give names to the columns of data set
colnames(Data) <- c("X1", "X2", "Y")

# number of observations
n <- dim(Data)[1]  

# Scatter plot of matrix
pairs(Data[,c(3,1,2)])

# correlation matrix
cor(Data[,c(3,1,2)])



######################################
## You can use some built-in R functions
#  Use the ANOVA function in R
# 1)
fit <- lm(Y ~ X1+X2, data=Data)
summary(fit)

# 2)
Yhat <- fit$fitted.values
e <- fit$residuals

#3 REsidual plots
par(mfrow=c(2,2))
plot(e~Yhat, xlab="Fitted", ylab="Residual", main="Residual plot against Yhat")
plot(e~Data$X1, xlab="X1", ylab="Residual", main="Residual plot against X1")
plot(e~Data$X2, xlab="X2", ylab="Residual", main="Residual plot against X2")
X1X2 <- Data$X1*Data$X2
plot(e~X1X2, xlab="X1X2", ylab="Residual", main="Residual plot against X1X2")

#4 Normal Probability plot
p <- 3
MSE <- sum(e^2)/(n-p)

# sort the residuals from the smallest to largest 
res.sorted <- sort(e)
seq <- c(1:n)
Percentile <- (seq - 0.375)/(n+0.25)
res.expected <- sqrt(MSE)*qnorm(Percentile)

# Normal probability plot
plot(res.expected, res.sorted, xlab="Expected", ylab="Residual", main="Normal probability plot", pch=19)

# Compute the correlation between the ordered residuals and expected values under normality
cor(res.sorted, res.expected)

## 5)
anova(fit)

# 6)  check F-test from ANOVA 
# 7) 

# 8) To have 90% simultaneous confidence intervals for beta1 and beta2, 
# each is constructed using 95% confidence coefficient
confint(fit, c("X1", "X2"), level=0.95)

## 9)
# Estimation of mean response
New <- data.frame(X1=65.4, X2=17.6)
# Confidence interval of the mean response
predict(fit, New, interval="confidence")


#10 Prediction limits for new observations
# City A with X1=65.4 and X2=17.6  and City B with X1=53.1 and X2=17.7 
# 90% joint CI (each one is 95% CI)

CityA <- data.frame(X1=65.4, X2=17.6)
CityB <- data.frame(X1=53.1, X2=17.7)

#prediction limits for new observation
predict(fit, CityA, interval="prediction")
predict(fit, CityB, interval="prediction")
##############################################




