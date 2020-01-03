# Read in the data
# File CH01PR19.txt is saved under the working directory
Data <- read.table(file="~/Box Sync/Hongmei/Teaching/STAT350/DataSet/CH01PR19.txt", header=FALSE)
# Give names to the columns of data set
colnames(Data) <- c("Y", "X")

n <- dim(Data)[1]  # number of observations
fit <- lm(Y ~ X, data=Data)
fit
summary(fit)

# ANOVA table
anova(fit)

# general linear test approach
Full <- lm(Y~X, data=Data)
Reduced <- lm(Y~1, data=Data) #slope=0
#general linear test 
anova(Reduced, Full)

# The estimated variance-covariance matrix for b, s2{b}
vcov(fit)

# To have 95% confidence intervals for beta0 and beta1 
confint(fit, level=0.95)
# To have 99% confidence intervals for beta1 only 
confint(fit, "X", level=0.99)

alpha <- 0.05  #1-alpha confidence level

#  Estimation of mean response
new <- data.frame(X=28)
# (1) Confidence interval of the mean response
predict(fit, new, interval="confidence", level=1-alpha, se.fit=TRUE)
# (2) prediction limits for new observation
predict(fit, new, interval="prediction", level=1-alpha)

## (3) Confidence band for the entire regression line
CI <- predict(fit, new, interval="confidence", level=1-alpha, se.fit=TRUE)
Yh.hat <- CI$fit[1]
SE.Yh.hat <- CI$se.fit
W <- sqrt(2*qf(1-alpha, 2, n-2))
LowerBound <- Yh.hat - W*SE.Yh.hat
UpperBound <- Yh.hat + W*SE.Yh.hat
Band <- c(LowerBound, UpperBound)
Band

Yhat <- fit$fitted.values   # Predicted values
e <- fit$residuals          # Residuals
   
# Residual plots
par(mfrow=c(2,2))
plot(Y~X, data=Data)

plot(Y~X, data=Data)

abline(fit)

plot(e~Yhat, xlab="Fitted", ylab="Residual", main="Residual plot against Yhat")
abline(h=0)
plot(e~Data$X, xlab="X", ylab="Residual", main="Residual plot against X")
abline(h=0)

par(mfrow=c(1,2))
Dev1 <- fit$residuals
Dev2 <- fit$fitted.values - mean(Data$Y)

plot(Data$X, Dev1, ylim=c(min(Dev1, Dev2),max(Dev1, Dev2)))  #Y-Y.hat versus X
plot(Data$X, Dev2, ylim=c(min(Dev1, Dev2),max(Dev1, Dev2)))  #Y-Y.hat versus X