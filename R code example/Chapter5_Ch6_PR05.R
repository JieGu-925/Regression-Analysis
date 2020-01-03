# Read in the data
Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH06PR05.txt", header=FALSE)

# Give names to the columns of data set
colnames(Data) <- c("Y", "X1", "X2")

# number of observations
n <- dim(Data)[1]  

#### We only look at the Y and X1 ########################

# 1. Estimate the regression function
# the design matrix X which has ones in the first column
X <- cbind(rep(1,n), Data$X1)
Y <- Data$Y

# Compute X'X,  t(X) is the transpose of X
t(X)%*%X        

# Compute X'Y
t(X)%*%Y

# Compute inverse of X'X
solve(t(X)%*%X)

# Compute b
b <- solve(t(X)%*%X) %*% t(X)%*%Y
b

#2 Compute Yhat
Yhat <- X %*% b

#3 Compute residuals
e <- Y- Yhat

#4 REsidual plots
par(mfrow=c(2,2))
plot(e~Yhat, xlab="Fitted", ylab="Residual", main="Residual plot against Yhat")
plot(e~Data$X1, xlab="X1", ylab="Residual", main="Residual plot against X1")
plot(e~Data$X2, xlab="X2", ylab="Residual", main="Residual plot against X2")
# any pattern?

#5 Analysis of Variance
J <- matrix(1, n, n)
SSTO <- t(Y) %*% Y - 1/n * t(Y) %*% J %*% Y
SSE <-  t(Y) %*% Y - t(b) %*% t(X) %*% Y
SSR <- SSTO - SSE

p <- 2   #number of parameters
MSR <- SSR/(p-1)
MSE <- SSE/(n-p)

#6 Test where Y is realted to X1 and X2
# test statistic
F.test <- MSR/MSE
F.test

# Critical value
alpha <- 0.05
qf(1-alpha, p-1, n-p)

# Reject the null when the F.test is bigger than the critical value

#7 R-squared
R2 <- SSR/SSTO
R2

#8 Construct 90% simultaneous confidence intervals for beta1 and beta2

# The estimated variance-covariance matrix for b
# as MSE is a scalar, we have to use drop
cov.b <- drop(MSE) * solve(t(X) %*% X)
cov.b

s.b1 <- sqrt(cov.b[2,2])
alpha <- 0.10
t <- qt(1-alpha/(2*2), n-p)
b1 <- b[2]
CI.b1 <- c(b1-t*s.b1, b1+t*s.b1)
CI.b1


######################################
## You can use some built-in R functions
#  Use the ANOVA function in R
fit <- lm(Y ~ X1, data=Data)
# or use fit <- lm(Data$Y ~ Data$X1)
fit
anova(fit)

# The estimated variance-covariance matrix for b
cov.b <- vcov(fit)
cov.b

# To have 90%  confidence intervals for beta1, 
confint(fit, c("X1"), level=0.95)

# Estimation of mean response
new <- data.frame(X1=65.4)
# Confidence interval of the mean response
predict(fit, new, interval="confidence")

#prediction limits for new observation
predict(fit, new, interval="prediction")
##############################################
