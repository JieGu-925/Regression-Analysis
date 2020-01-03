# Read in the data

# (1) File CH01PR19.thourst is saved under the working directorsize
# Data <- read.table(file="CH01PR19.txt", header=FALSE)

# (2) or give the specific path like below
Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH01TA01.txt", header=FALSE)


# To get size and hours separately, you can use
size <- Data[,1]
hours <- Data[,2]

# fit the simple linear regression line and get the confidence interval for beta1
fit <- lm(hours ~ size)
b1 <- fit$coef[2]
confint(fit, "size", level=0.95)


##################
## construct confidence intervals using bootstrap
Fitted.Value <- fit$fitted.values
Residual <- fit$residuals

Boot.Num <- 1000
b1.boot <- rep(0, 1000)
for(i in 1:Boot.Num)
{
 Residual.boot <- sample(Residual, replace=TRUE)
 Sample.boot <- Fitted.Value+Residual.boot
 fit.boot <- lm(Sample.boot ~ size)
 b1.boot[i] <- fit.boot$coef[2]
}
sd(b1.boot)
hist(b1.boot)
abline(v=b1, col=2)

alpha <- 0.05
d1 <- b1 - quantile(b1.boot, alpha/2)
d2 <- quantile(b1.boot, 1-alpha/2)-b1
c(b1-d2, b1+d1)