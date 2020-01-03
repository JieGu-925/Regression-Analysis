Data <- read.table(file="C://Hongmei/Teaching/stat350/DataSet/CH07TA01.txt", header=FALSE)

###########################
### Generate Figure 10.4
##########################
# Give names to the columns of data set
colnames(Data) <- c("X1", "X2", "X3", "Y")
X1 <- Data$X1
X2 <- Data$X2
X3 <- Data$X3
Y <- Data$Y

fit.Yvs12 <- lm(Y~X1+X2, data=Data)
res.fit.Yvs12  <- fit.Yvs12$residuals

fit.Yvs2 <- lm(Y~X2, data=Data)
res.fit.Yvs2  <- fit.Yvs2$residuals

fit.Yvs1 <- lm(Y~X1, data=Data)
res.fit.Yvs1  <- fit.Yvs1$residuals


fit.2vs1 <- lm(X2~X1, data=Data)
res.fit.2vs1  <- fit.2vs1$residuals

fit.1vs2 <- lm(X1~X2, data=Data)
res.fit.1vs2  <- fit.1vs2$residuals


par(mfrow=c(2,2))
plot(X1,  res.fit.Yvs12, xlab="X1", ylab="Residual", main="Residual plot againt X1") 
abline(h=0, lty=2)
plot(res.fit.1vs2, res.fit.Yvs2, xlab="e(X1|X2)", ylab="e(Y|X2)", main="Added-variable plot for X1")
abline(h=0, lty=2)
abline(lm(res.fit.Yvs2~res.fit.1vs2))

plot(X2,  res.fit.Yvs12, xlab="X2", ylab="Residual", main="Residual plot againt X2") 
abline(h=0, lty=2)
plot(res.fit.2vs1, res.fit.Yvs1, xlab="e(X2|X1)", ylab="e(Y|X1)", main="Added-variable plot for X2")
abline(h=0, lty=2)
abline(lm(res.fit.Yvs1~res.fit.2vs1))


###########################
### Generate Figure 10.7
##########################
par(mfrow=c(1,1))
plot(X1, X2, xlab="X1", ylab="X2", main="", type="n")
text(X1, X2, c(1:20), cex=1.2)




