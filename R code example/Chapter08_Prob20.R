Data1 <- read.table("CH01PR19.txt", header=FALSE)
Data2 <- read.table("CH08PR16.txt", header=FALSE)

X1 <- Data1[,2]
X2 <- Data2[,1]
Y <- Data1[,1]
Data <- data.frame(X1, X2, Y)

fit <- lm(Y~X1+X2+I(X1*X2))

par(ask=TRUE)

plot(Y~X1, col=X2+1)

par(mfrow=c(1,2))
plot(Y~X1, col=2, pch=21,  data=Data[Data$X2==1,], main="X2=1")
abline( a=3.226318-1.649577,    b=-0.002757 +0.062245 , col=2, lty=1)
text(20,1.5, paste("slope=", -0.002757 +0.062245))

plot(Y~X1, col=3, pch=22, data=Data[Data$X2==0,], main="X2=0")
abline(a=3.226318,    b=-0.002757, col=3, lty=2)
text(20, 2,  paste("Slope=",-0.002757))
