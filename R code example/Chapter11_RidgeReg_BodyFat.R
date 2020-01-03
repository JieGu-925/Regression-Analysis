######## Ridge regression ###########

Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH07TA01.txt", header=FALSE)

###########################
### Generate Figure 11.3, Table 11.2 and Table 11.3
##########################
# Give names to the columns of data set
colnames(Data) <- c("X1", "X2", "X3", "Y")
cor(Data)
rXX <- cor(Data)[1:3,1:3]
rXY <- cor(Data)[1:3,4]
#c <- c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.02, 0.03, 0.04, 0.05, 0.1, 0.5, 1)
c<- seq(0, 0.03, 0.002)
level.c <- length(c)

# Identity matrix
I <- diag(1,3,3)

# Ridge standardized regression coefficients
bR <- matrix(0, level.c, 3)

# VIF
VIF <- matrix(0, level.c, 3)

for(i in 1:length(c))
{ 
 bR[i,] <- solve(rXX+ drop(c[i])*I)%*%rXY
 VIF[i,] <- diag(solve(rXX+ drop(c[i])*I)%*%rXX%*%solve(rXX+ drop(c[i])*I))
}

## Ridge trace plot ########
plot(c, bR[,1], lty=1, col=1, type="l", xlim=c(0,0.03), ylim=c(-1,4), lwd=2,
 main="Ridge trace of estimated std. reg. coef.")
points(c, bR[,2],type="l", lty=2, col=2, lwd=2)
points(c, bR[,3], type="l",lty=3, col=3, lwd=2)
abline(h=0)
legend(0.015, 3, c("X1","X2","X3"), col=1:3, lty=1:3, lwd=2, cex=2)

Result <- cbind(c, bR, VIF)
colnames(Result) <- c("c", "b1R", "b2R", "b3R", "VIF1", "VIF2", "VIF3")
print(Result)
write.table(Result, file="C:/Hongmei/Teaching/stat350/Codes/Chapter11_Ridge_BodyFat_Result.csv", sep=",")


#####################################################
# Choosen model and transform back to the original varialbes (See Chapter 7)
b <- bR[c==0.02,]
b3 <- sd(Data$Y)/sd(Data$X3)*b[3]
b2 <- sd(Data$Y)/sd(Data$X2)*b[2]
b1 <- sd(Data$Y)/sd(Data$X1)*b[1]
b0 <- mean(Data$Y)-b1*mean(Data$X1) - b2*mean(Data$X2)-b3*mean(Data$X3)

print(c(b0,b1,b2, b3))





##############################
######### loess and lowess
colnames(Data) <- c("X1", "X2", "X3", "Y")
plot(Y~X1, data=Data)
lowess.fit <- lowess(Data$Y~Data$X1)
lines(lowess.fit)

loess.fit <- loess(Data$Y~Data$X1+Data$X2)
predict(loess.fit)