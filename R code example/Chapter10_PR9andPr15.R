# Read in the data
Data <- read.table(file="CH06PR05.txt", header=FALSE)

# Give names to the columns of data set
colnames(Data) <- c("Y", "X1", "X2")

# number of observations
n <- dim(Data)[1]  

# 10.9(d) extrapolation?
# the design matrix X which has ones in the first column
X <- cbind(rep(1,n), Data$X1, Data$X2)
X.new <- c(1,10,3)
#h.new.new (10.29)
t(X.new) %*% solve(t(X)%*%X) %*% X.new

## 10.15 (b) VIF
R1.sq <- summary(lm(X1~X2, data=Data))$r.squared
VIF1 <- 1/(1-R1.sq)

R2.sq <- summary(lm(X2~X1, data=Data))$r.squared
VIF2 <- 1/(1-R2.sq)
c(VIF1, VIF2)