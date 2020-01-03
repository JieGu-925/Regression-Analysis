# Brand Preference

# Small-scale study
# The relationship between degree of brand liking (Y) and
# moisture content (X1) and sweetness (X2) of the product

# Read in the data
Data <- read.table(file="C:/users/hji403/Hongmei/Teaching/stat350/DataSet/CH06PR05.txt", header=FALSE)

# Give names to the columns of data set
colnames(Data) <- c("Y", "X1", "X2")

Y <- Data[,1]
X1 <- Data[,2]
X2 <- Data[,3]

par(mfrow=c(1,1))
plot(Y~X1, pch=19, col=X2)
legend(4, 100, pch=19, c("X2=2", "X2=4"), col=c(2,4)) 

par(mfrow=c(1,2))
plot(Y[X2==2]~X1[X2==2],pch=19,  main="X2=2")
plot(Y[X2==4]~X1[X2==4], pch=19, main="X2=4")

