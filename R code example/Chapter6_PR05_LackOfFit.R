# Read in the data
Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH06PR05.txt", header=FALSE)

# Give names to the columns of data set
colnames(Data) <- c("Y", "X1", "X2")


# number of observations
n <- dim(Data)[1]  

# Full model
Full <- lm(Y~as.factor(X1)*as.factor(X2), data=Data)
anova(Full)

Reduced <- lm(Y~X1+X2, Data)
anova(Reduced, Full)