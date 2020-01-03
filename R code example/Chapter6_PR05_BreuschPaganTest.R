# Read in the data
Data <- read.table(file="~/Box Sync/Hongmei/Teaching/STAT350/DataSet/CH06PR05.txt", header=FALSE)

# Give names to the columns of data set
colnames(Data) <- c("Y", "X1", "X2")

### Use the bptest in the library of lmtest
library(lmtest)
fit <- lm(Y~X1+X2, Data)
bptest(fit, studentize = FALSE)




#### step by step calculation
n <- dim(Data)[1]
fit <- lm(Y~X1+X2, Data)
res <- fit$res
SSE <- anova(fit)[3,2]

fit2 <- lm(res^2~X1+X2, Data)
SSR.star <- sum(anova(fit2)[1:2,2])

TestStat <- SSR.star/2/(SSE/n)^2
TestStat
pvalue <- 1 - pchisq(TestStat, 2)
pvalue
