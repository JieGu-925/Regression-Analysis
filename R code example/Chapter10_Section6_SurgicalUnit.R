Data <- read.table(file="~/Box Sync/Hongmei/Teaching/STAT350/DataSet/CH09TA01.txt", header=FALSE)
colnames(Data) <- c("X1","X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y", "LnY")

## Look at the X1, X2, X3, and X8 only
cor(Data[,c(10,1,2,3,8)])
pairs(Data[,c(10,1,2,3,8)])
n <- dim(Data)[1]
p <- 5
############################
#   Influence measurements
############################
fit <- lm(LnY~X1+X2+X3+X8, data=Data)
summary(fit)
#anova(fit)

######## Question (1)  # calculate individually
VIF <- rep(0,4)
VIF[1] <- 1/(1-summary(lm(X1~X2+X3+X8, data=Data))$r.squared)
VIF[2] <- 1/(1-summary(lm(X2~X1+X3+X8, data=Data))$r.squared)
VIF[3] <- 1/(1-summary(lm(X3~X1+X2+X8, data=Data))$r.squared)
VIF[4] <- 1/(1-summary(lm(X8~X1+X2+X3, data=Data))$r.squared)
VIF

######### Question (2)
# Studentized deleted residuals  #rstudent(fit)
Case <- c(1:n)
plot(Case, rstudent(fit), type="l")
text(Case, rstudent(fit), Case)
alpha <- 0.05
crit <- qt(1-alpha/2/n, n-p-1)
which(abs(rstudent(fit)) >=crit )

######### Question (3)
leverage <- hatvalues(fit)
plot(Case, leverage, type="l")
text(Case, leverage,  Case)
abline(h=2*p/n, col=2)
whichones <- which(leverage>2*p/n)
leverage[whichones]

######## Question (4)
cooks.distance(fit)
plot(Case, cooks.distance(fit), type="l")
text(Case, cooks.distance(fit))

# Notice case 17 has the largest cook's distantce
pf(cooks.distance(fit)[17], p, n-p)

# Check the DFFITS and DFBETAS
plot(Case, dffits(fit), type="l")
text(Case, dffits(fit))

dfbeta(fit)

########################### 
## Compute the average of the absolute percent difference in the fitted values
##   with and without case 17  (as on page 412)
########################

## fitted values using all observations
pred1 <- fitted.values(fit)
### Delete the 17th obs
NewData <- Data[-17,]
fit2 <-  lm(LnY~X1+X2+X3+X8, data=NewData)
pred2 <- predict(fit2, data.frame(X1=Data[,1], X2=Data[,2], X3=Data[,3], X8=Data[,8]))
mean(abs((pred2-pred1)/pred1*100))



# Type help(influence.measures) to find more
help(influence.measures)

influence.measures(fit)

