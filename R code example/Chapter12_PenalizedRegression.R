library("ISLR") # Book: Introduction to Statistical Learning with applications in R
library("glmnet") # penalized regression with Lasso, Ridge, Elastic Net
?Hitters
Hitters = na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

summary(Hitters)
x = model.matrix(Salary ~ . - 1, data = Hitters)
y = Hitters$Salary

#First we willdim() fit a ridge-regression model. This is achieved by calling glmnet with alpha=0 (see the helpfile). 
#There is also a cv.glmnet function which will do the cross-validation for us.
fit.ridge = glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)
cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

#Now we fit a lasso model; for this we use the default alpha=1
# Lasso

fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)
