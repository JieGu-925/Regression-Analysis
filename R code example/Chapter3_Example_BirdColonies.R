### Chapter3_Example_BirdColonies.R

Return <- c(74,66,81,52,73,62,52,45,62,46,60,46,38)
New <-c(5,6,8,11,12,15,16,17,18,18,19,20,20)

# plot the scatter plot
plot(Return, New, xlab="Percent of adults returning", ylab="Number of new birds")
fit <- lm(New ~ Return)       # fit the least squars regression line
abline(fit)				# add the regression line to the scatter plot
summary(fit)			


Data <- cbind( Return, New)
Data[order(Return),]
C <- length(unique(Return))    # unique levels of Return
table(Return)

Unique.x <- unique(Return)
y.bar <- rep(0, C)
for(i in 1:length(unique(Return)))
{
 y.bar[i] <- mean(New[Return==Unique.x[i]])
}

points(Unique.x, y.bar, pch ="*", col=2)


#lack of fit test
Full <- lm(New~as.factor(Return))
anova(Full)
anova(fit, Full)