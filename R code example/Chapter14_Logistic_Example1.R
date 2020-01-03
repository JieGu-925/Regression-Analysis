#A system analyst studied the effect of computer programming experience on ability to complete
#within a specfied time a complex programming task, including debugging. Twenty-five  persons 
#were selected with varying amounts of programming experences (measured in months of experience).
#The results were coded in binary fashion: Y=1 if the task was completed successfully in the 
#allotted time, Y=0 if the task was not completed successfully.

setwd("~/Box Sync/Hongmei/Teaching/STAT350/DataSet")
Data <- read.table("CH14TA01.txt",header=FALSE)
colnames(Data) <- c("Experience", "Success", "Fitted")

#### logistic regression ####  
glm.out = glm(Success~Experience, family=binomial(logit), data=Data)
summary(glm.out)

### plot the logistic regression line and smoothing line ###
plot(Success~Experience, data=Data)
lines(Data$Experience[order(Data$Experience)], glm.out$fitted[order(Data$Experience)], 
       type="l", col="red")
title(main="Table 14.1 Data with Fitted Logistic Regression Line")

Data.smooth <- predict(loess(Success~Experience, data=Data, span=0.75))
points(Data$Experience[order(Data$Experience)], Data.smooth[order(Data$Experience)], 
      type="b",lty=2, col="green")
legend(5,0.9, c("logistic","loess smooth"), col=c("red", "green"), lty=c(1:2))

### confidence intervale for exp(beta_1)
conf.beta1 <- confint(glm.out, "Experience")
exp(conf.beta1)
