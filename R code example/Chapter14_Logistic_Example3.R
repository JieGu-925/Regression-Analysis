
# multiple logistic regression
#In a health study to invetigate an epidemic outbreak of a disease 
# that is spread by mosquitoes, individuals were randomly sampled 
#within two sectors in a city to determine if the person has 
#recently contracted the disease under study. 
#The response varible Y was coded 1 if this disease was determined
# to have been present, and 0 if not. 
#The predictors are age, socioeconomic status of household, 
# and sector within city.

setwd("~/Box Sync/Hongmei/Teaching/STAT350/DataSet")

Data <- read.table("CH14TA03.txt",header=FALSE)
colnames(Data) <- c("Case", "Age", "Status1","Status2","CitySector","Disease")
glm.out = glm(Disease ~ Age+Status1+Status2+CitySector, family=binomial(logit), data=Data)
summary(glm.out)

anova(glm.out, test='Chisq')

###### prediction for a new observation
new <- data.frame(Age=33, Status1=0, Status2=0, CitySector=0)
y.hat <- predict(glm.out, new)  #predict log(p/(1-p))
p.hat <- exp(y.hat)/(1+exp(y.hat))
p.hat

# or use option of "response" directly
p.hat <-predict(glm.out, new, type="response") 

###### Compare two models
glm2.out = glm(Disease ~ Age+CitySector, family=binomial(logit), data=Data)
anova(glm2.out, glm.out, test="Chisq")


######################
## stepwise selection
fullmod = glm(Disease ~ Age+Status1+Status2+CitySector, family=binomial(logit), data=Data)
nothing <- glm(Disease ~ 1,family=binomial, data=Data)
#Choose a model by AIC in a Stepwise Algorithm
bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),
                direction="both",trace=1)
formula(bothways)



### split data into training and testing
n <- dim(Data)[1]
library(caTools)
split <- sample.split(Data$Disease, SplitRatio=3/4)
training <- subset(Data, split==TRUE)
testing <- subset(Data, split==FALSE)

fullmod.training = glm(Disease ~ Age+Status1+Status2+CitySector, family=binomial(logit), data=training)
nothing.training <- glm(Disease ~ 1,family=binomial, data=testing)
#Choose a model by AIC in a Stepwise Algorithm
bothways.training = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),
                direction="both",trace=0)
formula(bothways.training)
model <- glm(formula(bothways.training), family=binomial(logit), data=training)
#model <- glm(Disease ~ Age+CitySector, family=binomial(logit), data=training)
predicted <- predict(model, training, type="response")
hist(predicted)

# how to assign 1 and 0 according to the predicted probabilities
table(Truth=training$Disease, Prediction=predicted>=0.5)
table(Truth=training$Disease, Prediction=predicted>=0.4)

# Use ROC curve
library(ROCR)
ROCRpred <- prediction(predicted, training$Disease)
ROCRpref <- performance(ROCRpred, measure="tpr",x.measure = "fpr")
plot(ROCRpref, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
abline(0,1)
## prediction on testing data
pred.test <- predict(model, testing, type="response")
table(Truth=testing$Disease, Prediction=pred.test>=0.4)
