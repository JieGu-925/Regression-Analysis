### Things to do in R/RStudio lab
###  Show R and RStudio. Will focus on RStudio for the course
###  Four panels of RStudio


### How to use help in R and Rstudio
?plot					      # (1) help page for plot
help(plot)  				# (2) help page for plot
          
                    # (3) RStudio has help window #
##############



## severals methods to set up working directory
setwd("~/Box Sync/Hongmei/Teaching/STAT350/DataSet")
# (1) write out the whole path
# or (2) find the desired folder in the lower right panel, and click "more",
#       set as the working directory
# or (3) use RStudio main tab "File" -> "New Project"

## read in the data set
Data <- read.table("CH01PR19.txt", header=FALSE)

names(Data) <- c("GPA", "ACT")  # give the column names
plot(Data$ACT, Data$GPA)  #draw the scatter plot
plot(Data$ACT, Data$GPA, main="Problem 1.19", 
       xlab="ACT Test Score", ylab="Freshman GPA", pch=19)
fit <- lm(Data$GPA ~ Data$ACT)  # linear model
# Or
fit <- lm(GPA~ACT, data=Data)
abline(fit)   #add regression line to the scatter plot
summary(fit)
fit$fitted.values #fitted values, y_hat
e <- fit$residuals #get the residuals
sum(e)
sum(e^2)
hist(e)


### input data by hand
Return <- c(74,66,81,52,73,62,52,45,62,46,60,46,38)
New <-c(5,6,8,11,12,15,16,17,18,18,19,20,20)

# Create a data matrix
Data <- cbind(Return, New)

# Save data to your folder
# Save it as .csv type file (.txt type is fine too)
write.table(Data, "Chapter00Intro.csv", col.names=TRUE, sep=",")

### Input data from a file
Data <- read.table(file="Chapter00Intro.csv", header=TRUE, sep=",")
Data <- read.csv(file="Chapter00Intro.csv")


# plot the scatter plot
plot(Return, New, xlab="Percent of adults returning", ylab="Number of new birds")
fit <- lm(New ~ Return)    # fit the least squars regression line
abline(fit)		   # add the regression line to the scatter plot
summary(fit)			


### Find mean and standard deviation of a vector
mean(Return)
sd(Return)


### Data is a matrix, you can have access the columns by
Data[,1]   # first column

### or the rows by
Data[1,]   # first observation

### The dimension of Data
dim(Data)

dim(Data)[1] #number of observations

### Find the mean for the columns respectively
apply(Data, 2, mean)


### Normal distriubtion
# Find the area to the left of -2 for a standard normal distribution
# Same as to find the p-value
pnorm(-2)
# Find the area to the right of -2 for a normal distribution with mean 1 and sd 1.5
1-pnorm(-2, 1, 1.5)
# Find the critical value such that the area to the right of it is 0.05
qnorm(1-0.05)


### t distriubtion with 10 degrees of freedom
# Find the area to the left of -2 for a t distribution with 10 degrees of freedom
# Same as to find the p-value
pt(-2, 10)

# Find the area to the right of -2 for a t distribution with 10 degrees of freedom
# Same as to find the p-value
1-pt(-2, 10)

# Find the critical value such that the area to the right of it is 0.05
qt(1-0.05, 10)


