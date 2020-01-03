###################
# How to make dotplot, stem-leaf plot, boxplot and 
# Normal probability plot using R 
# Brown-Forsythe test
###################

par(mfrow=c(3,2))

###################
### Run the program
Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH01TA01.txt", header=FALSE)

# To get size and hours separately, you can use
size <- Data[,1]
hours <- Data[,2]
n <- dim(Data)[1]   
#n is the number of data points

# R does not have a procedure to produce the same dot plot as in Figure 3.1(a) 
# R function to graph a dotplot
# The following is inefficient code, but is fine for small examples
# Written by Bret Larget, September 3, 2003
#
dotplot = function(x,tol=1e-08) {
  y = rep(1,length(x))
  sx = sort(x)
  dx = diff(sx)
  maxHt = 1
  ht = 1
  for(i in 2:length(sx)) {
    if(abs(dx[i-1])<tol)
      ht = ht+0.1
    else
      ht = 1
    if(ht > maxHt)
      maxHt = ht
  }
  
  plot(x,y,type="n",axes=F,xlab="",ylab="",ylim=c(1,maxHt+1))
  axis(1,pretty(x))
  points(sx[1],1,pch=16)
  ht = 1
  for(i in 2:length(sx)) {
    if(abs(dx[i-1])<tol)
      ht = ht+0.1
    else
      ht = 1
    points(sx[i],ht,pch=16)
  }
  invisible(NULL)
}

# To use above function, type
dotplot(size)

# stem-leaf plot
 stem(size, scale=2)

# boxplot
boxplot(size) 

#histogram
hist(size)

# Sequence plot
plot(1:n, size, type="b")


#################################################################
# R does not have a function for normal probability plot 
# You can use the following commands to generate your own normal probability plot
# Written by Hongmei Jiang in Winter 2007


Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH01PR19.txt", header=FALSE)
Y <- Data[,1]
X <- Data[,2]
n <- dim(Data)[1]   
#n is the number of data points

fit <- lm(Y~X)
res <- fit$residuals
MSE <- anova(fit)[2,3]

# sort the residuals from the smallest to largest 
res.sorted <- sort(res)
seq <- c(1:n)
Percentile <- (seq - 0.375)/(n+0.25)
res.expected <- sqrt(MSE)*qnorm(Percentile)

# Normal probability plot
plot(res.expected, res.sorted, xlab="Expected", 
         ylab="Residual", main="Normal probability plot")
abline(lm(res.sorted~res.expected), col=2)

# Compute the correlation between the ordered residuals and expected values under normality
cor(res.sorted, res.expected)



#################################################
# R does not have a function for Brown Forsythe test 
# You can use the following commands 
# Written by Michael McAssey, UC Davis


#1. Break the residuals into two groups. 
Group1 <- res[X<26]
Group2 <- res[X>=26]

#2. Obtain the median of each group, using the commands: 
M1 <- median(Group1) 
M2 <- median(Group2) 

#3. Obtain the mean absolute deviation for each group, using the commands: 
D1 <- sum( abs( Group1 - M1 )) / length(Group1) 
D2 <- sum( abs( Group2 - M2 )) / length(Group2) 

#4. Calculate the pooled standard error, using the command: 
s <- sqrt( ( sum( ( abs(Group1 - M1) - D1 )^2 ) + sum( ( abs(Group2 - M2) - D2 )^2 ) ) / (n-2) ) 

#5. Finally, calculate the Brown-Forsythe test statistic, using the command: 
 t <- ( D1 - D2 ) / ( s * sqrt( 1/length(Group1) + 1/length(Group2) ) ) 

#6 Once you obtain this value, you can compare it to the critical value for any given alpha level to determine whether or not to conclude constancy of error variance, 
# or you can find its P-value. 
alpha <- 0.01
qt(1-alpha/2, n-2)   # find the catical value

# And the P-value can be found by typing: 
 2*(1-pt( abs(t), n-2))