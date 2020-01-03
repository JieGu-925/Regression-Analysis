## simple linear regression
# Table B.5
# n is the sample, df=n-2
# delta is effect size
# alpha is significance level or Type I error 
power <- function(n, delta, alpha)
{
  crit <- qt(1-alpha/2, n-2)
  power <- pt(-crit, n-2, ncp=delta)+ 1- pt(crit, n-2, ncp=delta)
  power
}

power(100, 1, 0.05)

power(100, 3, 0.05)
