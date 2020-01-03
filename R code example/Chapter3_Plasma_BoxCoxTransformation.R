


Data <- read.table(file="C:/Hongmei/Teaching/stat350/DataSet/CH03TA08.txt", header=FALSE)
# age plasma lplasma
 
age <- Data[,1]
plasma <- Data[,2]

library(MASS) 	#call the library MASS (Modern Applied Statistics with S)
boxcox(plasma~age)

#check the graph
plot(log(plasma)~age)
abline(lm(log(plasma)~age))