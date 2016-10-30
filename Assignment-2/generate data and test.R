#generate data
y = rnorm(100)
y = y - mean(y) #center the response variable
x1 = rnorm(100) #first attribute
x1 = (x1 - mean(x1))/sd(x1) #normalize the first attribute
x2 = y + x1 + rnorm(100) #second attribute
x2 = (x2 - mean(x2))/sd(x2) #normalize the second attribute
#put two attributes into one matrix
x = cbind(x1, x2)
x = matrix(x, nrow=100, ncol=2, byrow=F)

#test data
pls(x,y,1)
pls(x,y,2)
pls(x,y,5)