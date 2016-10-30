#read dataset from http://data.princeton.edu/eco572/datasets/cohhpop.dat
data = read.table('cohhpop.txt')
colnames(data) = c("x", "y")
attach(data)
#library for cv.glm
library("lattice")
library("boot")
#test data by picking number of knots from 1 to 20
adf = c()
rss = c()
for (k in 1:20) {
  a = cubic_spline(k, 3, data)
  #store residuals in adf list
  adf[k] = a$df.residual
  #store rss in rss list
  rss[k] = cv.glm(data, a)$delta[2]
}

#the best spline is when num of knots is 20, obtain the residuals and fitted values from 20 knots spline
optimal_spline = cubic_spline(20, 3, data)
optimal_spline_residual = optimal_spline$residuals
optimal_spline_fittedvalues = optimal_spline$fitted.values
fittedvalues_residuals = cbind(optimal_spline_fittedvalues, optimal_spline_fittedvalues)
rss_df = cbind(rss, adf, order(rss))
colnames(rss_df) = c("RSS", "DF", "ORDER")
#graph fitted values and points
plot(data$x, data$y)
lines(data$x, optimal_spline_fittedvalues, col = "blue")
#graph df and rss
plot(adf, rss, type = "b")
