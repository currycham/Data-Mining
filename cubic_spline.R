#function "cubic_spline", given knots "k", order "D", and dataset
cubic_spline <- function(k, D, data) {
  r = range(data$x)
  range_x = diff(r)
  knots = (range_x/(k+1)) * (1:k)
  x1 = outer(data$x, 1:D, "^")
  x2 = outer(data$x, knots, ">") * outer(data$x, knots, "-")^D
  x = cbind(x1,x2)
  #since cv.glm is used to obtain RSS, use glm here to build linear model, which should be the same thing with lm
  lmfit = glm(y~x)
  return(lmfit)
}

