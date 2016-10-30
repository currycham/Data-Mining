#build function pls
#x: attribute function
#y: regression object
#z: pls components
#k: num of pls components, which is num of iteration at the same time
pls=function(x,y,k){
  theta = rep(0,k) #initialize the theta vector by zero vector
  z = matrix(0,nrow(x),k) #initialize the components matrix by zero matrix
  for (i in 1:k){
    ph=matrix(0,nrow(x),ncol(x)) 
    for(j in 1:ncol(x)){
      ph[,j]=sum(y*x[,j])
      z[,i]=z[,i]+ph[,j]*x[,j]
    }
    z[,i]=(z[,i]-mean(z[,i]))/sd(z[,i]) #orthogonalize attributes to the ith components
    theta[i]=lsfit(z[,i],y,int=F)$coef #the ith regression coefficient
    y=y-theta[i]*z[,i] #update regression object y
    for(j in 1:ncol(x)){
      x[,j]=x[,j]-sum(x[,j]*z[,i])*z[,i]/sum(z[,i]*z[,i])
    }
  }
  pairs(cbind(y,x,z)) #draw the pair plots between 
  return(list(theta, z, y)) #function return with regression coefficients, k pls components, and regression object 
}

