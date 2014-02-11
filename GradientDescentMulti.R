#. Example of Gradient Descent algorithm - Multiple features
#. Author: @dspk


#. Define hypothesis function for solving linear regression problem with multiple variables
#. The hypothesis function represents the line to be fitted 
#. X is a matrix of features
#. Theta is a vector of parameters to be estimated
hypothesis = function(X, theta){
  h =  X %*% theta
  return(h)
}


#. Define cost function
#. The cost function represents the error to be minimized
#. X is a matrix of features
#. y is the vector of dependent variable 
#. Theta is the vector of parameters to be estimated
cost = function(X, y, theta){
  J = (1/(2*m))*sum((hypothesis(X, theta) - y)^2)
  return(J)
}


#. Gradient descent computation
#. alpha - learning rate specified by user
#. num_iters is the number of iterations for which the gradient descent algorithm will run
gradient = function(theta, alpha, X, y, num_iters){
  beta = theta
  beta_history = matrix(0, nrow=num_iters, ncol=dim(X)[2])
  J_history = rep(0, num_iters) 
  for(i in 1:num_iters){
    J_history[i] = cost(X, y, beta)
    new_matrix = rep(hypothesis(X, beta) - y, dim(beta)[1])
    new_matrix2 = colSums(matrix(new_matrix, nrow=nrow(X), ncol=dim(beta)[1]) * X)
    beta = beta - alpha*(1/m)*(new_matrix2)
    beta_history[i,] = beta
  }
  return(list(J_history=J_history, theta=beta, theta_history=beta_history)) 
}


#. Function to compare results from gradient descent model with normal equations model- difference in root mean squared error  
rmse = function(x, y, m, gradientdescent.fit){
  lm.model = lm(y ~ x)
  rmse.lm.model = sqrt(sum((y - lm.model$fitted)^2 / m))    #root mean squared error - normal equations model
  rmse.gradient.model = sqrt(sum((y - gradientdescent.fit)^2 / m))   #root mean squared error - gradient descent model
  rmse.difference = rmse.lm.model - rmse.gradient.model # . difference in root mean squared errors 
  print("The absolute difference in rmse from gradient descent and normal equations models is:")
  print(abs(rmse.difference))
}


#. Example of Gradient descent - multiple features
setwd("SET CORRECT WORKING DIRECTORY")

#. Read data
ex2 = read.table("./ex1data2.txt", header=FALSE, sep=",") 
colnames(ex2) = c("size", "rooms", "price")
m = length(ex2[, 1]) # number of training examples

# Construct input matrices X and y
X = ex2[, -3]
colnames(X) = c("size", "rooms")
y = ex2[, 3]

#. Perform feature normalization
mu = matrix(sapply(X, mean), nrow=1)
sigma = sapply(X, sd)
h = matrix(1, nrow=nrow(X), ncol=1)
h.mu = h%*%mu
Xminushmu = X - h.mu
h.sigma = h%*%sigma 
X_norm = Xminushmu / h.sigma   #.normalized feature vector
x0 = rep(1, m) 
X_new = as.matrix(cbind(rep(1, m), X_norm))  #.create new feature vector and add a vector of ones to accommodate the intercept term, theta0

#. Initialize theta, set alpha and iterations
theta = matrix(0, nrow=dim(X_new)[2], ncol=1)
alpha = 0.01
num_iters = 1500

#. Call gradient descent function
result = gradient(theta=theta, alpha=alpha, X=X_new, y=y, num_iters=num_iters)
result$theta  #. final theta values

#. Plot cost history 
plot(result$J_history, xlab="iterations", ylab = "cost", main="Cost function J(theta)") 

#. Calculate gradient descent fit and plot
gradientdescent.fit = X_new %*% result$theta
install.packages("ggplot2")
library(ggplot2)
ggplot(data=ex2, aes(x = size, y =price)) +
  geom_point(aes(size = rooms), col="red") +
  geom_line(aes(x=size, y=gradientdescent.fit), col="blue") +
  labs(title = "Data vs. Model fit")
  
#. Call rmse function
result.rmse = rmse(x = X_new[,2:3], y=y, m=m, gradientdescent.fit = gradientdescent.fit)
