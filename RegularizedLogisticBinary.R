#. Example of regularized logistic regression using an optimization solver for an unconstrained nonlinear optimization problem
#. Here we apply regularized logistic to solve a nonlinear binary classification problem with many features (using polynomial feature creation)

#. sigmoid/logistic function - synonymous with the hypothesis function of linear regression with gradient descent
#. In logistic regression the hypothesis is wrapped in a sigmoid function such that the hypothesis output is a probability
sigmoid = function(z){
  g = 1 / (1 + exp(-z))
  return(g)
}


#. Cost function for regularized logistic regression
cost = function(theta.initial, X, y, m, lambda.init){
  J = 1/m*sum((t(-y) %*% log(sigmoid(X %*% theta.initial))) - (t(1 - y) %*% log(1 - sigmoid(X %*% theta.initial)))) + 
    (lambda.init/(2*m))*sum(theta.initial[2:length(theta.initial)]^2) 
  return(J)
}


#. Gradient of the cost for regularized logistic regression 
gradient = function(theta.initial, X, y, m, lambda.init){
  grad_1 = 0  #. separate out theta0 parameter (which is not regularized) from the rest of the parameters in  theta
  grad_rem = rep(0, length(theta.initial[-1])) #. the remaining parameters in theta which are regularized
  #. separately update the bias term
  grad_1 = 1/m * sum((sigmoid(X %*% theta.initial) - y)*X[,1])
  #. Update the remaining parameters in two steps
  #. first compute the non-regularized term of the gradient
  new_matrix = rep((sigmoid(X %*% theta.initial) - y), dim(X[,-1])[2])
  new_matrix2 = matrix(colSums(new_matrix * X[,-1]), nrow=1, ncol= dim(X[,-1])[2])  #. * instead of %*% since this is element by element
  new_matrix3 = 1/m*(new_matrix2)
  #. second, compute the second term of the gradient i.e. the regularized parameters : (lambda/m)*theta
  new_theta = matrix(theta.initial[-1], nrow=dim(X_newmatrix[,-1])[2], ncol=1)
  new_theta2 = t(matrix((lambda.init/m)*new_theta, nrow=dim(X_newmatrix[,-1])[2], ncol=1))
  #. combine both terms for the complete update
  grad_rem = new_matrix3 + new_theta2
  #. collect the terms 
  return(matrix(c(grad_1, grad_rem), ncol=1))
}



#. Try out the logistic model with a dataset
getwd()
setwd("SET CORRECT WORKING DIRECTORY")
#. ex2 = read.table("./YOUR_DATA_FILE.txt", header=FALSE, sep=",")
colnames(ex2) = c("exam1score", "exam2score", "admission")
m = nrow(ex2) # number of training examples

#. Feature Mapping - create more features from each data point
#. Here I have created polynomials up to the 6th degree
X.matrix = as.matrix(ex2[, -3])
y.matrix = as.matrix(ex2[, 3])
new.matrix = poly(cbind(X.matrix[,1], X.matrix[,2]), degree=6, raw=TRUE)
x0 = rep(1, m)
X_newmatrix = cbind(x0, new.matrix)

#. Plot data
install.packages("ggplot2")
library(ggplot2)
qplot(x = exam1score, y =exam2score, data=ex2, colour=factor(admission)) +
  geom_point(aes(shape=factor(admission)), size=3) +
  labs(title = "Student exam scores", colour = "admit", shape="admit") +
  scale_colour_manual(values = c("red", "green"))

#. Plot sigmoid function
def.newdata = data.frame(y = c(rep(0, 50), rep(1, 50)), x = seq(-4, 4, length.out=100) )
qplot(x = x, y = y, data=def.newdata) +
  geom_point() + 
  stat_function(fun = sigmoid) +
  labs(title = "Sigmoid function")

#. Initialize theta
theta.initial = matrix(0, nrow=dim(X_newmatrix)[2], ncol=1)
#. Set regularization parameter lambda 
lambda = 10


#. Use an optimization solver that finds the minimum of an unconstrained function
#. We want to find the parameters theta that minimize the cost function 
theta_optimal = optim(theta.initial, cost, gradient, method="BFGS", control=list(maxit=400), X=X_newmatrix, y = y.matrix, m = nrow(X_newmatrix), lambda.init = lambda)
print(theta_optimal)  #. final theta values


#. Calculate predicted probabilities
#. and model accuracy
h = sigmoid(X_newmatrix %*% theta_optimal$par) # predicted probabilites
loss.matrix = table(as.numeric(h>=0.5), ex2$admission)  # table predicted vs observed
error = 1.0 - (loss.matrix[1,1]+loss.matrix[2,2])/sum(loss.matrix)
accuracy =  (loss.matrix[1,1]+loss.matrix[2,2])/sum(loss.matrix)   # model's training accuracy is 75%  

