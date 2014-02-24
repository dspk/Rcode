#. Handwritten digit recognition problem
#. Example of solving a problem using multiclass regularized logistic regression : one vs all approach

#. Sigmoid/logistic function - synonymous with the hypothesis function of linear regression with gradient descent
#. In logistic regression the hypothesis is wrapped in a sigmoid function so that the hypothesis output is a probability
sigmoid = function(z){
  g = 1 / (1 + exp(-z))
  return(g)
}


#. Cost function for regularized logistic regression
cost = function(theta.initial, X, y, m, lambda.init){
  J = 1/m*sum((t(-as.numeric(y)) %*% log(sigmoid(X %*% theta.initial))) - (t(1 - as.numeric(y)) %*% log(1 - sigmoid(X %*% theta.initial)))) + 
    (lambda.init/(2*m))*sum(theta.initial[2:length(theta.initial)]^2) 
  return(J)
}

#. Gradient of the cost for regularized logistic regression 
gradient = function(theta.initial, X, y, m, lambda.init){
  grad_1 = 0  #. separate out theta0 parameter (which is not regularized) from the rest of the parameters in  theta
  grad_rem = rep(0, length(theta.initial[-1])) #. the remaining parameters in theta which are regularized
  #. separately update the bias term
  grad_1 = 1/m * sum((sigmoid(X %*% theta.initial) - as.numeric(y))*X[,1])
  #. Update the remaining parameters in two steps
  #. first compute the non-regularized term of the gradient
  new_matrix = rep((sigmoid(X %*% theta.initial) - as.numeric(y)), dim(X[,-1])[2])
  new_matrix2 = matrix(colSums(new_matrix * X[,-1]), nrow=1, ncol= dim(X[,-1])[2])  #. * instead of %*% since this is element by element
  new_matrix3 = 1/m*(new_matrix2)
  #. second, compute the second term of the gradient i.e. the regularized parameters : (lambda/m)*theta
  new_theta = matrix(theta.initial[-1], nrow=dim(X_newmatrix[,-1])[2], ncol=1)
  new_theta2 = t(matrix((lambda.init/m)*new_theta, nrow=dim(X_newmatrix[,-1])[2], ncol=1))
  #. combine both terms for the complete update
  grad_rem = new_matrix3 + new_theta2
  return(matrix(c(grad_1, grad_rem), ncol=1))
}



setwd("SET CORRECT WORKING DIRECTORY")
install.packages("R.matlab")
library(R.matlab)
ex3 = readMat('ex3data1.mat')
#. a single row denotes a single training example for a handwritten digit in the matrix X which is a 5000 by 400 dimensional matrix
#. y is a 5000 dim vector containing labels for the training set
#. the digit '0' is labeled as '10'
sapply(ex3$X,  class) #. make sure is numeric
ex3$y = as.matrix(as.factor(ex3$y)) #. make sure is factor
m = nrow(ex3$X) # number of training examples

#. Construct matrices X and y
X.matrix = as.matrix(ex3$X)
y.matrix = as.matrix(ex3$y)
x0 = rep(1, dim(X.matrix)[1])
X_newmatrix = cbind(x0, X.matrix)

#. Initialize theta
theta.initial = matrix(0, nrow=dim(X_newmatrix)[2], ncol=1)
#. Set regularization parameter lambda 
lambda = 0

#. Use an optimization solver to find the parameters theta that will minimize the cost function 
y_copy = y.matrix
theta_optimal_1va = matrix(0, nrow=dim(X_newmatrix)[2], ncol=length(unique(ex3$y)))
theta_fin = vector("list", length=0)
theta_finMatrix = vector("list", length=0)
#. this is a one vs. all approach
for(i in 1:length(unique(ex3$y))){
  theta.initial = matrix(0, nrow=dim(X_newmatrix)[2], ncol=1)
  y_copy = y.matrix #. Because we will use a new y for every iteration
  #. set up as a binary class problem
  y_copy[which(y.matrix != i)] = 0  #. set labels for remaining 9 classes to 0
  y_copy[which(y.matrix == i)] = 1  #. set labels for class i to 1
  theta_optimal_lva = optim(theta.initial, cost, gradient, method = "CG", X=X_newmatrix, y = y_copy, m = nrow(X_newmatrix), lambda.init = lambda) #. use the CG methos which works better when there are a large number of parameters
  theta_fin = c(theta_fin, theta_optimal_lva)  #. collect all the results from the optimization
  theta_finMatrix = cbind(theta_finMatrix, theta_optimal_lva$par)  #. collect the optimal parameters for each iteration(class) into theta_finMatrix
}

#. Note - the paramteres in theta_finMatrix are ordered by class from 1 to 10, so the optimal parameters when class 1 == 1 and the remaining classes
#. are set = 0 is in column 1 and the optimal parameters for when class 10 is set == 1 are in column 10
#. so, before doing any calculations for predicted probabilities this order must be checked and matched with that corresponding to the 
#. y class labels in the original data
#. Therefore, re-order theta_finMatrix
A = theta_finMatrix[,10]
B = theta_finMatrix[,1:9]
C = cbind(A, B)
theta_finMat_reorder = matrix(as.numeric(unlist(C)),nrow=nrow(C)) #. convert to numeric matrix from list matrix

#. Calculate predicted probabilities
predicted.probs = sigmoid(X_newmatrix %*% theta_finMat_reorder)
class.labels = as.character(unique(ex3$y))
colnames(predicted.probs) = class.labels  #. assign class labels for predicted probabilities - note that the digit 0 is mapped to 10
predicted.probsMax = apply(predicted.probs, 1, max) #. compute max probability values across all rows. i.e. pick the class for which the logistic classifier outputs the highest probabilities
max.col(predicted.probs)
a = colnames(predicted.probs)
predicted.class = a[max.col(predicted.probs)]  #. predicted class based on max probability values

#. Compute training model accuracy
loss.matrix = table(predicted.class, ex3$y)  #. table predicted vs observed
error = 1.0 - (sum(diag(loss.matrix)))/sum(loss.matrix)
accuracy =  sum(diag(loss.matrix))/sum(loss.matrix)   #. model's training accuracy is 94%
