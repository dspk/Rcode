#. Example of logistic regression (binary)

#. Sigmoid/logistic function - synonymous with the hypothesis function of linear regression 
#. In logistic regression the hypothesis is wrapped in a sigmoid function so that the hypothesis output is a probability
sigmoid = function(z){
  g = 1 / (1 + exp(-z))
  return(g)
}


#. Cost function
cost = function(theta.initial, X, y, m){
  J = 1/m*sum((t(-y) %*% log(sigmoid(X %*% theta.initial))) - (t(1 - y) %*% log(1 - sigmoid(X %*% theta.initial))))
  return(J)
}


#. Gradient of the cost for logistic regression
gradient = function(theta.initial, X, y, m){
  grad = rep(0, length(theta.initial))
  new_matrix = rep((sigmoid(X %*% theta.initial) - y), dim(X)[2])
  new_matrix2 = matrix(colSums(new_matrix * X ), nrow=1, ncol= dim(X)[2])  
  grad = 1/m*(new_matrix2)
  return(grad) 
}


#. Try out the logistic model with a dataset
getwd()
setwd("SET CORRECT WORKING DIRECTORY")
Usedat = read.table("./Usedat.txt", header=FALSE, sep=",")
colnames(Usedat) = c("xvar1", "xvar2", "yvaradmit")
m = nrow(Usedat) # number of training examples

#. Construct matrices X and y
x0 = rep(1,m)
X.matrix = as.matrix(cbind(x0, Usedat[, -3]))
y.matrix = as.matrix(Usedat[, 3])

#. Summarize and plot data
summary(Usedat)
install.packages("ggplot2")
library(ggplot2)
qplot(x = xvar1, y =xvar2, data=Usedat, colour=factor(yvaradmit)) +
  geom_point(aes(shape=factor(yvaradmit)), size=3) +
  labs(title = "Student exam scores", colour = "admit", shape="admit")

#. Plot sigmoid function
def.newdata = data.frame(y = c(rep(0, 50), rep(1, 50)), x = seq(-4, 4, length.out=100) )
qplot(x = x, y = y, data=def.newdata) +
  geom_point() +
  stat_function(fun = sigmoid) +
  labs(title = "Sigmoid function")

#. Initialize theta
theta.initial = matrix(0, nrow=dim(X.matrix)[2], ncol=1)

#. Use an optimization solver that finds the minimum of an unconstrained function
#. In our case we want to find the parameters theta that minimize the cost function
theta_optimal = optim(theta.initial, cost, gradient, X=X.matrix, y=y.matrix, m=nrow(X.matrix))
print(theta_optimal) #. final theta values
