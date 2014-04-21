#. Recognize handwritten digits using Decision Trees
#. Author: @ dspk

#. IMPORTANT USAGE INSTRUCTIONS: 
#. Get the data for the MNIST database of handwritten digits
#. Arrange the data in a list named ex3 with two fields:
#. X : a matrix of all the images so that a single row contains a single image
#. y : the labels of the images

setwd("SET CORRECT WORKING DIRECTORY")

#. The digit '0' is labeled as '10'
#. Construct matrices X and y and separate data into training and test sets
X.matrix = as.matrix(ex3$X)
y.matrix = as.factor(as.matrix(ex3$y))
newdat3 = data.frame(X.matrix, y.matrix)
table(y.matrix) # shows how the y classes are  ordered
#. Divide the data into test and training sets
set.seed(123)
for(i in 1:dim(table(newdat3$y.matrix))) {
  leveldat = which(newdat3$y.matrix == names(table(newdat3$y.matrix)[i]))  
  # use 2/3 of each one of leveldat for training, 1/3 for test
  traindat = sample(leveldat,floor(length(leveldat)*2/3))
  if(i==1){
    train = traindat
  }
  else{
    train = c(train, traindat)
  }
}
train.set = newdat3[train,]
test.set = newdat3[-train,]

require(tree)
tree.digit1 = tree(y.matrix ~., data=train.set)
#. Plot and print decision rules and errors
plot(tree.digit1)
title(main="Decision Tree Classification Model - Handwritten digits" )
text(tree.digit1)
print(tree.digit1)
summary(tree.digit1)

#. Predict model accuracy and print confusion matrix for train & test data error
predict.tree1 = predict(tree.digit1, train.set, type="class")
loss.mat.train = table("predicted class" = predict.tree1, "actual class"= train.set$y.matrix)
error.train = 1.0 - (sum(diag(loss.mat.train)))/sum(loss.mat.train)   # 31% error; # 69% accuracy on training data
classAgreement(loss.mat.train) #. examine other coefficients Comparing Classification Agreement

predict.tree1 = predict(tree.digit1, test.set, type="class")
loss.mat.test = table("predicted class" = predict.tree1, "actual class"= test.set$y.matrix)
error.train = 1.0 - (sum(diag(loss.mat.test)))/sum(loss.mat.test)   # 34% error; # 66% accuracy on test data
classAgreement(loss.mat.test) #. print classification agreement coefficients
mean(predict.test == test.set$y.matrix)

#. Tune the model parameters - here, tune mincut - minimum number of observations to include in every child node
#. smallest allowed node size
#. Find training and test data errors for 6 different trees with minsizes between 300 and 400
train_error = rep(0, 6)
test_error = rep(0, 6)  
for(i in 1:6) {
  fit.digit = tree(y.matrix ~., data=train.set, control=tree.control(nobs=3330, minsize=seq(300, 400, 20)[i]))
  predict.train = predict(fit.digit, train.set, type="class")
  train_error[i] = 1 - mean(predict.train == train.set$y.matrix)
  predict.test = predict(fit.digit, test.set, type="class")
  test_error[i] = 1 - mean(predict.test == test.set$y.matrix)
}


#. Print and plot errors
print(train_error)
print(test_error) 
plot(seq(1,6), test_error, ylim=c(0, 1.0), col="navy", type="o", pch=19, xlab='')
axis(1, at=1:6, labels=seq(1,6))
points(train_error,col="red", type="o", pch=19)
legend(3,1, c("test_error","training_error"),lwd=rep(2,3), col=c('navy','red'))


#. Tune based on cross-validation - select an optimal value for the cost complexity parameter, tree size, using cross-validation
cv.digit1 = cv.tree(tree.digit1, FUN = prune.misclass)
cv.digit1
plot(cv.digit1)

prune.tree1 = prune.misclass(tree.digit1, best = 16) #. snip off the least important splits
plot(prune.tree1)
text(prune.tree1, pretty = 0)


#. Evaluate this new pruned tree on the test data
pruned.pred = predict(prune.tree1, test.set, type = "class")
loss.mat.test = table("predicted class" = pruned.pred, "actual class"= test.set$y.matrix)
print(loss.mat.test)
test_accuracy = sum(diag(loss.mat.test))/sum(loss.mat.test) # 65% accuracy


#. Collect all the examples in the test set which are classified correctly and misclassified (i.e. all the errors)
#. we're going to plot these examples and compare them
example.misclass = test.set[which(pruned.pred != test.set[, 401]), ]  # from the test set 577 examples were misclassified
example.right = test.set[which(pruned.pred == test.set[, 401]), ]  # the remaining 1093 examples which were correctly classified


#. View a sample of 9 correctly classified test examples 
color_spec = colorRampPalette(colors = c('grey', 'black'))
jpeg("Tree_correctlyclassifieddigits.jpg")
rownum_increment = 0
row_numbers = c(125, 250, 325, 365, 425, 585, 675, 825, 905, 1050) + rownum_increment
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.right[i, -401])), dim=c(20,20))
  z = t(z[20:1,  ])
  image(1:20,1:20,z,main=example.right[i, 401], col=color_spec(256))
}
dev.off()


#. View a sample of those examples which were misclassified
jpeg("Tree_Misclassifieddigits.jpg")
rownum_increment = 0
row_numbers = c(1, 50, 100, 150, 275, 400, 450, 500, 525, 575)
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n') 
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.misclass[i, -401])), dim=c(20,20))
  z = t(z[20:1,  ])
  image(1:20,1:20,z,main=example.misclass[i, 401], col=color_spec(256))
}
dev.off()
