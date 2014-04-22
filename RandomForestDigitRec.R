#. Recognize handwritten digits using Random Forests
#. Author: @ dspk

#. IMPORTANT USAGE INSTRUCTIONS: 
#. Get the data for the MNIST database of handwritten digits
#. Arrange the data in a list named Usedat with two fields:
#. X : a matrix of all the images so that a single row contains a single image
#. y : the labels of the images

setwd("SET CORRECT WORKING DIRECTORY")

#. A single row denotes a single training example for a handwritten digit in the matrix X 
#. The digit '0' is labeled as '10'
#. Construct matrices X and y and separate data into training and test sets
X.matrix = as.matrix(Usedat$X)
y.matrix = as.factor(as.matrix(Usedat$y))
newdat3 = data.frame(X.matrix, y.matrix)
table(y.matrix) # shows how the y classes are ordered
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


require(randomForest)
forest.digit1 = randomForest(y.matrix ~., data=train.set, prox=TRUE)

print(forest.digit1) #. printing Confusion matrix, no. of variables randomly chosen at each split, OOB estimate of error
forest.digit1$ntree #. printing number of trees grown
forest.digit1$err.rate[50] #. printing OOB error rate for all trees up to the 50th tree


#. Obtain individual trees
getTree(forest.digit1, k=2) #. second tree


#. Predict model accuracy and print confusion matrix for test data error
predict.digit = predict(forest.digit1, test.set, type="class") #. class label outputs
predict.digit.prob = predict(forest.digit1, test.set) #. probability outputs
loss.matrix.test = table("predicted class" = predict.digit, "actual class"= test.set$y.matrix) #. confusion matrix test data(predicted vs observed)
error.test = 1.0 - (sum(diag(loss.matrix.test)))/sum(loss.matrix.test)
print(error.test) #. 9% error
accuracy.test = (sum(diag(loss.matrix.test)))/sum(loss.matrix.test)
print(accuracy.test) #. 91% accuracy on testing data
library(e1071)
classAgreement(loss.matrix.test) #. print classification agreement coefficients


#. Tune the model parameters - here, tune mintry - number of variables randomly chosen at each split
oob.error = rep(0, 100)
test.error = rep(0, 100)
for (i in 1:100) {
  fit = randomForest(y.matrix ~., data=train.set, mtry = i, ntree = 50)
  oob.error[i] = fit$err.rate[50]
  predict.test = predict(fit, test.set, type="class")
  test.error[i] = 1 - mean(predict.test == test.set$y.matrix)
}

#. Print and plot errors
print(oob.error)
print(test.error)
plot(seq(1,100), oob.error, ylim=c(0.05, 0.3), col="blue", type="o", pch=19, xlab='')
points(test.error,col="red", type="o", pch=19)
legend("topright", c("oob.error","test.error"),lwd=rep(2,3), col=c('blue','red'))


#. Collect all the examples in the test set which are classified correctly and misclassified (i.e. all the errors)
#. we're going to plot these examples and compare them
example.misclass = test.set[which(predict.digit != test.set[, 401]), ] # from the test set 142 examples were misclassified
example.right = test.set[which(predict.digit == test.set[, 401]), ] # the remaining 1528 examples were correctly classified

#. Print a sample of 9 correctly classified test examples 
#. The rf model was able to correctly classify some rather difficult examples
color_spec = colorRampPalette(colors = c('grey', 'black'))
jpeg("RF_correctlyclassifieddigits.jpg")
rownum_increment = 0
row_numbers = c(10, 200, 350, 525, 650, 825, 955, 1125, 1260, 1400) + rownum_increment
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.right[i, -401])), dim=c(20,20))
  z = t(z[20:1, ])
  image(1:20,1:20,z,main=example.right[i, 401], col=color_spec(256))
}
dev.off()

#. Print a sample of those examples which were misclassified
jpeg("RF_Misclassifieddigits.jpg")
rownum_increment = 0
row_numbers = c(1, 10, 15, 30, 45, 60, 75, 90, 105, 120, 135)
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.misclass[i, -401])), dim=c(20,20))
  z = t(z[20:1, ])
  image(1:20,1:20,z,main=example.misclass[i, 401], col=color_spec(256))
}
dev.off()
