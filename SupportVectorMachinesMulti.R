#. Handwritten Digit Recognition 
#. Author: @dspk

#. Recognizing handwritten digits using the Support Vector Machine (SVM) algorithm
#. IMPORTANT USAGE INSTRUCTIONS: 
#. Get the data for the MNIST database of handwritten digits
#. Arrange the data in a list named Usedat with two fields:
#. X : a matrix of all the images so that a single row contains a single image
#. y : the labels of the images

#. The digit '0' is labeled as '10'
#. Construct matrices X and y and separate data into training and test sets
X.matrix = as.matrix(Usedat$X)
y.matrix = as.factor(as.matrix(Usedat$y))
newdat3 = data.frame(X.matrix, y.matrix)

#. Divide the data into test and training sets
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

#. Model training data using SVM
library(e1071)
svmd.radialkern = svm(y.matrix ~., data=train.set, scale=FALSE) #. cost denotes the regularization term C
summary(svmd.radialkern)

#. Assess model performance on the test data
predict.test = predict(svmd.radialkern, newdata=test.set)
loss.matrix.test = table("predicted class" = predict.test, "actual class"= test.set[, 401]) # table predicted vs. actual
print(loss.matrix.test)
error.test = 1.0 - (sum(diag(loss.matrix.test)))/sum(loss.matrix.test)
print(error.test)
accuracy.test = (sum(diag(loss.matrix.test)))/sum(loss.matrix.test)
print(accuracy.test) # 90% accuracy on testing data
classAgreement(loss.matrix.test) #. examine other coefficients Comparing Classification Agreement

#. Tune the model - determine the best C and gamma parameters to use
#. We use 10 fold cross-validation
#. Here we perform cross validation using 10 different values of gamma from (0.001, 0.01, 0.1...30)
#. and 3 different values of cost from(1, 10, 30) which gives us 30 different models based on the training data
tuned.train = tune.svm(y.matrix ~., data = train.set, gamma = c(10^(-3:1), 3*10^(-3:1)), cost = c(1, 10^(1:2)), scale=FALSE)
summary(tuned.train)
#. The cross validation results indicate that the lowest misclassification error (4%)
#. is attained for when gamma = 0.1 and cost =1

#. Rerun model with these new parameters derived from cross-validation and assess model performance
svmd.radialkerncv = svm(y.matrix ~., data=train.set, gamma=0.1, cost=1, scale=FALSE)
summary(svmd.radialkerncv)
predict.test = predict(svmd.radialkerncv, newdata=test.set) # predicted classes
loss.matrix.test = table("predicted class" = predict.test, "actual class"= test.set[, 401])
print(loss.matrix.test)
error.test = 1.0 - (sum(diag(loss.matrix.test)))/sum(loss.matrix.test)
print(error.test)
accuracy.test = (sum(diag(loss.matrix.test)))/sum(loss.matrix.test)
print(accuracy.test) # 96% accuracy on testing data
classAgreement(loss.matrix.test) #. examine other coefficients Comparing Classification Agreement


#. Collect all the examples in the test set which are classified correctly and misclassified (i.e. all the errors)
#. we're going to plot these examples and compare them
example.misclass = test.set[which(predict.test != test.set[, 401]), ] # from the test set 65 examples were misclassified
example.right = test.set[which(predict.test == test.set[, 401]), ] # the remaining 1605 examples which were correctly classified

#. View a sample of 9 correctly classified test examples 
color_spec = colorRampPalette(colors = c('grey', 'black'))
rownum_increment = 0
row_numbers = c(1, 200, 350, 500, 650, 950, 1100, 1250, 1400, 1600) + rownum_increment
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.right[i, -401])), dim=c(20,20))
  z = t(z[20:1, ])
  image(1:20,1:20,z,main=example.right[i, 401], col=color_spec(256))
}

#. View some examples which were misclassified
row_numbers = c(1, 8, 10, 18, 23, 30, 33, 40, 48, 56)
par(mfrow=c(4,4),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.misclass[i, -401])), dim=c(20,20))
  z = t(z[20:1, ])
  image(1:20,1:20,z,main=example.misclass[i, 401], col=color_spec(256))
}

