#. Recognize handwritten digits with Boosting (adaboost.M1)
#. Author: @ dspk

#. IMPORTANT USAGE INSTRUCTIONS:
#. Get the data for the MNIST database of handwritten digits
#. Arrange the data in a list named ex3 with two fields:
#. X : a matrix of all the images so that a single row contains a single image
#. y : the labels of the images

setwd("SET CORRECT WORKING DIRECTORY")

#. a single row denotes a single training example for a handwritten digit in the matrix X 
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



library(adabag)
adaboost.digit1 = boosting(y.matrix ~., data = train.set, mfinal=10) #. Boosting ensemble of 10 trees (default value for mfinal=100) i.e. fit 10 trees
adaboost.digit1$weights   # weights assigned to each of the trees
head(adaboost.digit1$votes)  #. for each observation, the number of trees that assigned it to each class weighted by its alpha coeff
head(adaboost.digit1$prob)  #. for each observation, the posterior probability/support of each class(i.e. for each obs. the probability that the obs represents class1, class2,... )
tail(adaboost.digit1$class) #. the class predicted by the classifier
adaboost.digit1$importance  #. relative importance of each variable for classification

#. plot relative importance of each variable in the classification
barplot(adaboost.digit1$importance[order(adaboost.digit1$importance, decreasing=TRUE)], col="blue")

#. Predict model accuracy and print confusion matrix for test data error
predboost.digit = predict.boosting(adaboost.digit1, test.set) 
predboost.digit
loss.matrix.test = predboost.digit$confusion  #confusion matrix test data(predicted vs observed)
error.test = predboost.digit$error  
print(error.test)              # 18% error
library(e1071)
classAgreement(loss.matrix.test) #. examine other coefficients Comparing Classification Agreement


#. Tune parameters using cross validation with boosting
# Here, pick number of trees using cv - vary mfinal(number of trees) between 10 and 50 for each 10 fold cross validation run
boostcv_error = rep(0, 50)
for(mfin in 10:50){
  boostcv.digit = boosting.cv(y.matrix ~., data = train.set, v=10, mfinal=mfin)
  boostcv_error[mfin] = boostcv.digit$error  # average error
}


#. Plot the error for different tree sizes, boostcv_error
plot(seq(10, 50, 1), boostcv_error[10:50], type="l", ylab="cv error", xlab="number of trees",
     main = "Boosting cross validation error")
abline(h = min(boostcv_error[10:50]), col = "red")  #. error is lowest when num trees = 50

#. Rerun model for mfinal(num trees) = 50
nboost.digit1 = boosting(y.matrix ~., data = train.set, mfinal=50)

#. plot relative importance of each variable in the classification
barplot(nboost.digit1$importance[order(nboost.digit1$importance, decreasing=TRUE)], col="blue")

#. Predict model accuracy and print confusion matrix for test data error
npredboost.digit = predict.boosting(nboost.digit1, test.set) 
loss.matrix.test = npredboost.digit$confusion  #confusion matrix test data(predicted vs observed)
error.test = npredboost.digit$error  
print(error.test)              # 15% error
#. using number of trees based on the lowest cv error there's a 3% reduction in test error down to 15% from the previous 18%


#. Collect all the examples in the test set which are classified correctly and misclassified (i.e. all the errors)
#. we're going to plot these examples and compare them
example.misclass = test.set[which(npredboost.digit$class != test.set[, 401]), ]  # from the test set 249 examples were misclassified
example.right = test.set[which(npredboost.digit$class == test.set[, 401]), ]  # the remaining 1421 examples were correctly classified


#. View a sample of 9 correctly classified test examples 
color_spec = colorRampPalette(colors = c('grey', 'black'))
jpeg("BOOSTING_correctlyclassifieddigits.jpg")
rownum_increment = 0
row_numbers = c(10, 200, 350, 525, 655, 815, 955, 1125, 1260, 1400) + rownum_increment
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.right[i, -401])), dim=c(20,20))
  z = t(z[20:1,  ])
  image(1:20,1:20,z,main=example.right[i, 401], col=color_spec(256))
}
dev.off()


#. View a sample of those examples which were misclassified
jpeg("BOOSTING_Misclassifieddigits.jpg")
rownum_increment = 0
row_numbers = c(1, 10, 45, 75, 100, 135, 160, 180, 200, 245)
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n') 
for(i in row_numbers){
  z = array(as.vector(as.matrix(example.misclass[i, -401])), dim=c(20,20))
  z = t(z[20:1,  ])
  image(1:20,1:20,z,main=example.misclass[i, 401], col=color_spec(256))
}
dev.off()
