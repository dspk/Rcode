#. Decision Tree Model - predict Airbags based on other variables
#. Author: @ dspk

library(rpart)

#. Read data 
dat = read.csv(".\\Carsnew.csv", header=TRUE)

#. Remove NAs
good = complete.cases(dat)
newdat = dat[good,]

#. define y variable
y_column = which(names(newdat) == "Type") 

#. Create separate datasets for training and testing
for(i in 1:dim(table(newdat[,y_column]))) {
  leveldat = which(newdat[, y_column] == names(table(newdat[,y_column])[i]))
  # use 2/3 of each one of leveldat for training, 1/3 for test
  traindat = sample(leveldat,floor(length(leveldat)*2/3))
  if(i==1){
    train =  traindat
  }
  else{
    train = c(train, traindat)
  }  
}
train.set = newdat[train,]
test.set = newdat[-train,]

#. Run decision tree model
rpart.Cars93 = rpart(Type ~., data=train.set,method="class",
                     parms=list(split="gini"))

#. Plot and print decision rules
plot(rpart.Cars93)
text(rpart.Cars93)
post(rpart.Cars93, filename='')

print(rpart.Cars93)
summary(rpart.Cars93)
printcp(rpart.Cars93)
plotcp(rpart.Cars93)

#. Predict model accuracy and print confusion matrix for test data error
predict.Cars.test = predict(rpart.Cars93, test.set, type="class") #class label outputs
predict.Cars.testprob = predict(rpart.Cars93, test.set) #probability outputs 

mean(predict.Cars.test == test.set$Type) #fitted model accurately classifies 68% of the test data
table(predict.Cars.test, test.set$Type) #confusion matrix test data(predicted vs observed)

#. Predict model accuracy and print confusion matrix for training data error
predict.Cars.train = predict(rpart.Cars93, train.set, type="class") #class label outputs 
predict.Cars.trainprob = predict(rpart.Cars93, train.set) #probability outputs 
mean(predict.Cars.train == train.set$Type) #fitted model accurately classifies 81% of the training data
table(predict.Cars.train, train.set$Type) #confusion matrix train data(predicted vs observed)

#. Find training and test data errors for trees with different depths
train_error = rep(0, 6)
test_error = rep(0, 6)
for(i in 1:6) {
  fit.Cars = rpart(Type ~., data=train.set,method="class",
                   parms=list(split="gini"), control=rpart.control(maxdepth=i))
  predict.train = predict(fit.Cars, train.set, type="class")
  train_error[i] = 1 - mean(predict.train == train.set$Type)
  predict.test = predict(fit.Cars, test.set, type="class")
  test_error[i] = 1 - mean(predict.test == test.set$Type)
  
}
#. Print and plot errors
print(train_error)
print(test_error) #test_error flattens out at 3, hence maxdep = 3
plot(seq(1,6), test_error, ylim=c(0,.5), col="navy", type="o", pch=19)
points(train_error,col="red", type="o", pch=19)
legend(3,.5, c("test_error","training_error"),lwd=rep(2,3), col=c('navy','red'))

#. Find training and test error for trees with different depths and tweaked control parameters
train_error = rep(0, 6)
test_error = rep(0, 6)
for(i in 1:6) {
  fit.Cars = rpart(Type ~., data=train.set, method="class",control=rpart.control(minsplit=0,minbucket=0,
                                                                 cp=-1, maxcompete=0, maxsurrogate=0, usesurrogate=0,
                                                                 xval=0,maxdepth=i))
  predict.train = predict(fit.Cars, train.set, type="class")
  train_error[i] = 1 - mean(predict.train == train.set$Type)
  predict.test = predict(fit.Cars, test.set, type="class")
  test_error[i] = 1 - mean(predict.test == test.set$Type)
  
}
#. Print and plot errors
print(train_error)
print(test_error)
plot(seq(1,6), test_error, ylim=c(0,.5), col="navy", type="o", pch=19)
points(train_error,col="red", type="o", pch=19)
legend(3,.5, c("test_error","training_error"),lwd=rep(2,3), col=c('navy','red'))
