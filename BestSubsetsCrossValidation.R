#. Best Subsets model with Cross Validation
#. Author: @dspk

library(leaps)

#. Function to compute Best Subsets based on Cp and Cross Validation
#. Input 
#. xvar - character array
#. yvar - numeric vector containing the regressand
#. dat - data frame containing x and y variables
#. K - number of folds for cross validation
#. verbose - TRUE if detailed results should be printed, FALSE(default)

Best.Subsets = function(xvar, yvar, dat, K, verbose=FALSE) {
  if(typeof(xvar) != 'character' )
    'Type of xvar should be characters'
  Model.formula = as.formula(paste("yvar", "~", paste(xvar, collapse = "+")))
  X = model.matrix(lm(Model.formula, data = dat))[,-1]
  leaps.Model = leaps(x=X,y=yvar,nbest=5,method="Cp")
  Result.leaps = cbind(leaps.Model$size, leaps.Model$which, leaps.Model$Cp) 
  colnames(Result.leaps)[1] = "ModelSize"
  colnames(Result.leaps)[ncol(Result.leaps)] = "$Cp"
  for(i in 2:length(xvar)) {
    colnames(Result.leaps)[i] = paste("var", i-1, sep="")
  }
    
  #. Cross-Validating each model
  library(boot)
  #. Create vector to store cross-validation errors
  leaps.Model$CV = 0 * leaps.Model$Cp
  for (i in 1:nrow(leaps.Model$which)) {
    subset = c(1:ncol(X))[leaps.Model$which[i, ]]
    if (length(subset) > 1) {
      Xnew = X[, subset]
      Newlm = glm(yvar ~ Xnew)
      leaps.Model$CV[i] = cv.glm(model.frame(Newlm), Newlm, K = K)$delta[1]
    } else {
      Xnew = X[, subset[1]]
      Newlm = glm(yvar ~ Xnew)
      leaps.Model$CV[i] = cv.glm(model.frame(Newlm), Newlm, K = K)$delta[1]
    }
  }
  leaps.Model.best.Cp = leaps.Model$which[which(leaps.Model$Cp == min(leaps.Model$Cp)), ] #.Best Model based on Cp
  leaps.Model.best.CV = leaps.Model$which[which(leaps.Model$CV == min(leaps.Model$CV)), ] #.Best Model based on CV
  
  #. Plot model size vs. Cp
  plot(leaps.Model$size, leaps.Model$Cp, pch=23, bg='orange', cex=2)
  
  #. Print Model Results
  if(verbose)
    print("Detailed Results")
  print(leaps.Model) 
  print("Results Summary")
  #. Column 1 gives the number of variables/attributes in the model; 
  #. Column2 gives the number of parameters(size incl. intercept)
  print(Result.leaps)
  print(paste("Minimum Cp = ", round(min(leaps.Model$Cp), 3), sep=""))
  print("Best Subset Model based on Cp statistic:")
  print(leaps.Model.best.Cp)
  print("Best Subset Model based on CV error:")
  print(leaps.Model.best.CV)
  
  }


#. Trying out the function
#. Set the current working directory
getwd()
setwd("Set Correct Working Directory")
getwd()

#. Read data 
dat = read.csv(".\\loansC.Tot.csv", header=TRUE)

#. Call the function ASSUMING column 19 contains the regressand
xvar = names(dat[c(1:7)]) 
yvar = as.matrix(dat[19]) 
K = 5
Best.Subsets(xvar, yvar, dat, K, TRUE)