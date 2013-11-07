#. Best Subsets model
#. Author: @dspk

library(leaps)

#. Function to compute Best Subsets based on Cp
#. Input 
#. xvar - character array
#. yvar - numeric vector containing the regressand
#. dat - data frame containing x and y variables
#. verbose - TRUE if detailed results should be printed, FALSE(default)

Best.Subsets = function(xvar, yvar, dat, verbose=FALSE) {
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
  #. Best Model based on Cp
  leaps.best.Cp =  leaps.Model$which[which(leaps.Model$Cp == min(leaps.Model$Cp)), ] 
  
  if(verbose)
    print("Detailed Results")
    print(leaps.Model) 
  
  print("Results Summary")
  #. Column 1 gives the number of variables/attributes in the model; 
  #. Column2 gives the number of parameters(size incl. intercept)
  print(Result.leaps)
  print(paste("Minimum Cp = ", round(min(leaps.Model$Cp), 3), sep=""))
  print("Best Subset Model is formed with the variables:")
  print(leaps.best.Cp)
  
  plot(leaps.Model$size, leaps.Model$Cp, pch=23, bg='orange', cex=2)
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
Best.Subsets(xvar, yvar, dat, TRUE)