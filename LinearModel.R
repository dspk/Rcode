#. Simple linear model
#. Author: @dspk

linear.Model = function(xvar, yvar, dat, verbose=FALSE) {
  if(typeof(xvar) != 'character' )
    'Type of xvar should be characters'
  linear.model.formula = as.formula(paste(yvar, "~", paste(xvar, collapse = "+")))
  Result.Model = lm(linear.model.formula, data = dat)
  if(verbose)
    print(summary(Result.Model))
  par(mfrow=c(3,2))
  plot(Result.Model)
  plot(t(dat[which(names(dat) == yvar)]), rstandard(Result.Model), xlab= "yvar", ylab="Standardized residuals", pch=15, col="red")
  par(mfrow=c(1,1))
}

#. Trying out the function
#. Set the current working directory
getwd()
setwd("SET CORRECT WORKING DIRECTORY")
getwd()

#. Read data 
dat = read.csv(".\\loansC.Tot.csv", header=TRUE)
                         
#. Call the function ASSUMING column 19 contains the regressand
xvar = names(dat[c(-19)])
yvar = names(dat[19])
linear.Model(xvar, yvar, dat, TRUE)