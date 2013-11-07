#. Simple linear model with Dummy variables and Interaction terms
#. Author: @dspk

Interaction.variables = function(Indicator.var, ToInteract.var, xvars1) {
  xvars2 = paste(Indicator.var, ToInteract.var, sep = "*")
  c(xvars1, xvars2)
}


linearInt.Model = function(RHS, yvar, dat, verbose=FALSE) {
  if(typeof(xvar) != 'character' )
    'Type of xvar should be characters'
  linearInt.model.formula = as.formula(paste(yvar, "~", paste(RHS, collapse = "+")))
  ResultInt.Model = lm(linearInt.model.formula, data = dat)
  if(verbose)
    print(summary(ResultInt.Model))
  par(mfrow=c(3,2))
  plot(ResultInt.Model)
  plot(t(dat[which(names(dat) == yvar)]), rstandard(ResultInt.Model), xlab= "yvar", ylab="Standardized residuals", pch=15, col="red")
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
yvar = names(dat[19])
xvars1 = names(dat[c(-19)])
Indicator.var = names(dat[c(13)])  #. Indicator variable aka dummy variable
ToInteract.var = names(dat[c(3:4)])

RHS = Interaction.variables(Indicator.var, ToInteract.var, xvars1)
linearInt.Model(RHS, yvar, dat, TRUE)


