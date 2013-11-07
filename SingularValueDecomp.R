#. Singular Value Decomposition
#. Author: @dspk


#. Function to perform SVD and plot singular vectors
#. Input 
#. variable.names - character array of variables to include in svd analysis
#. dat - data frame containing variables
#. verbose - TRUE if detailed results should be printed, FALSE(default)


SingularValueDecomp = function(data, variable.names, verbose=FALSE){
  if(typeof(variable.names) != 'character' )
    'Type of variable.names should be characters'
  svd.model = svd(scale(data[,c(variable.names)]))
  par(mfrow=c(1,2))
  #. print to pdf : variation  explained by each component/singular value
  pdf("SingularValuesSVD.pdf")   
  plot(svd.model$d, pch=19, col="blue", xlab="column", ylab="singular values")
  plot(svd.model$d^2/sum(svd.model$d^2), pch=19, xlab="column", ylab="singular values: percentages")
  dev.off()
  #. print to screen : variation  explained by each component/singular value
  plot(svd.model$d, pch=19, col="blue", xlab="column", ylab="singular values")
  plot(svd.model$d^2/sum(svd.model$d^2), pch=19, xlab="column", ylab="singular values: percentages")
  par(mfrow=c(1,1))
  
  #. plot right singular vectors
   for (i in 1:dim(svd.model$v)[2]){ 
     print(i)
     if(verbose)
       print(svd.model$v[,i])
     #. print to pdf : right singular vectors
     pdf(paste("RightSingularVector", i, ".pdf", sep=""))   
     plot(svd.model$v[,i], pch=19, xlab="column", ylab=paste("right singular vector ", i, sep="" ))
     dev.off()
  }

  #. plot left singular vectors
  for (i in 1:dim(svd.model$u)[2]){ 
    print(i)
    if(verbose)
      print(svd.model$u[,i])
    #. print to pdf : left singular vectors
    pdf(paste("LeftSingularVector", i, ".pdf", sep=""))   
    plot(svd.model$u[,i], dim(dat)[1]:1, pch=19, xlab=paste("left singular vector ", i, sep="" ), ylab="row")
    dev.off()
  }
}

#. Trying out the function
#. Set the current working directory
getwd()
setwd("Set Correct Working Directory")
getwd()

#. Read data 
dat = read.csv(".\\loansC.Tot.csv", header=TRUE)

#. Call the function 
svd.varnames = names(dat[c(1:4)])
SingularValueDecomp(dat, svd.varnames, TRUE)