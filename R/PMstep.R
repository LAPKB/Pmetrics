#' @title Stepwise covariate-parameter regressions
#' @description
#' `r lifecycle::badge("superseded")`
#' 
#' This function is largely superseded as it is accessed through the `$step` methods
#' for [PM_result] and [PM_cov] objects. There is rarely a need to call it directly
#' any longer. 
#' @details It will perform stepwise linear regressions on a [PM_cov] object or
#' to maintain backwards compatibility on a *PMcov* object made by [makeCov].
#' Every covariate in the model will be tested in a stepwise linear regression for their relationships
#' to each parameter in the model.  Bayesian posterior parameters and individual covariates are used.
#'
#' @param x A PMcov object which is the `$data` field of a [PM_cov] object
#' @param icen A character vector to summarize covariate values.  Default is "median", but can also be 
#' "mean".  
#' @param direction The direction for covariate elmination can be "backward" (default), "forward", or "both".  
#' @return A matrix with covariates in the rows and parameters in the columns.  Values for the matrix are the multi-variate P-values.
#' A value of `NA` indicates that the variable was not retained in the final model.
#' @author Michael Neely
#' @seealso [stats::step()]
#' @export

PMstep <- function(x,icen="median",direction="backward"){
  if(!inherits(x,"PMcov")) stop("Please supply a PMcov object made by makeCov, usually the $data field of a PM_cov object.\n")
  ncov <- attr(x,"ncov")
  if(is.null(ncov)){
    ncov <- as.numeric(readline("Your covariate object is from a previous version of Pmetrics.  Enter the number of covariates: "))
  }
  if(ncov==0) stop("\nThere are no covariates in the data.\n")
  if(!"icen" %in% names(x)){
    cat("Please update your PMcov object with makeCov.\n")
    x$icen <- icen
  }
  nvar <- ncol(x)-ncov-3
  #get start and end column numbers for covariates and par
  covStart <- 3
  covEnd <- 2+ncov
  parStart <- covEnd+1
  parEnd <- ncol(x)-1 #leave out icen column
  
  #summarize cov object by icen
  sumX <- summary(x,icen)
  
  cov.cross <- data.frame(matrix(NA,ncol=nvar,nrow=ncov,dimnames=list(cov=names(sumX)[covStart:covEnd],par=names(sumX)[parStart:parEnd])))
  
  for(i in 1:ncol(cov.cross)){
    temp <- data.frame(cbind(sumX[,(parStart+i-1)],sumX[,covStart:covEnd]))
    names(temp) <- c(names(sumX)[parStart+i-1],names(sumX)[covStart:covEnd])
    fo <- as.formula(paste(names(temp)[1]," ~ ",paste(names(temp)[-1],collapse=" + "),sep=""))
    lm.temp <- eval(substitute(lm(fo, temp)))
    step.temp <- step(lm.temp,direction=direction,trace=0)
    p.val <- summary(step.temp)$coefficients[,4]
    cov.cross[,i] <- sapply(row.names(cov.cross),function(x) p.val[match(x,names(p.val))])    
  }  
  return(cov.cross)
}
