#' Sample size calculations for Phase 1 PK study design
#'
#' This function calculates sample size based on a desired standard error of the mean,
#' to a specified confidence, for a given mean and standard deviation.
#' 
#' The formula is n = qnorm((1+ci)/2)**2 * sd**2 / (precision*mean)**2
#' 
#'   
#'
#' @param n Sample size.  This value can be missing if sample size is desired, or 
#' specified to calculate the maximum sd for given \code{mean}, \code{precision}, and \code{ci}.
#' @param mean Mean prameter value.  User value is mandatory.
#' @param sd Standard deviation of parameter values.  If present, the function will return \code{n}.
#' If missing and \code{n} is specified, will return the maximum sd as detailed above.
#' @param precision Desired width of the standard error of the mean (SEM).  Default is 0.2, i.e. 20\% or
#' 10\% below and 10\% above the mean.  If missing, and \code{mean}, \code{sd} and \code{n} are specified,
#' \code{precision} will be calculated.
#' @param ci Confidence for the desired width of the SEM.  Default is 0.95.
#' @return The missing argument: \code{n}, \code{sd} or \code{precision}.
#' @author Michael Neely

ss.PK <- function(n,mean,sd,precision,ci=0.95){
  
  if(missing(mean)){stop("\nYou must supply a mean.\n")}
  if(missing(n)){
    if(missing(sd)){stop("\nYou must supply sd to calculate n.\n")}
    if(missing(precision)){
      cat("\nDefault precision of 0.2 (20%) applied.\n")
      precision <- 0.2
    }
    
    n <- ceiling(qnorm((1+ci)/2)**2 * sd**2 / (precision*mean)**2)
    cat(paste("n: ",n,"\n",sep=""))
    return(invisible(n))
  }
  if(missing(sd)){
    if(missing(n)){stop("\nYou must supply n to calculate sd.\n")}
    if(missing(precision)){
      cat("\nDefault precision of 20% applied.\n")
      precision <- 0.2
    }
    sd <- sqrt(n * (precision*mean)**2 / qnorm((1+ci)/2)**2)
    cat(paste("SD: ",sd,"\n",sep=""))
    return(invisible(sd))
  }
  if(missing(precision)){
    if(missing(n)){stop("\nYou must supply n to calculate precision.\n")}
    if(missing(sd)){stop("\nYou must supply sd to calculate precision.\n")}
    precision <- sqrt(qnorm((1+ci)/2)**2 * sd**2 / n)/mean
    cat(paste("\nPrecision: ",round(precision,2)," (",round(precision*100,0),"%)\n",sep=""))
    return(invisible(precision))
  }

} #end function