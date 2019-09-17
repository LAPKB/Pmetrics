#' Generates summary statistics of final population model parameters.
#'
#' For NPAG runs, this function will generate weighted medians as central tendencies of the
#' population points with a 95\% confidence interval (95\% CI) around the median, 
#' and the median absolute weighted deviation (MAWD) from the median as a measure 
#' of the variance, with its 95\% CI.  These estimates correspond to weighted mean, 
#' 95\% CI of the mean, variance, and 95\% CI of the variance, respectively, for a 
#' sample from a normal distribution.  To estimate these non-parametric summaries, 
#' the function uses a Monte Carlo simulation approach, creating  1000 x npoint samples 
#' with replacement from the weighted marginal distribution of each parameter, 
#' where npoint is the number of support points in the model.  As an example, 
#' if there are 100 support points, npoint = 100, and for Ka, there will be 
#' 1000 sets of 100 samples drawn from the weighted marginal distribution of the 
#' values for Ka.  For each of the 1,000 sets of npoint values, the median and MAWD are 
#' calculated, with MAWD equal to the median absolute difference between each point 
#' and the median of that set.  The output is npoint estimates of the weighted median 
#' and npoint estimates of the MAWD for each parameter, from which the median, 2.5th, 
#' and 97.5th percentiles can be found as point estimates and 95\% confidence 
#' interval limits, respectively, of both the weighted median and MAWD.
#' 
#' For IT2B runs, the function will return the mean and variance of each parameter,
#' and the standard errors of these terms, using SE (mean) = SD/sqrt(nsub) and 
#' SE (var) = var * sqrt(2/(nsub-1)).
#'
#' @title Summary Statistics for PMfinal Objects
#' @method summary PMfinal
#' @param x The PMfinal object made after an NPAG or IT2B, e.g. final.1 after run 1.
#' @param lower Desired lower confidence interval boundary.  Default is 0.025. Ignored for IT2B objects.
#' @param upper Desired upper confidence interval boundary.  Default is 0.975. Ignored for IT2B objects.
#' @return The output is a data frame.
#' For NPAG this has 4 columns:
#' \item{value }{The value of the summary statistic}
#' \item{par }{The name of the parameter}
#' \item{type }{Either \emph{WtMed} for weighted median, or \emph{MAWD} for MAWD (see details)}
#' \item{quantile }{Requested \code{lower}, 0.5 (median), and \code{upper} quantiles}
#' For IT2B this has 5 columns:
#' \item{mean }{Parameter mean value}
#' \item{se.mean }{Standard error of the mean}
#' \item{cv.mean }{Error of the mean divided by mean}
#' \item{var }{Variance of the parameter values}
#' \item{se.var }{Standard error of the variance}
#' @author Michael Neely
#' @seealso \code{\link{makeFinal}}, \code{\link{ITparse}},  \code{\link{plot.PMfinal}}
#' @export
#' @examples
#' data(PMex1)
#' final <- makeFinal(NPdata.1)
#' summary(final)


summary.PMfinal <- function(x,lower=0.025,upper=0.975){
  if(length(grep("reshape2",installed.packages()[,1]))==0){
    install.packages("reshape2",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  reshape2.installed <- require(reshape2)
  if(!reshape2.installed) stop("Error: connect to internet download and install reshape2 package.\n")
  
  if(inherits(x,"IT2B")){ #IT2B object
    if(is.null(x$nsub)){
      nsub <- as.numeric(readline("Update your IT2B PMfinal object with makeFinal() or PMreport() (see help).\nFor now, enter the number of subjects. "))
    }else{nsub <- x$nsub}
    mean=x$popMean
    se.mean=x$popSD/sqrt(nsub)
    cv.mean=se.mean/mean
    var=x$popVar
    se.var=x$popVar*sqrt(2/(nsub-1))
    sumstat <- data.frame(mean,se.mean,cv.mean,var,se.var)
    return(sumstat)
  }else{ #NPAG object
    medMAD <- function(x){
      med <- median(x)
      MAD <- median(abs(x-med))
      return(list(med,MAD))
    }
    
    mcsim <- function(x,prob){
      set.seed(17)
      sim <- apply(matrix(sample(x,rep=T,10^3*length(x),prob=prob),nrow=10^3),1,medMAD)
      ciMed <- quantile(sapply(sim,function(x) x[[1]]),c(lower,0.5,upper))
      ciMAD <- quantile(sapply(sim,function(x) x[[2]]),c(lower,0.5,upper))
      return(list(ciMed,ciMAD))
    }
    if(inherits(x,"NPAG")){
      popPoints <- x$popPoints
    } else {popPoints <- x}
    
    nvar <- ncol(popPoints) - 1
    
    #trick it if there is only one point
    if(nrow(popPoints)==1){
      popPoints <- rbind(popPoints,popPoints)
      popPoints$prob <- c(0.5,0.5)
    }
    
    sumstat <- apply(popPoints[,1:nvar],2,function(x) mcsim(x,popPoints[,nvar+1]))
    
    sumstat2 <- melt(sumstat)[,c(1,3)]
    names(sumstat2) <- c("value","par")
    sumstat2$type <- rep(c("WtMed","MAWD"),each=3,times=nvar)
    sumstat2$quantile <- rep(c(lower,0.5,upper),times=2*nvar)
    sumstat2 <- sumstat2[,c("par","type","quantile","value")]
    return(sumstat2)
  }
  
}