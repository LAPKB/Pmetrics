#' Summarize a Pmetrics Observed vs. Predicted x
#'
#' Summarize observations, predictions and errors in a PMop x made by \code{\link{makeOP}}.
#'
#' @title Summarize Observations and Predictions
#' @method summary PMop
#' @param x A PMop object made by \code{\link{makeOP}}.
#' @param digits Integer, used for number of digits to print.
#' @param pred.type Either 'post' for a posterior object or 'pop' for a population object.  Default is 'post'.
#' @param icen Can be either "median" for the predictions based on medians of \code{pred.type} parameter value
#' distributions, or "mean".  Default is "median".
#' @param outeq Output equation number.  Default is 1.
#' @param \dots Other parameters which can be passed to \code{summary}.
#' @return A list with two xs.  The first component of the list is a
#' matrix with the minimum, first quartile, median, third quartile, maximum,
#' mean and standard deviation for times, observations and predictions in \code{x}.
#' The second contains the mean prediction error,
#' the mean weighted prediction error (bias), the mean squared prediction error, root mean sqaured error (RMSE),
#' percent root mean squared error (%RMSE), the mean weighted
#' squared prediction error, the bias-adjusted mean squared prediction error, and the bias-
#' adjusted mean weighted squared prediction error (imprecision).  
#' @author Michael Neely
#' @seealso \code{\link{makeOP}}
#' @export

summary.PMop <- function(x,digits=max(3,getOption("digits")-3),pred.type="post",icen="median",outeq=1,...){
  
  argList <- list(...)
  if("type" %in% names(argList)){
    cat("The 'type' argument has been updated to 'pred.type'.\nPlease update your script.\n")
    return(invisible())
  }
  
  sumPMopWrk <- function(data){
    sumstat <- matrix(NA,nrow=7,ncol=3,dimnames=list(c("Min","25%","Median","75%","Max","Mean","SD"),c("Time","Obs","Pred")))
    #min
    sumstat[1,] <- round(apply(data[,2:4],2,min,na.rm=T),digits)
    #25th percentile
    sumstat[2,] <- round(apply(data[,2:4],2,quantile,0.25,na.rm=T),digits)
    #median
    sumstat[3,] <- round(apply(data[,2:4],2,median,na.rm=T),digits)
    #75th percentil
    sumstat[4,] <- round(apply(data[,2:4],2,quantile,0.75,na.rm=T),digits)
    #max
    sumstat[5,] <- round(apply(data[,2:4],2,max,na.rm=T),digits)
    #mean
    sumstat[6,] <- round(apply(data[,2:4],2,mean,na.rm=T),digits)
    #SD
    sumstat[7,] <- round(apply(data[,2:4],2,sd,na.rm=T),digits)
    sumstat <- data.frame(sumstat)
    #N
    N <- length(data$obs[!is.na(data$obs)])
    #mean prediction error
    mpe <- sum(data$d,na.rm=T)/N
    #wt = 1/sd, so mwpe = sum(wd)/sum(wt)
    #mean weighted prediction error or BIAS
    mwpe <- sum(data$wd,na.rm=T)/N
    #mean squared prediction error
    mspe <- sum(data$ds,na.rm=T)/N
    #root mean squared error (RMSE)
    rmse <- sqrt(mspe)
    #%rmse
    percent_rmse <- rmse * 100 * N / sum(data$obs,na.rm=T)
    #mean weighted squared prediction error
    mwspe <- sum(data$wds,na.rm=T)/N
    #bias-adjusted squared prediction error
    bamspe <- mspe - mpe**2
    #imprecision - bias-adjusted mean weighted squared error
    bamwspe <- mwspe - mwpe**2
  
    pe <- data.frame(mpe=mpe,mwpe=mwpe,mspe=mspe,rmse=rmse,percent_rmse=percent_rmse,mwspe=mwspe,bamspe=bamspe,bamwspe=bamwspe)
    wtd.t <- weighted.t.test(data)
    
    result <- list(sumstat=sumstat,pe=pe,wtd.t=wtd.t)
    return(result)
  } #end sumPMopWrk
  
  #function to make summary
  object <- x
  if(inherits(object,"list")){ #we are dealing with the old list PMop
    if(missing(outeq)) {outeq <- 1:length(object)} else {outeq <- c(sapply(outeq,function(x) c(2*x-1,2*x)))}
    sumresult <- list()
    for(i in outeq){
      if(i>length(object)) {
        sumresult[[i]] <- NA
      } else {
        sumresult[[i]] <- sumPMopWrk(object[[i]])
        names(sumresult)[i] <- names(object)[i]
      }
      sumresult <- sumresult[lapply(sumresult,length)!=0]
    }
    
  } else {
    object <- object[object$outeq==outeq & object$pred.type==pred.type & object$icen==icen,]
    if(all(is.na(object$obs))){
      sumstat <- NA
      pe <- data.frame(mpe=NA,mwpe=NA,mspe=NA,rmse=NA,percent_rmse=NA,mwspe=NA,bamspe=NA,bamwspe=NA)
      wtd.t <- NA
      result <- list(sumstat=sumstat,pe=pe,wtd.t=wtd.t)
      class(result) <- c("summary.PMop","list")
      return(result)
    } else {sumresult <- sumPMopWrk(object)}
  }  
  
  class(sumresult) <- c("summary.PMop","list")
  sumresult
}

