#' Summarize a Pmetrics Observed vs. Predicted x
#'
#' @title Summarize Observations and Predictions
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Summarize observations, predictions and errors in a PMop object made by [makeOP],
#' usually in the `$data` field of a [PM_op] object.
#'
#' @method summary PMop
#' @param object A PMop object made by [makeOP].
#' @param digits Integer, used for number of digits to print.
#' @param pred.type Either 'post' for a posterior object or 'pop' for a population object.  Default is 'post'.
#' @param icen Can be either "median" for the predictions based on medians of `pred.type` parameter value
#' distributions, or "mean".  Default is "median".
#' @param outeq Output equation number.  Default is 1.
#' @param ... Not used.

#' @return A list with three elements.  
#' * sumstat A data frame with the minimum, first quartile, median, third quartile, maximum,
#' mean and standard deviation for times, observations and predictions in `x`.
#' * pe A named vector with mean prediction error (mpe),
#' the mean weighted prediction error (mwpe), the mean squared prediction error (mspe), root mean sqaured error (rmse),
#' percent root mean squared error (percent_rmse), the mean weighted
#' squared prediction error (mwspe), the bias-adjusted mean squared prediction error (bamspe), and the bias-
#' adjusted mean weighted squared prediction error (bamwspe).  The mwpe is bias and the bamwspe is imprecision on 
#' plots of PM_op objects.
#' * wtd.t A list of 6 elements based on a t test that the weighted mean prediction bias is different than zero
#'  - estimate: the weighted mean of the prediction bias for each observation
#'  - se: the standard error of the estimate
#'  - conf.int: the 95% confidence interval of the mean
#'  - statistic: the t statistic of the standardized difference between mean and zero
#'  - df: degrees of freedom equal to number of observations minus one
#'  - p.value: the probability that the weighted mean is different than zero
#' @author Michael Neely
#' @seealso [makeOP], [PM_op]
#' @export

summary.PMop <- function(object, digits = max(3, getOption("digits")-3),
                         pred.type = "post", icen = "median", 
                         outeq = 1, ...){
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
  if(inherits(object,"list")){ #we are dealing with the old list PMop
    if(missing(outeq)) {outeq <- 1:length(object)} else {outeq <- c(sapply(outeq,function(x) c(2*x-1,2*x)))}
    sumresult <- list()
    for(i in outeq){
      if(i>length(object)) {
        sumresult[[i]] <- NA
      } else {
        print(object[[i]])
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
  attr(sumresult,"pred.type") <- pred.type
  sumresult
}

