#' Summarize a Pmetrics Covariate object
#'
#' Summarize covariates and Bayesian posterior parameter values for each subject.
#'
#' @title Summarize Covariates and Bayesian Posterior Parameter Values
#' @method summary PMcov
#' @param x A PMcov object made by \code{\link{makeCov}}.
#' @param icen Summary function for covariates and posterior parameters. Default is \dQuote{median}, but can specify \dQuote{mean}.
#' @return A data frame with the summary of the PMcov object for each subject's covariates and 
#' Bayesian posterior parameter values.
#' @author Michael Neely
#' @seealso \code{\link{makeCov}}
#' @export

summary.PMcov <- function(x,icen="median"){
  if("icen" %in% names(x)){
    data <- x[x$icen==icen,]
    data <- subset(data,select=-icen) 
  } else {data <- x}
  #get order of ID in case non-numeric
  allID <- unique(data$id)
  orderID <- rank(allID)
  sumCov <- aggregate(data[,-1],list(data$id),match.fun(icen),na.rm=T)
  #reorder in ID order
  sumCov <- sumCov[orderID,]
  #replace the first grouping column with ID again
  sumCov[,1] <- allID
  names(sumCov)[1] <- "id"
  #set the attribute to be the type of summary
  attr(sumCov,"icen") <- icen
  return(sumCov)
}

