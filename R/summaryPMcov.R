#' @title Summarize Covariates and Bayesian Posterior Parameter Values
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Summarize a Pmetrics Covariate object
#' @details
#' Summarize covariates and Bayesian posterior parameter values for each subject.
#'
#' @method summary PMcov
#' @param object A PMcov object made by [makeCov].
#' @param icen Summary function for covariates and posterior parameters. Default is "median", but can specify "mean".
#' @param ... Not used.
#' @return A data frame with the summary of the PMcov object for each subject's covariates and 
#' Bayesian posterior parameter values.
#' @author Michael Neely
#' @seealso [makeCov], [PM_cov]
#' @export

summary.PMcov <- function(object, icen = "median", ...){
  if("icen" %in% names(object)){
    data <- object[object$icen==icen,]
    data <- subset(data,select=-icen) 
  } else {data <- object}
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

