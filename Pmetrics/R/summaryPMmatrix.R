#' Summarize a Pmetrics PMmatrix object
#'
#' Summarize the raw data used for a Pmetrics run.
#'
#' @title Summarize PMmatrix objects
#' @method summary PMmatrix
#' @param x A PMmatrix object loaded by \code{\link{PMreadMatrix}} or \code{\link{PMload}}.
#' @param formula Optional formula for specifying custom summaries.  See \code{\link{aggregate}}
#' and \code{\link{formula}} for details on how to specify formulae in R. If, for example, the data contain
#' a covariate for weight named 'wt', then to summarize the mean dose in mg/kg per subject specify 
#' \code{formula=dose/wt~id, FUN=mean}.
#' @param FUN The summary function to apply to \code{formula}, if specified.
#' @param \dots Additional arguments to \code{FUN}, e.g. \code{na.rm=T} 
#' @param include A vector of subject IDs to include in the summary, e.g. c(1:3,5,15)
#' @param exclude A vector of subject IDs to exclude in the summary, e.g. c(4,6:14,16:20)
#' @return A list of class \emph{summary.PMmatrix} with the summary of the PMmatrix object, 
#' containing the following items:
#' \item{nsub}{Number of subjects}
#' \item{ndrug}{Number of drug inputs}
#' \item{numeqt}{Number of outputs}
#' \item{nobsXouteq}{Number of observations by outeq}
#' \item{missObsXouteq}{Number of missing observations by outeq}
#' \item{ncov}{Number of covariates}
#' \item{covnames}{Covariate names}
#' \item{ndoseXid}{Number of doses per input per subject}
#' \item{nobsXid}{Number of observations per outeq per subject}
#' \item{doseXid}{Doses per input per subject}
#' \item{obsXid}{Observations per outeq per subject}
#' \item{formula}{Results of including \code{formula}} 
#' @author Michael Neely
#' @seealso \code{\link{print.summary.PMmatrix}}, \code{\link{aggregate}}
#' @export

summary.PMmatrix <- function(x,formula,FUN,...,include,exclude){
  
  #filter data if needed
  if(!missing(include)){
    x <- subset(x,sub("[[:space:]]+","",as.character(x$id)) %in% as.character(include))
  } 
  if(!missing(exclude)){
    x <- subset(x,!sub("[[:space:]]+","",as.character(x$id)) %in% as.character(exclude))
  } 
  
  #make results list
  results <- list()
  idOrder <- rank(unique(x$id))
  
  results$nsub <- length(unique(x$id))
  results$ndrug <- max(x$input,na.rm=T)
  results$numeqt <- max(x$outeq,na.rm=T)
  results$nobsXouteq <- tapply(x$evid,x$outeq,function(x) length(x==0))
  results$missObsXouteq <- by(x,x$outeq,function(x) length(x$out[x$evid==0 & x$out==-99] ))
  covinfo <- getCov(x)
  ncov <- covinfo$ncov
  results$ncov <- ncov
  results$covnames <- covinfo$covnames
  results$ndoseXid <- tapply(x$evid,list(x$id,x$input),function(x) length(x!=0))[idOrder,]
  results$nobsXid <- tapply(x$evid,list(x$id,x$outeq),function(x) length(x==0))[idOrder,]
  results$doseXid <- tapply(x$dose,list(x$id,x$input),function(x) x[!is.na(x)])[idOrder,]
  results$obsXid <- tapply(x$out,list(x$id,x$outeq),function(x) x[!is.na(x)])[idOrder,]
  if(ncov>0){
    #get each subject's covariate values
    results$cov <- lapply(1:ncov,function(y) tapply(x[[covinfo$covstart+y-1]],x$id,
                                                    function(z) z[!is.na(z)])[idOrder])
    names(results$cov) <- covinfo$covnames
  }
  if(!missing(formula)){
    results$formula <- aggregate(formula,x,FUN,...)
  }
  
  class(results) <- c("summary.PMmatrix","list")
  return(results)
  
 
  
} #end function