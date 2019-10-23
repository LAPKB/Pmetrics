#' Generates an data.frame with subject-specific covariate data from an \emph{NPAG} or \emph{IT2B} object
#'
#' For each subject, \code{makeCov} extracts covariate information and Bayesian posterior parameter estimates.
#' This output of this function is suitable for exploration of covariate-parameter, covariate-time, or parameter-time relationships.
#'
#' @title Extract covariate data
#' @param data A suitable data object of the \emph{NPAG} or \emph{IT2B} class (see \code{\link{NPparse}} or \code{\link{ITparse}}).
#' @return The output of \code{makeCov} is a dataframe of class \emph{PMcov},
#' which has the following columns:
#' \item{id }{Subject identification}
#' \item{time }{Times of covariate observations}
#' \item{covnames... }{Columns with each covariate observations in the dataset for each subject and \code{time} }
#' \item{parnames... }{Columns with each parameter in the model and the \code{icen} summary
#' for each subject, replicated as necessary for covariate observation times and duplicated for Bayesian 
#' parameter means and medians }
#' \item{icen}{The type of summarized Bayesian posterior individual parameter values: mean or median.}
#' @author Michael Neely
#' @seealso \code{\link{NPparse}}, \code{\link{ITparse}}, \code{\link{plot.PMcov}}, \code{\link{summary.PMcov}}
#' @examples
#' data(PMex1)
#' cov <- makeCov(NPdata.1)
#' cov
#' names(cov)

makeCov <- function(data){
  if(!inherits(data,"NPAG") & !inherits(data,"IT2B")) stop(paste("Use NPparse() or ITparse() to generate an Pmetrics NPAG or IT2B object.\n"))
  ncov <- data$ncov
  nvar <- data$nvar
  nsub <- data$nsub
  ndrug <- data$ndrug
  if(ncov>0){
    cov <- data$dosecov[,c(2,(3+ndrug*2):(2+ndrug*2+ncov))]
  } else {
    cov <- data$dosecov[,2]
  }
  cov <- data.frame(cov)  
  cov <- cbind(data$sdata$id[data$dosecov[,1]],cov)
  

  
  #get mean Bayesian parameter values
  if(inherits(data,"NPAG")){par1 <- matrix(as.numeric(data$bmean),ncol=nvar)}
  if(inherits(data,"IT2B")){par1 <- matrix(as.numeric(data$parbay[,,1]),ncol=nvar)}
  par.exp1 <- par1[rep(1:nsub,table(data$dosecov[,1])),]
  cov1 <- cbind(cov,par.exp1)
  cov1$icen <- "mean"
  
  #get median Bayesian parameter values
  if(inherits(data,"NPAG")){par2 <- matrix(as.numeric(t(data$baddl[6,,])),ncol=nvar)}
  if(inherits(data,"IT2B")){par2 <- matrix(as.numeric(data$parbay[,,2]),ncol=nvar)}
  par.exp2 <- par2[rep(1:nsub,table(data$dosecov[,1])),]
  cov2 <- cbind(cov,par.exp2)
  cov2$icen <- "median"
  
  #put them together
  cov <- rbind(cov1,cov2)
  
  

  if (ncov>0) {
    names(cov) <- c("id","time",tolower(data$covnames),data$par,"icen")
  } else {
    names(cov) <- c("id","time",data$par,"icen")
    
  }
  class(cov) <- c("PMcov","data.frame")
  attr(cov,"ncov") <- ncov
  return(cov)
}


