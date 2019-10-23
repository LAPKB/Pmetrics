#' Parses the cycle information from an NPAG or an IT2B object.
#'
#' This function will parse the output of \code{\link{NPparse}} or \code{\link{ITparse}} to generate a
#' list suitable for analysis and plotting of NPAG or IT2B cycle information.
#'
#' @title Summarize Pmetrics Run Cycle Information
#' @param data A suitable data object of the \emph{NPAG} or \emph{IT2B} class (see \code{\link{NPparse}} or \code{\link{ITparse}}).
#' @return The output of \code{makeCycle} is a list of class \emph{PMcycle},
#' which has 8 objects from NPAG or 6 objects from IT2B :
#' \item{names }{Vector of names of the random parameters}#' \item{names }{Vector of names of the random parameters}
#' \item{cycnum }{Vector cycle numbers, which may start at numbers greater than 1 if a non-uniform prior was specified for the run (NPAG only)}
#' \item{ll }{Matrix of cycle number and -2*Log-likelihood at each cycle}
#' \item{gamlam }{A matrix of cycle number and gamma or lambda at each cycle}
#' \item{mean }{A matrix of cycle number and the mean of each random parameter at each cycle,  normalized to initial mean}
#' \item{sd }{A matrix of cycle number and the standard deviation of each random parameter
#' at each cycle,  normalized to initial standard deviation}
#' \item{median }{A matrix of cycle number and the median of each random parameter at each cycle,  normalized to initial median}
#' \item{aic }{A matrix of cycle number and Akaike Information Criterion at each cycle}
#' \item{bic }{A matrix of cycle number and Bayesian (Schwartz) Information Criterion at each cycle}
#' A plot method exists in \code{\link{plot}} for \emph{PMcycle} objects.
#' @author Michael Neely
#' @seealso \code{\link{NPparse}}, \code{\link{ITparse}},  \code{\link{plot.PMcycle}}
#' @examples
#' data(PMex1)
#' cycle <- makeCycle(NPdata.1)
#' cycle
#' names(cycle)

makeCycle <- function(data){

    if(!inherits(data,"NPAG") & !inherits(data,"IT2B")) stop(paste("Use PMparse() to generate an Pmetrics NPAG or IT2B object.\n"))
    #error if no cycles
    if (data$icyctot==0) stop("This run had no cycles.\n")
    #remove zero rows
    data$isd <- data$isd[apply(data$isd,1,function(x) all(x!=0)),]

    #make sure still matrix if only one row left
    if(class(data$isd)=="numeric") data$isd <- matrix(data$isd,nrow=1)

    if(inherits(data,"NPAG")){ 
      cycle <- list(names=data$par,cycnum=data$icycst:(data$icycst+data$icyctot-1),ll=-2*data$ilog,gamlam=data$igamlam,mean=t(t(data$imean)/data$imean[1,]),
                    sd=t(t(data$isd)/data$isd[1,]),median=t(data$iaddl[6,,]/data$iaddl[6,,1]),
                    aic=data$iic[,1],bic=data$iic[,2])
      class(cycle) <- c("PMcycle","list")
      return(cycle)
    }
    if(inherits(data,"IT2B")){
      cycle <- list(names=data$par,cycnum=1:data$icyctot,ll=-2*data$ilog,gamlam=data$igamlam,mean=t(t(data$imean)/t(data$imean)[1,]),
                    sd=t(t(data$isd)/data$isd[1,]),median=t(t(data$imed)/data$imed[1,]),
                    aic=data$iic[,1],bic=data$iic[,2])
      class(cycle) <- c("PMcycle","list")
      return(cycle)
    }

}

