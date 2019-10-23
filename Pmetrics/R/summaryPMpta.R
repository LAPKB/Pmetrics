#' Summarize a Pmetrics Percent Target Attainment Object
#'
#' Summarize target statistics and success proportions in a PMpta object made by \code{\link{makePTA}}.
#'
#' @title Summarize Percent Target Attainment
#' @method summary PMpta
#' @param x A PMpta object made by \code{\link{makePTA}}.
#' @param ci Confidence interval for pharmacodynamic index reporting.  Default is 0.95.
#' @param \dots Other parameters which can be passed to \code{summary}.
#' @return A list with two named objects: pta (probability of target attainment)
#' and pti (pharmacodynamic index).
#' \item{pta }{A data frame with the following columns: simnum, target, prop.success, pdi.mean, and pdi.sd  
#' \emph{simnum} is the number of the simulation; \emph{target} is the specified target; 
#' \emph{success}  has the proportion with a ratio > \code{prop.success}; \emph{pdi.mean} and \emph{pdi.sd} 
#' are the mean and standard deviation of the pharmacodyamic index (e.g. AUC/MIC) for each simulation and target.}
#' \item{pdi }{A data frame with the following columns: target, simnum, lowerCI, median, upperCI.
#' \emph{target} and \emph{simnum} are as above. \emph{lowerCI}, \emph{median}, 
#' and \emph{upperCI} are the lower limit, median, and upper limit of the confidence
#' interval for the pdi whose width is specified by \code{ci}}  
#' @author Michael Neely
#' @seealso \code{\link{makePTA}}
#' @export

summary.PMpta <- function(x,ci=0.95,...){
  require(reshape2)
  simTarg <- 1+as.numeric(attr(x,"simTarg")) #1 if missing or set, 2 if random
  if(length(simTarg)==0) simTarg <- 1
  nsim <- length(unique(x$results$simnum))
  
  if(simTarg==1){ #set targets
    ntarg <- length(unique(x$results$target))
    pdi.median <- tapply(x$results$pdi,list(x$results$target,x$results$simnum),median,na.rm=T)
    pdi.lower <- tapply(x$results$pdi,list(x$results$target,x$results$simnum),quantile,probs=0.5-ci/2,na.rm=T)
    pdi.upper <- tapply(x$results$pdi,list(x$results$target,x$results$simnum),quantile,probs=0.5+ci/2,na.rm=T)
    pdi <- rbind(pdi.median,pdi.lower,pdi.upper)
    pdi2 <- melt(pdi,value.name="pdi",varnames=c("target","simnum"))
    pdi2$quantile <- rep(c("median","lowerCI","upperCI"),each=ntarg,times=nsim)
    pdi3 <- dcast(pdi2,target+simnum~quantile,value.var="pdi") 
  } else { #random targets
    pdi.median <- unlist(tapply(x$results$pdi,x$results$simnum,median,na.rm=T,simplify=F))
    pdi.lower <- tapply(x$results$pdi,x$results$simnum,quantile,probs=0.5-ci/2,na.rm=T)
    pdi.upper <- tapply(x$results$pdi,x$results$simnum,quantile,probs=0.5+ci/2,na.rm=T)
    pdi <- rbind(pdi.median,pdi.lower,pdi.upper)
    pdi2 <- melt(pdi,value.name="pdi",varnames=c("sumstat","simnum"))
    pdi2$quantile <- rep(c("median","lowerCI","upperCI"),times=nsim)
    pdi3 <- dcast(pdi2,simnum~quantile,value.var="pdi") 
  } 

  sumres <- list(pta=x$outcome,pdi=pdi3)
  return(sumres)
  
}
