#' @title Summarize Percent Target Attainment
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Summarize a Pmetrics Percent Target Attainment Object
#' @details
#' Summarize target statistics and success proportions in a *PMpta* object made by [makePTA]
#' and usually found in a [PM_pta] object.
#'
#' @method summary PMpta
#' @param object A PMpta object made by [makePTA].
#' @param ci Confidence interval for pharmacodynamic index reporting.  Default is 0.95.
#' @param ... Not used.
#' @return A list with two named objects: pta (probability of target attainment)
#' and pti (pharmacodynamic index).
#' \item{pta }{A data frame with the following columns: simnum, target, prop.success, pdi.mean, and pdi.sd  
#' *simnum* is the number of the simulation; *target* is the specified target; 
#' *success*  has the proportion with a ratio > `prop.success`; *pdi.mean* and *pdi.sd* 
#' are the mean and standard deviation of the pharmacodyamic index (e.g. AUC/MIC) for each simulation and target.}
#' \item{pdi }{A data frame with the following columns: target, simnum, lowerCI, median, upperCI.
#' *target* and *simnum* are as above. *lowerCI*, *median*, 
#' and *upperCI* are the lower limit, median, and upper limit of the confidence
#' interval for the pdi whose width is specified by `ci`.}  
#' @author Michael Neely
#' @seealso [makePTA], [PM_pta]
#' @export

summary.PMpta <- function(object, ci=0.95, ...){
  #require(reshape2)
  simTarg <- 1+as.numeric(attr(object,"simTarg")) #1 if missing or set, 2 if random
  if(length(simTarg)==0) simTarg <- 1
  nsim <- length(unique(object$results$simnum))
  
  if(simTarg==1){ #set targets
    ntarg <- length(unique(object$results$target))
    targets <- unique(object$results$target)
    pdi.median <- data.frame(tapply(object$results$pdi,list(object$results$target,object$results$simnum),median,na.rm=T))
    pdi.median$quantile <- "median"
    pdi.lower <- data.frame(tapply(object$results$pdi,list(object$results$target,object$results$simnum),quantile,probs=0.5-ci/2,na.rm=T))
    pdi.lower$quantile <- "lowerCI"
    pdi.upper <- data.frame(tapply(object$results$pdi,list(object$results$target,object$results$simnum),quantile,probs=0.5+ci/2,na.rm=T))
    pdi.upper$quantile <- "upperCI"
    pdi <- data.frame(rbind(pdi.median,pdi.lower,pdi.upper))
    pdi$target <- rep(targets,3)
    
    pdi2 <- pdi %>% 
      pivot_longer(cols=starts_with("X"),values_to="pdi",names_to="simnum",names_prefix = "X") %>%
      pivot_wider(id_cols=c("target","simnum"),values_from="pdi",names_from="quantile") %>%
      select(target,simnum,lowerCI,median,upperCI)
  
  } else { #random targets
    pdi.median <- unlist(tapply(object$results$pdi,object$results$simnum,median,na.rm=T,simplify=F))
    pdi.lower <- tapply(object$results$pdi,object$results$simnum,quantile,probs=0.5-ci/2,na.rm=T)
    pdi.upper <- tapply(object$results$pdi,object$results$simnum,quantile,probs=0.5+ci/2,na.rm=T)
    pdi <- data.frame(rbind(pdi.median,pdi.lower,pdi.upper))
    pdi$quantile <- c("median","lowerCI","upperCI")
    
    pdi2 <- pdi %>% 
      pivot_longer(cols=starts_with("X"),values_to="pdi",names_to="simnum",names_prefix = "X") %>%
      pivot_wider(id_cols="simnum",values_from="pdi",names_from="quantile") %>%
      select(simnum,lowerCI,median,upperCI)
  } 

  sumres <- list(pta=object$outcome,pdi=pdi2)
  return(sumres)
  
}
