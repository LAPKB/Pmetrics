#' Calculates the Percent Target Attainment (PTA)
#'
#' \code{makePTA} will calculate the PTA for any number of doses, targets and definitions of success.
#'
#' @title Calculation of PTAs
#' @param simdata A vector of simulator output filenames, e.g. c(\dQuote{simout1.txt},\dQuote{simout2.txt}),
#' with wildcard support, e.g. \dQuote{simout*} or \dQuote{simout?}, or
#' a list of PMsim objects made by \code{\link{SIMparse}} with suitable simulated doses and observations.  The number and times of simulated
#' observations does not have to be the same in all objects.
#' @param targets A vector of pharmacodynamic targets, such as Minimum Inhibitory Concentrations (MICs), e.g. c(0.25, 0.5,1,2,4,8,16,32)
#' @param target.type A numeric or character vector, length 1.  If numeric, must correspond to an observation time common to all PMsim objects in
#' \code{simdata}, rounded to the nearest hour.  In this case, the target statistic will be the ratio of observation at time \code{target.type} to target.  This enables 
#' testing of a specific timed concentration (e.g. one hour after a dose or C1) which may be called a peak, but is not actually the maximum drug
#' concentration.  Be sure that the time in the simulated data is used, e.g. 122 after a dose given at 120.  Character values may be one of 
#' \dQuote{time}, \dQuote{auc}, \dQuote{peak}, or \dQuote{min}, for, respectively, percent time above target within the time range
#' specified by \code{start} and \code{end}, ratio of area under the curve within the time range to target, ratio of peak concentration within the time range 
#' to target, or ratio of minimum concentration within the time range to target.  
#' @param success A single value specifying the success statistic, e.g. 0.4 for percent time above target, or 100 for peak:target.
#' @param outeq An integer specifying the number of the simulated output equation to use. Default is 1.
#' @param start Specify the time to begin PTA calculations. Default is a vector with the first observation time for subjects
#' in each element of \code{simdata}, i.e. dose regimen. If specified as a vector, values will be recycled as necessary.
#' @param end Specify the time to end PTA calculations so that PTA is calculated
#' from \code{start} to \code{end}.  Default for end is the maximum observation
#' time for subjects in each element of \code{simdata}, i.e. dose regimen.  If specified as a vector, values will be recycled
#' as necessary. Subjects with insufficient data (fewer than 5 simulated observations) for a specified interval will trigger a warning.
#' Ideally then, the simulated datset should contain sufficient observations within the interval specified by \code{start} and \code{end}.
#' @return The output of \code{makePTA} is a list of class \emph{PMpta},
#' which has 2 objects:
#' \item{results }{An array of 4 dimensions: number of doses, number of targets, maximum number of simulated subjects, 1.  This array specifies the target
#' statistic (e.g. $time > target, auc:target, etc.) for each dose and simulated subject.}
#' \item{outcome }{A dataframe with mean target statistic, standard deviation and proportion greater than \code{success} 
#' among all simulated subjects for each dose and target}
#' @author Michael Neely
#' @seealso \code{\link{plot.PMpta}}, \code{\link{SIMparse}}

makePTA <- function(simdata,targets,target.type,success,outeq=1,start,end){
  if(missing(simdata) | missing(target.type)) stop("Simulation output and target.type must be specified.\n")
  if(is.character(target.type) & !target.type %in% c("time","auc","peak","min")) stop("Please specify target.type as a numerical value corresponding to a common\ntime in all simulated datasets, or a character value of 'time', 'auc', 'peak' or 'min'.\n")
  if(!inherits(simdata,"list")){
    simfiles <- Sys.glob(simdata)
    if(length(simfiles)==0) stop("There are no files matching \"",simdata,"\".\n",sep="")
    simdata <- list()
    for(i in 1:length(simfiles)){
      simdata[[i]] <- tryCatch(SIMparse(simfiles[i]),
                               error=function(e) stop(paste(simfiles[i],"is not a PMsim object.\n")))
    }
  }
  ndose <- length(simdata)
  if(!missing(start)) {start <- rep(start,ndose)}
  if(!missing(end)) {end <- rep(end,ndose)}
  nsim <- unlist(lapply(simdata,function(x) dim(x$obs)[2]))
  nobs <- unlist(lapply(simdata,function(x) dim(x$obs)[3]))
  ntarg <- length(targets)
  results <- array(NA,dim=c(ndose,ntarg,max(nsim),1),dimnames=c("dose","target","sim","above"))
  for(dose in 1:ndose){
    cat(paste("Working on dose ",dose,"...\n",sep=""))
    flush.console()
    wrk.sim <- simdata[[dose]]
    wrk.times <- as.numeric(dimnames(wrk.sim$obs)$time)
    if(missing(start)) {wrk.start <- min(wrk.times)} else {wrk.start <- start[dose]}
    if(missing(end)) {wrk.end <- max(wrk.times)} else {wrk.end <- end[dose]} 
    if(wrk.start>=wrk.end) {stop(paste("For Dose ",dose,", start is not less than end/n",sep=""))}
    include <- which(wrk.times>=wrk.start & wrk.times<=wrk.end)
    if(length(include)==0){
      cat(paste("Note: Dose ",dose," omitted because no simulated observations fell within the time window defined by start and end.\n",sep=""))
      next
    } 
    wrk.times <- wrk.times[include]
    wrk.nsim <- dim(wrk.sim$obs)[2]
    wrk.nobs <- length(wrk.times)
    if(wrk.nobs<5) warning(paste("Only ",wrk.nobs," simulated observations available for Dose ",dose,".\nThis can compromise estimates of target attainment.\nStrongly consider increasing the number of simulated observations.\n",sep=""))
    
    pb <- txtProgressBar(min = 0, max = wrk.nsim*ntarg, style = 3)
    pta <- data.frame(id=rep(1:wrk.nsim,each=wrk.nobs),time=rep(as.numeric(wrk.times),wrk.nsim),obs=as.vector(t(wrk.sim$obs[outeq,,include])))
    if(target.type=="time"){
      for(t in 1:ntarg){
        targ <- targets[t]
        for(i in 1:wrk.nsim){
          setTxtProgressBar(pb, ((t-1)*wrk.nsim + i))
          temp <- pta[pta$id==i,]
          cumTime <- 0
          for(j in 1:(nrow(temp)-1)){
            if(!is.na(temp$obs[j]) & temp$obs[j]>=targ & temp$obs[j+1]>=targ) cumTime <- cumTime + (temp$time[j+1]-temp$time[j])
            if(!is.na(temp$obs[j]) & temp$obs[j]<targ & temp$obs[j+1]>=targ){
              time <- c(temp$time[j],temp$time[j+1])
              obs <- c(temp$obs[j],temp$obs[j+1])
              lm.1 <- lm(time~obs)
              cross1 <- predict(lm.1,data.frame(obs=targ))
              cumTime <- cumTime + (temp$time[j+1]-cross1)
            }
            if(!is.na(temp$obs[j]) & temp$obs[j]>=targ & temp$obs[j+1]<targ){
              time <- c(temp$time[j],temp$time[j+1])
              obs <- c(temp$obs[j],temp$obs[j+1])
              lm.1 <- lm(time~obs)
              cross1 <- predict(lm.1,data.frame(obs=targ))
              cumTime <- cumTime + (cross1-temp$time[j])
            }
            
          } 
          results[dose,t,i,1] <- cumTime/(wrk.end-wrk.start)
          
        }
      }
    }
    if(target.type=="auc"){      
      for(i in 1:wrk.nsim){
        setTxtProgressBar(pb, i*ntarg)
        temp <- pta[pta$id==i,]
        auc <- makeAUC(temp,obs~time,start=wrk.start,end=wrk.end)[,2]
        results[dose,1:ntarg,i,1] <- auc/targets
      }
    }
    if(target.type=="peak"){      
      for(i in 1:wrk.nsim){
        setTxtProgressBar(pb, i*ntarg)
        temp <- pta[pta$id==i,]
        peak <- max(temp$obs,na.rm=T)
        results[dose,1:ntarg,i,1] <- peak/targets
      }
    }
    if(target.type=="min"){      
      for(i in 1:wrk.nsim){
        setTxtProgressBar(pb, i*ntarg)
        temp <- pta[pta$id==i,]
        min <- min(temp$obs,na.rm=T)
        results[dose,1:ntarg,i,1] <- min/targets
      }
    }
    if(is.numeric(target.type)){      
      for(i in 1:wrk.nsim){
        setTxtProgressBar(pb, i*ntarg)
        temp <- pta[pta$id==i,]
        timed <- temp$obs[round(temp$time,2)==target.type]
        results[dose,1:ntarg,i,1] <- timed/targets
      }
    }
    close(pb)
    
  } #close dose for loop  
  pta.outcome <- data.frame(dose=rep(1:ndose,each=ntarg),target=rep(targets,ndose),success=c(t(apply(results,c(1,2,4),function(x) sum(x>=success,na.rm=T)/sum(!is.na(x)))[,,1])))
  
  rval <- list(results=results,outcome=pta.outcome)
  class(rval) <- c("PMpta","list")
  return(rval)
}

