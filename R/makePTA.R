#' Calculation of PTAs
#' 
#' Calculates the Percent Target Attainment (PTA)
#'
#' \code{makePTA} will calculate the PTA for any number of simulations, targets and definitions of success.
#' Simulations typically differ by dose, but may differ by other features such as children vs. adults. This function will also
#' accept data from real subjects either in the form of a \emph{PMpost} or a \emph{PMmatrix} object.
#' If a \emph{PMpta} object is passed to the function as the \code{simdata}, the only other parameter required is success. 
#' If desired, a new set of simlabels can be specified; all other parameters will be ignored.
#'
#' @param simdata Can be one of multiple inputs.  Typically it is a vector of simulator output filenames, e.g. c(\dQuote{simout1.txt},\dQuote{simout2.txt}),
#' with wildcard support, e.g. \dQuote{simout*} or \dQuote{simout?}, or
#' a list of PMsim objects made by \code{\link{SIMparse}} with suitable simulated regimens and observations.  The number and times of simulated
#' observations does not have to be the same in all objects.  It can also be a \emph{PMpta} object previously made with
#' \code{makePTA} can be passed for recalculation with a new success value or simlabels.  Finally, \emph{PMpost} and \emph{PMmatrix} objects are 
#' also allowed.
#' @param simlabels Optional character vector of labels for each simulation.  Default is \code{c('Regimen 1', 'Regimen 2',...)}.
#' @param targets A vector of pharmacodynamic targets, such as Minimum Inhibitory Concentrations (MICs), e.g. c(0.25, 0.5,1,2,4,8,16,32).
#' This can also be a sampled distribution using  \code{\link{makePTAtarget}}.
#' @param target.type A numeric or character vector, length 1.  If numeric, must correspond to an observation time common to all PMsim objects in
#' \code{simdata}, rounded to the nearest hour.  In this case, the target statistic will be the ratio of observation at time \code{target.type} to target.  This enables 
#' testing of a specific timed concentration (e.g. one hour after a dose or C1) which may be called a peak, but is not actually the maximum drug
#' concentration.  Be sure that the time in the simulated data is used, e.g. 122 after a dose given at 120.  Character values may be one of 
#' \dQuote{time}, \dQuote{auc}, \dQuote{peak}, or \dQuote{min}, for, respectively, percent time above target within the time range
#' specified by \code{start} and \code{end}, ratio of area under the curve within the time range to target, ratio of peak concentration within the time range 
#' to target, or ratio of minimum concentration within the time range to target.  
#' @param success A single value specifying the success statistic, e.g. 0.4 for proportion time (end-start) above target, or 100 for peak:target.
#' @param outeq An integer specifying the number of the simulated output equation to use. Default is 1.
#' @param free.fraction Proportion of free, active drug.  Default is 1, i.e. 100\% free drug or 0\% protein binding.
#' @param start Specify the time to begin PTA calculations. Default is a vector with the first observation time for subjects
#' in each element of \code{simdata}, e.g. dose regimen. If specified as a vector, values will be recycled as necessary.
#' @param end Specify the time to end PTA calculations so that PTA is calculated
#' from \code{start} to \code{end}.  Default for end is the maximum observation
#' time for subjects in each element of \code{simdata}, e.g. dose regimen.  If specified as a vector, values will be recycled
#' as necessary. Subjects with insufficient data (fewer than 5 simulated observations) for a specified interval will trigger a warning.
#' Ideally then, the simulated datset should contain sufficient observations within the interval specified by \code{start} and \code{end}.
#' @param icen Can be either "median" for the predictions based on medians of \code{pred.type} parameter value
#' distributions, or "mean".  Default is "median".
#' @param block Which block to plot, where a new block is defined by dose resets (evid=4); default is 1.
#' @return The output of \code{makePTA} is a list of class \emph{PMpta},
#' which has 2 objects:
#' \item{results }{A data frame with the following columns: simnum, id, target, pdi.  
#' \emph{simnum} is the number of the simulation; \emph{id} is the simulated profile number
#' within each simulation; \emph{target} is the specified target; and \emph{pdi} is
#' the target pharmacodynamic index, e.g. time > target, auc:target, etc.}
#' \item{outcome }{A data frame summarizing the results with the following columns: simnum, target, prop.success, pdi.mean, and pdi.sd.
#' If \code{targets} was specified via \code{\link{makePTAtarget}} to be a sampled distribution, then
#' the target column will be missing from the outcome table.
#' \emph{simnum} and \emph{target} are as for \code{results}.  The \emph{prop.success} column has the proportion with a pdi > \code{success},
#' as specified in the function call.  The \emph{pdi.mean} and \emph{pdi.sd} columns have the 
#' mean and standard deviation of the target pharmacodynamic index (e.g. proportion end-start above target, ratio of Cmax to target) for each simulation and target.}  
#' @noMd
#' @author Michael Neely and Jan Strojil
#' @seealso \code{\link{plot.PMpta}}, \code{\link{SIMparse}}
#' @export

#simdata,targets,target.type,
#simlabels,success,outeq=1,
#free.fraction=1,start,end,icen="median",block=1

makePTA <- function(simdata,targets,target.type, ...){
  dots <- list(...)
  simlabels<- if (exists("simlabels", where = dots)) {
     dots$simlabels
  } else {
    NULL
  }
  success<- if (exists("success", where = dots)) {
     dots$success
  } else {
    NULL
  }
  outeq<- if (exists("outeq", where = dots)) {
     dots$outeq
  } else {
    1
  }
  free.fraction<- if (exists("free.fraction", where = dots)) {
     dots$free.fraction
  } else {
    1
  }
  start<- if (exists("start", where = dots)) {
     dots$start
  } else {
    NULL
  }
  end<- if (exists("end", where = dots)) {
     dots$end
  } else {
    NULL
  }
  icen<- if (exists("icen", where = dots)) {
     dots$icen
  } else {
    "median"
  }
  block<- if (exists("block", where = dots)) {
     dots$block
  } else {
    1
  }

  ############ define subfunctions ############
  
  #function to calculate time above target for pair of times/outs
  timeabove <- function(times, outs, targ) { # returns time spend above target for pairs of time and outputs
    #both outs are below target
    if (outs[1] < targ & outs[2] < targ) {
      interval <- 0
    #both outs are at or above target
    } else if (outs[1] >= targ & outs[2] >= targ) {
      interval <- times[2] - times[1]
    #first is below, second is at or above
    } else {
      cross <- (times[2] - times[1]) * (targ - outs[1]) / (outs[2] - outs[1]) + times[1]
      if (outs[1] <  outs[2]) {
        interval <- times[2] - cross
      } else {
        #first is at or above, second is below
        interval <- cross - times[1]}
    }
    return(interval)
  }
  
  #function to split data into blocks of 2 rows
  pairUp <- function(sim) { # creates pairs of times and observations for timeabove() Is this necessary??
    outs <- lapply(1:(nrow(sim) - 1), function(x) c(sim$out[x], sim$out[x + 1]))
    times <- lapply(1:(nrow(sim) - 1), function(x) c(sim$time[x], sim$time[x + 1]))
    return(list(times, outs))
  }
  
  #function to calculate cumulative time above target
  cumTime <- function(simID, targ) { 
    pairs <- allpairs[[simID]]
    if (min(unlist(pairs[[2]])) >= targ[[simID]]) { # if whole interval above target, return 1
      return(1)
    }
    if (max(unlist(pairs[[2]])) < targ[[simID]]) { # if whole interval below target, return 0
      return(0)
    }
    npairs <- length(pairs[[1]])
    
    # sum individual intervals between observations, x here is number of partial intervals for this patient
    interval <- sum(unlist(lapply(1:npairs, function(x) timeabove(times = pairs[[1]][[x]], outs = pairs[[2]][[x]], targ = targ[[simID]]))))
    #divide total time in the interval by the end-start interval
    return(interval/(wrk.end - wrk.start))
  }
  
  ################### begining of makePTA ######################
  # initial checks
  #checkRequiredPackages("reshape2")
  if (missing(simdata) | missing(target.type)) 
    stop("Simulation output and target.type must be specified.\n")
  if (is.character(target.type) & !target.type %in% c("time", "auc", "peak", "min")) 
    stop("Please specify target.type as a numerical value corresponding to a common\ntime in all simulated datasets, or a character value of 'time', 'auc', 'peak' or 'min'.\n")
  if (is.character(free.fraction) & substr(success,nchar(free.fraction),nchar(free.fraction))=="%") # if passed as percents convert to a number
    free.fraction <- as.numeric(substr(free.fraction,1,nchar(free.fraction)-1))/100
  while(free.fraction <= 0 | free.fraction > 1) {
    free.fraction <- as.numeric(readline(cat("Invalid free fraction, please specify a fraction > 0 and <= 1.\n")))
  }
  if (is.character(success) & substr(success,nchar(success),nchar(success))=="%") # if passed as percents, convert to a number
    success <- as.numeric(substr(success,1,nchar(success)-1))/100
  if (success <= 0 | (target.type=="time" & success > 100)) stop("Invalid success threshold value. Aborting.", call.=F)
  if (target.type=="time" & success > 1 & success <= 100) {
    cat("Your specified success threshold for time above MIC of ", success, " is bigger than 1.", sep="")
    ans <- readline(cat("\nWhat would you like to do?\n1) set success to ",success/100," (i.e. ",success,"% of time above MIC)\n2) end ", sep=""))
    if (ans == 1) {
      success = success/100
      cat("Success threshold was set to ",success,".",sep="")
    } else stop("Function aborted.", call.=F)
  }
  
  #what kind of object is simdata?
  #lists, characters are assumed to be simulations 
  dataType <- switch(EXPR=class(simdata)[1], PM_sim=0,PMsim=1,list=2,character=3,PMpost=4,PMmatrix=5,PMta=6,-1)
  if(dataType==-1){
    stop("You must specify a PMsim, list of PMsim, character vector of simulator output files, PMpost, or PMmatrix object\n")
  }
  
  if (dataType!=6) { #check if object passed as simdata is already a PMpta object
    
    ########### new PTA calculation ##################  
    #need to get it into a list of PMsim objects
    if (dataType==1 || dataType==0) { #single PMsim object
      simdata <- list(simdata)
      #class(simdata) <- c("PMsim", "list")
    }
    
    #nothing to do for dataType=2, already in right format
    
    if (dataType==3) { #character vector of simulator output files
      simfiles <- Sys.glob(simdata)
      if (length(simfiles) == 0) 
        stop("There are no files matching \"", simdata, "\".\n", sep = "")
      simdata <- list()
      for (i in 1:length(simfiles)) {
        simdata[[i]] <- tryCatch(SIMparse(simfiles[i]), error = function(e) stop(paste(simfiles[i], "is not a PMsim object.\n")))
      }
    }
    
    if(dataType==4){  #PMpost object
      simdata <- simdata[simdata$icen==icen & simdata$block==block,]
      temp <- list(obs=data.frame(id=simdata$id,time=simdata$time,out=simdata$pred,outeq=simdata$outeq))
      simdata <- list(temp)
    }
    
    if(dataType==5){  #PMmatrix object
      simdata <- makePMmatrixBlock(simdata)
      simdata <- simdata[simdata$evid==0 & simdata$block==block,]
      temp <- list(obs=data.frame(id=simdata$id,time=simdata$time,out=simdata$out,outeq=simdata$outeq))
      simdata <- list(temp)
    }


    
    if (!outeq %in% simdata[[1]]$obs$outeq) {
      stop("There are no simulated outputs for output equation ", outeq, ". Aborting.", call. = F)
    }

    n_sim <- length(simdata) # number of regimens
    n_id <- max(sapply(simdata,function(x) nrow(x$parValues))) #number of simulated id per regimen (usually 1000)
    
    sim.labels <- paste("Regimen", 1:n_sim)
    
    if (!is.null(simlabels)) { #replace generic labels with user labels
      n_simlabels <- length(simlabels)
      if (n_simlabels < n_sim) warning("There are more simulations (n=",n_sim,") than labels (n=",n_simlabels,").", call.=F, immediate. = T)
      if (n_simlabels > n_sim) warning("There are fewer simulations (n=",n_sim,") than labels (n=",n_simlabels,"); some labels will be ignored.", call.=F, immediate. = T)
      sim.labels[1:min(n_simlabels, n_sim)] <- simlabels[1:min(n_simlabels, n_sim)]
    }
    
    # if START and END are specified, fill in start and end times for each regimen
    if (!is.null(start)) {start <- rep(start, n_sim)}
    if (!is.null(end)) {end <- rep(end, n_sim)}
    if (missing(targets)) {stop("You must supply at least one target.\n")}
    if (inherits(targets, "PMpta.targ")) {
      simTarg <- T
      n_targ <- n_id #one target per simulated subject
    } else {
      simTarg <- F
      n_targ <- length(targets) #number of targets
    }
    

    #results <- list()
    results <- array(NA,dim = c(n_sim,n_targ,n_id),dimnames = list(simnum=1:n_sim,target=1:n_targ,id=1:n_id))
    
    if (!simTarg) {
      cat("\nCalculating PTA for each simulation and target...\n")
    } else {
      cat("\nCalculating PTA for each simulation using a list of simulated targets...\n")
    }
    flush.console()
    
    
    # if free.fraction not 1, multiply by free.fraction
    if (free.fraction != 1) {simdata <- lapply(1:n_sim, function(x) {simdata[[x]]$obs$out <- simdata[[x]]$obs$out * free.fraction ; simdata[[x]]})} 

    if (!simTarg & target.type == "time") {maxpb <- n_sim * n_targ} else {maxpb <- n_sim}
    
    pb <- txtProgressBar(min = 0, max = maxpb, style = 3)
    flush.console()
    
    # Master FOR cycle - from 1 to number of DOSING REGIMENS  
    for (simnum in 1:n_sim) {
      
      # create a working SIM object to pass to PTA function
      wrk.sim <- simdata[[simnum]]$obs # copy observations from simdata
      
      # Get START and END times first
      wrk.times <- unique(wrk.sim$time) # get list of observation times
      if (is.null(start)) {
        wrk.start <- min(wrk.times) # if start not specified, start at earliest simulated time
      } else {
        wrk.start <- start[simnum] # or use start time specified for regimen number "simnum"
      }
      if (is.null(end)) {
        wrk.end <- max(wrk.times) # if end not specified, use last simulated observation for given regimen
      } else {
        wrk.end <- end[simnum] # or use specified end time for regimen number "simnum"
      }
      if (wrk.start >= wrk.end) {
        stop(paste("For simulation ", simnum, ", start is not less than end/n",sep = ""), call. = F)
      }
      
      # include only those that match OUTEQ, are not N/A and fall between START and END
      wrk.sim <- wrk.sim[wrk.sim$outeq == outeq & !is.na(wrk.sim$out) & wrk.sim$time >= wrk.start & wrk.sim$time <= wrk.end, ] 
      
      if (length(wrk.sim) == 0) {
        cat(paste("Note: Simulation ", simnum, " omitted because no simulated observations matched required time period/output equation.\n", sep = ""))
        next
      }
      
      wrk.times <- unique(wrk.sim$time)
      wrk.nobs <- length(wrk.times)
      
      if (wrk.nobs < 5) warning(paste("Only ", wrk.nobs, " simulated observations available for simulation ", simnum, ".\nThis can compromise estimates of target attainment.\nStrongly consider increasing the number of simulated observations.\n", sep = ""))
      
      wrk.nprofile <- length(unique(wrk.sim$id)) # number of simulated patients for given dosing regimen
      
      if (simTarg) { # if sampled targets, generate a list of nprofile random targets
        set.seed(-17)
        randTarg <- sample(x = targets$target, size = wrk.nprofile, replace = T, prob = targets$n)
        n_targ <- length(randTarg)
      }
      
      ##################    calculation for TIME ABOVE MIC ##################    
      if (target.type == "time") {
        
        ptaM <- vector("list", n_targ)
        
        allpairs <- by(wrk.sim, wrk.sim$id, function(x) pairUp(x))
        
        if (!simTarg) { # if not simulated targets
          # secondary FOR cycle, cycles TARGETS from 1 to number of targets 
          for (t in 1:n_targ) {
            targ <- targets[t] #get current target
            if (min(wrk.sim$out)>=targ) {
              # all succeeded 100% for this target (possible at very hight MICs)
              ptaM[[t]] <- unlist(lapply(1:wrk.nprofile, function(x) x <- 1))
            } else if (max(wrk.sim$out)<=targ) {
              # all failed at 0% for this target (unlikely to happen if there is at least 1 subject dropping to 0)
              ptaM[[t]] <- unlist(lapply(1:wrk.nprofile, function(x) x <- 0))
            } else {
              # get cumulative time above MIC for each profile and current target MIC
              ptaM[[t]] <- unlist(lapply(1:wrk.nprofile, function(x) cumTime(x, targ = rep(targ, wrk.nprofile))))
            }
            setTxtProgressBar(pb, (simnum - 1) * n_targ + t)
            flush.console()
          }
        } else {  ### simulated targets ####
          ptaM[[1]] <- unlist(lapply(1:wrk.nprofile, function(x) cumTime(x, targ = randTarg)))
          setTxtProgressBar(pb, simnum)
          flush.console()
        }
        results[simnum,,] <- do.call(rbind, ptaM)
        if (n_targ == 1) results[simnum,,] <- matrix(results[simnum,,], nrow = 1)
      }
      
      ##################    calculation for AUC  ##################    
      if (target.type == "auc") {
        auc <- by(wrk.sim, wrk.sim$id, function(x) makeAUC(x, out ~ time, start = wrk.start, end = wrk.end)[,2])
        if (simTarg) {
          results[simnum,,] <- matrix(c(randTarg, auc/randTarg), ncol = 2)
        } else {
          results[simnum,,] <- sapply(auc, function(x) x/targets)
        }
        if (n_targ == 1) results[simnum,,] <- matrix(results[simnum,,], nrow = 1)
        setTxtProgressBar(pb, simnum)
        flush.console()
      }
      
      ##################    calculation for PEAK  ##################    
      if (target.type == "peak") {
        peak <- tapply(wrk.sim$out, wrk.sim$id, max)
        if (simTarg) {
          results[simnum,,] <- matrix(c(randTarg, peak/randTarg), ncol = 2)
        } else {
          results[simnum,,] <- sapply(peak, function(x) x/targets)
        }
        if (n_targ == 1) results[simnum,,] <- matrix(results[simnum,,], nrow = 1)
        setTxtProgressBar(pb, simnum)
        flush.console()
      }
      
      ##################    calculation for MIN  ##################    
      if (target.type == "min") {
        minobs <- tapply(wrk.sim$out, wrk.sim$id, min)
        if (simTarg) {
          results[simnum,,] <- matrix(c(randTarg, minobs/randTarg),ncol = 2)
        } else {
          results[simnum,,] <- sapply(minobs, function(x) x/targets)
        }
        if (n_targ == 1) results[simnum,,] <- matrix(results[simnum,,], nrow = 1)
        setTxtProgressBar(pb, simnum)
        flush.console()
      }
      
      ##################    calculation for SPECIFIC TIME POINT  ##################    
      if (is.numeric(target.type)) {
        timed <- by(wrk.sim, wrk.sim$id, function(x) x$out[round(x$time,2) == target.type])
        if (simTarg) {
          results[simnum,,] <- matrix(c(randTarg, timed/randTarg),ncol = 2)
        } else {
          results[simnum,,] <- sapply(timed, function(x) x/targets)
        }
        if (n_targ == 1) results[simnum,,] <- matrix(results[simnum,,],nrow = 1)
        setTxtProgressBar(pb, simnum)
        flush.console()
      }
      if(n_targ >1) {dimnames(results[simnum,,]) <- list(target=1:n_targ,id=1:n_id)}

    } #end of master For loop
    close(pb)
    cat("\nProcessing results...")
    

    resultDF <- results %>%  
      as.tbl_cube(met_name="pdi") %>%
      as_tibble() %>%
      select(.data$simnum,.data$id,.data$target,.data$pdi)
    

    #resultsDF <- melt(results,value.name="pdi")
    #names(resultDF) <- c("target", "id", "pdi", "simnum")
    
    if (!simTarg) {
      resultDF$target <- targets[resultDF$target] # replace target numbers with values
    } else {
      resultDF$target <- rep(rep(randTarg, each = n_sim), n_id) # fill in generated targets for each simulation
    }
    
    #resultDF <- resultDF[, c("simnum", "id", "target", "pdi")]
    
  } else {########### END OF NEW PTA CALCULATION #########
  
    ########### if SIMDATA passed to the function is already a PMpta object ###########
    
    if (!missing(target.type)) warning("Target type was specified but was ignored; cannot change type when recalculating.", call. = F)
    if (!missing(targets)) warning("Targets were specified but were ignored; using targets from the original PMpta object.", call. = F)
    if (!is.null(start)) warning("Start time was specified but was ignored; cannot change time when recalculating.", call. = F)
    if (!is.null(end)) warning("End time was specified but was ignored; cannot change time when recalculating.", call. = F)
    if (!is.null(outeq)) warning(paste("Out equation was specified but was ignored; using values from the PMpta object."), call. = F)
    if (!is.null(free.fraction)) warning(paste("Free fraction was specified but was ignored; using values from the PMpta object."), call. = F)
    
    resultDF <- simdata$results               # get results from the PMpta object
    n_sim <- max(resultDF$simnum)              # get number of simulations
    n_targ <- length(unique(resultDF$target))  # get number of targets
    targets <- c(unique(resultDF$target))     # get list of targets
    
    if (!is.null(simlabels)) {
      cat("You have specified a new set of simulation labels.\n", sep="")
      ans <- readline(cat("What would you like to do?\n1) use labels from the original PMpta object\n2) use new labels\n3) press any other key to end", sep=""))
      if (ans == 2) {
        sim.labels <- paste("Regimen", 1:n_sim) # prefill labels with generic names
        n_simlabels <- length(simlabels) # check if there is enough labels
        if (n_simlabels < n_sim) warning("There are more simulations (n=",n_sim,") than labels (n=",n_simlabels,").", call.=F)
        if (n_simlabels > n_sim) warning("There are fewer simulations (n=",n_sim,") than labels (n=",n_simlabels,"); some labels will be ignored.", call.=F, immediate. = T)
        sim.labels[1:min(n_simlabels, n_sim)] <- simlabels[1:min(n_simlabels, n_sim)]    # get labels for all simulations, or as many as there are
      } else if (ans== 1){
        warning("simlabels were specified but were ignored; using labels from the original PMpta object.", call. = F)
      } else {
        stop("Function aborted", call.=F)
      }
    }
    
    pta <- list()
    pta.outcome <- matrix()
    
    if (!exists("sim.labels")) sim.labels <- attr(simdata, "simlabels")
    simTarg <- attr(simdata, "simTarg")
    target.type <- attr(simdata, "type")
    
    cat("A PMpta object of type \"", target.type, "\" and original success threshold of ", attr(simdata, "success"), " will be used for calculations.\n",  sep = "")
    cat("Recalculating with a new success threshold of ", success, ".\n",  sep = "")
    
  }
  
  ### calculation of success rate ###
  
  if (!simTarg) {
    succSimXtarg <- tapply(resultDF$pdi, list(resultDF$target,resultDF$simnum), function(x) sum(x >= success)/sum(!is.na(x)))
    meanpdi <- tapply(resultDF$pdi, list(resultDF$target, resultDF$simnum), mean, na.rm = T)
    sdpdi <- tapply(resultDF$pdi, list(resultDF$target, resultDF$simnum), sd, na.rm = T)
    pta.outcome <- data.frame(simnum = rep(1:n_sim, each = n_targ), 
                              target = rep(targets, n_sim), prop.success = c(succSimXtarg), 
                              pdi.mean = c(meanpdi), pdi.sd = c(sdpdi))
  } else {
    succSim <- tapply(resultDF$pdi, resultDF$simnum, function(x) sum(x >= success)/sum(!is.na(x)))
    meanpdi <- tapply(resultDF$pdi, resultDF$simnum, mean, na.rm = T)
    sdpdi <- tapply(resultDF$pdi, resultDF$simnum, sd, na.rm = T)
    pta.outcome <- data.frame(simnum = 1:n_sim, prop.success = c(succSim), pdi.mean = c(meanpdi), pdi.sd = c(sdpdi))
  }
  rval <- list(results = resultDF, outcome = pta.outcome)
  attr(rval, "simlabels") <- sim.labels
  attr(rval, "simTarg") <- simTarg
  attr(rval, "success") <- success
  attr(rval, "type") <- target.type
  class(rval) <- c("PMpta", "list")
  cat("Done.\n")
  return(rval)
}

