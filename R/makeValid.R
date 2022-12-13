#' Creates a Pmetrics validation object
#'
#' This function will create an object suitable for plotting visual predictive 
#' checks (VPCs) and prediction-corrected visual
#' predictive checks (pcVPCs). The function will guide the user
#' through appropriate clustering of doses, covariates and sample times for 
#' prediction correction using the methods of Bergstrand et al (2011). 
#' *NOTE:* Including `tad` is only
#' valid if steady state conditions exist for each patient.  
#' This means that dosing is stable and regular
#' for each patient, without changes in amount or timing, and that 
#' sampling occurs after the average concentrations
#' are the same from dose to dose.  Otherwise observations are *NOT* 
#' superimposable and `tad` should
#' *NOT* be used, i.e. should be set to `FALSE`.
#'
#' @title Create a Pmetrics validation object
#' @param result The result of a prior run, loaded with [PM_load].
#' @param data An optional external [PM_data] object with the same covariates
#' used in the model to test. If not specified, the original data will be obtained
#' from the `result`.
#' @param tad `r template("tad")` 
#' @param binCov A character vector of the names of covariates which are included in the model, i.e. in the
#' model equations and which need to be binned.  For example `binCov='wt'` if "wt" is included in a
#' model equation like V=V0*wt, or `binCov=c( 'wt', 'crcl')` if both "wt" and "crcl"
#' are included in model equations.
#' @param doseC An integer with the number of dose/covariate bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for doses/covariates.
#' @param timeC An integer with the number of observation time bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for observation times.
#' @param tadC An integer with the number of time after dose bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for time after dose. This argument
#' will be ignored if \code{tad=FALSE}.
#' @param \dots Other parameters to be passed to [SIMrun], especially `limits`.
#' @return The output of `make_valid` is a list of class `PMvalid`, which is a list with the following.
#' * simdata The combined, simulated files for all subjects using the population mean values and each subject
#' as a template. See [SIMparse] This object will be automatically saved to the run, to be loaded with
#' [PM_load] next time.
#' * timeBinMedian A data frame with the median times for each cluster bin.
#' * tadBinMedian A data frame with the median time after dose (tad) for each cluster bin.  
#' This will be `NA` if `tad = FALSE`.
#' * opDF A data frame with observations, predicitons, and bin-corrected predictions for each subject.
#' * ndpe An object with results of normalized distrubition of prediction errors analysis.
#' * npde_tad NPDE with time after dose rather than absolute time, if `tad = TRUE`
#' @author Michael Neely
#' @export
#' @seealso [SIMrun], [plot.PMvalid]


make_valid <- function(result, tad = F, binCov, doseC, timeC, tadC, limits, ...) {
  
  # verify packages used in this function
  # checkRequiredPackages("mclust")
  
  # save current wd
  currwd <- getwd()
  
  
  # parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  # namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  
  # Cluster raw data --------------------------------------------------------
  
  # grab raw data file
  # if(missing(data)){
    mdata <- result$data$standard_data
  # } else {
  #   mdata <- data
  # }
  # remove missing observations
  missObs <- obsStatus(mdata$out)$missing
  if (length(missObs) > 0) mdata <- mdata[-missObs, ]
  
  
  if ("include" %in% names(argsSIM)) {
    includeID <- argsSIM$include
    mdata <- mdata[mdata$id %in% includeID, ]
    argsSIM[[which(names(argsSIM) == "include")]] <- NULL
  } else {
    includeID <- NA
  }
  if ("exclude" %in% names(argsSIM)) {
    excludeID <- argsSIM$exclude
    mdata <- mdata[!mdata$id %in% excludeID, ]
    argsSIM[[which(names(argsSIM) == "exclude")]] <- NULL
  } else {
    excludeID <- NA
  }
  
  # get time after dose
  if (tad) {
    valTAD <- calcTAD(mdata)
  }
  
  # number of subjects
  nsub <- length(unique(mdata$id))
  
  # define covariates in model to be binned
  covData <- getCov(mdata)
  if (covData$ncov > 0) { # if there are any covariates...
    if (missing(binCov)) {
      covInData <- getCov(mdata)$covnames
      cat(paste("Covariates in your data file: ", paste(getCov(mdata)$covnames, collapse = ", ")))
      binCov <- readline("Enter any covariates to be binned, separated by commas (<Return> for none): ")
      binCov <- unlist(strsplit(binCov, ","))
      # remove leading/trailing spaces
      binCov <- gsub("^[[:space:]]|[[:space:]]$", "", binCov)
    }
    if (!all(binCov %in% names(mdata))) {
      stop("You have entered covariates which are not valid covariates in your data.")
    }
    # ensure binCov has covariates in same order as data file
    covSub <- covData$covnames[covData$covnames %in% binCov]
    binCov <- covSub
  } else { # there are no covariates
    binCov <- NULL
  }
  
  # set up data for clustering
  # fill in gaps for cluster analysis only for binning variables (always dose and time)
  dataSub <- mdata[, c("id", "evid", "time", "out", "dose", "out", all_of(binCov))]
  # add time after dose
  if (tad) {
    dataSub$tad <- valTAD
  } else {
    dataSub$tad <- NA
  }
  dataSub <- dataSub %>% select(c("id", "evid", "time", "tad", "out", "dose", all_of(binCov)))
  
  
  # restrict to doses for dose/covariate clustering (since covariates applied on doses)
  dataSubDC <- dataSub %>%
    filter(evid > 0) %>%
    select(c("id", "dose", all_of(binCov)))
  
  # set zero doses (covariate changes) as missing
  dataSubDC$dose[dataSubDC$dose == 0] <- NA
  for (i in 1:nrow(dataSubDC)) {
    missingVal <- which(is.na(dataSubDC[i, ]))
    if (2 %in% missingVal) { # dose is missing
      if (i == 1 | (dataSubDC$id[i - 1] != dataSubDC$id[i])) { # first record for patient has zero dose
        j <- 0
        while (is.na(dataSubDC$dose[i + j])) { # increment until non-zero dose is found
          j <- j + 1
        }
        dataSubDC$dose[i] <- dataSubDC$dose[i + j] # set dose equal to first non-zero dose
        missingVal <- missingVal[-which(missingVal == 3)] # take out missing flag for dose as it has been dealt with
      }
    }
    dataSubDC[i, missingVal] <- dataSubDC[i - 1, missingVal]
  }
  # restrict to observations for time clustering
  dataSubTime <- dataSub$time[dataSub$evid == 0]
  # restrict to observations for tad clustering
  if (tad) {
    dataSubTad <- dataSub$tad[dataSub$evid == 0]
  }
  
  # ELBOW PLOT for clustering if used
  elbow <- function(x) {
    set.seed(123)
    # Compute and plot wss for k = 2 to k = 15.
    # set k.max
    if (is.null(dim(x))) {
      k.max <- min(length(unique(x)), 15)
    } else {
      k.max <- min(nrow(unique(x)), 15)
    }
    
    wss <- sapply(
      2:k.max,
      function(k) {
        val <- kmeans(x, k, nstart = 50, iter.max = 15)
        val$tot.withinss
      }
    )
    wss
    plot(2:k.max, wss,
         type = "b", pch = 19, frame = FALSE,
         xlab = "Number of clusters",
         ylab = "Total within-clusters sum of squares (WSS)"
    )
  }
  
  
  if (missing(doseC)) {
    # DOSE/COVARIATES
    cat("Now optimizing clusters for dose/covariates.\n")
    cat("First step is a Gaussian mixture model analysis, followed by an elbow plot.\n")
    readline(paste("Press <Return> to start cluster analysis for ",
                   paste(c("dose", binCov), collapse = ", ", sep = ""), ": ",
                   sep = ""
    ))
    cat("Now performing Gaussian mixture model analysis.")
    mod1 <- Mclust(dataSubDC)
    cat(paste("Most likely number of clusters is ", mod1$G, ".", sep = ""))
    readline("Press <Return> to see classification plot: ")
    plot(mod1, "classification")
    readline("Press <Return> to see elbow plot: ")
    elbow(dataSubDC)
    doseC <- as.numeric(readline(paste("Specify your dose/covariate cluster number, <Return> for ", mod1$G, ": ", sep = "")))
    if (is.na(doseC)) doseC <- mod1$G
  } # end if missing doseC
  
  # function to cluster by time or tad
  timeCluster <- function(timevar) {
    if (timevar == "time") {
      use.data <- dataSubTime
      timeLabel <- "Time"
      timePlot <- as.formula(out ~ time)
    } else {
      use.data <- dataSubTad
      timeLabel <- "Time after dose"
      timePlot <- as.formula(out ~ tad)
    }
    readline("Press <Return> to start cluster analysis for sample times: ")
    mod <- Mclust(use.data)
    cat(paste("Most likely number of clusters is ", mod$G, ".\n", sep = ""))
    readline("Press <Return> to see classification plot: ")
    plot(mod, "classification")
    readline("Press <Return> to see cluster plot: ")
    
    timeClusterPlot <- function() {
      plot(timePlot, dataSub, xlab = timeLabel, ylab = "Observation", xlim = c(min(use.data), max(use.data)))
    }
    
    # plot for user to see
    timeClusterPlot()
    timeClusters <- stats::kmeans(use.data, centers = mod$G, nstart = 50)
    abline(v = timeClusters$centers, col = "red")
    
    # allow user to override
    readline("Press <Return> to see elbow plot: ")
    elbow(use.data)
    ans <- readline(cat(paste("Enter:\n<1> for ", mod$G, " clusters\n<2> for a different number of automatically placed clusters\n<3> to manually specify cluster centers ", sep = "")))
    if (ans == 1) {
      TclustNum <- mod$G
    }
    if (ans == 2) {
      confirm <- 2
      while (confirm != 1) {
        TclustNum <- readline("Specify your sample time cluster number \n")
        mod <- Mclust(use.data, G = TclustNum)
        timeClusterPlot()
        timeClusters <- kmeans(use.data, centers = mod$G, nstart = 50)
        abline(v = timeClusters$centers, col = "red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise number of times\n<3> to manually enter times"))
        if (confirm == 3) {
          ans <- 3
          confirm <- 1
        }
      }
    }
    if (ans == 3) {
      confirm <- 2
      while (confirm != 1) {
        timeClusterPlot()
        timeVec <- readline("Specify a comma-separated list of times, e.g. 1,2,8,10: ")
        timeVec <- as.numeric(strsplit(timeVec, ",")[[1]])
        abline(v = timeVec, col = "red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise times "))
      }
      TclustNum <- timeVec
    }
    if (all(is.na(TclustNum))) TclustNum <- mod$G
    return(as.numeric(TclustNum))
  } # end timeCluster function
  
  # cluster by time and tad if appropriate
  if (missing(timeC)) {
    cat("Now clustering for actual sample times...\n")
    timeC <- timeCluster("time")
  } # end if missing timeC
  if (tad & missing(tadC)) {
    cat("Now clustering for time after dose...\n")
    tadC <- timeCluster("tad")
  }
  
  # now set the cluster bins
  dcClusters <- stats::kmeans(dataSubDC, centers = doseC, nstart = 50)
  dataSub$dcBin[dataSub$evid > 0] <- dcClusters$cluster # m=dose,covariate bins
  
  timeClusters <- stats::kmeans(dataSubTime, centers = timeC, nstart = 50)
  # dataSub$timeBin[dataSub$evid == 0] <- sapply(timeClusters$cluster, function(x) which(order(timeClusters$centers) == x)) # n=ordered time bins
  dataSub$timeBin[dataSub$evid == 0] <- timeClusters$cluster
  
  if (tad) {
    tadClusters <- stats::kmeans(dataSubTad, centers = tadC, nstart = 50)
    # dataSub$tadBin[dataSub$evid == 0] <- sapply(tadClusters$cluster, function(x) which(order(tadClusters$centers) == x)) # n=ordered time bins
    dataSub$tadBin[dataSub$evid == 0] <- tadClusters$cluster
  } else {
    dataSub$tadBin <- NA
  }
  
  # Simulations -------------------------------------------------------------
  datafileName <- "gendata.csv"
  modelfile <- "genmodel.txt"
  
  result$data$write(datafileName)
  result$model$write(modelfile)
  
  # simulate PRED_bin from pop icen parameter values and median of each bin for each subject
  # first, calculate median of each bin
  dcMedian <- dataSub %>% group_by(bin = dcBin) %>% filter(!is.na(dose)) %>%
    summarize(dplyr::across(c(dose,!!binCov), median, na.rm=T))
  
  timeMedian <- dataSub %>% group_by(bin = timeBin) %>% filter(!is.na(timeBin)) %>%
    summarize(time = median(time, na.rm=T)) %>% arrange(time)
  
  if (tad) {
    tadMedian <- dataSub %>% group_by(bin = tadBin) %>% filter(!is.na(tadBin)) %>%
      summarize(time = median(tad, na.rm=T)) %>% arrange(time)
  } else {
    tadMedian <- NA
  }
  
  # create  datafile based on mdata, but with covariates and doses replaced by medians
  # and sample times by bin times
  mdataMedian <- mdata
  mdataMedian$dcBin <- dataSub$dcBin
  mdataMedian$timeBin <- dataSub$timeBin
  # no need for tadBin as we don't simulate with tad
  mdataMedian$dose <- dcMedian[[2]][match(mdataMedian$dcBin, dcMedian$bin)]
  mdataMedian$time[mdataMedian$evid == 0] <- timeMedian$time[match(mdataMedian$timeBin[mdataMedian$evid == 0], timeMedian$bin)]
  covCols <- which(names(mdataMedian) %in% binCov)
  if (length(covCols) > 0) {
    for (i in covCols) {
      dcMedianCol <- which(names(dcMedian) == names(mdataMedian[i]))
      mdataMedian[, i] <- dcMedian[match(mdataMedian$dcBin, dcMedian$bin), dcMedianCol]
    }
  }
  # write median file
  MedianDataFileName <- paste(substr(paste("m_", strsplit(datafileName, "\\.")[[1]][1], sep = ""), 0, 8), ".csv", sep = "")
  medianData <- PM_data$new(mdataMedian[, 1:(ncol(mdataMedian) - 2)], quiet = T)
  medianData$write(MedianDataFileName)
  
  # remove old files
  invisible(file.remove(Sys.glob("sim*.txt")))
  
  # get poppar and make one with zero covariance
  poppar <- result$final
  popparZero <- poppar
  popparZero$popCov[popparZero$popCov != 0] <- 0
  # do the simulation for each subject using the median dose, median covariates and pop parameters
  if ("seed" %in% names(argsSIM)) {
    seed.start <- argsSIM$seed
    argsSIM[[which(names(argsSIM) == "seed")]] <- NULL
  } else {
    seed.start <- -17
  }
  set.seed(seed.start)
  if ("nsim" %in% names(argsSIM)) {
    nsim <- argsSIM$nsim
    argsSIM[[which(names(argsSIM) == "nsim")]] <- NULL
  } else {
    nsim <- 1000
  }
  if ("limits" %in% names(argsSIM)) {
    limits <- argsSIM$limits
    argsSIM[[which(names(argsSIM) == "limits")]] <- NULL
  } else {
    limits <- NA
  }
  argsSIM1 <- c(list(
    poppar = popparZero, data = MedianDataFileName, model = modelfile, nsim = 1,
    seed = runif(nsub, -100, 100), outname = "simMed",
    limits = limits), argsSIM)
  cat("Simulating outputs for each subject using population means...\n")
  flush.console()
  do.call("SIMrun", argsSIM1)
  
  # read and format the results of the simulation
  PRED_bin <- SIMparse("simMed*", combine = T, quiet = T)
  PRED_bin$obs <- PRED_bin$obs %>% filter(!is.na(out))
  
  # make tempDF subset of PMop for subject, time, non-missing obs, outeq, pop predictions (PREDij)
  tempDF <- if (inherits(result$op, "PM_op")) {
    result$op$data
  } else {
    result$op
  }
  tempDF <- tempDF[obsStatus(tempDF$obs)$present, ] %>% 
    filter(time > 0, pred.type == "pop", icen == "median") %>%
    includeExclude(includeID, excludeID) %>%
    arrange(id, time, outeq)
  
  if (tad) {
    tempDF$tad <- dataSub$tad[dataSub$evid == 0]
  } else {
    tempDF$tad <- NA
  }
  
  
  # add PRED_bin to tempDF
  tempDF$PRED_bin <- PRED_bin$obs$out 
  
  # add pcYij column to tempDF as obs * PREDbin/PREDij
  tempDF$pcObs <- tempDF$obs * tempDF$PRED_bin / tempDF$pred
  
  # bin pcYij by time and add to tempDF
  tempDF$timeBinNum <- dataSub$timeBin[dataSub$evid == 0]
  tempDF$timeBinMedian <- timeMedian$time[match(tempDF$timeBinNum, timeMedian$bin)]
  if (tad) {
    tempDF$tadBinNum <- dataSub$tadBin[dataSub$evid == 0]
    tempDF$tadBinMedian <- tadMedian$time[match(tempDF$tadBinNum, tadMedian$bin)]
  } else {
    tempDF$tadBinNum <- NA
    tempDF$tadBinMedian <- NA
  }
  
  
  # Now, simulate using full pop model
  # write the adjusted mdata file first
  fullData <- PM_data$new(mdata, quiet = T)
  fullData$write(datafileName)
  
  set.seed(seed.start)
  argsSIM2 <- c(list(
    poppar = poppar, data = datafileName, model = modelfile, nsim = nsim,
    seed = runif(nsub, -100, 100), outname = "full", limits = limits),
    argsSIM)
  if (!is.na(includeID[1])) {
    argsSIM2$include <- includeID
  }
  if (!is.na(excludeID[1])) {
    argsSIM2$exclude <- excludeID
  }
  do.call("SIMrun", argsSIM2)
  # read and format the results of the simulation
  simFull <- SIMparse("full*", combine = T, quiet = T)
  # take out observations at time 0 from evid=4
  simFull$obs <- simFull$obs %>% filter(time > 0)
  # take out missing observations
  simFull$obs <- simFull$obs[obsStatus(simFull$obs$out)$present,]

  # add TAD for plotting options
  if (tad) {
    simFull$obs$tad  <- dataSub %>% 
      filter(evid == 0) %>% 
      group_by(id) %>% 
      group_map(~rep(.x$tad, nsim)) %>% unlist()
  } else {
    simFull$obs$tad <- NA
  }
  

  # pull in time bins from tempDF; only need median as tempDF contains median and mean,
  # but simulation is only from pop means
    simFull$obs$timeBinNum <- dataSub %>% 
    filter(evid == 0) %>% 
    group_by(id) %>% 
    group_map(~rep(.x$timeBin, nsim)) %>% 
    unlist()
  
  # pull in tad bins from tempDF
  simFull$obs$tadBinNum <- dataSub %>% 
    filter(evid == 0) %>%  
    group_by(id) %>% 
    group_map(~rep(.x$tadBin, nsim)) %>% 
    unlist()
  
    # make simulation number 1:nsim
  simFull$obs$simnum <- as.numeric(sapply(strsplit(simFull$obs$id, "\\."), function(x) x[1]))
  class(simFull) <- c("PMsim", "list")
  
  # NPDE --------------------------------------------------------------------
  
  
  # get npde from github
  # checkRequiredPackages("npde", repos = "LAPKB/npde")
  
  # prepare data for npde
  obs <- tempDF %>% select(id, time, tad, out = obs, outeq)
  
  
  # remove missing obs
  obs <- obs[obs$out != -99, ]
  
  simobs <- simFull$obs
  # remove missing simulations
  simobs <- simobs[simobs$out != -99, ]
  simobs$id <- rep(obs$id, times = nsim)
  
  simobs <- simobs %>% select(id, time, tad, out, outeq)
  
  
  #get number of outeq
  nout <- max(obs$outeq, na.rm=T)
  npde <- list()
  npdeTAD <- list()
  
  for(thisout in 1:nout){
    
    obs_sub <- obs %>% filter(outeq == thisout) %>% 
      select(id, time, out) %>% 
      arrange(id, time)
    sim_sub <- simobs %>% filter(outeq == thisout, id %in% obs_sub$id) %>% 
      select(id, time, out) %>% 
      arrange(id, time)
    obs_sub <- data.frame(obs_sub)
    sim_sub <- data.frame(sim_sub)
    
    if(tad){
      obs_sub2 <- obs %>% 
        filter(outeq == thisout) %>% 
        select(id, time = tad, out) %>% 
        arrange(id, time)
      sim_sub2 <- simobs %>% filter(outeq == thisout, id %in% obs_sub$id) %>% 
        select(id, time = tad, out) %>% 
        arrange(id, time)
      obs_sub2 <- data.frame(obs_sub2)
      sim_sub2 <- data.frame(sim_sub2)
    }
    # get NPDE decorr.method = "inverse",
    npde[[thisout]] <- tryCatch(npde::autonpde(obs_sub, sim_sub,
                                                  iid = "id", ix = "time", iy = "out",
                                                  detect = F,
                                                  verbose = F,
                                                  boolsave = F), 
                                   error = function(e) {e; return(e)})
   
     if(inherits(npde[[thisout]], "error")){ #error, often due to non pos-def matrix
      npde[[thisout]] <- tryCatch(npde::autonpde(obs_sub, sim_sub,
                                                    iid = "id", ix = "time", iy = "out",
                                                    detect = F,
                                                    verbose = F,
                                                    boolsave = F,
                                                    decorr.method = "inverse"), 
                                     error = function(e) {e; return(e)})
    
        if(inherits(npde[[thisout]], "error")){ #still with error
        errorMsg <- npde[[thisout]] 
        npde[[thisout]] <- paste0("Unable to calculate NPDE for outeq ",thisout,": ",errorMsg)
      } else {
        cat(paste0("NOTE: Due to numerical instability, for outeq ", thisout, " inverse decorrelation applied, not Cholesky (the default)."))
      }
      
    }
    
    # get NPDE for TAD
    if(tad){
      npdeTAD[[thisout]] <- tryCatch(npde::autonpde(obs_sub2, sim_sub2,
                                                     iid = "id", ix = "time", iy = "out",
                                                     detect = F,
                                                     verbose = F,
                                                     boolsave = F
                                                     ), 
                                      error = function(e) {e; return(e)})
      
      if(inherits(npdeTAD[[thisout]], "error")){ #error, often due to non pos-def matrix
        npdeTAD[[thisout]] <- tryCatch(npde::autonpde(obs_sub2, sim_sub2,
                                                       iid = "id", ix = "time", iy = "out",
                                                       detect = F,
                                                       verbose = F,
                                                       boolsave = F,
                                                       decorr.method = "inverse"), 
                                        error = function(e) {e; return(e)})
        
        if(inherits(npdeTAD[[thisout]], "error")){ #still with error
          errorMsg <- npdeTAD[[thisout]] 
          npdeTAD[[thisout]] <- paste0("Unable to calculate NPDE with TAD for outeq ",thisout,": ",errorMsg)
        } else {
          cat(paste0("NOTE: Due to numerical instability, for outeq ", thisout, " and TAD, inverse decorrelation applied, not Cholesky (the default)."))
        }
        
      }
    }
    
  }
  
  # Clean Up ----------------------------------------------------------------
  
  
  valRes <- list(simdata = PM_sim$new(simFull), 
                 timeBinMedian = timeMedian, 
                 tadBinMedian = tadMedian, 
                 opDF = tempDF, 
                 npde = npde, npde_tad = npdeTAD)
  class(valRes) <- c("PMvalid", "list")
  
  setwd(currwd)
  return(valRes)
} # end function


#' Creates a Pmetrics validation object
#'
#' `makeValid` will create an object suitable for plotting visual predictive 
#' checks (VPCs) and prediction-corrected visual
#' predictive checks (pcVPCs). The function will guide the user
#' through appropriate clustering of doses, covariates and sample times for 
#' prediction correction using the methods of Bergstrand et al (2011).
#' *NOTE:* Including TAD is only
#' valid if steady state conditions exist for each patient.  This means that dosing is stable and regular
#' for each patient, without changes in amount or timing, and that sampling occurs after the average concentrations
#' are the same from dose to dose.  Otherwise observations are *NOT* superimposable and `tad` should
#' *NOT* be used, i.e. should be set to `FALSE`.
#'
#' @title Create a Pmetrics validation object
#' @param run When the current working directory is the Runs folder, the folder name of a previous run that you wish to use for the npde,
#' which will typically be a number, e.g. 1.
#' @param tad `r template("tad")` 
#' @param binCov A character vector of the names of covariates which are included in the model, i.e. in the
#' model equations and which need to be binned.  For example `binCov='wt'` if "wt" is included in a
#' model equation like V=V0*wt, or `binCov=c( 'wt', 'crcl')` if both "wt" and "crcl"
#' are included in model equations.
#' @param doseC An integer with the number of dose/covariate bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for doses/covariates.
#' @param timeC An integer with the number of observation time bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for observation times.
#' @param tadC An integer with the number of time after dose bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for time after dose. This argument
#' will be ignored if `tad=FALSE`.
#' @param \dots Other parameters to be passed to [SIMrun], especially `limits`.
#' @return The output of `makeValid` is a list of class `PMvalid`, which is a list with the following.
#' * simdata The combined, simulated files for all subjects using the population mean values and each subject
#' as a template. See [SIMparse]. This object will be automatically saved to the run, to be loaded with
#' [PMload] next time.
#' * timeBinMedian A data frame with the median times for each cluster bin.
#' * tadBinMedian A data frame with the median time after dose (tad) for each cluster bin.  This will be `NA` if
#' `tad = FALSE`.
#' * opDF A data frame with observations, predicitons, and bin-corrected predictions for each subject.
#' @author Michael Neely
#' @seealso [SIMrun], [plot.PMvalid]
#' @export

makeValid <- function(run, tad = F, binCov, doseC, timeC, tadC, limits, ...) {
  
  # verify packages used in this function
  # checkRequiredPackages("mclust")
  
  # save current wd
  currwd <- getwd()
  
  # get the run
  if (missing(run)) run <- readline("Enter the run number: ")
  PMload(run)
  
  getName <- function(x) {
    return(get(paste(x, run, sep = ".")))
  }
  
  # parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  # namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  
  # Cluster raw data --------------------------------------------------------
  
  # grab raw data file
  mdata <- getName("data")
  # remove missing observations
  missObs <- obsStatus(mdata$out)$missing
  if (length(missObs) > 0) mdata <- mdata[-missObs, ]
  
  # #get input and output max
  # maxInput <- max(mdata$input,na.rm=T)
  # maxOuteq <- max(mdata$outeq,na.rm=T)
  # if(outeq > maxOuteq){
  #   stop("You entered an output equation number greater than the number of output equations.\n")
  # }
  # if(input > maxInput){
  #   stop("You entered a drug input number greater than the number of drug inputs.\n")
  # }
  #
  # filter to include/exclude subjects
  if ("include" %in% names(argsSIM)) {
    includeID <- argsSIM$include
    mdata <- mdata[mdata$id %in% includeID, ]
    argsSIM[[which(names(argsSIM) == "include")]] <- NULL
  } else {
    includeID <- NA
  }
  if ("exclude" %in% names(argsSIM)) {
    excludeID <- argsSIM$exclude
    mdata <- mdata[!mdata$id %in% excludeID, ]
    argsSIM[[which(names(argsSIM) == "exclude")]] <- NULL
  } else {
    excludeID <- NA
  }
  
  # get time after dose
  if (tad) {
    valTAD <- calcTAD(mdata)
  }
  
  # number of subjects
  nsub <- length(unique(mdata$id))
  
  # define covariates in model to be binned
  covData <- getCov(mdata)
  if (covData$ncov > 0) { # if there are any covariates...
    if (missing(binCov)) {
      covInData <- getCov(mdata)$covnames
      cat(paste("Covariates in your data file: ", paste(getCov(mdata)$covnames, collapse = ", ")))
      binCov <- readline("Enter any covariates to be binned, separated by commas (<Return> for none): ")
      binCov <- unlist(strsplit(binCov, ","))
      # remove leading/trailing spaces
      binCov <- gsub("^[[:space:]]|[[:space:]]$", "", binCov)
    }
    if (!all(binCov %in% names(mdata))) {
      stop("You have entered covariates which are not valid covariates in your data.")
    }
    # ensure binCov has covariates in same order as data file
    covSub <- covData$covnames[covData$covnames %in% binCov]
    binCov <- covSub
  } else { # there are no covariates
    binCov <- NULL
  }
  
  # set up data for clustering
  # fill in gaps for cluster analysis only for binning variables (always dose and time)
  dataSub <- mdata[, c("id", "evid", "time", "out", "dose", "out", binCov)]
  # add time after dose
  if (tad) {
    dataSub$tad <- valTAD
  } else {
    dataSub$tad <- NA
  }
  dataSub <- dataSub %>% select(c("id", "evid", "time", "tad", "out", "dose", binCov))
  
  
  # restrict to doses for dose/covariate clustering (since covariates applied on doses)
  dataSubDC <- dataSub %>%
    filter(evid > 0) %>%
    select(c("id", "dose", binCov))
  
  # set zero doses (covariate changes) as missing
  dataSubDC$dose[dataSubDC$dose == 0] <- NA
  for (i in 1:nrow(dataSubDC)) {
    missingVal <- which(is.na(dataSubDC[i, ]))
    if (2 %in% missingVal) { # dose is missing
      if (i == 1 | (dataSubDC$id[i - 1] != dataSubDC$id[i])) { # first record for patient has zero dose
        j <- 0
        while (is.na(dataSubDC$dose[i + j])) { # increment until non-zero dose is found
          j <- j + 1
        }
        dataSubDC$dose[i] <- dataSubDC$dose[i + j] # set dose equal to first non-zero dose
        missingVal <- missingVal[-which(missingVal == 3)] # take out missing flag for dose as it has been dealt with
      }
    }
    dataSubDC[i, missingVal] <- dataSubDC[i - 1, missingVal]
  }
  # restrict to observations for time clustering
  dataSubTime <- dataSub$time[dataSub$evid == 0]
  # restrict to observations for tad clustering
  if (tad) {
    dataSubTad <- dataSub$tad[dataSub$evid == 0]
  }
  
  # ELBOW PLOT for clustering if used
  elbow <- function(x) {
    set.seed(123)
    # Compute and plot wss for k = 2 to k = 15.
    # set k.max
    if (is.null(dim(x))) {
      k.max <- min(length(unique(x)), 15)
    } else {
      k.max <- min(nrow(unique(x)), 15)
    }
    
    wss <- sapply(
      2:k.max,
      function(k) {
        val <- kmeans(x, k, nstart = 50, iter.max = 15)
        val$tot.withinss
      }
    )
    wss
    plot(2:k.max, wss,
         type = "b", pch = 19, frame = FALSE,
         xlab = "Number of clusters",
         ylab = "Total within-clusters sum of squares (WSS)"
    )
  }
  
  
  if (missing(doseC)) {
    # DOSE/COVARIATES
    cat("Now optimizing clusters for dose/covariates.\n")
    cat("First step is a Gaussian mixture model analysis, followed by an elbow plot.\n")
    readline(paste("Press <Return> to start cluster analysis for ",
                   paste(c("dose", binCov), collapse = ", ", sep = ""), ": ",
                   sep = ""
    ))
    cat("Now performing Gaussian mixture model analysis.")
    mod1 <- Mclust(dataSubDC)
    cat(paste("Most likely number of clusters is ", mod1$G, ".", sep = ""))
    readline("Press <Return> to see classification plot: ")
    plot(mod1, "classification")
    readline("Press <Return> to see elbow plot: ")
    elbow(dataSubDC)
    doseC <- as.numeric(readline(paste("Specify your dose/covariate cluster number, <Return> for ", mod1$G, ": ", sep = "")))
    if (is.na(doseC)) doseC <- mod1$G
  } # end if missing doseC
  
  # function to cluster by time or tad
  timeCluster <- function(timevar) {
    if (timevar == "time") {
      use.data <- dataSubTime
      timeLabel <- "Time"
      timePlot <- as.formula(out ~ time)
    } else {
      use.data <- dataSubTad
      timeLabel <- "Time after dose"
      timePlot <- as.formula(out ~ tad)
    }
    readline("Press <Return> to start cluster analysis for sample times: ")
    mod <- Mclust(use.data)
    cat(paste("Most likely number of clusters is ", mod$G, ".\n", sep = ""))
    readline("Press <Return> to see classification plot: ")
    plot(mod, "classification")
    readline("Press <Return> to see cluster plot: ")
    
    timeClusterPlot <- function() {
      plot(timePlot, dataSub, xlab = timeLabel, ylab = "Observation", xlim = c(min(use.data), max(use.data)))
    }
    
    # plot for user to see
    timeClusterPlot()
    timeClusters <- stats::kmeans(use.data, centers = mod$G, nstart = 50)
    abline(v = timeClusters$centers, col = "red")
    
    # allow user to override
    readline("Press <Return> to see elbow plot: ")
    elbow(use.data)
    ans <- readline(cat(paste("Enter:\n<1> for ", mod$G, " clusters\n<2> for a different number of automatically placed clusters\n<3> to manually specify cluster centers ", sep = "")))
    if (ans == 1) {
      TclustNum <- mod$G
    }
    if (ans == 2) {
      confirm <- 2
      while (confirm != 1) {
        TclustNum <- readline("Specify your sample time cluster number \n")
        mod <- Mclust(use.data, G = TclustNum)
        timeClusterPlot()
        timeClusters <- kmeans(use.data, centers = mod$G, nstart = 50)
        abline(v = timeClusters$centers, col = "red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise number of times\n<3> to manually enter times"))
        if (confirm == 3) {
          ans <- 3
          confirm <- 1
        }
      }
    }
    if (ans == 3) {
      confirm <- 2
      while (confirm != 1) {
        timeClusterPlot()
        timeVec <- readline("Specify a comma-separated list of times, e.g. 1,2,8,10: ")
        timeVec <- as.numeric(strsplit(timeVec, ",")[[1]])
        abline(v = timeVec, col = "red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise times "))
      }
      TclustNum <- timeVec
    }
    if (all(is.na(TclustNum))) TclustNum <- mod$G
    return(as.numeric(TclustNum))
  } # end timeCluster function
  
  # cluster by time and tad if appropriate
  if (missing(timeC)) {
    cat("Now clustering for actual sample times...\n")
    timeC <- timeCluster("time")
  } # end if missing timeC
  if (tad & missing(tadC)) {
    cat("Now clustering for time after dose...\n")
    tadC <- timeCluster("tad")
  }
  
  # now set the cluster bins
  dcClusters <- stats::kmeans(dataSubDC, centers = doseC, nstart = 50)
  dataSub$dcBin[dataSub$evid > 0] <- dcClusters$cluster # m=dose,covariate bins
  
  timeClusters <- stats::kmeans(dataSubTime, centers = timeC, nstart = 50)
  dataSub$timeBin[dataSub$evid == 0] <- sapply(timeClusters$cluster, function(x) which(order(timeClusters$centers) == x)) # n=ordered time bins
  
  if (tad) {
    tadClusters <- stats::kmeans(dataSubTad, centers = tadC, nstart = 50)
    dataSub$tadBin[dataSub$evid == 0] <- sapply(tadClusters$cluster, function(x) which(order(tadClusters$centers) == x)) # n=ordered time bins
  } else {
    dataSub$tadBin <- NA
  }
  
  # Simulations -------------------------------------------------------------
  
  # create /vpc
  if (!file.exists(paste(run, "/vpc", sep = ""))) dir.create(paste(run, "/vpc", sep = ""))
  
  # get model file
  instrfile <- suppressWarnings(tryCatch(readLines(paste(run, "etc/instr.inx", sep = "/")), error = function(e) NULL))
  if (length(grep("IVERIFY", instrfile)) == 0) { # not updated instruction file
    modelfile <- readline("Your run used an old instruction file. Enter model name: ")
  } else { # ok we are using updated instruction file
    if (length(instrfile) > 0) { # ok we got one
      # model.for file name
      modelfile <- instrfile[5]
      # convert to original name
      modelfile <- basename(Sys.glob(paste(run, "/inputs/", strsplit(modelfile, "\\.")[[1]][1], "*", sep = "")))
      if (length(modelfile) > 1) {
        modelfile <- modelfile[grep(".txt", modelfile)]
      }
    } else {
      stop("Model file not found.\n")
    }
  }
  
  # copy this modelfile to new /vpc folder
  invisible(file.copy(from = paste(run, "/inputs/", modelfile, sep = ""), to = paste(run, "/vpc", sep = "")))
  
  # now get the data file
  RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste(run, "outputs/??_RF0001.TXT", sep = "/"))), error = function(e) NULL))
  if (length(RFfile) > 0) {
    datafileName <- tail(RFfile, 1)
    # remove trailing spaces
    datafileName <- sub(" +$", "", datafileName)
    file.copy(from = paste(run, "inputs", datafileName, sep = "/"), to = paste(run, "/vpc", sep = ""))
    datafile <- datafileName
  } else {
    stop("Data file not found\n")
  }
  
  # change wd to new /vpc folder which now contains data and model files
  setwd(paste(run, "/vpc", sep = ""))
  
  # simulate PRED_bin from pop icen parameter values and median of each bin for each subject
  # first, calculate median of each bin
  dcMedian <- aggregate(dataSub[, c("dose", binCov)], by = list(dataSub$dcBin), FUN = median, na.rm = T)
  names(dcMedian)[1] <- "bin"
  timeMedian <- aggregate(dataSub$time, by = list(dataSub$timeBin), FUN = median)
  names(timeMedian) <- c("bin", "time")
  
  if (tad) {
    tadMedian <- aggregate(dataSub$tad, by = list(dataSub$tadBin), FUN = median)
    names(tadMedian) <- c("bin", "time")
  } else {
    tadMedian <- NA
  }
  
  # create  datafile based on mdata, but with covariates and doses replaced by medians
  # and sample times by bin times
  mdataMedian <- mdata
  mdataMedian$dcBin <- dataSub$dcBin
  mdataMedian$timeBin <- dataSub$timeBin
  # no need for tadBin as we don't simulate with tad
  mdataMedian$dose <- dcMedian$x[match(mdataMedian$dcBin, dcMedian$bin)]
  mdataMedian$time[mdataMedian$evid == 0] <- timeMedian$time[match(mdataMedian$timeBin[mdataMedian$evid == 0], timeMedian$bin)]
  covCols <- which(names(mdataMedian) %in% binCov)
  if (length(covCols) > 0) {
    for (i in covCols) {
      dcMedianCol <- which(names(dcMedian) == names(mdataMedian[i]))
      mdataMedian[, i] <- dcMedian[match(mdataMedian$dcBin, dcMedian$bin), dcMedianCol]
    }
  }
  # write median file
  MedianDataFileName <- paste(substr(paste("m_", strsplit(datafileName, "\\.")[[1]][1], sep = ""), 0, 8), ".csv", sep = "")
  PMwriteMatrix(mdataMedian[, 1:(ncol(mdataMedian) - 2)], MedianDataFileName, override = T)
  
  # remove old files
  invisible(file.remove(Sys.glob("sim*.txt")))
  
  # get poppar and make one with zero covariance
  poppar <- getName("final")
  popparZero <- poppar
  popparZero$popCov[popparZero$popCov != 0] <- 0
  # do the simulation for each subject using the median dose, median covariates and pop parameters
  if ("seed" %in% names(argsSIM)) {
    seed.start <- argsSIM$seed
    argsSIM[[which(names(argsSIM) == "seed")]] <- NULL
  } else {
    seed.start <- -17
  }
  set.seed(seed.start)
  if ("nsim" %in% names(argsSIM)) {
    nsim <- argsSIM$nsim
    argsSIM[[which(names(argsSIM) == "nsim")]] <- NULL
  } else {
    nsim <- 1000
  }
  if ("limits" %in% names(argsSIM)) {
    limits <- argsSIM$limits
    argsSIM[[which(names(argsSIM) == "limits")]] <- NULL
  } else {
    limits <- NA
  }
  argsSIM1 <- c(list(
    poppar = popparZero, data = MedianDataFileName, model = modelfile, nsim = 1,
    seed = runif(nsub, -100, 100), outname = "simMed"
  ), limits = limits, argsSIM)
  cat("Simulating outputs for each subject using population means...\n")
  flush.console()
  do.call("SIMrun", argsSIM1)
  
  # read and format the results of the simulation
  PRED_bin <- SIMparse("simMed*", combine = T, quiet = T)
  
  # make tempDF subset of PMop for subject, time, non-missing obs, outeq, pop predictions (PREDij)
  tempDF <- getName("op")
  tempDF <- tempDF[tempDF$pred.type == "pop", ]
  tempDF <- tempDF[obsStatus(tempDF$obs)$present, ] %>% filter(time > 0)
  if (!is.na(includeID[1])) {
    tempDF <- tempDF[tempDF$id %in% includeID, ]
  }
  if (!is.na(excludeID[1])) {
    tempDF <- tempDF[!tempDF$id %in% excludeID, ]
  }
  
  if (tad) {
    tempDF$tad <- rep(dataSub$tad[dataSub$evid == 0], 2)
  } else {
    tempDF$tad <- NA
  }
  
  
  # add PRED_bin to tempDF
  tempDF$PRED_bin <- rep(PRED_bin$obs$out[!is.na(PRED_bin$obs$out)], times = 2) # one for icen="median" and icen="mean"
  
  # add pcYij column to tempDF as obs * PREDbin/PREDij
  tempDF$pcObs <- tempDF$obs * tempDF$PRED_bin / tempDF$pred
  
  # #take out observations at time 0 (from evid=4 events)
  # tempDF <- tempDF[tempDF$time>0,]
  
  # bin pcYij by time and add to tempDF
  tempDF$timeBinNum <- rep(dataSub$timeBin[dataSub$evid == 0], times = 2) # one for each icen
  tempDF$timeBinMedian <- timeMedian$time[match(tempDF$timeBinNum, timeMedian$bin)]
  if (tad) {
    tempDF$tadBinNum <- rep(dataSub$tadBin[dataSub$evid == 0], times = 2)
    tempDF$tadBinMedian <- tadMedian$time[match(tempDF$tadBinNum, tadMedian$bin)]
  } else {
    tempDF$tadBinNum <- NA
    tempDF$tadBinMedian <- NA
  }
  
  
  # Now, simulate using full pop model
  # write the adjusted mdata file first
  PMwriteMatrix(mdata, datafileName, override = T)
  
  set.seed(seed.start)
  argsSIM2 <- c(list(
    poppar = poppar, data = datafileName, model = modelfile, nsim = nsim,
    seed = runif(nsub, -100, 100), outname = "full"
  ), limits = limits, argsSIM)
  if (!is.na(includeID[1])) {
    argsSIM2$include <- includeID
  }
  if (!is.na(excludeID[1])) {
    argsSIM2$exclude <- excludeID
  }
  do.call("SIMrun", argsSIM2)
  # read and format the results of the simulation
  simFull <- SIMparse("full*", combine = T, quiet = T)
  # take out observations at time 0 from evid=4
  simFull$obs <- simFull$obs[simFull$obs$time > 0, ]
  # add TAD for plotting options
  if (tad) {
    simFull$obs$tad <- unlist(tapply(dataSub$tad[dataSub$evid == 0], dataSub$id[dataSub$evid == 0], function(x) rep(x, nsim)))
  }
  
  
  
  # pull in time bins from tempDF; only need median as tempDF contains median and mean,
  # but simulation is only from pop means
  
  simFull$obs$timeBinNum <- unlist(tapply(tempDF$timeBinNum[tempDF$icen == "median"], tempDF$id[tempDF$icen == "median"], function(x) rep(x, nsim)))
  # pull in tad bins from tempDF
  simFull$obs$tadBinNum <- unlist(tapply(tempDF$tadBinNum[tempDF$icen == "median"], tempDF$id[tempDF$icen == "median"], function(x) rep(x, nsim)))
  # make simulation number 1:nsim
  simFull$obs$simnum <- as.numeric(sapply(strsplit(simFull$obs$id, "\\."), function(x) x[1]))
  class(simFull) <- c("PMsim", "list")
  
  # NPDE --------------------------------------------------------------------
  
  
  # get npde from github
  checkRequiredPackages("npde", repos = "LAPKB/npde")
  
  # prepare data for npde
  obs <- tempDF[tempDF$icen == "mean", c("id", "time", "obs")]
  
  # remove missing obs
  obs <- obs[obs$obs != -99, ]
  names(obs)[3] <- "out"
  
  simobs <- simFull$obs
  # remove missing simulations
  simobs <- simobs[simobs$out != -99, ]
  simobs$id <- rep(obs$id, each = nsim)
  
  # get NPDE
  assign("thisobs", obs, pos = 1)
  assign("thissim", simobs, pos = 1)
  npdeRes <- tryCatch(npde::autonpde(namobs = thisobs, namsim = thissim, 1, 2, 3, verbose = T), error = function(e) {
    e
    return(NA)
  })
  
  
  
  
  
  
  # Clean Up ----------------------------------------------------------------
  
  valRes <- list(simdata = simFull, timeBinMedian = timeMedian, tadBinMedian = tadMedian, opDF = tempDF, npde = npdeRes)
  class(valRes) <- c("PMvalid", "list")
  
  # save it back to run so it can be loaded in the future
  NPAGout <- list(
    NPdata = getName("NPdata"),
    pop = getName("pop"),
    post = getName("post"),
    final = getName("final"),
    cycle = getName("cycle"),
    op = getName("op"),
    cov = getName("cov"),
    mdata = getName("data"),
    valid = valRes
  )
  save(NPAGout, file = "../outputs/NPAGout.Rdata")
  
  # #put sim in global environment
  # assign(paste("sim",as.character(run),sep="."),simFull,pos=1)
  
  setwd(currwd)
  return(valRes)
}