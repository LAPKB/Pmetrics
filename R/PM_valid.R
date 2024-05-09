#Use menu item Code -> Jump To... for rapid navigation
#Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


# R6 ----------------------------------------------------------------------

#' @title Pmetrics validation object
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains results of internal validation by simulation to permit generation of
#' visual predictive checks (VPCs), prediction corrected visual predictive checks,
#' (pcVPCs), normalized prediction distribution errors (NPDE), and
#' numerical predictive checks.
#'
#' @details
#' This object is created by running the `make_valid` method in a
#' [PM_result] object. It contains all the information necessary
#' to internally validate the result by simulation methods.
#' @seealso [PM_result], [make_valid]
#' @export
PM_valid <- R6::R6Class("PM_valid",
    public = list(
        #' @field simdata Simulated data created in the validation process
        simdata = NULL,
        #' @field timeBinMedian Median times for cluster bins
        timeBinMedian = NULL,
        #' @field tadBinMedian Median times after previous doses for cluster bins
        tadBinMedian = NULL,
        #' @field opDF Observed-predicted data frame
        opDF = NULL,
        #' @field npde Data for Normalized Prediction Distribution Error
        npde = NULL,
        #' @field npde_tad Data for Normalized Prediction Distribution Error
        #' using Time After Dose if available
        npde_tad = NULL,
        #' @description
        #' Create a new PM_valid object from a PM_result
        #' @param result The PM_result object
        #' @param ... Additional arguments to ultimately pass to makeValid
        initialize = function(result, ...) {
            valRes <- make_valid(result = result, ...)

            self$simdata <- valRes$simdata
            self$timeBinMedian <- valRes$timeBinMedian
            self$tadBinMedian <- valRes$tadBinMedian
            self$opDF <- valRes$opDF
            self$npde <- valRes$npde
            self$npde_tad <- valRes$npde_tad
        },
        #' @description
        #' Plot method. Calls [plot.PM_valid].
        #' @param ... Arguments to pass to \[plot.PM_valid].
        plot = function(...) {
            plot.PM_valid(self, ...)
        }
    )
)

# MAKE --------------------------------------------------------------------
#' @title Create a Pmetrics validation object
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function will create an object suitable for plotting visual predictive
#' checks (VPCs) and prediction-corrected visual
#' predictive checks (pcVPCs).
#'
#' @details
#' The function will guide the user
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
#' @param result The result of a prior run, loaded with [PM_load].
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
#' @param limits Limits on simulated parameters. See [SIMrun].
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
#' @examples
#' \dontrun{
#' library(PmetricsData)
#' valid <- NPex$validate(limits = c(0, 3))
#' }
#'
#' @export
#' @seealso [SIMrun], [plot.PM_valid]


make_valid <- function(result, tad = F, binCov, doseC, timeC, tadC, limits, ...) {
  # verify packages used in this function
  if(!checkRequiredPackages(c("mclust", "npde"), quietly = FALSE)){
    return(invisible(NULL))
  }
  require(mclust)
  
  # save current wd
  currwd <- getwd()
  
  # ensure data correct form
  if(!inherits(result, "PM_result")){
    stop(paste("Please supply a PM_result object to validate.\n",
               "PM_result objects are created with", crayon::green("PM_load().")))
  }
  
  
  # parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  # namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  
  # Cluster raw data --------------------------------------------------------
  
  # grab raw data file
  
  mdata <- result$data$standard_data
  
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
  dataSub <- mdata[, c("id", "evid", "time", "out", "dose", "out", dplyr::all_of(binCov))]
  # add time after dose
  if (tad) {
    dataSub$tad <- valTAD
  } else {
    dataSub$tad <- NA
  }
  dataSub <- dataSub %>% select(c("id", "evid", "time", "tad", "out", "dose", dplyr::all_of(binCov)))
  
  
  # restrict to doses for dose/covariate clustering (since covariates applied on doses)
  dataSubDC <- dataSub %>%
    filter(evid > 0) %>%
    select(c("id", "dose", dplyr::all_of(binCov)))
  
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
    mod1 <- mclust::Mclust(dataSubDC)
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
    mod <- mclust::Mclust(use.data)
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
        mod <- mclust::Mclust(use.data, G = TclustNum)
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
  dcMedian <- dataSub %>%
    group_by(bin = dcBin) %>%
    filter(!is.na(dose)) %>%
    summarize(dplyr::across(c(dose, !!binCov), median, na.rm = T))
  
  timeMedian <- dataSub %>%
    group_by(bin = timeBin) %>%
    filter(!is.na(timeBin)) %>%
    summarize(time = median(time, na.rm = T)) %>%
    arrange(time)
  
  if (tad) {
    tadMedian <- dataSub %>%
      group_by(bin = tadBin) %>%
      filter(!is.na(tadBin)) %>%
      summarize(time = median(tad, na.rm = T)) %>%
      arrange(time)
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
  # FIXME
  # TEMPORARY FIX - @Julian: I have an example of a $valid call that generates dosis at unordered times, just want to get pass that
  fil_data <- mdataMedian[, 1:(ncol(mdataMedian) - 2)]
  fil_data <- fil_data[order(fil_data$id, fil_data$time), ]
  # END TEMPORARY FIX
  medianData <- PM_data$new(fil_data, quiet = T)
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
    limits = limits
  ), argsSIM)
  cat("Simulating outputs for each subject using population means...\n")
  flush.console()
  system("echo 347 > SEEDTO.MON")
  do.call("SIMrun", argsSIM1)
  
  # read and format the results of the simulation
  PRED_bin <- SIMparse("simMed*", combine = T, quiet = T)
  PRED_bin$obs <- PRED_bin$obs %>% filter(!is.na(out))
  
  # make tempDF subset of PM_op for subject, time, non-missing obs, outeq, pop predictions (PREDij)
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
  argsSIM2 <- c(
    list(
      poppar = poppar, data = datafileName, model = modelfile, nsim = nsim,
      seed = runif(nsub, -100, 100), outname = "full", limits = limits
    ),
    argsSIM
  )
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
  simFull$obs <- simFull$obs[obsStatus(simFull$obs$out)$present, ]
  
  # add TAD for plotting options
  if (tad) {
    simFull$obs$tad <- dataSub %>%
      filter(evid == 0) %>%
      group_by(id) %>%
      group_map(~ rep(.x$tad, nsim)) %>%
      unlist()
  } else {
    simFull$obs$tad <- NA
  }
  
  
  # pull in time bins from tempDF; only need median as tempDF contains median and mean,
  # but simulation is only from pop means
  simFull$obs$timeBinNum <- dataSub %>%
    filter(evid == 0) %>%
    group_by(id) %>%
    group_map(~ rep(.x$timeBin, nsim)) %>%
    unlist()
  
  # pull in tad bins from tempDF
  simFull$obs$tadBinNum <- dataSub %>%
    filter(evid == 0) %>%
    group_by(id) %>%
    group_map(~ rep(.x$tadBin, nsim)) %>%
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
  
  
  # get number of outeq
  nout <- max(obs$outeq, na.rm = T)
  npde <- list()
  npdeTAD <- list()
  
  for (thisout in 1:nout) {
    obs_sub <- obs %>%
      filter(outeq == thisout) %>%
      select(id, time, out) %>%
      arrange(id, time)
    sim_sub <- simobs %>%
      filter(outeq == thisout, id %in% obs_sub$id) %>%
      select(id, time, out) %>%
      arrange(id, time)
    obs_sub <- data.frame(obs_sub)
    sim_sub <- data.frame(sim_sub)
    
    if (tad) {
      obs_sub2 <- obs %>%
        filter(outeq == thisout) %>%
        select(id, time = tad, out) %>%
        arrange(id, time)
      sim_sub2 <- simobs %>%
        filter(outeq == thisout, id %in% obs_sub$id) %>%
        select(id, time = tad, out) %>%
        arrange(id, time)
      obs_sub2 <- data.frame(obs_sub2)
      sim_sub2 <- data.frame(sim_sub2)
    }
    # get NPDE decorr.method = "inverse",
    npde[[thisout]] <- tryCatch(
      npde::autonpde(obs_sub, sim_sub,
                     iid = "id", ix = "time", iy = "out",
                     detect = F,
                     verbose = F,
                     boolsave = F
      ),
      error = function(e) {
        e
        return(e)
      }
    )
    
    if (inherits(npde[[thisout]], "error")) { # error, often due to non pos-def matrix
      npde[[thisout]] <- tryCatch(
        npde::autonpde(obs_sub, sim_sub,
                       iid = "id", ix = "time", iy = "out",
                       detect = F,
                       verbose = F,
                       boolsave = F,
                       decorr.method = "inverse"
        ),
        error = function(e) {
          e
          return(e)
        }
      )
      
      if (inherits(npde[[thisout]], "error")) { # still with error
        errorMsg <- npde[[thisout]]
        npde[[thisout]] <- paste0("Unable to calculate NPDE for outeq ", thisout, ": ", errorMsg)
      } else {
        cat(paste0("NOTE: Due to numerical instability, for outeq ", thisout, " inverse decorrelation applied, not Cholesky (the default)."))
      }
    }
    
    # get NPDE for TAD
    if (tad) {
      npdeTAD[[thisout]] <- tryCatch(
        npde::autonpde(obs_sub2, sim_sub2,
                       iid = "id", ix = "time", iy = "out",
                       detect = F,
                       verbose = F,
                       boolsave = F
        ),
        error = function(e) {
          e
          return(e)
        }
      )
      
      if (inherits(npdeTAD[[thisout]], "error")) { # error, often due to non pos-def matrix
        npdeTAD[[thisout]] <- tryCatch(
          npde::autonpde(obs_sub2, sim_sub2,
                         iid = "id", ix = "time", iy = "out",
                         detect = F,
                         verbose = F,
                         boolsave = F,
                         decorr.method = "inverse"
          ),
          error = function(e) {
            e
            return(e)
          }
        )
        
        if (inherits(npdeTAD[[thisout]], "error")) { # still with error
          errorMsg <- npdeTAD[[thisout]]
          npdeTAD[[thisout]] <- paste0("Unable to calculate NPDE with TAD for outeq ", thisout, ": ", errorMsg)
        } else {
          cat(paste0("NOTE: Due to numerical instability, for outeq ", thisout, " and TAD, inverse decorrelation applied, not Cholesky (the default)."))
        }
      }
    }
  }
  
  # Clean Up ----------------------------------------------------------------
  
  
  valRes <- list(
    simdata = PM_sim$new(simFull),
    timeBinMedian = timeMedian,
    tadBinMedian = tadMedian,
    opDF = tempDF,
    npde = npde, npde_tad = npdeTAD
  )
  class(valRes) <- c("PMvalid", "list")
  
  setwd(currwd)
  return(valRes)
} # end function




# PLOT --------------------------------------------------------------------
#' @title Plot Pmetrics Validation Objects
#' @description
#' #' `r lifecycle::badge('stable')`
#'
#' Plot [PM_valid] objects.
#' @details
#' Generates a plot of outputs (typically concentrations) on the y axis and time
#' on the x axis. If `tad` was set to `TRUE`
#' when [make_valid] was called, then time may be either absolute (default) or time
#' after dose, controlled by the `tad` argument to this plot function.
#' The following items are included in the plot:
#' * Observed outputs. These may be either as measured for `type = "vpc"` or
#' prediction corrected for `type = "pcvpc"`. Format of the observations is
#' controlled by the `marker` argument. The default is
#' `list(color = "black", symbol = "circle-open", size = 8)`.
#' * Quantiles vs. time for observations. These are plotted by default as dashed
#' blue lines for the 2.5th and 97.5th percentiles and a solid red line for the
#' median. Formatting and the value for each quantile can be controlled
#' with the `upper`, `mid`, and `lower` arguments.
#' * 95% CI around the same quantiles of combined simulations from each subject.
#' The values and formatting for these quantile CIs are the same as for the observations,
#' and also controlled with the `upper`, `mid`, and `lower` arguments.
#'
#' Good vpc/pcvpc plots are considered to be those where the quantile lines for
#' the oberservations lie within the 95%CI quantile regions for simulations,
#' indicated that the model is "centered" on the data and faithfully captures the
#' variability in the data. For an npde plot, one expects to see approximately
#' normally distributed normalized prediction errors.
#' @method plot PM_valid
#' @param x The name of an *PM_valid* data object generated by the [make_valid]
#' function, which is usually called by the `$validate` method for [PM_result]
#' objects.
#' @param type Default is "vpc" for a visual predictive check, but could be
#' "pcvpc" for a prediction-corrected visual predictive check, or
#' "npde" for a normalized prediction distribution error analysis/plot.
#' Choosing npde will call [npde::plot.NpdeObject]. To modify this plot,
#' supply argmuents as a named list: `npde = (...)`. Available arguments
#' are in the user manual for the [npde package](https://github.com/ecomets/npde30).
#' @param tad `r template("tad")`
#' @param outeq `r template("outeq")`
#' @param log `r template("log")`
#' @param marker `r template("marker")`
#' @param line A list of three elements `$upper`, `$mid`, and `$lower`,
#' each of which controls characteristics of corresponding quantiles.
#' The arguments to each of these list elements map to several plotly attributes.
#' Each can be a boolean value or a list.
#' `TRUE` will plot default characteristics. `FALSE` will suppress quantile plots.
#' The elements of the list for each argument are as follows:
#' * `value` The quantile value. Default for lower is 0.025, mid is 0.5,
#' and upper is 0.975.
#' * `color` The color for both the 95%CI region around simulated quantile vs. time,
#' and the color of the line for the observation quantile vs. time.
#' Default for lower and upper is "dodgerblue" and for mid it is "red".
#' * `dash` The style of the obervation quantile line.
#' See `plotly::schema()`, traces > scatter > attributes > line > dash > values.
#' Default for lower and upper is "dash" and for mid it is "solid".
#' * `width` Default is 1 for lower, mid, and upper.
#' * `opacity` The opacity of the 95%CI region around simulated quantile vs. time.
#' Default is 0.4 for lower, mid and upper,
#' but can range between 0 (fully transparent) to 1 (fully opaque).
#' Example: `line = list(upper = list(value = 0.9, color = "red", dash = "longdash", opacity = 0.5, width = 2))`
#' @param legend `r template("legend")` Default is `FALSE`
#' @param log `r template("log")`
#' @param grid `r template("grid")`
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param xlab `r template("xlab")` Default is "Time" or "Time after dose" if `tad = TRUE`.
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param \dots If `type` is not "npde", the following apply. `r template("dotsPlotly")`.
#' However, if `type` is "npde", to modify the appearance of the plot,
#' supply a list of options, `npde = list(...)`. See the documentation
#' for the `type` argument above.
#' @return Plots and returns the plotly object
#' @author Michael Neely
#' @seealso [make_valid]
#' @export
#' @examples
#' library(PmetricsData)
#' # VPC
#' NPex$valid$plot()
#'
#' # pcVPC
#' NPex$valid$plot(type = "pcvpc")
#'
#' # modify median line and marker
#' NPex$valid$plot(
#'   line = list(mid = list(color = "orange", dash = "dashdot")),
#'   marker = list(
#'     color = "blue", size = 12, symbol = "diamond",
#'     line = list(color = "navy")
#'   )
#' )
#' @family PMplots

plot.PM_valid <- function(x,
                          type = "vpc",
                          tad = FALSE,
                          outeq = 1,
                          line = TRUE,
                          marker = TRUE,
                          legend = FALSE,
                          log = FALSE,
                          grid = TRUE,
                          xlab, ylab,
                          title,
                          xlim, ylim, ...) {
  # to avoid modifying original object, x
  opDF <- x$opDF
  simdata <- x$simdata$obs
  
  
  if (outeq > max(opDF$outeq)) {
    stop(paste("Your data do not contain", outeq, "output equations.\n"))
  }
  
  opDF <- opDF %>% filter(outeq == !!outeq) # filter to outeq
  simdata <- simdata %>% filter(outeq == !!outeq) # filter to outeq
  
  
  
  # process CI lines
  if (is.logical(line)) {
    if (line) {
      line <- list(lower = TRUE, mid = TRUE, upper = TRUE)
    } else {
      line <- list(lower = FALSE, mid = FALSE, upper = FALSE)
    }
  } else {
    if (is.null(line$lower)) {
      line$lower <- T
    }
    if (is.null(line$mid)) {
      line$mid <- T
    }
    if (is.null(line$upper)) {
      line$upper <- T
    }
  }
  
  upper <- amendCI(line$upper, default = list(value = 0.975))
  mid <- amendCI(line$mid, default = list(value = 0.5, color = "red", dash = "solid"))
  lower <- amendCI(line$lower, default = list(value = 0.025))
  
  # process marker
  marker <- amendMarker(marker, default = list(color = "black", symbol = "circle-open", size = 8))
  
  # process dots
  dots <- list(...)
  npdeOpts <- pluck(dots, "npde")
  if (!is.null(npdeOpts)) {
    dots$npde <- NULL # remove
  }
  layout <- amendDots(dots)
  
  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)
  
  # axis labels if needed
  if (missing(xlab)) {
    xlab <- c("Time", "Time after dose")[1 + as.numeric(tad)]
  }
  if (missing(ylab)) {
    ylab <- "Output"
  }
  
  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }
  
  # axis ranges
  if (!missing(xlim)) {
    layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
  }
  if (!missing(ylim)) {
    layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
  }
  
  # log y axis
  if (log) {
    layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
  }
  
  # title
  if (missing(title)) {
    title <- ""
  }
  layout$title <- amendTitle(title, default = list(size = 20))
  
  # legend
  legendList <- amendLegend(legend)
  layout <- modifyList(layout, list(showlegend = legendList$showlegend))
  if (length(legendList) > 1) {
    layout <- modifyList(layout, list(legend = within(legendList, rm(showlegend))))
  }
  
  # select correct time
  if (!tad) {
    use.timeBinMedian <- sort(unique(opDF$timeBinMedian))
    use.optimes <- opDF$time
    use.opTimeBinMedian <- x$timeBinMedian$time
    use.opTimeBinNum <- x$timeBinMedian$bin
    use.simBinNum <- simdata$timeBinNum
  } else {
    if (!all(is.na(opDF$tadBinMedian))) {
      cat("Warning: Using time after dose is misleading if not under steady-state conditions.\n")
      use.timeBinMedian <- sort(unique(opDF$tadBinMedian))
      use.optimes <- opDF$tad
      use.opTimeBinMedian <- x$tadBinMedian$time
      use.opTimeBinNum <- x$tadBinMedian$bin
      use.simBinNum <- simdata$tadBinNum
    } else {
      stop("Rerun makePMvalid and set tad argument to TRUE.\n")
    }
  }
  
  # calculate lower, mid, and upper percentiles for pcYij by time bins
  groupVar <- if (tad) {
    quo(tadBinMedian)
  } else {
    quo(timeBinMedian)
  }
  quant_pcObs <- opDF %>%
    group_by(!!groupVar) %>%
    dplyr::reframe(
      value = quantile(pcObs, probs = c(lower$value, mid$value, upper$value), na.rm = TRUE),
      q = c(lower$value, mid$value, upper$value), .groups = "keep"
    ) %>%
    select(-.groups)
  names(quant_pcObs)[1] <- "time"
  
  # calculate lower, 50th and upper percentiles for Yij by time bin
  quant_Obs <- opDF %>%
    group_by(!!groupVar) %>%
    reframe(
      value = quantile(obs, probs = c(lower$value, mid$value, upper$value), na.rm = TRUE),
      q = c(lower$value, mid$value, upper$value), .groups = "keep"
    ) %>%
    select(-.groups)
  names(quant_Obs)[1] <- "time"
  
  # calculate median and CI for upper, median, and lower for each bin
  simGroupVar <- if (tad) {
    quo(tadBinNum)
  } else {
    quo(timeBinNum)
  }
  simCI <- simdata %>%
    group_by(simnum, !!simGroupVar) %>%
    reframe(
      value = quantile(out, probs = c(lower$value, mid$value, upper$value), na.rm = TRUE),
      q = c("lower", "mid", "upper"), .groups = "keep"
    ) %>%
    group_by(bin = !!simGroupVar, q) %>%
    reframe(
      value = quantile(value, probs = c(lower$value, upper$value), na.rm = TRUE),
      ci = c(lower$value, upper$value), .groups = "keep"
    ) %>%
    select(-.groups) %>%
    pivot_wider(names_from = ci, values_from = value, names_prefix = "q")
  
  # arrange simCI in order of time, not bin
  if (!tad) {
    simCI$time <- x$timeBinMedian$time[match(simCI$bin, x$timeBinMedian$bin)]
  } else {
    simCI$time <- x$tadBinMedian$time[match(simCI$bin, x$tadBinMedian$bin)]
  }
  simCI <- simCI %>% arrange(time, q)
  
  
  
  # combine obs and simCI
  quant_pcObs <- quant_pcObs %>%
    select(-q) %>%
    bind_cols(simCI[2:4])
  quant_Obs <- quant_Obs %>%
    select(-q) %>%
    bind_cols(simCI[2:4])
  
  
  
  # type specific options
  if (type == "vpc") {
    timeVar <- if (tad) {
      quo(tad)
    } else {
      quo(time)
    }
    plotData <- list(
      obsQuant = quant_Obs,
      obs = opDF %>%
        select(id, time = !!timeVar, obs)
    )
  }
  if (type == "pcvpc") {
    timeVar <- if (tad) {
      quo(tadBinMedian)
    } else {
      quo(timeBinMedian)
    }
    plotData <- list(
      obsQuant = quant_pcObs,
      obs = opDF %>%
        select(id, time = !!timeVar, obs)
    )
  }
  if (type == "vpc" | type == "pcvpc") {
    # GENERATE THE PLOT
    p <- plotData$obsQuant %>%
      group_by(q) %>%
      plotly::plot_ly(
        x = ~time, y = ~value,
        colors = c(upper$color, mid$color, lower$color),
        linetypes = c(upper$dash, mid$dash, lower$dash),
        color = ~q,
        linetype = ~q
      ) %>%
      # add simulation quantile CIs
      plotly::add_ribbons(
        ymin = ~q0.025, ymax = ~q0.975,
        opacity = upper$opacity,
        line = list(width = 2, color = "white", dash = "solid"),
        hoverinfo = "none"
      ) %>%
      # add observation quantiles
      plotly::add_lines(hovertemplate = "Time: %{x}<br>Out: %{y}<br><extra></extra>") %>%
      # add observations
      plotly::add_markers(
        data = plotData$obs,
        x = ~time, y = ~obs, marker = marker,
        hovertemplate = "Time: %{x}<br>Out: %{y}<br>ID: %{text}<extra></extra>",
        text = ~id,
        inherit = FALSE
      ) %>%
      # add layout
      
      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend,
        legend = layout$legend,
        title = layout$title
      )
    
    # SEND TO CONSOLE
    print(p)
  }
  
  if (type == "npde") {
    if(!checkRequiredPackages("npde", quietly = FALSE)){
      return(invisible(NULL))
    }
    if (!tad) {
      if (is.null(x$npde)) stop("No npde object found.  Re-run $validate or make_valid.\n")
      if (inherits(x$npde[[outeq]], "NpdeObject")) {
        npdeArgs <- c(x = x$npde[[outeq]], npdeOpts)
        do.call(plot, npdeArgs)
        # do.call(npde:::plot.NpdeObject, npdeArgs)
        par(mfrow = c(1, 1))
      } else {
        cat(paste0("Unable to calculate NPDE for outeq ", outeq))
      }
    } else {
      if (is.null(x$npde_tad)) stop("No npde_tad object found.  Re-run $validate or make_valid with tad = T.\n")
      if (inherits(x$npde_tad[[outeq]], "NpdeObject")) {
        npdeArgs <- c(x = x$npde_tad[[outeq]], npdeOpts)
        do.call(plot, npdeArgs)
        # do.call(npde:::plot.NpdeObject, npdeArgs)
        par(mfrow = c(1, 1))
      } else {
        cat(paste0("Unable to calculate NPDE with TAD for outeq ", outeq))
      }
    }
    
    p <- NULL
  }
  return(invisible(p))
}




#' @title Plot Pmetrics Validation Objects
#' @description
#' `r lifecycle::badge('superseded')`
#'
#' This is largely now a legacy plotting function,
#' with a variety of options. It has been superseded by [plot.PM_valid].
#' @method plot PMvalid
#' @param x The name of an \emph{PMvalid} data object generated by \code{\link{make_valid}}.
#' @param type Default is \dQuote{vpc} for a visual prective check, but could be \dQuote{pcvpc} for a
#' prediction-corrected visual predictive check.
#' @param tad Plot using time after dose if \code{TRUE}.  Default is \code{FALSE} which plots using standard
#' relative time.  This will be the only option if \code{tad} was not set to \code{TRUE} when making the
#' PMvalid object.
#' @param icen Can be either \dQuote{median} for the predictions based on medians of the population parameter value
#' distributions, or \dQuote{mean}.  Default is \dQuote{median}.
#' @param outeq The number of the output equation to simulate/test.  Default is 1.
#' @param lower The lower quantile displayed for the observed and simulated profiles. Default is 0.025.
#' @param upper The upper quantile displayed for the observed and simulated profiles. Default is 0.975.
#' @param log Boolean operator to plot in semilog space.  The default is \code{FALSE}.
#' @param pch.obs Control the plotting character used for observations.  Default is 1, i.e. an open circle.
#' See \code{\link{points}} for other values of \code{pch}.
#' @param col.obs Color for observations.  Default is black.
#' @param cex.obs Size for observatins.  Default is 1.
#' @param data_theme Default is \dQuote{color}, but could be \dQuote{grey} or \dQuote{gray}.
#' @param plot_theme Default is `theme_grey()` but could be any complete ggplot2 theme, e.g.
#'  [ggplot2::theme_minimal()].
#' @param col.obs.ci Color of the observation confidence interval (set by \code{lower} and \code{upper}).
#' Default is blue.
#' @param col.obs.med Color of the observation median.
#' Default is red.
#' @param col.sim.ci Color of the simulation confidence interval (set by \code{lower} and \code{upper}).
#' Default is dodgerblue.
#' @param col.sim.med Color of the simulation median.
#' Default is lightpink.
#' @param axis.x List of `$name` and `$limits`. Default name is \dQuote{Time}.
#' @param axis.y List of `$name` and `$limits`. Default name is \dQuote{Observation}.
#' @param ... Not currently used
#' @return Plots the object using ggplot2.
#' @author Michael Neely
#' @seealso \code{\link{make_valid}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{points}}
#' @export

plot.PMvalid <- function(x, type = "vpc", tad = FALSE, icen = "median", outeq = 1, lower = 0.025, upper = 0.975,
                         log = FALSE, pch.obs = 1, col.obs = "black", cex.obs = 1, data_theme = "color", plot_theme = theme_grey(),
                         col.obs.ci = "blue", col.obs.med = "red", col.sim.ci = "dodgerblue", col.sim.med = "lightpink",
                         axis.x = NULL,
                         axis.y = NULL, ...) {
  # parse dots
  # arglist <- list(...)
  # names_theme <- names(formals(ggplot2::theme)) #check for elements of ggplot2::theme
  # argsTheme <- arglist[which(names(arglist) %in% names_theme)]
  
  # checkRequiredPackages("ggplot2")
  if (outeq > max(x$opDF$outeq)) {
    stop(paste("Your data do not contain", outeq, "output equations.\n"))
  }
  if (icen != "mean" & icen != "median") {
    stop(paste("Use \"mean\" or \"median\" for icen.\n", sep = ""))
  }
  
  x$opDF <- x$opDF[x$opDF$icen == icen & x$opDF$outeq == outeq, ] # filter to icen & outeq
  x$simdata$obs <- x$simdata$obs[x$simdat$obs$outeq == outeq, ] # filter to outeq
  
  # select correct time
  if (!tad) {
    use.timeBinMedian <- x$timeBinMedian$time
    use.optimes <- x$opDF$time
    use.opTimeBinMedian <- x$opDF$timeBinMedian
    use.opTimeBinNum <- x$opDF$timeBinNum
    use.simBinNum <- x$simdata$obs$timeBinNum
  } else {
    if (!all(is.na(x$tadBinMedian))) {
      cat("Warning: Using time after dose is misleading if not under steady-state conditions.\n")
      use.timeBinMedian <- x$tadBinMedian$time
      use.optimes <- x$opDF$tad
      use.opTimeBinMedian <- x$opDF$tadBinMedian
      use.opTimeBinNum <- x$opDF$tadBinNum
      use.simBinNum <- x$simdata$obs$tadBinNum
      if (axis.x$name == "Time") {
        axis.y$name <- "Time after dose"
      }
    } else {
      stop("Rerun makePMvalid and set tad argument to TRUE.\n")
    }
  }
  
  # calculate lower, 50th and upper percentiles for pcYij by time bins
  quant_pcObs <- tapply(x$opDF$pcObs, use.opTimeBinNum, quantile, probs = c(lower, 0.5, upper), na.rm = TRUE)
  # calculate lower, 50th and upper percentiles for Yij by time bin
  quant_Obs <- tapply(x$opDF$obs, use.opTimeBinNum, quantile, probs = c(lower, 0.5, upper), na.rm = TRUE)
  
  # find lower, median, upper percentiles by sim and bin
  simMed <- tapply(x$simdata$obs$out, list(x$simdata$obs$simnum, use.simBinNum), FUN = median, na.rm = TRUE) # nsim row, timeBinNum col
  simLower <- tapply(x$simdata$obs$out, list(x$simdata$obs$simnum, use.simBinNum), FUN = quantile, na.rm = TRUE, lower) # nsim row, timeBinNum col
  simUpper <- tapply(x$simdata$obs$out, list(x$simdata$obs$simnum, use.simBinNum), FUN = quantile, na.rm = TRUE, upper) # nsim row, timeBinNum col
  
  # calculate median and CI for upper, median, and lower for each bin
  
  upperLower <- apply(simUpper, 2, quantile, lower, na.rm = TRUE)[order(use.timeBinMedian)]
  upperUpper <- apply(simUpper, 2, quantile, upper, na.rm = TRUE)[order(use.timeBinMedian)]
  medianLower <- apply(simMed, 2, quantile, lower, na.rm = TRUE)[order(use.timeBinMedian)]
  medianUpper <- apply(simMed, 2, quantile, upper, na.rm = TRUE)[order(use.timeBinMedian)]
  lowerLower <- apply(simLower, 2, quantile, lower, na.rm = TRUE)[order(use.timeBinMedian)]
  lowerUpper <- apply(simLower, 2, quantile, upper, na.rm = TRUE)[order(use.timeBinMedian)]
  
  # calculate time boundaries for each bin
  if (tad) {
    minBin <- tapply(x$opDF$tad, x$opDF$tadBinNum, min)
    maxBin <- tapply(x$opDF$tad, x$opDF$tadBinNum, max)
  } else {
    minBin <- tapply(x$opDF$time, x$opDF$timeBinNum, min)
    maxBin <- tapply(x$opDF$time, x$opDF$timeBinNum, max)
  }
  timeBinNum <- length(minBin)
  
  # polytime <- c(mitimeBinNum[1],rep(sapply(1:(timeBinNum-1),function(x) mean(c(mitimeBinNum[x+1],maxBin[x]))),each=2),maxBin[timeBinNum])
  polytime <- use.timeBinMedian
  
  # upperDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(upperUpper,each=2),rev(rep(upperLower,each=2))))
  # medDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(medianUpper,each=2),rev(rep(medianLower,each=2))))
  # lowerDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(lowerUpper,each=2),rev(rep(lowerLower,each=2))))
  upperDF <- data.frame(time = c(polytime, rev(polytime)), value = c(upperUpper, rev(upperLower)))
  medDF <- data.frame(time = c(polytime, rev(polytime)), value = c(medianUpper, rev(medianLower)))
  lowerDF <- data.frame(time = c(polytime, rev(polytime)), value = c(lowerUpper, rev(lowerLower)))
  
  
  # type specific options
  if (type == "vpc") {
    plotData <- list(
      obsQuant = quant_Obs, obs = x$opDF$obs, binTime = use.timeBinMedian,
      obsTime = use.optimes, upperDF = upperDF, lowerDF = lowerDF,
      medDF = medDF
    )
  }
  if (type == "pcvpc") {
    plotData <- list(
      obsQuant = quant_pcObs, obs = x$opDF$pcObs, binTime = use.timeBinMedian,
      obsTime = use.opTimeBinMedian, upperDF = upperDF, lowerDF = lowerDF,
      medDF = medDF
    )
  }
  
  # common options
  if (type == "vpc" | type == "pcvpc") {
    # set names if not specified
    if (!"name" %in% names(axis.x)) {
      axis.x$name <- "Time"
    }
    if (!"name" %in% names(axis.y)) {
      axis.y$name <- "Observation"
    }
    
    
    # set limits if not specified
    if (!"limits" %in% names(axis.x)) {
      axis.x$limits <- c(range(plotData$obsTime))
    }
    if (!"limits" %in% names(axis.y)) {
      axis.y$limits <- c(
        min(plotData$obs, plotData$lower$value),
        max(plotData$obs, plotData$upperDF$value)
      )
    }
    
    
    # override colors to make greyscale
    if (data_theme == "grey" | data_theme == "gray") { # set to grayscale
      col.obs <- "black"
      col.obs.ci <- "grey20"
      col.obs.med <- "grey20"
      col.sim.ci <- "grey75"
      col.sim.med <- "grey50"
    }
    # GENERATE THE PLOT
    p <- with(
      plotData,
      ggplot(mapping = aes(x = binTime, y = unlist(lapply(obsQuant, function(x) x[3])))) +
        geom_line(col = col.obs.ci, lty = 2, lwd = 1) +
        geom_polygon(aes(x = time, y = value), data = upperDF, fill = col.sim.ci, alpha = 0.25) +
        geom_polygon(aes(x = time, y = value), data = medDF, fill = col.sim.med, alpha = 0.25) +
        geom_polygon(aes(x = time, y = value), data = lowerDF, fill = col.sim.ci, alpha = 0.25) +
        geom_line(aes(
          x = binTime,
          y = unlist(lapply(obsQuant, function(x) x[2]))
        ), col = col.obs.med, lty = 1, lwd = 1) +
        geom_line(aes(
          x = binTime,
          y = unlist(lapply(obsQuant, function(x) x[1]))
        ), col = col.obs.ci, lty = 2, lwd = 1) +
        geom_point(aes(x = obsTime, y = obs), col = col.obs, pch = pch.obs, cex = cex.obs) +
        do.call(scale_x_continuous, axis.x) +
        do.call(scale_y_continuous, axis.y) +
        do.call(theme, plot_theme)
    )
    # SEND TO CONSOLE
    print(p)
  }
  
  if (type == "npde") {
    # cat("NPDE temporarily disabled pending code cleaning.\n")
    if (is.null(x$npde)) stop("No npde object found.  Re-run $validate or make_valid.\n")
    plot(x$npde)
    par(mfrow = c(1, 1))
    p <- NULL
  }
  return(p)
}

