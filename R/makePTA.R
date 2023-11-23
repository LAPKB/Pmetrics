#' @title Calculation of PTAs
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates the Percent Target Attainment (PTA)
#'
#' @details
#' `makePTA` will calculate the PTA for any number of simulations, targets and definitions of success.
#' Simulations typically differ by dose, but may differ by other features such as children vs. adults. This function will also
#' accept data from real subjects either in the form of a *PMpost* or a *PMmatrix* object.
#' If a *PMpta* object is passed to the function as the `simdata`, the only other parameter required is success.
#' If desired, a new set of simlabels can be specified; all other parameters will be ignored.
#'
#' @param simdata Can be one of multiple inputs.  Typically it is a vector of simulator output filenames, e.g. c("simout1.txt","simout2.txt"),
#' with wildcard support, e.g. "simout*" or "simout?", or
#' a list of PMsim objects made by `simdata` with suitable simulated regimens and observations.  The number and times of simulated
#' observations does not have to be the same in all objects.  It can also be a *PMpta* object previously made with
#' `makePTA` can be passed for recalculation with a new success value or simlabels.  Finally, *PMpost* and *PMmatrix* objects are
#' also allowed.
#' @param simlabels Optional character vector of labels for each simulation.  Default is `c('Regimen 1', 'Regimen 2',...)`.
#' @param target One of several options.
#' 
#' * A vector of pharmacodynamic targets, such as Minimum Inhibitory Concentrations (MICs), e.g. `c(0.25, 0.5, 1, 2, 4, 8, 16, 32)`.
#' * A single numerical value such as a concentration, e.g. 10.
#' * A sampled distribution using  [makePTAtarget]. 
#' * A list of multiple targets combining the above if multiple `target_type`s are used. If so, the first `target` can be a vector, 
#' but subsequent targets must be single values to avoid factorial expansion of combinations. For example, the first target could be a vector of MICs corresponding
#' to a `target_type` of "time", the second target a value of 10 corresponding to a `target_type` of "min", and the third target a value of 50
#' corresponding to a `target_type` of "max": `target = list(c(0.25, 0.5, 1, 2, 4, 8, 16, 32), 10, 50)`. The first value can also be a sampled
#' distribution made with [makePTAtarget].
#' 
#' @param target_type A vector of the type for each `target`.  For any, place a minus sign
#' in front to make the success less than the target ratio, e.g. `target_type = c("min", "-min")`. Available types:
#' 
#' * "time" is percent time above `target` within the time range specified by `start` and `end`
#' * "auc" is ratio of area under the curve within the time range to `target`
#' * "peak" or "max", ratio of peak/max (synonymous) concentration to `target` within the time range. Place a minus sign
#' in front to make the success less than the target ratio.
#' * "min", is the ratio of minimum concentration to `target` within the time range. Place a minus sign
#' in front to make the success less than the target ratio. 
#' * A single numeric value, which must correspond to an observation time common to all PMsim objects in
#' `simdata`, rounded to the nearest hour.  In this case, the target statistic will be the ratio of observation at that time to `target`.
#' 
#' This enables testing of a specific timed concentration (e.g. one hour after a dose or C1).  Be sure that the time in the simulated data is used, 
#' e.g., 122 after a dose given at 120. Place a minus sign
#' in front to make the success less than the target ratio.
#' @param success A vector specifying the success statistics, e.g. 0.4 for proportion time (end-start) above target, and/or 100 for max:target.
#' For example `success = 0.4` or `success = c(0.4, 100)`. The length must be the same as for `target` and `target_type`.
#' @param outeq An integer specifying the number of the simulated output equation to use. Default is 1.
#' @param free_fraction Proportion of free, active drug, expressed as a numeric value >=0 and <=1.  Default is 1, i.e.,
#' 100% free drug or 0% protein binding.
#' @param start Specify the time to begin PTA calculations. Default is a vector with the first observation time for subjects
#' in each element of `simdata`, e.g. dose regimen. If specified as a vector, values will be recycled as necessary.
#' @param end Specify the time to end PTA calculations so that PTA is calculated
#' from `start` to `end`.  Default for end is the maximum observation
#' time for subjects in each element of `simdata`, e.g. dose regimen.  If specified as a vector, values will be recycled
#' as necessary. Subjects with insufficient data (fewer than 5 simulated observations) for a specified interval will trigger a warning.
#' Ideally then, the simulated datset should contain sufficient observations within the interval specified by `start` and `end`.
#' @param icen Can be either "median" for the predictions based on medians of `pred.type` parameter value
#' distributions, or "mean".  Default is "median".
#' @param block Which block to plot, where a new block is defined by dose resets (evid = 4); default is 1.
#' @return The output of `makePTA` is a list of class *PMpta*,
#' which is a list with each `target_type` as an element, followed by a final `intersection` element showing the results
#' for profiles which meet ALL the conditions (intersection) or `NA` if only one `target_type` was specified. 
#' The individual elements are tibbles with all possible combinations
#' of `target`s and simulated regimens for a given `target_type`. The tibbles have the following columns:
#' * **sim_num** The simulation number in `simdata`.
#' * **label** Annotation of the simulation, supplied by the `simlabels` argument.
#' * **target** is the specified `target` for the results row. If a distribution created by [makePTAtarget],
#' this will be a tibble with  the simulated targets
#' * **type** is the specified `target_type` for the results row
#' * **success_ratio** The specified `success` metric for the results row
#' * **prop_success** The proportion of profiles meeting the `success_ratio` for the results row
#' * **success** A tibble of success (1) or not (0) for each profile for the results row
#' * **pdi** A tibble of the pharmacodynamic index, i.e. the ratio or time above for each profile for the results row
#' * **start** The start time used for the results row
#' * **end** The end time used for the results row.
#' For the `$intersect` item in the return list, the columns are the same, but the 
#' `target` and `target_type` will reflect all requested values expressed in
#' parenthetical multiplication format to emphasize intersection, e.g., (auc)(min). 
#' Simulated (rather than discrete) targets made with [makePTAtarget] will be 
#' abbreviated as "(sim)", e.g. (sim)(5) for a combination of simulated targets and
#' a single concentration target of 5.
#' 
#' @author Michael Neely and Jan Strojil
#' @seealso [plot.PM_pta], [PM_sim]
#' @examples
#' pta1 <- PM_pta$new(simEx, 
#'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
#'                  target = list(2^(-2:6), 1, 50),
#'                  target_type = c("time", 144, "-max"),
#'                  success = c(0.6, 1, 1),
#'                  start = 120, end = 144)
#' 
#' pta2 <- PM_pta$new(simEx,
#'                  target = c(2^(-2:6)),
#'                  target_type = "time",
#'                  success = 0.6,
#'                  start = 120, end = 144)
#' 
#' pta3 <- PM_pta$new(simEx,
#'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
#'                  target = list(5,10),
#'                  target_type = c("min", "-min"),
#'                  success = c(1,1),
#'                  start = 120, end = 144)
#' 
#' pta4 <- PM_pta$new(simEx,
#'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
#'                  target = makePTAtarget(mic1),
#'                  target_type = "auc",
#'                  success = 200,
#'                  start = 120, end = 144)
#' 
#' pta5 <- PM_pta$new(simdata = simEx,
#'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
#'                  target = list(makePTAtarget(mic1),5),
#'                  target_type = c("auc","min"),
#'                  success = c(200,1),
#'                  start = 120, end = 144)
#' @export


makePTA <- function(simdata, simlabels, target, target_type, success, outeq = 1,
                    free_fraction = 1, start = 0, end = Inf, icen = "median", block = 1,
                    ...){ #capture deprecated
  
  #handle deprecated arguments
  extraArgs <- list(...)
  deprecated_args <- c("targets", "target.type", "free.fraction")
  new_args <- c("target", "target_type", "free_fraction")
  which_deprecated <- which(deprecated_args %in% names(extraArgs))
  if(length(which_deprecated)>0){
    stop("The following arguments have been deprecated: ",paste0(deprecated_args[which_deprecated], collapse = ", "),
         ". Use the following instead: ", paste0(new_args[which_deprecated], collapse = ", "),".")
  }
  
  
  # initial check
  if (missing(simdata) | missing(target) | missing(target_type) | missing(success)){
    stop("Simulation output (simdata), target, target_type, and success are all mandatory.\n")
  }
  
  #what kind of object is simdata?
  #lists, characters are assumed to be simulations 
  dataType <- switch(EXPR=class(simdata)[1], PM_sim = 0, PMsim = 1, PM_simlist = 2,
                     list = 3, character = 4, PMpost = 5, PMmatrix = 6 ,
                     PMpta = 7, PM_data = 8, -1)
  if(dataType==-1){
    stop("You must specify a PM_sim, PMsim (legacy), list of simulations, character vector of simulator output files, PMpost, PMmatrix (legacy), or PM_data object\n")
  }
  
  if (dataType!=7) { #if simdata is not already a PMpta object
    
    ########### new PTA calculation ##################  
    #need to get it into a list of PMsim objects
    if (dataType==0) { #PM_sim object
      if(inherits(simdata$data, "PM_simlist")){ #multiple sims
        simdata <- simdata$data
      } else {  #just one sim
        simdata <- list(simdata$data)
      }
    }

    if (dataType == 1) { # PMsim object
      simdata <- list(simdata)
    }

    if (dataType == 2) { # PM_simlist
      simdata <- purrr::map(simdata$data, \(x) x$data) # extract data
    }

    # nothing to do for dataType=3 already in right format

    if (dataType == 4) { # character vector of simulator output files
      simfiles <- Sys.glob(simdata)
      if (length(simfiles) == 0) {
        stop("There are no files matching \"", simdata, "\".\n", sep = "")
      }
      simdata <- list()
      for (i in 1:length(simfiles)) {
        simdata[[i]] <- tryCatch(SIMparse(simfiles[i]), error = function(e) stop(paste(simfiles[i], "is not a PMsim object.\n")))
      }
    }

    if (dataType == 5) { # PMpost object
      simdata <- simdata %>% filter(icen == !!icen & block == !!block)
      temp <- list(obs=data.frame(id=simdata$id,time=simdata$time,out=simdata$pred,outeq=simdata$outeq))
      simdata <- list(temp)
    }

    if (dataType == 6 | dataType == 8) { # PMmatrix or PM_data object
      if (dataType == 8) {
        simdata <- simdata$data
      }
      simdata <- makePMmatrixBlock(simdata)
      simdata <- simdata %>% filter(evid == 0 & block == !!block)
      temp <- list(obs = data.frame(id = simdata$id, time = simdata$time, out = simdata$out, outeq = simdata$outeq))
      simdata <- list(temp)
    }
  }
  
  if (is.numeric(target) | inherits(target,"PMpta.targ")){
    target <- list(target) #make a list
  }
  
  #define some global variables
  n_sim <- length(simdata) # number of regimens
  n_id <- max(sapply(simdata,function(x) nrow(x$parValues))) #number of simulated id per regimen (usually 1000)
  n_type <- length(target_type)
  n_success <- length(success)
  n_target <- length(target) #length of the whole list, should correspond with n_type, success
  if (inherits(target[[1]], "PMpta.targ")) {
    simTarg <- T
  } else {
    simTarg <- F
  }
  
  #fill in start and end times for each regimen
  if(length(start) < n_sim){
    start <- rep(start, n_sim)[1:n_sim]
  }
  if(length(end) < n_sim){
    end <- rep(end, n_sim)[1:n_sim]
  }
  
  #check for valid arguments
  target_type <- stringr::str_replace_all(target_type,"peak","max")
  invalid_types <- purrr::map_lgl(target_type, \(x){
    !x %in% c("time", "auc", "max", "peak", "min", "-time", "-auc", "-max", "-peak", "-min") &&
      suppressWarnings(is.na(as.numeric(x)))
  })
  
  if (any(invalid_types)){
    stop("Please specify target_type as a numerical value corresponding to a common\ntime in all simulated datasets, or a character value of 'time', 'auc', 'max' or 'min'.\n")
  }
  
  #adjust start and end for any specific times
  start <- unlist(map(1:n_type, \(x) {
    if(suppressWarnings(!is.na(as.numeric(target_type[x])))) {
      abs(as.numeric(target_type[x]))
    } else {
      start[x]
    }}))
  end <- unlist(map(1:n_type, \(x) {
    if(suppressWarnings(!is.na(as.numeric(target_type[x])))) {
      abs(as.numeric(target_type[x]))
    } else {
      end[x]
    }}))
  
  
  #check to make sure secondary targets are only length 1
  if (n_type > 1){ 
    invalid_sec_type <- purrr::map_lgl(target[2:n_target], \(x){
      length(x) > 1
    })
    if (any(invalid_sec_type)){
      stop("For multiple target_types, types after the first cannot have more than one target, i.e., they are typically are min, max, or specific.\n")
    }
  }
  
  if (!identical(n_target, n_type, n_success)){
    stop("Target, target_type, and success vectors must all be the same length for discrete targets.\n")
  } 
  
  if (stringr::str_detect(free_fraction,"%")){ # if passed as percents convert to a number
    free_fraction <- as.numeric(stringr::str_replace(free_fraction,"%",""))/100
  }
  while(free_fraction <= 0 | free_fraction > 1) {
    free_fraction <- as.numeric(readline(cat("Invalid free fraction, please specify a fraction > 0 and <= 1.\n")))
  }
  if (stringr::str_detect(success[1],"%")){ # if passed as percents convert to a number
    success[1] <- as.numeric(stringr::str_replace(success[1],"%",""))/100
  }
  if (success[1] <= 0 | ((target_type[1] == "time" | target_type[1] == "-time") & success[1] > 100)) stop("Invalid success threshold value. Aborting.", call.=F)
  if (target_type[1] =="time" & success[1] > 1 & success[1] <= 100) {
    cat("Your specified success threshold for time above target of ", success[1], " is bigger than 1.", sep="")
    ans <- readline(cat("\nWhat would you like to do?\n1) set success to ",success[1]/100," (i.e. ",success[1],"% of time relative to target)\n2) end ", sep=""))
    if (ans == 1) {
      success[1] = success[1]/100
      cat("Success threshold for time was set to ",success[1],".",sep="")
    } else stop("Function aborted.", call.=F)
  }
  
  #### PREPARE DATA 
  
  #check outeq
  if (!outeq %in% simdata[[1]]$obs$outeq) {
    stop("There are no simulated outputs for output equation ", outeq, ". Aborting.", call. = F)
  }
  
  #filter and multiply free fraction
  simdata <- purrr::map(1:n_sim, \(x) {
    simdata[[x]]$obs <- simdata[[x]]$obs %>% filter(outeq == !!outeq, !is.na(out)) %>%
      mutate(outeq = 1) #after filter, change to 1
    simdata[[x]]$obs$out <- simdata[[x]]$obs$out * free_fraction
    simdata[[x]]
  })
  
  
  #Check the simulation labels
  sim_labels <- paste("Regimen", 1:n_sim)
  
  if (!missing(simlabels)) { #replace generic labels with user labels
    n_simlabels <- length(simlabels)
    if (n_simlabels < n_sim) warning("There are more simulated regimens (n=",n_sim,") than labels (n=",n_simlabels,").", call.=FALSE, immediate. = TRUE)
    if (n_simlabels > n_sim) warning("There are fewer simulated regimens (n=",n_sim,") than labels (n=",n_simlabels,"); some labels will be ignored.", call.=FALSE, immediate. = TRUE)
    sim_labels[1:min(n_simlabels, n_sim)] <- simlabels[1:min(n_simlabels, n_sim)]
  }
  
  #calculate number of iterations for progress bar
  if (!simTarg) {
    cat("\nCalculating PTA for each simulated regimen and target...\n")
  } else {
    cat("\nCalculating PTA for each simulated regimen using simulated targets...\n")
  }
  flush.console()
  
  #create the progress bar
  maxpb <- sum(unlist(purrr::map(target,\(x) ifelse(inherits(x, "PMpta.targ"), 1, length(x)))))  * n_sim #target * simulations
  pb <- txtProgressBar(min = 0, max = maxpb, style = 3)
  
  
  ###### MAKE THE PTA OBJECT
  master_pta <- map_df(1:n_type, \(x) expand_grid(sim_num = 1:n_sim, 
                                                  target = if(simTarg){ #simulated targets
                                                    if(x == 1) {
                                                      list(
                                                        tidyr::tibble(id = 1:n_id,
                                                                      target = sample(x = target[[x]]$target, size = n_id[x], replace = T, prob = target[[x]]$n)
                                                        ))
                                                    } else {
                                                      target[x]
                                                    }
                                                  } else {target[[x]]}, #discrete targets
                                                  this_type = x,
                                                  type = target_type[[x]], 
                                                  success_ratio = success[[x]],
                                                  start = start[x],
                                                  end = end[x])) %>%
    mutate(type = stringr::str_replace_all(type,"\\d+", "specific")) %>%
    rowwise() %>%
    mutate(pdi = list(do.call(paste0("pta_",stringr::str_replace_all(type, "-","")), 
                              list(sims = simdata[[sim_num]]$obs, 
                                   .target = target,
                                   .simTarg = simTarg,
                                   .start = start, 
                                   .end = end,
                                   .pb = pb)))) %>%
    mutate(success = list(purrr::map_dbl(pdi, \(x) {
      if(str_detect(type, "-")){
        x <= success_ratio #will return NA is x is NA
      } else {
        x >= success_ratio
      }
    }))) %>%
    mutate(prop_success = sum(success)/length(success)) %>%
    mutate(label = sim_labels[sim_num]) %>%
    relocate(sim_num, label, target, type, success_ratio, prop_success, success, pdi, start, end) %>%
    ungroup() #remove rowwise
  
  #add intersection if multiple target types
  #browser()
  if(n_type > 1){
    master_pta <- split(master_pta, master_pta$this_type) %>% #split by target type number
      .[order(match(names(.),target_type))] 
    master_pta <- map(master_pta, \(x) {x$this_type = NULL; x})
    names(master_pta) <- NULL
    master_pta$intersect <- master_pta[[1]] %>% select(sim_num, target, success_1 = success, label) #get primary success
    for(i in 2:n_type){ #add additional success
      master_pta$intersect[[paste0("success_", i)]] <- master_pta[[i]]$success[match(master_pta[[1]]$sim_num, master_pta[[i]]$sim_num)] 
    }
    all_success <- master_pta$intersect %>%
      select(tidyr::starts_with("success")) 
    total_success <- lapply(apply(all_success, 1, function(x) as.data.frame(x)), rowSums) #sum all success matrices for each sim/target
    master_pta$intersect$prop_success <- purrr::map_dbl(total_success, \(x) sum(x == n_type)/length(x))
    master_pta$intersect <- master_pta$intersect %>% 
      rowwise() %>%
      mutate(target = paste0("(",c(target, !!target[2:n_type]),")", collapse = "")) %>%
      mutate(target = stringr::str_replace(target,"\\(1:(.|\\n)*\\){2}","(sim)")) %>%
      mutate(type = paste0("(",target_type,")", collapse = "")) %>%
      mutate(success_ratio = paste0("(",success,")", collapse = "")) %>%
      mutate(success = list(total_success)) %>%
      select(sim_num, label, target, type, success_ratio, prop_success, success) %>%
      ungroup()
    
  } else { #only one target_type
    master_pta <- master_pta %>% 
      select(-this_type) %>%
      list(., intersect = NA)
  }
  
  class(master_pta) <- c("PMpta", "list")
  return(master_pta)
  
  
}

#accessory internal functions

pta_auc <- function(sims, .target, .simTarg, .start, .end, .pb){
  
  cycle <- getTxtProgressBar(.pb)
  setTxtProgressBar(.pb, cycle+1)
  
  auc <- tryCatch(makeAUC(sims, out ~ time, start = .start, end = .end), error = function(e) NA)
  if(nrow(auc)>0){
    if(.simTarg & length(.target) > 1){
      auc <- dplyr::left_join(auc, .target, by = "id")
    } else {
      auc$target <- .target
    }
    pdi <- auc$tau/auc$target
  } else {
    pdi <- NA #filtered to 0 rows or other AUC error
  }
  return(pdi)
}

pta_min <- function(sims, .target, .simTarg, .start, .end, .pb){
  
  cycle <- getTxtProgressBar(.pb)
  setTxtProgressBar(.pb, cycle+1)
  
  mins <- sims %>% group_by(id) %>% filter(time >= .start, time <= .end)
  if(.simTarg & length(.target) > 1){
    mins <- dplyr::left_join(mins, .target, by = "id")
  } else {
    mins$target <- .target
  }
  if(nrow(mins)>0){
    mins <- mins %>%
      summarise(min = min(out, na.rm = TRUE), target = target[1]) %>%
      ungroup() 
    pdi <- mins$min/mins$target
  } else {
    pdi <- NA #filtered to 0 rows
  }
  return(pdi)
}

pta_max <- function(sims, .target, .simTarg, .start, .end, .pb){
  
  cycle <- getTxtProgressBar(.pb)
  setTxtProgressBar(.pb, cycle+1)
  
  maxes <- sims %>% group_by(id) %>% filter(time >= .start, time <= .end) 
  if(.simTarg & length(.target) > 1){
    maxes <- dplyr::left_join(maxes, .target, by = "id")
  } else {
    maxes$target <- .target
  }
  if(nrow(maxes)>0){
    maxes <- maxes %>%
      summarise(max = max(out, na.rm = TRUE), target = target[1]) %>%
      ungroup() 
    pdi <- maxes$max/maxes$target
  } else {
    pdi <- NA #filtered to 0 rows
  }
  return(pdi)
}

pta_specific <- function(sims, .target, .simTarg, .start, .end, .pb){
  
  cycle <- getTxtProgressBar(.pb)
  setTxtProgressBar(.pb, cycle+1)
  
  concs <- sims %>% group_by(id) %>% filter(time == .start) 
  if(.simTarg & length(.target) > 1){
    concs <- dplyr::left_join(concs, .target, by = "id")
  } else {
    concs$target <- .target
  }
  if(nrow(concs)>0){
    pdi <- concs$out/concs$target
  } else {
    pdi <- NA #filtered to 0 rows
  }
  return(pdi)
}

pta_time <- function(sims, .target, .simTarg, .start, .end, .pb){
  
  cycle <- getTxtProgressBar(.pb)
  setTxtProgressBar(.pb, cycle+1)
  
  interval <- diff(sims$time)[1] #will be regular for sims
  
  times <- sims %>% group_by(id) %>% filter(time >= .start, time <= .end) 
  if(.simTarg & length(.target) > 1){
    times <- dplyr::left_join(times, .target, by = "id")
  } else {
    times$target <- .target
  }
  if(nrow(times)>0){
    times <- times %>%
      mutate(above = interval * (out>target)) %>%
      mutate(cross = above - dplyr::lag(above, n = 1))
    
    crossing_rows <- which(times$cross == 0.5 | times$cross == -0.5)
    if(length(crossing_rows)>0){
      crossing1 <- times[c(crossing_rows-1, crossing_rows),] %>% 
        arrange(id, time) %>%
        ungroup() %>%
        mutate(pair = rep(seq_along(1:(0.5*n())),each = 2)) %>%
        group_by(pair) 
      
      crossing2 <- crossing1 %>%
        summarize( above = interval / (out[2] - out[1]) *
                     (target[1] - out[1]))
      
      crossing3 <- crossing1 %>%
        filter(above == interval) %>%
        ungroup() %>%
        mutate(above = crossing2$above)
      
      times2 <- times %>%
        filter(is.na(cross) | cross == 0)
      
      pdi <- bind_rows(times2, crossing3) %>%
        group_by(id) %>%
        summarize(sum = sum(above[-1])/24) %>%
        pull(sum)
    } else {
      pdi <- times %>% 
        group_by(id) %>%
        summarize(sum = sum(above[-1])/24) %>%
        pull(sum)
    }
    pdi[pdi>1] <- 1 #in case of rounding errors
  } else {
    pdi <- NA #filtered to 0 rows
  }
  
  return(pdi)
}