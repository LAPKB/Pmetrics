
target <- list(2^(-2:6), 1, 50)
target <- list(2^(-2:6))
target_type <- c("time", 144, "-max")
target_type <- "time"
sim_num <- 1:4
success <- c(0.6, 1, 1)
success <- c(0.6)
simlabels <- c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid")
simdata <- simEx

pta1 <- makePTA2(simEx, 
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = list(2^(-2:6), 1, 50),
                 target_type = c("time", 144, "-max"),
                 success = c(0.6, 1, 1),
                 start = 120, end = 144)

pta2 <- makePTA2(simEx,
                 target = c(2^(-2:6)),
                 target_type = "time",
                 success = 0.6,
                 start = 120, end = 144)

pta3 <- makePTA2(simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = list(5,10),
                 target_type = c("min", "-min"),
                 success = c(1,1),
                 start = 120, end = 144)

pta4 <- makePTA2(simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = makePTAtarget(mic1),
                 target_type = "auc",
                 success = 200,
                 start = 120, end = 144)

pta5 <- makePTA2(simdata = simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = list(makePTAtarget(mic1),5),
                 target_type = c("auc","min"),
                 success = c(200,1),
                 start = 120, end = 144)


makePTA2 <- function(simdata, simlabels, target, target_type, success, outeq = 1,
                     free_fraction = 1, start = 0, end = Inf, icen = "median", block = 1){
  
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
    
    if (dataType==1) { #PMsim object
      simdata <- list(simdata)
    }
    
    if(dataType == 2){ #PM_simlist
      simdata <- purrr::map(simdata$data, \(x) x$data) #extract data
    }
    
    #nothing to do for dataType=3 already in right format
    
    if (dataType==4) { #character vector of simulator output files
      simfiles <- Sys.glob(simdata)
      if (length(simfiles) == 0) 
        stop("There are no files matching \"", simdata, "\".\n", sep = "")
      simdata <- list()
      for (i in 1:length(simfiles)) {
        simdata[[i]] <- tryCatch(SIMparse(simfiles[i]), error = function(e) stop(paste(simfiles[i], "is not a PMsim object.\n")))
      }
    }
    
    if(dataType==5){  #PMpost object
      simdata <- simdata %>% filter(icen == !!icen & block == !!block)
      #simdata <- simdata[simdata$icen==icen & simdata$block==block,]
      temp <- list(obs=data.frame(id=simdata$id,time=simdata$time,out=simdata$pred,outeq=simdata$outeq))
      simdata <- list(temp)
    }
    
    if(dataType == 6 | dataType == 8){  #PMmatrix or PM_data object
      if(dataType == 8){
        simdata <- simdata$data
      }
      simdata <- makePMmatrixBlock(simdata)
      simdata <- simdata %>% filter(evid==0 & block== !!block)
      temp <- list(obs=data.frame(id=simdata$id,time=simdata$time,out=simdata$out,outeq=simdata$outeq))
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
  invalid_types <- purrr::map_lgl(target_type, \(x){
    !x %in% c("time", "auc", "max", "peak", "min", "-max", "-peak", "-min") &&
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
  if (success[1] <= 0 | (target_type[1] == "time" & success[1] > 100)) stop("Invalid success threshold value. Aborting.", call.=F)
  if (target_type[1] =="time" & success[1] > 1 & success[1] <= 100) {
    cat("Your specified success threshold for time above target of ", success[1], " is bigger than 1.", sep="")
    ans <- readline(cat("\nWhat would you like to do?\n1) set success to ",success[1]/100," (i.e. ",success[1],"% of time above target)\n2) end ", sep=""))
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
