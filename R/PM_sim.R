#' Object to contain results of simulation
#'
#' This object is created after a successful run of the simulator.
#'
#' There are two methods of creating a PM_sim object.
#' * [PM_result$sim]
#' * [PM_sim$run]
#'
#' These both call [SIMrun] to execute the simulation and [SIMparse] to process
#' the results and return the PM_sim objects. See help on both of these functions
#' for further details.
#'
#' @export
PM_sim <- R6::R6Class(
  "PM_sim",
  public <- list(
    #' @field obs Observations
    obs = NULL,
    #' @field amt Amounts
    amt = NULL,
    #' @field parValues Retained simulated parameter values after discarding
    #' any due to truncation limits
    parValues = NULL,
    #' @field totalSets Number of all simulated parameter values
    totalSets = NULL,
    #' @field totalMeans Mean of all simulated parameter values
    totalMeans = NULL,
    #' @field totalCov Covariance of all simulated parameter values
    totalCov = NULL,
    #' @field data A matrix that contains all the above elements as columns
    data = NULL,
    #' @description Create new simulation objects with results of `$sim`
    #' method for [PM_result]
    #' @usage PM_sim$run
    #' @param list List of output passed by `$sim`.
    initialize = function(list) {
      self$obs <- list$obs
      self$amt <- list$amt
      self$parValues <- list$parValues
      self$totalMeans <- list$totalMeans
      self$totalCov <- list$totalCov
      self$data <- list
    },
    #' @description
    #' Save the current PM_sim object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMsim.rds
    save = function(file_name = "PMsim.rds") {
      saveRDS(self, file_name)
    },
    #' @description
    #' Plot `PM_sim` object.
    #' @param ... Arguments passed to [plot.PMsim].
    plot = function(...) {
      plot.PM_sim(self, ...)
    },
    #' @description
    #' Estimates the Probability of Target Attaintment (PTA), based on the results
    #' of the current Simulation.
    #' @param ... Additional parameters, refer to [makePTA]
    pta = function(...) {
      PM_pta$new(self,...)
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC]
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self, ...)
    },
    #' @description
    #' Summarize simulation
    #' @details
    #' Choose whether to sim
    #' @param field Quoted character value, one of
    #' * obs for simulated observations
    #' * amt for simulated amounts in each compartment
    #' * parValues for simulated parameter values
    #' @param by Optional quoted column name(s) to group by, e.g. `by = "id"` or 
    #' `by = c("id", "outeq")`.
    #' @param individual If "id" is included within `by`, the summary statistics
    #' will be calculated for each individual, i.e. each individual's mean, sd, median,
    #' min and max observations. If `individual` is `FALSE` (the default), then those 
    #' summaries will again be summarized to present, for example, the mean of
    #' all subjects' mean observations, or the mean of their maximum observations. 
    #' This argument will be ignored if "id" is not in `by`.
    #' @return If `by` is ommitted, a data frame with rows for each data element except ID,
    #' and columns labeled as mean, sd, median, min and max. If `by` is specified,
    #' return will be a list with named elements mean, sd, median, min and max, each containing 
    #' the corresponding value for each group in `by`.
    summary = function(field, by, individual = FALSE) {
      summaries <- c("mean", "sd", "median", "min", "max")
      dat <- self[[field]] 
      if(!missing(by)){
        summ <- purrr::map(summaries, \(x) dplyr::summarize(dat, 
                                                            dplyr::across(everything(),!!!rlang::syms(x)),
                                                            .by = by
        )
        ) 
        names(summ) <- summaries
        if("id" %in% by){
          if(!individual){
            #group by individual, then summarize
            by <- by %>% purrr::discard(\(x) x == "id") #remove id
            if(length(by)==0) {by <- NULL} #set to NULL if character(0)
            summ <- purrr::map(summ, \(x){
              purrr::map_df(summaries, \(y) {
                dplyr::summarize(x, 
                                 dplyr::across(everything(),!!!rlang::syms(y)),
                                 .by = by) %>%
                  select(-id) }) %>%
                t() %>% as.data.frame() %>% rename_with(~summaries)
            })
          }
        } else {
          #id is not in grouping, so just remove id column
          summ <- purrr::map(summ, \(x) x %>% select(-id))
        }
      } else { #no grouping
        summ <- purrr::map_df(summaries, \(x) summarize(dat, across(everything(),!!!syms(x)))) 
        if("id" %in% names(summ)){
          summ <- summ %>% select(-id)
        }
        summ <- summ %>% t() %>% as.data.frame()
        names(summ) <- summaries
      }
      
      return(summ)
    }
    
  )
)

#' Read results of previously saved simulation.
#'
#' If the `$save` method has previously been invoked on a [PM_sim]
#' object, this function will load those results.
#'
#' The saved object is an .rds file. When loaded, it should be assigned to an R
#' object, e.g. `sim1 <- PM_sim$load("filename")`.
#' @param file_name The name of the .rds file to load.
#' @return A [PM_sim] object
#' @export
#' @name PM_sim
PM_sim$load <- function(file_name = "PMsim.rds") {
  readRDS(file_name)
}

#' Wrapper function for SIMrun in R6
#'
#' Provides an alternative method to call the simulator directly from output
#' of a model fitting run.
#'
#' Calling this function is equivalent to `PM_result$sim()`.
#'
#' @param poppar A population parameter result, which is a PM_final object. This
#' can be found in `PM_result$final$data`.
#' @param ... Additional parameters to be passed to [SIMrun] and optionally,
#' "combine = T" as an argument will be passed to [SIMparse].
#' @return A `PM_sim` object created by calling [SIMparse] at the completion of the
#' simulation.
#' @export
#' @name PM_sim
PM_sim$run <- function(poppar, ...) {
  dots <- list(...)
  combine <- if (exists("combine", where = dots)) {
    dots$combine
  } else {
    FALSE
  }
  system("echo 347 > SEEDTO.MON") # TODO: look to fix the simulator without this
  SIMrun(poppar, ...)
  
  # TODO: read files and fix the missing E problem
  sim_files <- list.files() %>% .[grepl("simout*", .)]
  if (length(sim_files) == 1) {
    parseRes <- SIMparse(file = "simout*") %>% PM_sim$new()
  } else {
    if (combine) {
      parseRes <- SIMparse(file = "simout*", combine = T) %>% PM_sim$new()
    } else {
      parseRes <- list()
      for (i in seq_len(length(sim_files))) {
        parseRes <- append(parseRes, SIMparse(file = sprintf("simout%i.txt", i)) %>% PM_sim$new())
      }
      parseRes <- PM_simlist$new(parseRes) # Returns a PM_simlist object
    }
  }
  system("rm simout*")
  return(parseRes)
}

#' Object to contain list of results of simulation
#'
#' This object is created after a successful run of the simulator when
#' there are multiple subjects in the data template and `combine = F` is
#' used as an argument to `PM_sim$run`, which is the default.
#'
#' @export

PM_simlist <- R6::R6Class(
  "PM_simlist",
  public <- list(
    #' @field data List of all the individual [PM_sim] objects
    data = NULL,
    #' @description Create new PM_simlist.
    #' @param data The list of PM_sim objects.
    initialize = function(data) {
      self$data <- data
    },
    #' @description
    #' Plot `PM_sim` object.
    #' @param at Index of the PM_sim object to be plotted.
    #' @param ... Arguments passed to [plot.PM_sim].
    plot = function(at = 1, ...) {
      if (at > length(self$data)) {
        stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
      }
      self$data[[at]]$plot(...)
    },
    #' @description
    #' Estimates the Probability of Target Attaintment (PTA), based on the results
    #' of the specified Simulation.d.
    #' @param ... Additional parameters, refer to [makePTA]
    pta = function(...) {
      PM_pta$new(self,...)
    },
    #' @description
    #' Calculates the AUC of the specified simulation
    #' @param at Index of the PM_sim object to be plotted.
    #' @param ... Arguments passed to [makeAUC].
    auc = function(at = 1, ...) {
      if (at > length(self$data)) {
        stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
      }
      self$data[[at]]$auc(...)
    },
    #' @description
    #' Save the current PM_sim object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMsim.rds
    save = function(file_name = "PMsim.rds") {
      saveRDS(self, file_name)
    },
    #' @description
    #' Summarizes  the specified simulation
    #' @param at Index of the PM_sim object to be summarized
    #' @param ... Arguments passed to the `$summary` method for [PM_sim] objects.
    summary = function(at = 1, ...) {
      if (at > length(self$data)) {
        stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
      }
      self$data[[at]]$summary(...)
    }
    
    
  )
)
