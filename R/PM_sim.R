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
    #' @param targets A vector of pharmacodynamic targets, such as
    #' Minimum Inhibitory Concentrations (MICs), e.g. c(0.25, 0.5,1,2,4,8,16,32).
    #' This can also be a sampled distribution using  [makePTAtarget].
    #' @param target.type A numeric or character vector, length 1.  If numeric,
    #' must correspond to an observation time common to all PMsim objects in
    #' `simdata`, rounded to the nearest hour.  In this case, the target
    #' statistic will be the ratio of observation at time `target.type`
    #' to target.  This enables
    #' testing of a specific timed concentration (e.g. one hour after a dose
    #' or C1) which may be called a peak, but is not actually the maximum drug
    #' concentration.  Be sure that the time in the simulated data is used,
    #' e.g. 122 after a dose given at 120.  Character values may be one of
    #' "time", "auc", "peak", or "min", for,
    #' respectively, percent time above target within the time range
    #' specified by `start` and `end`, ratio of area under the curve
    #' within the time range to target, ratio of peak concentration within the time range
    #' to target, or ratio of minimum concentration within the time range to target.
    #'
    #' @param ... Additional parameters, refer to [makePTA]
    pta = function(targets, target.type, ...) {
      PM_pta$new(self,
        targets = targets,
        target.type = target.type,
        ...
      )
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC]
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self, ...)
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
    F
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
    #' of the specified Simulation.
    #' @param at Index of the PM_sim object to be plotted.
    #' @param ... Arguments passed to [PM_pta$new].
    pta = function(at = 1, ...) {
      if (at > length(self$data)) {
        stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
      }
      self$data[[at]]$pta(...)
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
    }
  )
)
