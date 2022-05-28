#' Object to contain results of simulation
#' 
#' To do
#' 
#' To do
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
    #' @description Create new simulation objects with results of `$sim`
    #' method for [PM_result]
    #' @param list List of output passed by `$sim`.
    initialize = function(list) {
      self$obs <- list$obs
      self$amt <- list$amt
      self$parValues <- list$parValues
      self$totalMeans <- list$totalMeans
      self$totalCov <- list$totalCov
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
      plot.PMsim(self, ...)
    },
    #' @description
    #' Estimates the Probability of Target Attaintment (PTA), based on the results of the current Simulation.
    #' @param targets A vector of pharmacodynamic targets, such as Minimum Inhibitory Concentrations (MICs), e.g. c(0.25, 0.5,1,2,4,8,16,32).
    #' This can also be a sampled distribution using  \code{\link{makePTAtarget}}.
    #' @param target.type A numeric or character vector, length 1.  If numeric, must correspond to an observation time common to all PMsim objects in
    #' \code{simdata}, rounded to the nearest hour.  In this case, the target statistic will be the ratio of observation at time \code{target.type} to target.  This enables
    #' testing of a specific timed concentration (e.g. one hour after a dose or C1) which may be called a peak, but is not actually the maximum drug
    #' concentration.  Be sure that the time in the simulated data is used, e.g. 122 after a dose given at 120.  Character values may be one of
    #' \dQuote{time}, \dQuote{auc}, \dQuote{peak}, or \dQuote{min}, for, respectively, percent time above target within the time range
    #' specified by \code{start} and \code{end}, ratio of area under the curve within the time range to target, ratio of peak concentration within the time range
    #' to target, or ratio of minimum concentration within the time range to target.
    #'
    #' @param \dots Additional parameters, refer to \code{\link{makePTA}}
    pta = function(targets, target.type, ...) {
      PM_pta$new(self,
                 targets = targets,
                 target.type = target.type,
                 ...
      )
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
PM_sim$run <- function(poppar, ...) {
  dots <- list(...)
  combine <- if (exists("combine", where = dots)) {
    dots$combine
  } else {
    F
  }    
  SIMrun(poppar, ...)
  
  # TODO: read files and fix the missing E problem
  sim_files <- list.files() %>% .[grepl("simout*", .)]
  if (length(sim_files) == 1) {
    parse <- SIMparse(file = "simout*") %>% PM_sim$new()
  } else {
    if (combine) {
      parse <- SIMparse(file = "simout*", combine = T) %>% PM_sim$new()
    } else {
      parse <- list()
      for (i in seq_len(length(sim_files))) {
        parse <- append(parse, SIMparse(file = sprintf("simout%i.txt", i)) %>% PM_sim$new())
      }
    }
  }
  system("rm simout*")
  parse
}