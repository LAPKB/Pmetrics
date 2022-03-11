# PM_result ---------------------------------------------------------------

#' R6 object containing the results of a Pmetrics run
#'
PM_result <- R6::R6Class(
  "PM_result",
  public <- list(
    #' @field npdata List with all output from NPAG
    npdata = NULL,
    #' @field pop  NPAG only: Population predictions for each output equation
    pop = NULL,
    #' @field post  NPAG only: Individual posterior predictions for each output equation
    post = NULL,
    #' @field final Final cycle population support points and parameter summary statistics
    final = NULL,
    #' @field cycle Cycle log-likelihood, AIC, BIC, Gamma/lambda, and normalized parameter means, medians and SDs
    cycle = NULL,
    #' @field op List of observed vs. population and posterior predicted plots for each output equation
    op = NULL,
    #' @field cov Data frame of subject ID, covariate values, and Bayesian posterior parameter estimates
    cov = NULL,
    #' @field data \link{PM_data} object representing the original .csv data file used in the run
    data = NULL,
    #' @field model text string representing the original model file used in the run
    model = NULL,
    #' @field errfile Name of error file if it exists
    errfile = NULL,
    #' @field success Boolean if successful run
    success = NULL,
    #' @field valid If \code{\link{makeValid}} has been executed after a run, this object will be added to
    #' the save data.  It contains the information required to plot visual predictive checks and normalized prediction
    #' error discrepancies via the npde code developed by Comets et al
    valid = NULL,

    #' @description
    #' Create new object populated with data from previous run
    #' @details
    #' Creation of new \code{PM_result} objects is via
    #' \code{\link{PM_load}}
    #' @param out The parsed output from \code{\link{PM_load}}


    initialize = function(out) {
      self$npdata <- out$NPdata
      self$pop <- out$pop
      self$post <- out$post
      self$final <- result_block$new(out$final, "final")
      self$cycle <- result_block$new(out$cycle, "cycle")
      self$op <- result_block$new(out$op, "op")
      self$cov <- result_block$new(out$cov, "cov")
      self$data <- PM_data$new(data = out$data)
      self$model <- out$model
      self$errfile <- out$errfile
      self$success <- out$success
    },

    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param \dots Plot-specific arguments

    plot = function(type, ...) {
      self[[type]]$plot(...)
    },

    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param \dots Summary-specific arguments

    summary = function(type, ...) {
      self[[type]]$summary(...)
    }
  ) # end public
) # end PM_result

result_block <- R6Class(
  "result_block",
  public <- list(
    data = NULL,
    type = NULL,
    initialize = function(data, type) {
      stopifnot(type %in% c("op", "cov", "cycle", "final"))
      self$type <- type
      self$data <- data
    },
    plot = function(...) {
      if (self$type == "op") {
        plot.PMfit(self$data, ...)
      } else if (self$type == "cov") {
        plot.PMcov(self$data, ...)
      } else if (self$type == "cycle") {
        plot.PMcycle(self$data, ...)
      } else if (self$type == "final") {
        plot.PMfinal(self$data, ...)
      }
    },
    summary = function(...) {
      if (self$type == "op") {
        summary.PMop(self$data, ...)
      } else if (self$type == "cov") {
        summary.PMcov(self$data, ...)
      } else if (self$type == "final") {
        summary.PMfinal(self$data, ...)
      }
    }
  )
)