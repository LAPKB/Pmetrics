#' @title Create Probability of Target Attainment (PTA) object
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object class contains results of simulations and a probability of
#' target attainment analysis.
#'
#' @details
#' #' There are two ways of creating a *PM_pta* object.
#'
#' * **PM_sim$pta()** This way uses the simulation method directly from
#' a [PM_sim] object.
#' * **PM_pta$new()** This way takes an external [PM_sim] result as an argument
#' and creates the PTA. It is described here.
#'
#' Both methods require the prior creation of a simulation of
#' appropriate regimens. They use [makePTA] to create the *PM_pta* object.
#'
#' @author Julian Otalvaro and Michael Neely
#' @export
PM_pta <- R6::R6Class(
  "PM_pta",
  public <- list(
    #' @field results Contains the raw results. See [makePTA].
    results = NULL,
    #' @description 
    #' Create a new `PM_pta` object.
    #' @param simdata One of two possibilities:
    #' * A [PM_sim] object, which typically will contain the results of
    #' multiple simulations OR
    #' * The quoted filename of a previously saved .rds file
    #' (saved with the `$save` method below) in the current
    #' working directory (or the path plus filename) with results of a prior
    #' PTA analysis.
    #' @param target See [makePTA].
    #' @param target_type See [makePTA].
    #' @param success See [makePTA]
    #' @param ... Other arguments passed to [makePTA].
    initialize = function(simdata, target, target_type, success, ...) {
      if(!inherits(simdata, "character")){
        pta <- makePTA(simdata = simdata, target = target, 
                       target_type = target_type, success = success, ...)
        private$populate(pta)
      } else { # try as filename
        pta <- readRDS(simdata)
        private$populate(pta)
      }
    },
    #' @description
    #' Save the current PM_pta object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMpta.rds
    save = function(file_name = "PMpta.rds") {
      saveRDS(self, file_name)
    },
    #' @description
    #' Summarize the `PM_pta` object. See [summary.PMpta].
    #' @param ... Arguments passed to [summary.PMpta]
    summary = function(...) {
      summary.PMpta(self, ...)
    },
    #' @description
    #' Plot the `PM_pta` object. See [plot.PM_pta].
    #' @param ... Arguments passed to [plot.PM_pta]
    plot = function(...) {
      plot.PM_pta(self, ...)
    },
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load a prior PTA Replaced by `PM_pta$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_pta$load()", details = "Please use PM_pta$new() instead. ?PM_pta for details.")
    }
  ), # end public
  private = list(
    populate = function(pta){
      self$results <- pta
      return(self)
    }
  ) # end private
)

#' @keywords internal
#' @name PM_pta
#' @export
PM_pta$load <- function(file_name = "PMpta.rds") {
  lifecycle::deprecate_warn("2.1.0", "PM_pta$load()", details = "Please use PM_pta$new() instead. ?PM_pta for details.")
}

#' @title Summarize PM_pta
#' @description
#' `r lifecycle::badge("stable")
#'
#' Wrapper function for summmary.PMpta
#'
#' @details
#' This redirects to [summary.PMpta] for PM_pta R6 objects
#' The R6 method to summarize is `PM_pta$summary()`.
#'
#' @param object The *PM_pta* object to summarize
#' @param ... Arguments passed to [summary.PMpta]
#' @return A [summary.PMpta] object
#' @export
summary.PM_pta <- function(object, ...) {
  object$summary(...)
}
