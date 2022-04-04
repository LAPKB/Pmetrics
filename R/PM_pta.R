#' Define object for PTA
#' 
#' This object class contains results of simulations and a probability of
#' target attainment analysis.
#' 
#' Creating this object first requires a simulation of appropriate regimens
#' from a model using the `$sim` method for a [PM_result] object. That simulation
#' result is passed to the [makePTA] function when a new `PM_pta` is created.
#' 
#' @author Julian Otalvaro and Michael Neely
#' @export
PM_pta <- R6Class(
    "PM_pta",
    public <- list(
        #' @field results Contains the raw results. See [makePTA].
        results = NULL,
        #' @field outcome Contains summary of raw results. See [makePTA].
        outcome = NULL,
        #' @description 
        #' Create a new `PM_pta` object
        #' @param simdata Output of `$sim` method for [PM_result] object 
        #' @param targets See [makePTA].
        #' @param target.type See [makePTA].
        #' @param ... Other arguments passed to [makePTA].
        initialize = function(simdata, targets, target.type, ...) {
            pta <- makePTA(simdata, targets, target.type, ...)
            self$results <- pta$results
            self$outcome <- pta$outcome
            attr(self, "simlabels") <- attr(pta, "simlabels")
            attr(self, "simTarg") <- attr(pta, "simTarg")
            attr(self, "success") <- attr(pta, "success")
            attr(self, "type") <- attr(pta, "type")
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
        #' Plot the `PM_pta` object. See [plot.PMpta].
        #' @param ... Arguments passed to [plot.PMpta]
        plot = function(...) {
            plot.PMpta(self, ...)
        },
        #' @description
        #' Returns a PM_pta object based on the information found in a specified rds file.
        #' @param file_name Name of the file to be read, the default is PMpta.rds
        load = function(file_name){ #dummy function
          return(invisible())
        }
    )
)

#' @export
PM_pta$load <- function(file_name = "PMpta.rds") {
    readRDS(file_name)
}

summary.PM_pta <- function(obj, ...) {
    obj$summary(...)
}