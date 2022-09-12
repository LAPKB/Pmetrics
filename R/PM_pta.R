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
PM_pta <- R6::R6Class(
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
        #' Plot the `PM_pta` object. See [plot.PM_pta].
        #' @param ... Arguments passed to [plot.PM_pta]
        plot = function(...) {
            plot.PM_pta(self, ...)
        },
        #' @description
        #' Returns a PM_pta object based on the information found in a specified rds file.
        #' @param file_name Name of the file to be read, the default is PMpta.rds
        load = function(file_name){ #dummy function
          return(invisible())
        }
    )
)

#' Read results of previously saved PTA analyses.
#' 
#' If the `$save` method has previously been invoked on a [PM_pta] 
#' object, this function will load those results.
#' 
#' The saved object is an .rds file. When loaded, it should be assigned to an R
#' object, e.g. `pta1 <- PM_pta$load("filename")`.
#' @param file_name The name of the .rds file to load.
#' @return A [PM_pta] object
#' @export
#' @name PM_pta
PM_pta$load <- function(file_name = "PMpta.rds") {
    readRDS(file_name)
}

#' Wrapper function for summmary.PMpta
#' 
#' This redirects to summary.PMpta for PM_pta R6 objects
#' 
#' See [summary.PMpta]. Alternative way to summarize is
#' `PM_pta$summary()`.
#' 
#' @param obj The *PM_pta* object to summarize
#' @param ... Arguments passed to [summary.PMpta]
#' @return A [summary.PMpta] object
#' @export
summary.PM_pta <- function(obj, ...) {
    obj$summary(...)
}