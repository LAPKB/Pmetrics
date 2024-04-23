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
