#' Pmetrics validation object
#' 
#' This object is created by running the \code{make_valid} method in a 
#' \code{\link{PM_result}} object. It contains all the information necessary
#' to internally validate the result by simulation methods. 
#' @seealso \code{\link{PM_result}}, \code{\link{makeValid}}
#' @export
PM_valid <- R6Class("PM_valid",
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
        },
        #' @description 
        #' Plot method. Calls \code{\link{plot.PMvalid}}.
        #' @param ... Arguments to pass to \code{\link{plot.PMvalid}}.
        plot = function(...) {
            plot.PMvalid(self, ...)
        }
    )
)