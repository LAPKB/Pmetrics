#' @export
PM_valid <- R6Class("PM_valid",
    public = list(
        simdata = NULL,
        timeBinMedian = NULL,
        tadBinMedian = NULL,
        opDF = NULL,
        npde = NULL,
        initialize = function(result, ...) {
            valRes <- make_valid(result = result, ...)

            self$simdata <- valRes$simdata
            self$timeBinMedian <- valRes$timeBinMedian
            self$tadBinMedian <- valRes$tadBinMedian
            self$opDF <- valRes$opDF
            self$npde <- valRes$npde
        },
        plot = function(...) {
            plot.PMvalid(self, ...)
        }
    )
)