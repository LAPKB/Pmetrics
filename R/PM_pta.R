#' @export
PM_pta <- R6Class(
    "PM_pta",
    public <- list(
        results = NULL,
        outcome = NULL,
        initialize = function(simdata, targets, target.type, ...) {
            pta <- makePTA(simdata, targets, target.type, ...)
            self$results <- pta$results
            self$outcome <- pta$outcome
        },
        #' @description
        #' Save the current PM_pta object into a .rds file.
        #' @param file_name Name of the file to be created, the default is PMpta.rds
        save = function(file_name = "PMpta.rds") {
            saveRDS(self, file_name)
        },
        summary = function(...) {
            summary.PMpta(self, ...)
        },
        plot = function(...) {
            plot.PMpta(self, ...)
        }
    )
)

#' @description
#' Returns a PM_pta object based on the information found in a specified rds file.
#' @param file_name Name of the file to be read, the default is PMpta.rds
PM_pta$load <- function(file_name = "PMpta.rds") {
    readRDS(file_name)
}

#' @export
summary.PM_pta <- function(obj, ...) {
    obj$summary(...)
}