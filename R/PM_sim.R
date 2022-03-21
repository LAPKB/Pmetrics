#' @export
PM_sim <- R6Class(
    "PM_sim",
    public <- list(
        obs = NULL,
        amt = NULL,
        parValues = NULL,
        totalSets = NULL,
        totalMeans = NULL,
        totalCov = NULL,
        initialize = function(poppar, ...) {
            SIMrun(poppar, ...)
            # TODO: read files and fix the missing E problem
            parse <- SIMparse(file = "sim*.txt")
            # TODO: remove all sim* files
            self$obs <- parse$obs
            self$amt <- parse$amt
            self$parValues <- parse$parValues
            self$totalMeans <- parse$totalMeans
            self$totalCov <- parse$totalCov
        },
        #' @description
        #' Save the current PM_sim object into a .rds file.
        #' @param file_name Name of the file to be created, the default is PMsim.rds
        save = function(file_name = "PMsim.rds") {
            saveRDS(self, file_name)
        },
        #' @description
        #' Returns a PM_sim object based on the information found in a specified rds file.
        #' @param file_name Name of the file to be read, the default is PMsim.rds
        load = function(file_name = "PMsim.rds") {
            readRDS(file_name)
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