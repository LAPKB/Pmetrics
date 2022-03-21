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
        initialize = function(poppar, ...){
            SIMrun(poppar, ...)
            # TODO: read files and fix the missing E problem
            parse<-SIMparse(file = "sim*.txt") 
            # TODO: remove all sim* files
            self$obs <- parse$obs
            self$amt <- parse$amt
            self$parValues <- parse$parValues
            self$totalMeans <- parse$totalMeans
            self$totalCov <- parse$totalCov
            
            
        },
        save = function(file_name="PMsim.rds"){
            saveRDS(self,file_name)
        },
        load = function(file_name="PMsim.rds"){
            readRDS(file_name)
        },
        pta = function(targets, target.type,...){
            PM_pta$new(self,
                    targets=targets,
                    target.type=target.type,
                    ...)
        }
    )
)