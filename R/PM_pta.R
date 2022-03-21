#' @export
PM_pta <- R6Class(
    "PM_pta",
    public <- list(
        results = NULL,
        outcome = NULL,
        initialize = function(simdata, targets, target.type,...){
           pta<- makePTA(simdata, targets, target.type, ...)
           self$results <- pta$results
           self$outcome <- pta$outcome
            
        },
        save = function(file_name="PMpta.rds"){
            saveRDS(self,file_name)
        },
        load = function(file_name="PMpta.rds"){
            readRDS(file_name)
        }
    )
)
