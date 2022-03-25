# library(R6)
# library(JuliaCall)
# j <- julia_setup()
# julia_library("npag")
# j$library("npag")
# public classes

# may need to separate out error model as separate, e.g. need data, model, error to run

# PM_fit ------------------------------------------------------------------


#' Object to define and run models/data in Pmetrics
#'
#' @export
PM_fit <- R6::R6Class("PM_fit",
  public = list(

    #' @description
    #' Create a new object
    #' @param data Either a PMmatrix object loaded via
    #' \code{\link{PMreadMatrix}} or the quoted name of a Pmetrics
    #' data file in the current working directory
    #' @param model Either the name of a \code{\link{PM_model}}
    #' object or the quoted name of a Pmetrics text model file
    #' in the current working directory
    initialize = function(data = data, model = model, ...) {
      if (is.character(data)) {
        data <- PM_data$new(data)
      }
      if (is.character(model)) {
        model <- PM_model$new(model, ...)
      }
      stopifnot(inherits(data, "PM_data"))
      stopifnot(inherits(model, "PM_model"))
      private$data <- data
      private$model <- model
    },

    #' @description Fit the model to the data
    #' @param engine Currently only npag.
    #' @param \dots Other arguments passed to \code{\link{NPrun}}

    run = function(..., engine = "npag") {
      if (inherits(private$model, "PM_model_legacy")) {
        cat(sprintf("Runing Legacy"))
        Pmetrics::NPrun(private$model$legacy_file_path, private$data$standard_data, ...)
      } else if (inherits(private$model, "PM_model_list")) {
        engine <- tolower(engine)
        model_path <- private$model$write_model_file(engine = engine)
        cat(sprintf("Creating model file at: %s\n", model_path))
        Pmetrics::NPrun(model_path, private$data$standard_data, ...)
      } else if (inherits(private$model, "PM_model_julia")) {
        cat(sprintf("Runing Julia: %s-%s\n", private$data, private$model$name))
        return(
          julia_call(
            "npag.run",
            private$model$model_function,
            private$data,
            private$model$min,
            private$model$max,
            private$model$error[1],
            private$model$error[2],
            private$model$error[3],
            private$model$n_theta0
          )
        )
      }
    },
    #' @description
    #' Save the current PM_fit object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMfit.rds
    save = function(file_name = "PMfit.rds") {
      saveRDS(self, file_name)
    },
    check = function() {
      if (inherits(private$model, "PM_model_list")) {
        cat(sprintf("Checking...\n"))
        file_name <- random_name()
        private$model$write_model_file(file_name)
        Pmetrics::PMcheck(private$data$standard_data, file_name)
        system(sprintf("rm %s", file_name))
      }
    }
  ),
  private = list(
    data = NULL,
    model = NULL,
    engine = "NPAG" # eventually should be public
  )
)

#' @description
#' Returns a PM_fit object based on the information found in a specified rds file.
#' @param file_name Name of the file to be read, the default is PMfit.rds
PM_fit$load <- function(file_name = "PMfit.rds") {
  readRDS(file_name)
}