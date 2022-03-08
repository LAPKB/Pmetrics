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
PM_fit <- R6Class("PM_fit",
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
        model <- PM_model(model, ...)
      }
      stopifnot(inherits(data, "PM_data"))
      stopifnot(inherits(model, "PM_Vmodel"))
      private$data <- data
      private$model <- model
    },

    #' @description Fit the model to the data
    #' @param engine Currently only npag.
    #' @param \dots Other arguments passed to \code{\link{NPrun}}

    run = function(..., engine = "npag") {
      if (inherits(private$model, "PM_model_legacy")) {
        cat(sprintf("Runing Legacy"))
        Pmetrics::NPrun(private$model$legacy_file_path, private$data$data, ...)
      } else if (inherits(private$model, "PM_model_list")) {
        engine <- tolower(engine)
        model_path <- private$model$write_model_file(engine = engine)
        cat(sprintf("Creating model file at: %s\n", model_path))
        Pmetrics::NPrun(model_path, private$data$data, ...)
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
    check = function() {
      if (inherits(private$model, "PM_model_legacy")) {
        cat(sprintf("Runing Legacy\n"))
        Pmetrics::PMcheck(private$data$data, private$model$legacy_file_path)
      }
    }
  ),
  private = list(
    data = NULL,
    model = NULL,
    engine = "NPAG" # eventually should be public
  )
)




