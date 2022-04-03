#' Object to define and run models/data in Pmetrics
#' 
#' @description 
#' PM_fit objects comprise a PM_data and PM_model object ready for analysis
#' 
#' @details 
#' Data and model objects can be previously created as PM_data or PM_fit objects,
#' or created on the fly when making a new PM_fit object. PM_fit objects contain
#' methods to cross-check data and model objects for compatibility, as well as to 
#' run the analysis.
#'
#' @export
PM_fit <- R6::R6Class("PM_fit",
  public = list(

    #' @description
    #' Create a new object
    #' @param data Either the name of a  \code{\link{PM_data}}
    #' object in memory or the quoted name of a Pmetrics
    #' data file in the current working directory, which will crate a \code{PM_data}
    #' object on the fly. However, if created on the fly, this object 
    #' will not be available to other
    #' methods or other instances of \code{PM_fit}.
    #' @param model Similarly, this is either the name of a \code{\link{PM_model}}
    #' object in memory or the quoted name of a Pmetrics text model file
    #' in the current working directory. Again, if created on the fly,
    #' the object will not be available to other
    #' methods or other instances of \code{PM_fit}.
    #' @param ... Other parameters passed to \code{\link{PM_model}} if created
    #' from a filename
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
    #' @param ... Other arguments passed to \code{\link{NPrun}}
    #' @param engine Currently only \dQuote{npag}.
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
    #' Save the current PM_fit object to a .rds file.
    #' @param file_name Name of the file to be created. The
    #' default is \dQuote{PMfit.rds}.
    save = function(file_name = "PMfit.rds") {
      saveRDS(self, file_name)
    },
    #' @description 
    #' Checks for errors in data and model objects and agreement between them.
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


#' Load a PM_fit from a previously saved rds file.
#' 
#' This function loads an rds file created using the \code{$save} method on a 
#' \code{PM_fit} object.
#' @param file_name Name of the file to be read, the default is \dQuote{PMfit.rds}.
#' @return A \code{\link{PM_fit}} object.
#' @export
#' 
PM_fit$load <- function(file_name = "PMfit.rds") {
  readRDS(file_name)
}