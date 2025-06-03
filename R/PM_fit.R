# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


# R6 ----------------------------------------------------------------------


#' @title
#' Object to define and run a model and data in Pmetrics
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `PM_fit` objects comprise a `PM_data` and `PM_model` object ready for analysis
#'
#' @details
#' Data and model objects can be previously created as [PM_data] or [PM_model] objects,
#' or created on the fly when making a new PM_fit object. PM_fit objects contain
#' methods to cross-check data and model objects for compatibility, as well as to
#' run the analysis.
#' @importFrom stringr str_glue
#' @export

PM_fit <- R6::R6Class(
  "PM_fit",
  public = list(
    #' @field data [PM_data] object
    data = NULL,
    #' @field model [PM_model] object
    model = NULL,
    
    #' @description
    #' Create a new object
    #' @param data Either the name of a  [PM_data]
    #' object in memory or the quoted name of a Pmetrics
    #' data file in the current working directory, which will crate a [PM_data]
    #' object on the fly. However, if created on the fly, this object
    #' will not be available to other
    #' methods or other instances of [PM_fit].
    #' @param model Similarly, this is either the name of a [PM_model]
    #' object in memory or the quoted name of a Pmetrics text model file
    #' in the current working directory. Again, if created on the fly,
    #' the object will not be available to other
    #' methods or other instances of [PM_fit].
    initialize = function(data = data, model = model) {
      if (is.character(data)) {
        data <- PM_data$new(data, ...)
      }
      if (is.character(model)) {
        model <- PM_model$new(model, ...)
      }
      if (!inherits(data, "PM_data")) {
        cli::cli_abort(c("x" = "{.code data} must be a {.cls PM_data} object"))
      }
      if (!inherits(model, "PM_model")) {
        cli::cli_abort(c("x" = "{.code model} must be a {.cls PM_model} object"))
      }
      
      #### checks
      
      # covariates
      dataCov <- tolower(getCov(data)$covnames)
      modelCov <- tolower(model$model_list$covariates)
      if (length(modelCov) == 0) {
        modelCov <- NA
      }
      if (!all(is.na(dataCov)) && !all(is.na(modelCov))) { # if there are covariates
        if (!identical(dataCov, modelCov)) { # if not identical, abort
          msg <- glue::glue("Model covariates: {paste(modelCov, collapse = ', ')}; Data covariates: {paste(dataCov, collapse = ', ')}")
          cli::cli_abort(c(
            "x" = "Error: Covariates in data and model do not match.",
            "i" = msg
          ))
        }
      }
      
      # output equations
      
      if (!is.null(data$standard_data$outeq)) {
        dataOut <- max(data$standard_data$outeq, na.rm = TRUE)
      } else {
        dataOut <- 1
      }
      
      modelOut <- model$model_list$n_out
      if (dataOut != modelOut) {
        cli::cli_abort(c(
          "x" = "Error: Number of output equations in data and model do not match.",
          "i" = "Check the number of output equations in the data and model."
        ))
      }
      
      self$data <- data
      self$model <- model

      # check if compiled and if not, do so
      self$model$compile()
      
    },
    
    #' @description
    #' Run a fit of model to the data (deprecated)
    #' @details The `$run` method for `PM_fit` objects has been deprecated in 
    #' favor of the `$fit` method in [PM_model()].
    run = function(){
      cli::cli_warn(c(
        "!" = "The `$run` method for `PM_fit` objects has been deprecated.",
        "i" = "Use the `$fit` method in [PM_model()] instead."
      ))
      return(invisible(NULL))
    },

    #' @description
    #' Save the current PM_fit object to a .rds file.
    #' @param file_name Name of the file to be created. The
    #' default is "PMfit.rds".
    save = function(file_name = "PMfit.rds") {
      saveRDS(self, file_name)
    },
    
    #' @description
    #' `PM_fit` objects contain a `save` method which invokes [saveRDS] to write
    #' the object to the hard drive as an .rds file. This is the corresponding load
    #' function.
    #' @param file_name Name of the file to be read, the default is "PMfit.rds".
    #' @return A `PM_fit` object.
    load = function(file_name) {
      return(invisible())
    },
    #' @description
    #' Checks for errors in data and model objects and agreement between them.
    check = function() {
      if (inherits(self$model, "PM_model_list")) {
        cat(sprintf("Checking...\n"))
        file_name <- random_name()
        self$model$save(file_name)
        Pmetrics::PMcheck(self$data$standard_data, file_name)
        system(sprintf("rm %s", file_name))
      }
    }
  )
) # end PM_fit



#' @export
PM_fit$load <- function(file_name = "PMfit.rds") {
  readRDS(file_name)
}

.logical_to_rust <- function(bool) {
  if (!is.logical(bool)) {
    stop("This functions expects a logical value")
  }
  
  rust_logical <- ifelse(bool, "true", "false")
}
