
#' @title 
#' Run a nonparmetric population model
#' @description
#' This function runs a nonparametric population model using the provided data and model object.
#' @details
#' This is a wrapper around the current method of fitting models to data in Pmetrics,
#' available to maintain backwards compatibility with versions of Pmetrics prior to 3.0.0.
#' @param data A [PM_data()] object, the character name of a data .csv file, or an 
#' appropriate data frame that can be coerced into a [PM_data()] object.
#' @param model A [PM_model()] object, or a list that can be coerced into one.
#' @param ... Additional arguments passed to the `$fit` method in [PM_model()].
#' @return Unlike older versions of Pmetrics, this wrapper function will return
#' a [PM_result()] directly.
#' @examples
#' \dontrun{
#' run1 <- NPrun(data = dataEx, model = modEx, cycles = 10)
#' run1$op$plot()
#' }
#' @author Michael Neely
#' @export
#' @keywords internal
#' 
NPrun <- function(data, model, ...){
  
  if(!inherits(model, "PM_model")){
    mod <- tryCatch({
      PM_model$new(model)
      error = function(e) {
        cli::cli_abort("The model must be an instance of PM_model or a valid model object.")
      }
    })
  } else {
    mod <- model
  }
  
  run <- mod$fit(data = data,...)
  return(invisible(run))
}