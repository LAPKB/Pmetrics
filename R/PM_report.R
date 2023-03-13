#' Generates a report from a specified Rmd template
#' 
#' @title Generate a report
#' @param PM_result A `PM_result` object obtained from \code{PM_load(x)}, where `x` is the run number.
#' @param template The path to a template. If not specified, uses the default Pmetrics report template.
#' Pmetrics will prompt the user to set this address the first time the \code{remote} argument is set to \code{TRUE}
#' in \code{\link{NPrun}}. 
#' @import pander
#' @import DT
#' @import patchwork
#' @import rmarkdown
#' @return Generates an HTML-report in the current working directory.
#' @author Markus Hovd and Julian Otalvaro
#' @seealso \code{\link{PM_load}}
#' @export

PM_report <- function(PM_result, template) {
  
  if (!is(PM_result, "PM_result")) {
    stop("This function expects a valid PM_result object from PM_load\n")
  }

  if (missing(template)) {
    template = system.file("report/templates/default.Rmd", package = "Pmetrics")
  }
  
  rmarkdown::render(
    input = template, 
    params = list(res = PM_result),
    clean = TRUE,
    quiet = TRUE
  )
}