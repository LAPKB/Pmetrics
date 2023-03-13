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

PM_report <- function(PM_result, template, outfile, show = TRUE) {
  
  if (!is(PM_result, "PM_result")) {
    stop("This function expects a valid PM_result object from PM_load\n")
  }

  if (missing(template)) {
    template = system.file("report/templates/default.Rmd", package = "Pmetrics")
  }
  
  if (missing(outfile)) {
    outfile = tempfile(fileext = ".html")
  }
  
  ### TEMPORARY LIMITATION FOR OUTEQ ###
  # Currently, the report only s
  if (PM_result$NPdata$numeqt > 1) {
    stop("Currently only one output equation is supported.\n")
  }
  
  cat("Generating report based on specified template...\n")
  
  rmarkdown::render(
    input = template, 
    output_file = outfile,
    params = list(res = PM_result),
    clean = TRUE,
    quiet = TRUE
  )
  
  if (show & file.exists(outfile)) {
    pander::openFileInOS(outfile)
  }
  
  cat(paste("Report generated at", outfile, "\n"))
  
}