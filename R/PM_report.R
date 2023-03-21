#' Generates a report from a specified Rmd template
#'
#' @title Generate a report
#' @param PM_result A `PM_result` object obtained from \code{PM_load(x)}, where `x` is the run number.
#' @param template The filename of a report template in the current working directory, 
#' or the full path/filename of a template in another directory. 
#' If not specified, the default Pmetrics report template as specified in [getPMoptions]
#' is used.
#' @param outfile The location of the generated report, defaults to a temporary file.
#' @param show Controls if the report should be automatically opened on generation, defaults to `TRUE`
#' Pmetrics will prompt the user to set this address the first time the \code{remote} argument is set to \code{TRUE}
#' in \code{\link{NPrun}}.
#' @return Generates an HTML-report in the current working directory.
#' @author Markus Hovd and Julian Otalvaro
#' @seealso \code{\link{PM_load}}
#' @export

PM_report <- function(PM_result, template = getPMoptions("report_template"), outfile, show = TRUE) {

    if (!is(PM_result, "PM_result")) {
    stop("This function expects a valid PM_result object from PM_load\n")
  }
  
  templateFile <- switch(template, 
                         plotly = system.file("report/templates/plotly.Rmd", package = "Pmetrics"),
                         ggplot = system.file("report/templates/ggplot.Rmd", package = "Pmetrics"))
  if(is.null(templateFile)){
    if(!file.exists(templateFile)) stop(crayon::red("ERROR: "), templateFile, " does not exist.\n")
  }
  
  
  if (missing(outfile)) {
    outfile = tempfile(fileext = ".html")
  }
  
  ### TEMPORARY LIMITATION FOR OUTEQ ###
  # Currently, the report only s
  #if (PM_result$NPdata$numeqt > 1) {
  #  stop("Currently only one output equation is supported.\n")
  #}
  
  cat("Generating report based on specified template...\n")
  
  # Check if pandoc is exposed to Rstudio
  if (!rmarkdown::pandoc_available()) {
    # Check if pandoc is installed
    if (!pandoc::pandoc_available()) {
      pandoc::pandoc_install()
    }
    
    # Set the correct environmental variable for use in shell
    Sys.setenv(RSTUDIO_PANDOC = pandoc::pandoc_locate())
    
    if (!rmarkdown::pandoc_available()) {
      stop("Unable to install pandoc, or expose it to Rmarkdown.")
    }
    
  }
  
  if (missing(outfile)) {
    outfile = tempfile(fileext = ".html")
  }
  
  ### TEMPORARY LIMITATION FOR OUTEQ ###
  # Currently, the report only s
  #if (PM_result$NPdata$numeqt > 1) {
  #  stop("Currently only one output equation is supported.\n")
  #}
  
  cat("Generating report based on specified template...\n")
  
  # Check if pandoc is exposed to Rstudio
  if (!rmarkdown::pandoc_available()) {
    # Check if pandoc is installed
    if (!pandoc::pandoc_available()) {
      pandoc::pandoc_install()
    }
    
    # Set the correct environmental variable for use in shell
    Sys.setenv(RSTUDIO_PANDOC = pandoc::pandoc_locate())
    
    if (!rmarkdown::pandoc_available()) {
      stop("Unable to install pandoc, or expose it to Rmarkdown.")
    }
    
  }
  
  rmarkdown::render(
    input = templateFile,
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
