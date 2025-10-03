#' @title Generate a report
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Generates a report from a specified Rmd template
#'
#' @param x A [PM_result] object obtained from [PM_load].
#' @param template If missing, the default Pmetrics report template as specified in [getPMoptions]
#' is used. It can be changed with [setPMoptions]. Otherwise, the value for `template`
#' can be "plotly", "ggplot", or "none".
#' @param out_path The path for the generated report, defaults to a temporary file
#' in the current working directory.
#' @param show Controls if the report should be automatically opened on generation, defaults to `TRUE`
#' @param quiet If `TRUE`, suppresses the message about report generation, defaults to `FALSE`.
#' @return Generates an HTML-report in the current working directory.
#' @author Markus Hovd, Julian Otalvaro, and Michael Neely
#' @seealso [PM_load]
#' @export

PM_report <- function(x, template, out_path, show = TRUE, quiet = FALSE) {
  if (!is(x, "PM_result")) {
    cli::cli_abort(c("x" = "This function expects a valid PM_result object from PM_load."))
  }
  
  if (missing(template)) {
    template <- getPMoptions("report_template")
  }
  
  if (template == "none") {
    return()
  }
  
  templateFile <- switch(template,
    plotly = system.file("report/templates/plotly.Rmd", package = "Pmetrics"),
    ggplot = system.file("report/templates/ggplot.Rmd", package = "Pmetrics"),
    ggplot_rust = system.file("report/templates/ggplot_rust.Rmd", package = "Pmetrics")
  )
  # templateFile = system.file("report/templates/ggplot.Rmd", package = "Pmetrics")
  
  if (is.null(templateFile)) {
    if (!file.exists(templateFile)) cli::cli_warn(c("!" = "ERROR: {templateFile} does not exist."))
    return(invisible(-1))
  }
  
  
  if (missing(out_path)) {
    out_path <- tempdir()
  } else {
    out_path <- file.path(getwd(), out_path) # knitr needs full path
  }
  
  #if(!quiet) cat("Generating report based on the", template, "template...\n")
  rlang::try_fetch(
    rmarkdown::render(
      input = templateFile,
      output_file = file.path(out_path, "report.html"),
      params = list(res = x),
      clean = TRUE,
      quiet = TRUE,
    ),
    error = function(e) {return(invisible(-1))}
  )
  
  # quarto::quarto_render(
  #   input = templateFile,
  #   output_file = outfile,
  #   execute_params = list(res = x),
  #   quiet = TRUE,
  #   debug = TRUE
  # )
  
  if (show & file.exists(file.path(out_path, "report.html"))) {
    pander::openFileInOS(file.path(out_path, "report.html"))
  }
  
  return(invisible(1))
}
