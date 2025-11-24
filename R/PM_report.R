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
#' @param path The path for the generated report, defaults to a temporary folder.
#' @param show Controls if the report should be automatically opened on generation, defaults to `TRUE`
#' @param quiet If `TRUE` (default), suppresses knitr output about report generation. Progress messages will still be displayed.
#' @return Generates an HTML-report in the folder specified by `path`.
#' @author Markus Hovd, Julian Otalvaro, and Michael Neely
#' @seealso [PM_load]
#' @export

PM_report <- function(x, template, path, show = TRUE, quiet = TRUE) {
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
  
  
  if (missing(path)) {
    out_path <- tempdir()
  } else {
    out_path <- normalizePath(path, winslash = "/") # knitr needs full path
  }
  
  if (is.null(x$final$data) & is.null(x$op$data) & is.null(x$cycle$data)){
    return(invisible(-1)) # no data found
  } else {
  rmarkdown::render(
    input = templateFile,
    output_file = file.path(out_path, "report.html"),
    params = list(res = x),
    clean = TRUE,
    quiet = quiet,
  )
}

  
  if (file.exists(file.path(out_path, "report.html"))) {
    if (show){
      pander::openFileInOS(file.path(out_path, "report.html"))
    }
    return(invisible(1))
  } else {
    return(invisible(-1)) # something went wrong and report doesn't exist
  }
  
  }
