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
#' @param outfile The location of the generated report, defaults to a temporary file.
#' @param show Controls if the report should be automatically opened on generation, defaults to `TRUE`
#' @param quiet If `TRUE`, suppresses the message about report generation, defaults to `FALSE`.
#' @return Generates an HTML-report in the current working directory.
#' @author Markus Hovd, Julian Otalvaro, and Michael Neely
#' @seealso [PM_load]
#' @export

PM_report <- function(x, template, outfile, show = TRUE, quiet = FALSE) {
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
    if (!file.exists(templateFile)) stop(crayon::red("ERROR: "), templateFile, " does not exist.\n")
  }


  if (missing(outfile)) {
    outfile <- tempfile(fileext = ".html")
  } else {
    outfile <- paste(getwd(), outfile, sep = "/")
  }

  if(!quiet) cat("Generating report based on the", template, "template...\n")


  # # Check if pandoc is exposed to Rstudio
  # not necessary since pandoc ships with Rstudio
  #
  # if (!rmarkdown::pandoc_available()) {
  #   # Check if pandoc is installed
  #   if (!pandoc::pandoc_available()) {
  #     pandoc::pandoc_install()
  #   }
  #
  #   # Set the correct environmental variable for use in shell
  #   Sys.setenv(RSTUDIO_PANDOC = pandoc::pandoc_locate())
  #
  #   if (!rmarkdown::pandoc_available()) {
  #     stop("Unable to install pandoc, or expose it to Rmarkdown.")
  #   }
  # }

  rmarkdown::render(
    input = templateFile,
    output_file = outfile,
    params = list(res = x),
    clean = TRUE,
    quiet = TRUE
  )

  # quarto::quarto_render(
  #   input = templateFile,
  #   output_file = outfile,
  #   execute_params = list(res = x),
  #   quiet = TRUE,
  #   debug = TRUE
  # )

  if (show & file.exists(outfile)) {
    pander::openFileInOS(outfile)
  }

  if(!quiet) cat(paste("Report generated at", outfile, "\n"))
}
