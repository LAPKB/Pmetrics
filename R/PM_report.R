#' @title Generate a report
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Launches the Pmetrics reporting app for a completed run
#'
#' @param x A [PM_result] object obtained from [PM_load].
#' @param template Deprecated for app mode. Used only for HTML fallback report
#'   generation when app launch fails.
#' @param path Deprecated for app mode. Used only for HTML fallback report
#'   generation when app launch fails.
#' @param show Controls whether the Shiny app should be opened automatically, defaults to `TRUE`.
#' @param quiet Retained for compatibility.
#' @return Launches the Pmetrics reporting app.
#' @author Markus Hovd, Julian Otalvaro, and Michael Neely
#' @seealso [PM_load]
#' @export

PM_report <- function(x, template, path, show = TRUE, quiet = TRUE) {
  template_missing <- missing(template)
  path_missing <- missing(path)

  if (!is(x, "PM_result")) {
    cli::cli_abort(c("x" = "This function expects a valid PM_result object from PM_load."))
  }

  if (!template_missing && identical(template, "none")) {
    return(invisible(0))
  }

  if (is.null(x$final$data) & is.null(x$op$data) & is.null(x$cycle$data)) {
    return(invisible(-1)) # no data found
  }

  render_html_fallback <- function() {
    fallback_template <- if (template_missing) getPMoptions("report_template") else template
    if (identical(fallback_template, "none")) {
      return(invisible(0))
    }
    # Normalize the template. Anything that is not a recognized HTML template
    # (e.g. "app", or -1/NULL when options were not written on a fresh install)
    # falls back to the default so that report generation never errors.
    valid_templates <- c("plotly", "ggplot", "ggplot_rust")
    if (!is.character(fallback_template) || length(fallback_template) != 1 ||
      !fallback_template %in% valid_templates) {
      fallback_template <- "plotly"
    }

    template_file <- switch(fallback_template,
      plotly = system.file("report/templates/plotly.Rmd", package = "Pmetrics"),
      ggplot = system.file("report/templates/ggplot.Rmd", package = "Pmetrics"),
      ggplot_rust = system.file("report/templates/ggplot_rust.Rmd", package = "Pmetrics")
    )

    if (is.null(template_file) || !file.exists(template_file)) {
      cli::cli_warn(c(
        "!" = "HTML fallback failed: unknown or missing report template {.val {fallback_template}}."
      ))
      return(invisible(-1))
    }

    if (!requireNamespace("DT", quietly = TRUE)) {
      cli::cli_warn(c(
        "!" = "HTML fallback failed: {.pkg DT} package is required for this template.",
        "i" = "Please install it with {.code install.packages('DT')}."
      ))
      return(invisible(-1))
    }

    out_path <- if (path_missing) {
      tempdir()
    } else {
      normalizePath(path, winslash = "/", mustWork = FALSE)
    }

    if (!dir.exists(out_path)) {
      dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
    }

    
    rmarkdown::render(
      input = template_file,
      output_file = file.path(out_path, "report.html"),
      params = list(res = x),
      clean = TRUE,
      quiet = quiet
    )

    if (file.exists(file.path(out_path, "report.html"))) {
      if (show) {
        utils::browseURL(file.path(out_path, "report.html"))
      }
      return(invisible(1))
    }

    invisible(-1)
  }

  if (!requireNamespace("PmetricsReports", quietly = TRUE)) {
    cli::cli_warn(c(
      "!" = "The {.pkg PmetricsReports} package is not available.",
      "i" = "Falling back to legacy HTML report generation.",
      "i" = "{.code install.packages('PmetricsReports', repos = 'https://lapkb.r-universe.dev')} for a better experience."
    ))
    return(render_html_fallback())
  }

  tryCatch(
    {
      run_app <- getExportedValue("PmetricsReports", "run_app")
      run_app(res = x, launch.browser = show)
      invisible(1)
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Reporting app launch failed.",
        "i" = "Falling back to legacy HTML report generation.",
        "i" = "App error: {.field {conditionMessage(e)}}"
      ))
      render_html_fallback()
    }
  )
}
