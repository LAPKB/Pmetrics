#' @title Generate a report
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Launches the Pmetrics reporting app or generates an HTML report for a completed run
#'
#' @param x A [PM_result] object obtained from [PM_load].
#' @param template If missing, the current report mode from [getPMoptions] is used.
#'   Use `"app"` to launch the PmetricsReports app or `"plotly"`, `"ggplot"`,
#'   or `"ggplot_rust"` to generate the legacy HTML report.
#' @param path Output folder for the generated HTML report. Ignored for app mode
#'   unless the app launch fails and HTML fallback is used.
#' @param show Controls whether the Shiny app should be opened automatically, defaults to `TRUE`.
#' @param quiet Retained for compatibility.
#' @return Launches the selected report mode.
#' @author Markus Hovd, Julian Otalvaro, and Michael Neely
#' @seealso [PM_load]
#' @export

PM_report <- function(x, template, path, show = TRUE, quiet = TRUE) {
  template_missing <- missing(template)
  path_missing <- missing(path)

  resolve_run_app <- function() {
    pmetrics_path <- tryCatch(
      getNamespaceInfo(asNamespace("Pmetrics"), "path"),
      error = function(e) ""
    )
    reports_candidates <- unique(c(
      Sys.getenv("PMETRICS_REPORTS_PATH", unset = ""),
      file.path(dirname(pmetrics_path), "PmetricsReports"),
      file.path(dirname(pmetrics_path), "Pmetricsreports"),
      file.path(pmetrics_path, "..", "Apps", "PmetricsReports"),
      file.path(pmetrics_path, "..", "Apps", "Pmetricsreports")
    ))

    reports_candidates <- reports_candidates[nzchar(reports_candidates)]
    reports_candidates <- normalizePath(reports_candidates, mustWork = FALSE)
    dev_reports_path <- reports_candidates[
      file.exists(file.path(reports_candidates, "DESCRIPTION"))
    ][[1]]

    if (!is.null(dev_reports_path) && nzchar(dev_reports_path) &&
      requireNamespace("pkgload", quietly = TRUE)) {
      loaded <- tryCatch(
        {
          pkgload::load_all(dev_reports_path, quiet = TRUE, export_all = FALSE)
          TRUE
        },
        error = function(e) FALSE
      )

      if (isTRUE(loaded)) {
        return(getExportedValue("PmetricsReports", "run_app"))
      }
    }

    if (!requireNamespace("PmetricsReports", quietly = TRUE)) {
      return(NULL)
    }

    getExportedValue("PmetricsReports", "run_app")
  }

  resolve_report_mode <- function() {
    mode <- if (template_missing) {
      getPMoptions("report_template", warn = FALSE, quiet = TRUE)
    } else {
      template
    }

    if (is.null(mode) || identical(mode, -1)) {
      return("app")
    }

    mode <- as.character(mode[[1]])
    if (is.na(mode) || !nzchar(mode)) {
      return("app")
    }

    mode
  }

  report_mode <- resolve_report_mode()

  if (!is(x, "PM_result")) {
    cli::cli_abort(c("x" = "This function expects a valid PM_result object from PM_load."))
  }

  if (identical(report_mode, "none")) {
    return(invisible(0))
  }

  if (is.null(x$final$data) && is.null(x$op$data) && is.null(x$cycle$data)) {
    return(invisible(-1)) # no data found
  }

  render_html_fallback <- function(fallback_template = report_mode) {
    if (identical(fallback_template, "none")) {
      return(invisible(0))
    }
    if (identical(fallback_template, "app") || is.null(fallback_template)) {
      fallback_template <- "ggplot"
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

  if (!identical(report_mode, "app")) {
    return(render_html_fallback(report_mode))
  }

  run_app <- resolve_run_app()
  if (is.null(run_app)) {
    cli::cli_warn(c(
      "!" = "The {.pkg PmetricsReports} package is not available.",
      "i" = "Falling back to legacy HTML report generation.",
      "i" = "{.code install.packages('PmetricsReports', repos = 'https://lapkb.r-universe.dev')} for a better experience."
    ))
    return(render_html_fallback())
  }

  tryCatch(
    {
      app_process <- run_app(res = x, launch.browser = show)
      app_url <- attr(app_process, "app_url", exact = TRUE)
      if (!is.null(app_url) && nzchar(app_url)) {
        cli::cli_inform(c("i" = "Reporting app URL: {app_url}"))
      }
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
