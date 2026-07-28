.report_process_error <- function(process) {
  if (!inherits(process, "process") || isTRUE(process$is_alive())) {
    return(NULL)
  }

  status <- tryCatch(process$get_exit_status(), error = function(e) NA_integer_)
  stderr <- tryCatch(process$read_error_lines(), error = function(e) character())
  stderr <- stderr[nzchar(stderr)]

  details <- if (length(stderr)) {
    paste(stderr, collapse = "\n")
  } else if (!is.na(status)) {
    paste0("The reporting process exited with status ", status, ".")
  } else {
    "The reporting process exited before the report was launched."
  }

  details
}

.report_dependency_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

.report_app_runner <- function() {
  getExportedValue("PmetricsReports", "run_app")
}

.render_report <- function(...) {
  rmarkdown::render(...)
}

.valid_report_modes <- function() {
  c("app", "plotly", "ggplot", "ggplot_rust", "none")
}

.resolve_report_mode <- function(requested, warn = TRUE) {
  valid_modes <- .valid_report_modes()
  requested_valid <- is.character(requested) &&
    length(requested) == 1L &&
    !is.na(requested) &&
    requested %in% valid_modes

  if (requested_valid) {
    return(list(mode = requested, requested = requested, fallback = FALSE))
  }

  saved_default <- getPMoptions("report_template")
  default_valid <- is.character(saved_default) &&
    length(saved_default) == 1L &&
    !is.na(saved_default) &&
    saved_default %in% valid_modes
  fallback_mode <- if (default_valid) saved_default else "app"

  if (isTRUE(warn)) {
    requested_label <- if (is.character(requested) && length(requested) == 1L) {
      requested
    } else {
      "<invalid value>"
    }
    cli::cli_warn(c(
      "!" = "Invalid report mode {.val {requested_label}}.",
      "i" = if (default_valid) {
        "Falling back to the saved default report mode {.val {fallback_mode}}."
      } else {
        "The saved default is also invalid; falling back to {.val app}."
      }
    ))
  }

  list(mode = fallback_mode, requested = requested, fallback = TRUE)
}

#' @title Generate a report
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Generates a report for a completed run
#'
#' @param x A [PM_result] object obtained from [PM_load].
#' @param template Report mode. One of `"app"`, `"plotly"`, `"ggplot"`,
#'   `"ggplot_rust"`, or `"none"`. If omitted, uses the `report_template`
#'   Pmetrics option. An invalid value falls back to that saved default.
#' @param path Output directory for HTML reports.
#' @param show Controls whether the report should be opened automatically,
#'   defaults to `TRUE`.
#' @param quiet Suppress output from HTML report rendering.
#' @return Invisibly returns `1` on success, `0` when reporting is disabled,
#'   and `-1` when no report can be generated.
#' @author Markus Hovd, Julian Otalvaro, and Michael Neely
#' @seealso [PM_load]
#' @export
PM_report <- function(x, template, path, show = TRUE, quiet = TRUE) {
  template_missing <- missing(template)
  path_missing <- missing(path)

  if (!is(x, "PM_result")) {
    cli::cli_abort(c("x" = "This function expects a valid PM_result object from PM_load."))
  }

  requested_mode <- if (template_missing) getPMoptions("report_template") else template
  report_mode <- .resolve_report_mode(requested_mode)$mode

  if (identical(report_mode, "none")) {
    return(invisible(0))
  }

  if (is.null(x$final$data) & is.null(x$op$data) & is.null(x$cycle$data)) {
    return(invisible(-1)) # no data found
  }

  render_html_report <- function(html_template) {
    template_file <- switch(html_template,
      plotly = system.file("report/templates/plotly.Rmd", package = "Pmetrics"),
      ggplot = system.file("report/templates/ggplot.Rmd", package = "Pmetrics"),
      ggplot_rust = system.file("report/templates/ggplot_rust.Rmd", package = "Pmetrics")
    )

    if (is.null(template_file) || !file.exists(template_file)) {
      cli::cli_warn(c(
        "!" = "HTML report generation failed: missing report template {.val {html_template}}."
      ))
      return(invisible(-1))
    }

    if (!.report_dependency_available("DT")) {
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

    .render_report(
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
    return(render_html_report(report_mode))
  }

  if (!.report_dependency_available("PmetricsReports")) {
    cli::cli_warn(c(
      "!" = "The {.pkg PmetricsReports} package is not available.",
      "i" = "Falling back to legacy HTML report generation.",
      "i" = "{.code install.packages('PmetricsReports', repos = 'https://lapkb.r-universe.dev')} for a better experience."
    ))
    return(render_html_report("plotly"))
  }

  tryCatch(
    {
      run_app <- .report_app_runner()
      app_process <- run_app(res = x, launch.browser = show)
      process_error <- .report_process_error(app_process)
      if (!is.null(process_error)) {
        stop(process_error, call. = FALSE)
      }
      invisible(1)
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Reporting app launch failed.",
        "i" = "Falling back to legacy HTML report generation.",
        "i" = "App error: {.field {conditionMessage(e)}}"
      ))
      render_html_report("plotly")
    }
  )
}
