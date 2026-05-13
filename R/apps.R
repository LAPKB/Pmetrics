#' Launch standalone Pmetrics apps
#'
#' `lit_sim()`, `model_lib()`, and `pm_plot()` launch standalone golem app
#' packages.
#'
#' `apps()` shows an interactive numbered menu (via `cli`) and launches the
#' selected app using `readline()` input.
#'
#' @param launch.browser Logical. Should the app launch in a browser?
#' @param data_env Environment passed to `pm_plot()` containing the data
#'   objects the app should discover and use. Defaults to `.GlobalEnv`.
#'   Ignored by launchers that do not accept this argument.
#' @return Invisibly returns the value from the standalone app package's
#'   `run_app()` function for launcher functions. `apps()` returns invisibly
#'   `NULL` on cancel/invalid input.
#' @name apps
NULL

.app_registry <- function() {
  list(
    list(
      id = "lit_sim",
      title = "Literature model simulator",
      fn = lit_sim
    ),
    list(
      id = "model_lib",
      title = "Model library browser",
      fn = model_lib
    ),
    list(
      id = "pm_plot",
      title = "Pmetrics plot helper",
      fn = pm_plot
    )
    # list(
    #   id = "pm_run",
    #   title = "Legacy Pmetrics run helper",
    #   fn = pm_run_app
    # )
  )
}

.launch_golem_app <- function(pkg, fun, launch.browser = TRUE,...) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "The {.pkg {pkg}} package is required to launch this app.",
      "i" = "See the {.url https://lapkb.r-universe.dev} repository for installation instructions."
    ))
  }

  app_fun <- getExportedValue(pkg, fun)
  app_fun(launch.browser = launch.browser, ...)
}

#' @rdname apps
#' @export
lit_sim <- function(launch.browser = TRUE) {
  .launch_golem_app("PmetricsLitSim", "run_app", launch.browser = launch.browser)
}

#' @rdname apps
#' @export
pm_plot <- function(launch.browser = TRUE, data_env = .GlobalEnv) {
  .launch_golem_app(
    "PmetricsPlot",
    "run_app",
    launch.browser = launch.browser,
    data_env = data_env
  )
}

# #' @rdname apps
# #' @export
# pm_run <- function(launch.browser = TRUE) {
#   .launch_packaged_app("pm_run", launch.browser = launch.browser)
# }

#' @rdname apps
#' @export
apps <- function(launch.browser = TRUE) {
  registry <- .app_registry()

  cli::cli_h2("Pmetrics apps")
  for (i in seq_along(registry)) {
    cli::cli_text("{.val {i}} {.strong {registry[[i]]$title}} {.field ({registry[[i]]$id})}")
  }

  choice <- readline("Choose an app number (press Enter to cancel): ")
  if (!nzchar(choice)) {
    cli::cli_alert_info("Cancelled.")
    return(invisible(NULL))
  }

  idx <- suppressWarnings(as.integer(choice))
  if (is.na(idx) || idx < 1 || idx > length(registry)) {
    cli::cli_alert_danger("Invalid app selection.")
    return(invisible(NULL))
  }

  selected <- registry[[idx]]
  cli::cli_alert_info("Launching {.strong {selected$title}}")
  selected$fn(launch.browser = launch.browser)
}
