#' Launch packaged Shiny apps
#'
#' `lit_sim()`, `model_lib()`, `pm_plot_app()`, and `pm_run_app()` launch specific packaged
#' apps from `inst/apps`.
#'
#' `apps()` shows an interactive numbered menu (via `cli`) and launches the
#' selected app using `readline()` input.
#'
#' @param launch.browser Logical. Should the app launch in a browser?
#' @return Invisibly returns the value from [shiny::runApp()] for launcher
#'   functions. `apps()` returns invisibly `NULL` on cancel/invalid input.
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
      fn = function(launch.browser = TRUE) {
        shiny::runApp(build_plot(), launch.browser = launch.browser)
      }
    ),
    list(
      id = "pm_run",
      title = "Legacy Pmetrics run helper",
      fn = pm_run_app
    )
  )
}

.launch_packaged_app <- function(app_id, launch.browser = TRUE) {
  app_dir <- system.file("apps", app_id, package = "Pmetrics")

  if (identical(app_dir, "") || !dir.exists(app_dir)) {
    stop(
      sprintf("Could not find packaged app directory for '%s'.", app_id),
      call. = FALSE
    )
  }

  shiny::runApp(appDir = app_dir, launch.browser = launch.browser)
}

#' @rdname apps
#' @export
lit_sim <- function(launch.browser = TRUE) {
  .launch_packaged_app("lit_sim", launch.browser = launch.browser)
}

#' @rdname apps
#' @export
pm_plot_app <- function(launch.browser = TRUE) {
  .launch_packaged_app("pm_plot", launch.browser = launch.browser)
}

#' @rdname apps
#' @export
pm_run_app <- function(launch.browser = TRUE) {
  .launch_packaged_app("pm_run", launch.browser = launch.browser)
}

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
