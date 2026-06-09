# nolint start

pmetrics_live_app_dir <- function() {
    installed <- system.file("apps", "live-status", package = "Pmetrics")
    if (nzchar(installed) && file.exists(file.path(installed, "app.R"))) {
        return(installed)
    }

    pkg_path <- tryCatch(
        getNamespaceInfo(asNamespace("Pmetrics"), "path"),
        error = function(e) ""
    )

    if (!nzchar(pkg_path)) {
        return("")
    }

    candidates <- unique(c(
        file.path(pkg_path, "inst", "apps", "live-status"),
        file.path(pkg_path, "inst", "apps", "live-report"),
        file.path(pkg_path, "inst", "apps", "live_report")
    ))

    candidates <- normalizePath(candidates, mustWork = FALSE)
    found <- candidates[file.exists(file.path(candidates, "app.R"))]
    if (!length(found)) {
        return("")
    }

    found[[1]]
}

pmetrics_is_dev_package_path <- function(path) {
    if (is.null(path) || !nzchar(path)) {
        return(FALSE)
    }

    file.exists(file.path(path, "DESCRIPTION")) &&
        file.exists(file.path(path, "R"))
}

launch_live_report_app <- function(live_session, launch.browser = TRUE, timeout_seconds = 30) {
    app_dir <- pmetrics_live_app_dir()
    if (!nzchar(app_dir)) {
        cli::cli_abort(c(
            "x" = "The internal live reporting app could not be found.",
            "i" = "Expected app files under {.path inst/apps/live-status}."
        ))
    }
    app_dir <- normalizePath(app_dir, mustWork = TRUE)

    launch_dir <- tempfile("Pmetrics-live-status-")
    dir.create(launch_dir, recursive = TRUE, showWarnings = FALSE)

    port_file <- file.path(launch_dir, "port.txt")
    pkg_path <- tryCatch(
        getNamespaceInfo(asNamespace("Pmetrics"), "path"),
        error = function(e) ""
    )

    process <- callr::r_bg(
        function(pkg_path, app_dir, port_file, live_session) {
            # In development checkouts, load local code. In installed environments,
            # load the installed package to avoid pkgload-only assumptions.
            loaded <- FALSE
            pkg_lib <- ""
            if (nzchar(pkg_path)) {
                pkg_lib <- normalizePath(dirname(pkg_path), mustWork = FALSE)
            }

            if (
                nzchar(pkg_path) &&
                file.exists(file.path(pkg_path, "DESCRIPTION")) &&
                file.exists(file.path(pkg_path, "R")) &&
                requireNamespace("pkgload", quietly = TRUE)
            ) {
                loaded <- tryCatch(
                    {
                        pkgload::load_all(pkg_path, quiet = TRUE, export_all = TRUE)
                        TRUE
                    },
                    error = function(e) FALSE
                )
            }

            if (!isTRUE(loaded)) {
                if (nzchar(pkg_lib) && dir.exists(pkg_lib)) {
                    .libPaths(unique(c(pkg_lib, .libPaths())))
                    loaded <- tryCatch(
                        {
                            loadNamespace("Pmetrics", lib.loc = pkg_lib)
                            TRUE
                        },
                        error = function(e) FALSE
                    )
                }

                if (!isTRUE(loaded)) {
                    loadNamespace("Pmetrics")
                }
            }

            options(Pmetrics.live_session = unclass(live_session))

            port <- httpuv::randomPort()
            writeLines(as.character(port), port_file)

            shiny::runApp(
                appDir = app_dir,
                launch.browser = FALSE,
                port = port,
                host = "127.0.0.1"
            )
        },
        args = list(
            pkg_path = pkg_path,
            app_dir = app_dir,
            port_file = port_file,
            live_session = live_session
        ),
        supervise = TRUE,
        stdout = "|",
        stderr = "|"
    )

    deadline <- Sys.time() + as.numeric(timeout_seconds)
    while (!file.exists(port_file) && process$is_alive() && Sys.time() < deadline) {
        Sys.sleep(0.05)
    }

    if (!process$is_alive()) {
        startup_output <- c(
            tryCatch(process$read_all_error_lines(), error = function(e) character()),
            tryCatch(process$read_all_output_lines(), error = function(e) character())
        )
        startup_output <- startup_output[nzchar(startup_output)]
        cli::cli_abort(c(
            "x" = "Pmetrics live reporting app exited before startup completed.",
            if (length(startup_output)) paste(startup_output, collapse = "\n")
        ))
    }

    port <- if (file.exists(port_file)) {
        suppressWarnings(as.integer(readLines(port_file, warn = FALSE)[1]))
    } else {
        NA_integer_
    }

    if (!is.finite(port) || is.na(port)) {
        cli::cli_abort(c(
            "x" = "Pmetrics live reporting app did not report a local URL before startup timed out."
        ))
    }

    app_url <- sprintf("http://127.0.0.1:%s", port)
    if (isTRUE(launch.browser)) {
        utils::browseURL(app_url)
    }

    attr(process, "app_url") <- app_url
    attr(process, "launch_dir") <- launch_dir
    invisible(process)
}

start_live_report_session <- function(show = TRUE, timeout_ms = 10000L) {
    live_session <- call_pm_bridge(
        start_live_session(),
        default_stage = "handoff",
        default_code = "live_session_start_failed"
    )

    process <- tryCatch(
        {
            launch_live_report_app(
                live_session = live_session,
                launch.browser = show
            )
        },
        error = function(e) {
            try(close_live_session(live_session$session_id), silent = TRUE)
            stop(e)
        }
    )

    connected <- tryCatch(
        call_pm_bridge(
            wait_live_session_connected(live_session$session_id, as.integer(timeout_ms)),
            default_stage = "handoff",
            default_code = "live_session_wait_failed"
        ),
        error = function(e) FALSE
    )

    list(
        session = live_session,
        process = process,
        connected = isTRUE(connected),
        url = attr(process, "app_url", exact = TRUE),
        close_grace_seconds = 1
    )
}

format_live_report_generated_at <- function(generated_at = Sys.time()) {
    format(
        as.POSIXct(generated_at, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%OS3Z",
        tz = "UTC"
    )
}

encode_live_report_result <- function(res) {
    raw <- serialize(res, NULL, xdr = FALSE)
    jsonlite::base64_enc(memCompress(raw, type = "gzip"))
}

send_live_report_result <- function(live_report_session, res, generated_at = Sys.time()) {
    if (is.null(live_report_session) || is.null(live_report_session$session$session_id)) {
        return(invisible(FALSE))
    }

    call_pm_bridge(
        publish_live_report_result(
            session_id = live_report_session$session$session_id,
            result_payload = encode_live_report_result(res),
            report_generated_at = format_live_report_generated_at(generated_at)
        ),
        default_stage = "handoff",
        default_code = "live_report_publish_failed"
    )

    invisible(TRUE)
}

send_live_report_failure <- function(live_report_session, message) {
    if (is.null(live_report_session) || is.null(live_report_session$session$session_id)) {
        return(invisible(FALSE))
    }

    call_pm_bridge(
        publish_live_report_failed(
            session_id = live_report_session$session$session_id,
            message = as.character(message[[1]])
        ),
        default_stage = "handoff",
        default_code = "live_report_failure_publish_failed"
    )

    invisible(TRUE)
}

close_live_report_session <- function(live_report_session) {
    if (is.null(live_report_session) || is.null(live_report_session$session$session_id)) {
        return(invisible(FALSE))
    }

    grace <- suppressWarnings(as.numeric(live_report_session$close_grace_seconds[[1]]))
    if (is.finite(grace) && grace > 0) {
        Sys.sleep(grace)
    }

    try(
        call_pm_bridge(
            close_live_session(live_report_session$session$session_id),
            default_stage = "handoff",
            default_code = "live_session_close_failed"
        ),
        silent = TRUE
    )
    invisible(TRUE)
}

# nolint end
