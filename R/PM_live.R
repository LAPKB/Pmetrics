# nolint start

resolve_pmetrics_reports_run_app <- function() {
    pmetrics_path <- tryCatch(
        getNamespaceInfo(asNamespace("Pmetrics"), "path"),
        error = function(e) ""
    )
    sibling_reports_path <- normalizePath(
        file.path(dirname(pmetrics_path), "Pmetricsreports"),
        mustWork = FALSE
    )

    if (nzchar(sibling_reports_path) &&
        file.exists(file.path(sibling_reports_path, "DESCRIPTION")) &&
        requireNamespace("pkgload", quietly = TRUE)) {
        loaded <- tryCatch(
            {
                pkgload::load_all(sibling_reports_path, quiet = TRUE, export_all = FALSE)
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

start_live_report_session <- function(show = TRUE, timeout_ms = 10000L) {
    run_app <- resolve_pmetrics_reports_run_app()
    if (is.null(run_app)) {
        return(NULL)
    }

    live_session <- call_pm_bridge(
        start_live_session(),
        default_stage = "handoff",
        default_code = "live_session_start_failed"
    )

    process <- tryCatch(
        {
            run_app(
                res = NULL,
                launch.browser = show,
                background = TRUE,
                live_session = live_session
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
