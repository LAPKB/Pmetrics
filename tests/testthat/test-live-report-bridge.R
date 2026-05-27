testthat::skip_on_cran()

testthat::skip_if_not(
    is_cargo_installed(),
    message = "Cargo is required to run live report bridge tests."
)

testthat::skip_if_not_installed("pkgload")

testthat::test_that("live report session launches app and connects before fit", {
    reports_path <- testthat::test_path("..", "..", "..", "Pmetricsreports")
    pkgload::load_all(reports_path, quiet = TRUE, export_all = FALSE)

    start_live <- getFromNamespace("start_live_report_session", "Pmetrics")
    close_live <- getFromNamespace("close_live_report_session", "Pmetrics")

    live_session <- start_live(show = FALSE, timeout_ms = 5000L)

    withr::defer({
        if (!is.null(live_session$process)) {
            try(live_session$process$kill(), silent = TRUE)
        }
        close_live(live_session)
    })

    testthat::expect_type(live_session, "list")
    testthat::expect_true(is.list(live_session$session))
    testthat::expect_true(nzchar(live_session$session$session_id))
    testthat::expect_true(isTRUE(live_session$connected))
})

testthat::test_that("live report close leaves time for finished report handoff", {
    reports_path <- testthat::test_path("..", "..", "..", "Pmetricsreports")
    pkgload::load_all(reports_path, quiet = TRUE, export_all = FALSE)

    start_raw_live <- getFromNamespace("start_live_session", "Pmetrics")
    close_live <- getFromNamespace("close_live_report_session", "Pmetrics")
    send_result <- getFromNamespace("send_live_report_result", "Pmetrics")
    open_connection <- getFromNamespace("open_live_session_connection", "PmetricsReports")
    close_connection <- getFromNamespace("close_live_session_connection", "PmetricsReports")
    read_messages <- getFromNamespace("read_live_session_messages", "PmetricsReports")

    live_session <- list(
        session = start_raw_live(),
        close_grace_seconds = 0.5
    )
    connection <- open_connection(live_session$session)
    session_closed <- FALSE

    withr::defer({
        close_connection(connection)
        if (!session_closed) {
            close_live(live_session)
        }
    })

    send_result(
        live_session,
        structure(list(marker = paste(rep("x", 100000), collapse = "")), class = "PM_result")
    )
    close_live(live_session)
    session_closed <- TRUE

    messages <- list()
    deadline <- Sys.time() + 1
    while (Sys.time() < deadline) {
        batch <- read_messages(connection, max_messages = 50L)
        if (length(batch)) {
            messages <- c(messages, batch)
        }

        kinds <- vapply(messages, function(message) {
            if (is.null(message$kind) || !length(message$kind)) {
                ""
            } else {
                as.character(message$kind[[1]])
            }
        }, character(1))

        if ("session_closed" %in% kinds) {
            break
        }

        Sys.sleep(0.05)
    }

    kinds <- vapply(messages, function(message) {
        if (is.null(message$kind) || !length(message$kind)) {
            ""
        } else {
            as.character(message$kind[[1]])
        }
    }, character(1))

    testthat::expect_true("final_report_ready" %in% kinds)
    testthat::expect_true("session_closed" %in% kinds)
})

testthat::test_that("send_live_report_result rethrows structured handoff errors", {
    send_result <- getFromNamespace("send_live_report_result", "Pmetrics")
    original_publish <- get("publish_live_report_result", envir = asNamespace("Pmetrics"))

    withr::defer(assignInNamespace("publish_live_report_result", original_publish, ns = "Pmetrics"))
    assignInNamespace(
        "publish_live_report_result",
        function(...) {
            stop(
                mock_bridge_error_message(
                    stage = "handoff",
                    code = "live_report_publish_failed",
                    message = "publish failed"
                ),
                call. = FALSE
            )
        },
        ns = "Pmetrics"
    )

    err <- rlang::catch_cnd(
        send_result(
            list(session = list(session_id = "pm-live-test")),
            structure(list(marker = "ready"), class = "PM_result")
        ),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_bridge_handoff_error")
    testthat::expect_equal(err$stage, "handoff")
    testthat::expect_equal(err$code, "live_report_publish_failed")
})
