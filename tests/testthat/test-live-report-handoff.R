testthat::test_that("send_live_report_result serializes and publishes the finished report payload", {
    session <- list(session = list(session_id = "pm-live-test"))
    result <- structure(list(marker = "ready"), class = "PM_result")
    published <- NULL

    testthat::with_mocked_bindings(
        publish_live_report_result = function(session_id, result_payload, report_generated_at) {
            published <<- list(
                session_id = session_id,
                result_payload = result_payload,
                report_generated_at = report_generated_at
            )
            invisible(NULL)
        },
        {
            send_live_report_result(
                live_report_session = session,
                res = result,
                generated_at = as.POSIXct("2026-05-25 12:34:56", tz = "UTC")
            )
        }
    )

    decoded <- unserialize(memDecompress(jsonlite::base64_dec(published$result_payload), type = "gzip"))

    testthat::expect_equal(published$session_id, "pm-live-test")
    testthat::expect_match(published$report_generated_at, "^2026-05-25T12:34:56")
    testthat::expect_s3_class(decoded, "PM_result")
    testthat::expect_equal(decoded$marker, "ready")
})

testthat::test_that("send_live_report_failure publishes the handoff error message", {
    session <- list(session = list(session_id = "pm-live-test"))
    published <- NULL

    testthat::with_mocked_bindings(
        publish_live_report_failed = function(session_id, message) {
            published <<- list(session_id = session_id, message = message)
            invisible(NULL)
        },
        {
            send_live_report_failure(session, "Finished report handoff failed")
        }
    )

    testthat::expect_equal(published$session_id, "pm-live-test")
    testthat::expect_equal(published$message, "Finished report handoff failed")
})

testthat::test_that("PM_model$fit publishes the finished report to the live app when report = 'app'", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    run_root <- withr::local_tempdir(pattern = "live-handoff-fit-")

    fake_result <- structure(list(marker = "ready"), class = "PM_result")
    sent_result <- NULL
    sent_failure <- NULL

    result <- testthat::with_mocked_bindings(
        validate_model_source = function(...) invisible(NULL),
        start_live_report_session = function(show = TRUE, timeout_ms = 3000L) {
            list(
                session = list(session_id = "pm-live-test"),
                process = structure(list(), class = "process"),
                connected = TRUE,
                url = "http://127.0.0.1:1234"
            )
        },
        close_live_report_session = function(...) invisible(TRUE),
        fit = function(model_source, data, params, output_path, kind, solver = NULL) {
            "{\"ok\":true}"
        },
        build_pm_result_from_fit_payload = function(...) fake_result,
        send_live_report_result = function(live_report_session, res, generated_at = Sys.time()) {
            sent_result <<- list(session = live_report_session, res = res)
            invisible(TRUE)
        },
        send_live_report_failure = function(live_report_session, message) {
            sent_failure <<- list(session = live_report_session, message = message)
            invisible(TRUE)
        },
        {
            suppressWarnings(suppressMessages(
                model$fit(
                    data = data,
                    path = run_root,
                    run = 1,
                    overwrite = TRUE,
                    cycles = 1,
                    points = 20,
                    report = "app",
                    quiet = TRUE
                )
            ))
        }
    )

    testthat::expect_s3_class(result, "PM_result")
    testthat::expect_null(sent_failure)
    testthat::expect_true(is.list(sent_result))
    testthat::expect_equal(sent_result$session$session$session_id, "pm-live-test")
    testthat::expect_identical(sent_result$res, fake_result)
})

testthat::test_that("PM_model$fit reports a final handoff failure to the live app", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    run_root <- withr::local_tempdir(pattern = "live-handoff-fail-")

    sent_result <- NULL
    sent_failure <- NULL

    err <- rlang::catch_cnd(
        testthat::with_mocked_bindings(
            validate_model_source = function(...) invisible(NULL),
            start_live_report_session = function(show = TRUE, timeout_ms = 3000L) {
                list(
                    session = list(session_id = "pm-live-test"),
                    process = structure(list(), class = "process"),
                    connected = TRUE,
                    url = "http://127.0.0.1:1234"
                )
            },
            close_live_report_session = function(...) invisible(TRUE),
            fit = function(model_source, data, params, output_path, kind, solver = NULL) {
                "{\"ok\":true}"
            },
            build_pm_result_from_fit_payload = function(...) {
                stop("decode failed", call. = FALSE)
            },
            send_live_report_result = function(live_report_session, res, generated_at = Sys.time()) {
                sent_result <<- list(session = live_report_session, res = res)
                invisible(TRUE)
            },
            send_live_report_failure = function(live_report_session, message) {
                sent_failure <<- list(session = live_report_session, message = message)
                invisible(TRUE)
            },
            {
                suppressMessages(
                    model$fit(
                        data = data,
                        path = run_root,
                        run = 1,
                        overwrite = TRUE,
                        cycles = 1,
                        points = 20,
                        report = "app",
                        quiet = TRUE
                    )
                )
            }
        ),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_bridge_handoff_error")
    testthat::expect_equal(err$stage, "handoff")
    testthat::expect_equal(err$code, "fit_result_build_failed")
    testthat::expect_match(conditionMessage(err), "Failed to build PM_result from fit payload")
    testthat::expect_match(conditionMessage(err), "decode failed")
    testthat::expect_null(sent_result)
    testthat::expect_true(is.list(sent_failure))
    testthat::expect_equal(sent_failure$session$session$session_id, "pm-live-test")
    testthat::expect_match(sent_failure$message, "Failed to build PM_result from fit payload")
})
