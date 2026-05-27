testthat::test_that("PM_report respects the configured HTML report mode", {
    testthat::skip_if_not_installed("rmarkdown")

    res <- structure(
        list(
            final = list(data = TRUE),
            op = list(data = TRUE),
            cycle = list(data = TRUE)
        ),
        class = "PM_result"
    )

    app_called <- FALSE
    out_dir <- withr::local_tempdir(pattern = "pm-report-html-")

    original_render <- get("render", envir = asNamespace("rmarkdown"))
    withr::defer(assignInNamespace("render", original_render, ns = "rmarkdown"))

    assignInNamespace(
        "render",
        function(input, output_file, params, clean, quiet) {
            dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
            file.create(output_file)
            invisible(output_file)
        },
        ns = "rmarkdown"
    )

    report_status <- testthat::with_mocked_bindings(
        getPMoptions = function(opt, warn = TRUE, quiet = FALSE) {
            if (identical(opt, "report_template")) {
                return("plotly")
            }
            NULL
        },
        getExportedValue = function(...) {
            app_called <<- TRUE
            function(...) invisible(NULL)
        },
        requireNamespace = function(package, quietly = TRUE) TRUE,
        {
            suppressMessages(PM_report(res, path = out_dir, show = FALSE, quiet = TRUE))
        }
    )

    testthat::expect_false(app_called)
    testthat::expect_equal(report_status, 1)
    testthat::expect_true(file.exists(file.path(out_dir, "report.html")))
})

testthat::test_that("PM_report launches the app when report mode is app", {
    res <- structure(
        list(
            final = list(data = TRUE),
            op = list(data = TRUE),
            cycle = list(data = TRUE)
        ),
        class = "PM_result"
    )

    launched <- NULL
    report_status <- NULL

    testthat::expect_message(
        report_status <- testthat::with_mocked_bindings(
            getPMoptions = function(opt, warn = TRUE, quiet = FALSE) {
                if (identical(opt, "report_template")) {
                    return("app")
                }
                NULL
            },
            requireNamespace = function(package, quietly = TRUE) TRUE,
            getExportedValue = function(pkg, name) {
                testthat::expect_equal(pkg, "PmetricsReports")
                testthat::expect_equal(name, "run_app")
                function(res, launch.browser) {
                    launched <<- list(res = res, launch.browser = launch.browser)
                    process <- structure(list(), class = "process")
                    attr(process, "app_url") <- "http://127.0.0.1:1234"
                    process
                }
            },
            {
                PM_report(res, show = TRUE, quiet = TRUE)
            }
        ),
        "Reporting app URL"
    )

    testthat::expect_equal(report_status, 1)
    testthat::expect_true(is.list(launched))
    testthat::expect_identical(launched$res, res)
    testthat::expect_true(isTRUE(launched$launch.browser))
})
