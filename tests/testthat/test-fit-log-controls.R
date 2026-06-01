testthat::skip_on_cran()

testthat::skip_if_not(
    is_cargo_installed(),
    message = "Cargo is required to run fit log control tests."
)

testthat::skip_if_not_installed("devtools")

run_log_control_fit <- function(
    progress = FALSE,
    write_logs = NULL,
    stdout_logs = NULL,
    log_level = NULL) {
    pkg_root <- normalizePath(".")
    temp_dir <- withr::local_tempdir(pattern = "fit-log-controls-")
    script_path <- file.path(temp_dir, "run-fit-log-controls.R")
    run_path <- file.path(temp_dir, "run")

    args <- c(
        "data = dat",
        "path = run_path",
        "run = 1",
        "cycles = 1",
        "points = 20",
        sprintf("progress = %s", if (isTRUE(progress)) "TRUE" else "FALSE"),
        'report = "none"',
        "quiet = TRUE"
    )

    if (!is.null(write_logs)) {
        args <- c(args, sprintf("write_logs = %s", if (isTRUE(write_logs)) "TRUE" else "FALSE"))
    }

    if (!is.null(stdout_logs)) {
        args <- c(args, sprintf("stdout_logs = %s", if (isTRUE(stdout_logs)) "TRUE" else "FALSE"))
    }

    if (!is.null(log_level)) {
        args <- c(args, sprintf('log_level = "%s"', log_level))
    }

    fit_call <- sprintf("invisible(mod$fit(%s))", paste(args, collapse = ", "))

    writeLines(
        c(
            'Sys.setenv(NOT_CRAN = "true")',
                        sprintf("devtools::load_all(%s, quiet = TRUE)", shQuote(pkg_root)),
                        sprintf("run_path <- %s", shQuote(run_path)),
            "dir.create(run_path, recursive = TRUE, showWarnings = FALSE)",
            "mod <- PM_model$new(",
            "  pri = list(ke = ab(0.1, 1.0), v = ab(5, 20)),",
            "  eqn = function() { dx[1] <- -ke * x[1] + b[1] },",
            "  out = function() { y[1] <- x[1] / v },",
            "  err = list(additive(1.25, c(0.1, 0, 0, 0), fixed = TRUE)),",
            "  compile = FALSE",
            ")",
            "dat <- PM_data$new(",
            "  data.frame(",
            '    id = c("1", "1", "1"),',
            "    time = c(0, 1, 2),",
            "    evid = c(1L, 0L, 0L),",
            "    dose = c(100, NA_real_, NA_real_),",
            "    dur = c(0, NA_real_, NA_real_),",
            "    input = c(1L, NA_integer_, NA_integer_),",
            "    out = c(NA_real_, 10, 9),",
            "    outeq = c(NA_integer_, 1L, 1L)",
            "  ),",
            "  quiet = TRUE",
            ")",
            fit_call
        ),
        script_path
    )

    output <- system2(
        file.path(R.home("bin"), "Rscript"),
        script_path,
        stdout = TRUE,
        stderr = TRUE,
        env = c("NOT_CRAN=true")
    )

    log_path <- file.path(run_path, "1", "outputs", "log.txt")
    list(
        output = output,
        log_path = log_path,
        log_exists = file.exists(log_path),
        log_lines = if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()
    )
}

contains_line <- function(lines, text) {
    any(grepl(text, lines, fixed = TRUE))
}

testthat::test_that("fit does not write log.txt by default", {
    result <- run_log_control_fit()

    testthat::expect_false(result$log_exists)
})

testthat::test_that("fit writes log.txt when write_logs = TRUE", {
    result <- run_log_control_fit(write_logs = TRUE)

    testthat::expect_true(result$log_exists)
    testthat::expect_true(contains_line(result$log_lines, "Objective function ="))
})

testthat::test_that("fit suppresses tracing console logs when stdout_logs = FALSE", {
    result <- run_log_control_fit(stdout_logs = FALSE)

    testthat::expect_false(contains_line(result$output, "Objective function ="))
    testthat::expect_false(contains_line(result$output, "Maximum number of cycles reached"))
})

testthat::test_that("fit keeps tracing console logs enabled by default", {
    result <- run_log_control_fit()

    testthat::expect_true(contains_line(result$output, "Objective function ="))
})

testthat::test_that('fit respects log_level = "WARN" on the console path', {
    result <- run_log_control_fit(log_level = "WARN")

    testthat::expect_false(contains_line(result$output, "Objective function ="))
    testthat::expect_true(contains_line(result$output, "Maximum number of cycles reached"))
})

testthat::test_that("fit keeps INFO console logs enabled by default log_level", {
    result <- run_log_control_fit()

    testthat::expect_true(contains_line(result$output, "Objective function ="))
    testthat::expect_true(contains_line(result$output, "Maximum number of cycles reached"))
})
