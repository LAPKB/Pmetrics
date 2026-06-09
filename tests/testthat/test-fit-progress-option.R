testthat::skip_on_cran()

testthat::skip_if_not(
    is_cargo_installed(),
    message = "Cargo is required to run fit progress option tests."
)

testthat::skip_if_not_installed("devtools")

run_progress_fit <- function(progress = NULL) {
    pkg_root <- normalizePath(".")
    temp_dir <- withr::local_tempdir(pattern = "fit-progress-script-")
    script_path <- file.path(temp_dir, "run-progress-fit.R")
    run_path <- file.path(temp_dir, "run")

    fit_call <- if (is.null(progress)) {
        "invisible(mod$fit(data = dat, path = run_path, cycles = 1, points = 20, report = \"none\", quiet = TRUE))"
    } else {
        sprintf(
            "invisible(mod$fit(data = dat, path = run_path, cycles = 1, points = 20, progress = %s, report = \"none\", quiet = TRUE))",
            if (isTRUE(progress)) "TRUE" else "FALSE"
        )
    }

    load_cmd <- if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
        sprintf("devtools::load_all(%s, quiet = TRUE)", shQuote(pkg_root))
    } else {
        'library("Pmetrics")'
    }

    writeLines(
        c(
            'Sys.setenv(NOT_CRAN = "true")',
                        load_cmd,
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

    system2(
        file.path(R.home("bin"), "Rscript"),
        script_path,
        stdout = TRUE,
        stderr = TRUE,
        env = c("NOT_CRAN=true")
    )
}

has_matrix_progress <- function(output) {
    any(grepl("Computing log-likelihood matrix:", output, fixed = TRUE))
}

testthat::test_that("fit suppresses likelihood matrix progress when progress = FALSE", {
    output <- run_progress_fit(FALSE)

    testthat::expect_false(has_matrix_progress(output))
})

testthat::test_that("fit keeps likelihood matrix progress enabled by default", {
    output <- run_progress_fit()

    testthat::expect_true(has_matrix_progress(output))
})
