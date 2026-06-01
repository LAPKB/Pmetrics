testthat::skip_on_cran()

testthat::skip_if_not(
    is_cargo_installed(),
    message = "Cargo is required to run fit cache option tests."
)

build_cache_fit_model <- function() {
    ke <- b <- v <- NULL

    PM_model$new(
        pri = list(
            ke = ab(0.1, 1.0),
            v = ab(5, 20)
        ),
        eqn = function() {
            dx[1] <- -ke * x[1] + b[1]
        },
        out = function() {
            y[1] <- x[1] / v
        },
        err = list(additive(1.25, c(0.1, 0, 0, 0), fixed = TRUE)),
        compile = FALSE
    )
}

make_cache_fit_data <- function() {
    PM_data$new(
        data.frame(
            id = c("1", "1", "1"),
            time = c(0, 1, 2),
            evid = c(1L, 0L, 0L),
            dose = c(100, NA_real_, NA_real_),
            dur = c(0, NA_real_, NA_real_),
            input = c(1L, NA_integer_, NA_integer_),
            out = c(NA_real_, 10, 9),
            outeq = c(NA_integer_, 1L, 1L)
        ),
        quiet = TRUE
    )
}

testthat::test_that("fit accepts cache = FALSE on the runtime path", {
    mod <- build_cache_fit_model()
    dat <- make_cache_fit_data()
    run_path <- withr::local_tempdir(pattern = "fit-cache-")

    res <- mod$fit(
        data = dat,
        path = run_path,
        cycles = 1,
        points = 20,
        cache = FALSE,
        report = "none",
        quiet = TRUE
    )

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_true(file.exists(file.path(run_path, "1", "outputs", "PMout.Rdata")))
})

testthat::test_that("fit accepts integer cycles on the runtime path", {
    mod <- build_cache_fit_model()
    dat <- make_cache_fit_data()
    run_path <- withr::local_tempdir(pattern = "fit-cycles-integer-")

    res <- mod$fit(
        data = dat,
        path = run_path,
        cycles = 1L,
        points = 20,
        report = "none",
        quiet = TRUE
    )

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_true(file.exists(file.path(run_path, "1", "outputs", "PMout.Rdata")))
})
