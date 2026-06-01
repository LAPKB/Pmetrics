testthat::skip_on_cran()

testthat::skip_if_not(
    is_cargo_installed(),
    message = "Cargo is required to run fixed error model fit tests."
)

build_fixed_error_fit_model <- function(err_model) {
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
        err = list(err_model),
        compile = FALSE
    )
}

make_fixed_error_fit_data <- function() {
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

write_fixed_error_model_file <- function(path, gamlam_line) {
    writeLines(
        c(
            "#pri",
            "Ke, 0.1, 1.0",
            "V, 5, 20",
            "",
            "#cov",
            "WT",
            "",
            "#eqn",
            "dX[1] = B[1] - Ke * X[1]",
            "",
            "#out",
            "Y[1] = X[1] / V",
            "",
            "#err",
            gamlam_line,
            "0.1, 0, 0, 0"
        ),
        path
    )

    invisible(path)
}

test_that("fixed additive error model stays fixed on the fit path", {
    mod <- build_fixed_error_fit_model(additive(1.25, c(0.1, 0, 0, 0), fixed = TRUE))
    dat <- make_fixed_error_fit_data()
    run_path <- withr::local_tempdir(pattern = "fixed-additive-")

    res <- mod$fit(
        data = dat,
        path = run_path,
        cycles = 1,
        points = 20,
        report = "none",
        quiet = TRUE
    )

    gamlam <- res$cycle$gamlam

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_true(isTRUE(mod$model_list$err[[1]]$fixed))
    testthat::expect_equal(mod$model_list$err[[1]]$initial, 1.25)
    testthat::expect_gte(nrow(gamlam), 1)
    testthat::expect_true(file.exists(file.path(run_path, "1", "outputs", "PMout.Rdata")))
})

test_that("fixed proportional error model stays fixed on the fit path", {
    mod <- build_fixed_error_fit_model(proportional(5, c(0.1, 0, 0, 0), fixed = TRUE))
    dat <- make_fixed_error_fit_data()
    run_path <- withr::local_tempdir(pattern = "fixed-proportional-")

    res <- mod$fit(
        data = dat,
        path = run_path,
        cycles = 1,
        points = 20,
        report = "none",
        quiet = TRUE
    )

    gamlam <- res$cycle$gamlam

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_true(isTRUE(mod$model_list$err[[1]]$fixed))
    testthat::expect_equal(mod$model_list$err[[1]]$initial, 5)
    testthat::expect_gte(nrow(gamlam), 1)
    testthat::expect_true(file.exists(file.path(run_path, "1", "outputs", "PMout.Rdata")))
})

test_that("PM_model file import preserves fixed gamma or lambda markers", {
    tmp_dir <- withr::local_tempdir(pattern = "fixed-error-file-")
    gamma_path <- file.path(tmp_dir, "gamma-fixed.txt")
    lambda_path <- file.path(tmp_dir, "lambda-fixed.txt")

    write_fixed_error_model_file(gamma_path, "G=5!")
    write_fixed_error_model_file(lambda_path, "L=1.5!")

    gamma_model <- PM_model$new(x = gamma_path, compile = FALSE)
    lambda_model <- PM_model$new(x = lambda_path, compile = FALSE)

    testthat::expect_true(isTRUE(gamma_model$model_list$err[[1]]$fixed))
    testthat::expect_identical(gamma_model$model_list$err[[1]]$type, "proportional")
    testthat::expect_equal(gamma_model$model_list$err[[1]]$initial, 5)

    testthat::expect_true(isTRUE(lambda_model$model_list$err[[1]]$fixed))
    testthat::expect_identical(lambda_model$model_list$err[[1]]$type, "additive")
    testthat::expect_equal(lambda_model$model_list$err[[1]]$initial, 1.5)
})
