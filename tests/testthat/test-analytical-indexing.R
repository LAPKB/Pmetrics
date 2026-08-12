testthat::skip_on_cran()

one_comp_iv <- NULL

build_one_comp_iv_analytical_model <- function(compile = FALSE) {
  lib_entry <- getFromNamespace("get_model_library_entry", "Pmetrics")("one_comp_iv")

  PM_model$new(
    pri = as.list(lib_entry$arg_list$pri),
    eqn = function() {
      one_comp_iv
    },
    out = lib_entry$arg_list$out,
    err = as.list(lib_entry$arg_list$err),
    compile = compile
  )
}

make_one_comp_iv_fit_data <- function() {
  PM_data$new(
    data.frame(
      id = c("1", "1", "1"),
      time = c(0, 1, 2),
      evid = c(1L, 0L, 0L),
      dose = c(100, NA_real_, NA_real_),
      dur = c(1, NA_real_, NA_real_),
      input = c(1L, NA_integer_, NA_integer_),
      out = c(NA_real_, 10, 9),
      outeq = c(NA_integer_, 1L, 1L)
    ),
    quiet = TRUE
  )
}

test_that("Analytical generation preserves 1-based indices", {
  mod <- build_one_comp_iv_analytical_model(compile = FALSE)
  mod$compile(quiet = TRUE)
  dsl <- mod$dsl

  testthat::expect_equal(mod$model_list$n_out, 1)
  testthat::expect_match(dsl, "kind = analytical")
  testthat::expect_match(dsl, "structure = one_compartment")
  testthat::expect_match(dsl, "out\\(outeq_1\\)")
  # Outputs are 1-based to match the Pmetrics data OUTEQ column.
  testthat::expect_false(grepl("outeq_0", dsl, fixed = TRUE))
})

test_that("Analytical fit runs one NPAG cycle with y[1] and one error model", {
  mod <- build_one_comp_iv_analytical_model(compile = FALSE)
  dat <- make_one_comp_iv_fit_data()
  run_path <- withr::local_tempdir()

  res <- mod$fit(
    data = dat,
    path = run_path,
    cycles = 1,
    points = 20,
    report = "none",
    quiet = TRUE
  )

  testthat::expect_s3_class(res, "PM_result")
  testthat::expect_true(file.exists(file.path(run_path, "1", "outputs", "PMout.Rdata")))
})

test_that("Analytical fit records gamma/lambda for the fitted error model", {
  mod <- build_one_comp_iv_analytical_model(compile = FALSE)
  dat <- make_one_comp_iv_fit_data()
  run_path <- withr::local_tempdir()

  res <- mod$fit(
    data = dat,
    path = run_path,
    cycles = 1,
    points = 20,
    report = "none",
    quiet = TRUE
  )

  cycles_file <- file.path(run_path, "1", "outputs", "cycles.csv")
  testthat::expect_true(file.exists(cycles_file))

  # The engine only writes gamlam columns when the error model is bound to an
  # output slot, so a missing column means the model was silently dropped.
  cycles_raw <- readr::read_csv(cycles_file, show_col_types = FALSE)
  testthat::expect_true(any(startsWith(names(cycles_raw), "gamlam")))

  gamlam <- res$cycle$gamlam
  testthat::expect_true(tibble::is_tibble(gamlam))
  testthat::expect_gt(nrow(gamlam), 0)
  testthat::expect_true(all(c("cycle", "value", "outeq", "type") %in% names(gamlam)))
  testthat::expect_equal(unique(gamlam$outeq), 1)
  testthat::expect_true(all(is.finite(gamlam$value) & gamlam$value > 0))
})

make_two_output_error_model <- function(outeq_1, outeq_2) {
  PM_model$new(
    pri = list(ke = ab(0.01, 3), v = ab(1, 200)),
    eqn = function() {
      dX[1] <- R[1] - ke * X[1]
    },
    out = function() {
      Y[1] <- X[1] / v
      Y[2] <- X[1] / (2 * v)
    },
    # Declared out of order so a positional fallback cannot accidentally agree.
    err = list(
      proportional(3, c(0.1, 0.1, 0, 0), outeq = outeq_2),
      additive(0.4, c(0.2, 0.05, 0, 0), outeq = outeq_1)
    ),
    solver = "TSIT45"
  )
}

make_two_output_fit_data <- function() {
  PM_data$new(
    data.frame(
      id = rep(c("1", "2"), each = 5),
      time = rep(c(0, 1, 1, 4, 4), 2),
      evid = rep(c(1L, 0L, 0L, 0L, 0L), 2),
      dose = rep(c(100, NA_real_, NA_real_, NA_real_, NA_real_), 2),
      dur = rep(c(0.5, NA_real_, NA_real_, NA_real_, NA_real_), 2),
      input = rep(c(1L, NA_integer_, NA_integer_, NA_integer_, NA_integer_), 2),
      out = c(NA_real_, 8, 4, 5, 2.5, NA_real_, 9, 4.5, 6, 3),
      outeq = rep(c(NA_integer_, 1L, 2L, 1L, 2L), 2)
    ),
    quiet = TRUE
  )
}

test_that("Error models bind to the declared outeq regardless of R numeric type", {
  dat <- make_two_output_fit_data()

  fit_gamlam <- function(mod) {
    res <- mod$fit(
      data = dat,
      path = withr::local_tempdir(),
      cycles = 1,
      points = 20,
      report = "none",
      quiet = TRUE
    )
    res$cycle$gamlam[order(res$cycle$gamlam$outeq), ]
  }

  # `2L` is an integer vector, which is not readable as an R double. Reading it
  # as absent silently rebinds each error model to its positional slot instead.
  by_double <- fit_gamlam(make_two_output_error_model(1, 2))
  by_integer <- fit_gamlam(make_two_output_error_model(1L, 2L))

  testthat::expect_equal(by_integer$outeq, c(1, 2))
  testthat::expect_equal(by_integer$type, c("Additive", "Proportional"))
  testthat::expect_equal(by_integer$value, by_double$value)
})

test_that("An output equation without an error model is rejected", {
  mod <- suppressMessages(PM_model$new(
    pri = list(ke = ab(0.01, 3), v = ab(1, 200)),
    eqn = function() {
      dX[1] <- R[1] - ke * X[1]
    },
    out = function() {
      Y[1] <- X[1] / v
      Y[2] <- X[1] / (2 * v)
    },
    err = list(additive(0.4, c(0.2, 0.05, 0, 0))),
    solver = "TSIT45"
  ))

  testthat::expect_error(
    mod$fit(
      data = make_two_output_fit_data(),
      path = withr::local_tempdir(),
      cycles = 1,
      points = 20,
      report = "none",
      quiet = TRUE
    ),
    "error models cover only"
  )
})
