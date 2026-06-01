testthat::skip_on_cran()

testthat::skip_if_not(
  is_cargo_installed(),
  message = "Cargo is required to compile analytical indexing tests."
)

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
  dsl <- mod$dsl()

  testthat::expect_equal(mod$model_list$n_out, 1)
  testthat::expect_match(dsl, "kind = analytical")
  testthat::expect_match(dsl, "structure = one_compartment")
  testthat::expect_match(dsl, "outputs = outeq_1")
  testthat::expect_match(dsl, "out\\(outeq_1\\)")
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
