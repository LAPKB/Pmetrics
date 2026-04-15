test_that("Data object creation", {
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)
  summary <- exData$summary()
  expect_equal(summary$nsub, 20)
  expect_equal(summary$ndrug, 1)
  expect_equal(summary$numeqt, 1)
  expect_equal(summary$nobsXouteq[[1]], 139)
  expect_equal(summary$covnames, c("wt", "africa", "age", "gender", "height"))
})


test_that("PM_data print", {
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)
  expect_no_error(capture.output(exData$print(viewer = FALSE)))
})

test_that("Model object creation", {
  mod1 <- build_example_ode_model(compile = FALSE)

  expect_equal(mod1$model_list$pri$ka, ab(0.1, 1.0))
  expect_equal(mod1$model_list$pri$ke, ab(0.01, 0.5))
  expect_equal(mod1$model_list$pri$v, ab(10, 100))
  expect_equal(mod1$model_list$n_eqn, 2)
  expect_equal(mod1$model_list$n_out, 1)
  expect_equal(mod1$model_list$type, "ODE")
  expect_true(is.null(mod1$binary_path))
})

test_that("Model can be reconstructed from an existing PM_model", {
  mod1 <- build_example_ode_model(compile = FALSE)
  mod2 <- PM_model$new(x = mod1, compile = FALSE)

  expect_s3_class(mod2, "PM_model")
  expect_equal(names(mod2$model_list$pri), c("ka", "ke", "v"))
  expect_equal(mod2$model_list$type, "ODE")
  expect_true(is.null(mod2$binary_path))
})


test_that("Current workflow: PM_model + PM_data + PM_model$fit", {
  skip_on_cran()
  skip_if_not(
    is_cargo_installed(),
    message = "Cargo is required to compile and run PM_model$fit tests."
  )
  local_exa_tmp_cleanup()

  mod1 <- build_example_ode_model(compile = TRUE)
  ex_data <- PM_data$new(data = "ex.csv", quiet = TRUE)

  expect_true(file.exists(mod1$binary_path))

  run_path <- withr::local_tempdir()
  ex_res <- mod1$fit(
    data = ex_data,
    path = run_path,
    cycles = 1,
    points = 20,
    report = "none",
    quiet = TRUE
  )

  expect_s3_class(ex_res, "PM_result")
  expect_true(file.exists(file.path(run_path, "1", "outputs", "PMout.Rdata")))
})

test_that("Analytical models allow multi-line secondary conditionals", {
  mod <- PM_model$new(
    pri = list(
      Ka = ab(0.5, 6),
      Ke = ab(0.1, 1.5),
      v0 = ab(25, 120)
    ),
    cov = list(
      gender = interp("none"),
      wt = interp()
    ),
    sec = function() {
      v = v0 * wt
      if (gender < 1) {
        v = v0 * 0.8
      }
    },
    eqn = function() {
      two_comp_bolus
    },
    lag = function() {
      lag[1] = 0
    },
    out = function() {
      y[1] = x[2] / v
    },
    err = list(
      proportional(5, c(0.1, 0.15, 0, 0))
    ),
    compile = FALSE
  )

  expect_s3_class(mod, "PM_model")
  sec_code <- paste(deparse(mod$arg_list$sec), collapse = "\n")
  expect_true(grepl("if (gender < 1)", sec_code, fixed = TRUE))
  expect_true(grepl("v = v0 * wt", sec_code, fixed = TRUE))
})

# test_that("Load model",{
#   exRes <- PM_load(1)
#   expect_equal(exRes$data$data,PM_data$new(data = "ex.csv", quiet=T)$data, ignore_attr = T)
#   expect_equal(exRes$model$model_list, PM_model$new(list(
#     pri = list(
#       Ka = ab(0.1, 0.9),
#       Ke = ab(0.001, 0.1),
#       V = ab(30, 120),
#       Tlag1 = ab(0, 4)
#     ),
#     cov = list(
#       covariate("WT"),
#       covariate("AFRICA"),
#       covariate("AGE"),
#       covariate("GENDER"),
#       covariate("HEIGHT")),
#     lag = list("TLAG[1] = Tlag1"),
#     out = list(
#       Y1 = list(
#         val = "X[2]/V",
#         err = list(
#           model = proportional(5),
#           assay = errorPoly(c(0.02, 0.05, -0.0002, 0))
#         )
#       )
#     )
#   ))$model_list)
#   expect_true({exRes$success})
#   expect_true(all(class(exRes$cov) == c("PM_cov", "R6")))
#   expect_output(print(exRes$cov$summary()),"20 20   60 59.0      1  31      1    170 0.2160000 0.08366500  34.95000")
#   expect_true(all(class(exRes$op) == c("PM_op", "R6")))
#   expect_output(print(exRes$op$summary()),"Mean weighed squared prediction error: 0.99")
#   expect_true(all(class(exRes$cycle) == c("PM_cycle", "R6")))
#   expect_output(print(exRes$cycle$ll),"440.1974")

# })
