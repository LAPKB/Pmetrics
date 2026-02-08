# Integration tests for PM_model class
# Verifies model creation, parameter access, covariates, error model,
# compilation, and cloning.

# Helper: standard two-compartment oral model for the rifapentine example data
make_test_model <- function() {
  PM_model$new(
    pri = list(
      Ka = ab(0.1, 0.9),
      Ke = ab(0.001, 0.1),
      V = ab(30, 120),
      Tlag1 = ab(0, 4)
    ),
    cov = list(
      WT = interp(),
      AFRICA = interp(),
      AGE = interp(),
      GENDER = interp(),
      HEIGHT = interp()
    ),
    lag = function() {
      lag[1] = Tlag1
    },
    eqn = function() {
      dx[1] = -Ka * x[1]
      dx[2] = Ka * x[1] - Ke * x[2]
    },
    out = function() {
      y[1] = x[2] / V
    },
    err = list(
      proportional(5, c(0.02, 0.05, -0.0002, 0))
    )
  )
}

test_that("PM_model creation stores parameters and structure", {
  mod <- make_test_model()

  expect_s3_class(mod, "PM_model")

  # Primary parameters are accessible via arg_list
  expect_s3_class(mod$arg_list$pri$Ka, "PM_pri")
  expect_equal(mod$arg_list$pri$Ka$min, 0.1)
  expect_equal(mod$arg_list$pri$Ka$max, 0.9)
  expect_equal(mod$arg_list$pri$Ke$min, 0.001)
  expect_equal(mod$arg_list$pri$V$max, 120)
  expect_equal(mod$arg_list$pri$Tlag1$max, 4)

  # Covariates are declared
  cov_names <- tolower(names(mod$arg_list$cov))
  expect_true(all(c("wt", "africa", "age", "gender", "height") %in% cov_names))

  # Error model
  err <- mod$arg_list$err[[1]]
  expect_s3_class(err, "PM_err")
  expect_equal(err$type, "proportional")
  expect_equal(err$initial, 5)
  expect_equal(err$coeff, c(0.02, 0.05, -0.0002, 0))
})

test_that("PM_model compiles and produces a binary", {
  skip_if_not(is_cargo_installed(), "Cargo not installed")

  mod <- make_test_model()

  # PM_model$new() auto-compiles; binary_path should be set
  expect_true(!is.null(mod$binary_path))
  expect_true(file.exists(mod$binary_path))
})

test_that("PM_model can be cloned from an existing model", {
  mod1 <- make_test_model()
  mod2 <- PM_model$new(mod1)

  expect_s3_class(mod2, "PM_model")
  expect_equal(mod2$arg_list$pri$Ka$min, mod1$arg_list$pri$Ka$min)
  expect_equal(mod2$arg_list$pri$Ka$max, mod1$arg_list$pri$Ka$max)
  expect_equal(mod2$arg_list$pri$V$max, mod1$arg_list$pri$V$max)
})

test_that("PM_model with simpler specification compiles", {
  skip_if_not(is_cargo_installed(), "Cargo not installed")

  # Minimal model: no covariates, no lag
  mod <- PM_model$new(
    pri = list(
      Ka = ab(0.1, 0.9),
      Ke = ab(0.001, 0.1),
      V = ab(30, 120)
    ),
    eqn = function() {
      dx[1] = b[1] - Ka * x[1]
      dx[2] = Ka * x[1] - Ke * x[2]
    },
    out = function() {
      y[1] = x[2] / V
    },
    err = list(
      proportional(5, c(0.1, 0.15, 0, 0))
    )
  )

  expect_s3_class(mod, "PM_model")
  expect_true(!is.null(mod$binary_path))
  expect_true(file.exists(mod$binary_path))
  expect_equal(sort(names(mod$arg_list$pri)), c("Ka", "Ke", "V"))
})
