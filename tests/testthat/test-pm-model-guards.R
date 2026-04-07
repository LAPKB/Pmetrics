library(Pmetrics)

make_minimal_ode_model <- function(with_bolus = TRUE, with_infusion = TRUE, with_cov = FALSE) {
  if (with_cov) {
    sec_block <- function() {
      cl_eff <- cl * wt
    }
    eqn_block <- if (with_bolus && with_infusion) {
      function() {
        dx[1] <- -(cl_eff / v) * x[1] + b[1] + rateiv[1]
      }
    } else if (with_bolus && !with_infusion) {
      function() {
        dx[1] <- -(cl_eff / v) * x[1] + b[1]
      }
    } else if (!with_bolus && with_infusion) {
      function() {
        dx[1] <- -(cl_eff / v) * x[1] + rateiv[1]
      }
    } else {
      function() {
        dx[1] <- -(cl_eff / v) * x[1]
      }
    }

    PM_model$new(
      pri = list(
        cl = ab(0.5, 1.5),
        v = ab(5, 15)
      ),
      cov = list(wt = interp()),
      sec = sec_block,
      eqn = eqn_block,
      out = function() {
        y[1] <- x[1] / v
      },
      err = list(additive(1, c(0.1, 0, 0, 0))),
      compile = FALSE
    )
  } else {
    eqn_block <- if (with_bolus && with_infusion) {
      function() {
        dx[1] <- -(cl / v) * x[1] + b[1] + rateiv[1]
      }
    } else if (with_bolus && !with_infusion) {
      function() {
        dx[1] <- -(cl / v) * x[1] + b[1]
      }
    } else if (!with_bolus && with_infusion) {
      function() {
        dx[1] <- -(cl / v) * x[1] + rateiv[1]
      }
    } else {
      function() {
        dx[1] <- -(cl / v) * x[1]
      }
    }

    PM_model$new(
      pri = list(
        cl = ab(0.5, 1.5),
        v = ab(5, 15)
      ),
      eqn = eqn_block,
      out = function() {
        y[1] <- x[1] / v
      },
      err = list(additive(1, c(0.1, 0, 0, 0))),
      compile = FALSE
    )
  }
}

make_bolus_data <- function() {
  PM_data$new()$addEvent(
    id = 1,
    time = 0,
    dose = 100
  )$addEvent(
    id = 1,
    time = 2,
    out = -1,
    validate = TRUE
  )
}

make_infusion_data <- function() {
  PM_data$new()$addEvent(
    id = 1,
    time = 0,
    dose = 100,
    dur = 1
  )$addEvent(
    id = 1,
    time = 2,
    out = -1,
    validate = TRUE
  )
}

test_that("PM_model$new validates x type and file existence", {
  expect_error(
    PM_model$new(x = 123),
    "Non supported input"
  )

  expect_error(
    PM_model$new(x = "does_not_exist_abc.txt"),
    "does not exist"
  )
})

test_that("PM_model$new flags reserved names and dynamic ODE indices", {
  reserved <- PM_model$new(
    pri = list(x = ab(1, 2)),
    eqn = function() {
      dx[1] <- -x[1]
    },
    out = function() {
      y[1] <- x[1]
    },
    err = list(additive(1, c(0.1, 0, 0, 0))),
    compile = FALSE
  )

  expect_s3_class(reserved, "PM_model")
  expect_true(is.null(reserved$binary_path))

  dynamic_index <- PM_model$new(
    pri = list(
      ke = ab(0.1, 1),
      v = ab(10, 20)
    ),
    eqn = function() {
      i <- 1
      dx[i] <- -(ke / v) * x[i] + b[1]
    },
    out = function() {
      y[1] <- x[1] / v
    },
    err = list(additive(1, c(0.1, 0, 0, 0))),
    compile = FALSE
  )

  expect_s3_class(dynamic_index, "PM_model")
  expect_true(is.null(dynamic_index$binary_path))
})

test_that("PM_model$map enforces required arguments", {
  mod <- make_minimal_ode_model(with_bolus = TRUE, with_infusion = TRUE)
  dat <- make_bolus_data()

  expect_error(
    mod$map(prior = 1),
    "Data must be specified"
  )

  expect_error(
    mod$map(data = dat, prior = "sobol"),
    "Please specify a non-uniform prior"
  )
})

test_that("PM_model$sim validates data and theta", {
  mod <- make_minimal_ode_model(with_bolus = TRUE, with_infusion = TRUE)
  dat <- make_bolus_data()

  expect_error(
    mod$sim(data = data.frame(id = 1), theta = matrix(1, nrow = 1, ncol = 2)),
    "Data must be a PM_data object"
  )

  expect_error(
    mod$sim(data = dat, theta = c(1, 2)),
    "theta must be a matrix"
  )

  expect_error(
    mod$sim(data = dat, theta = matrix(c("a", "b"), nrow = 1)),
    "theta must be a matrix of numeric values"
  )

  expect_error(
    mod$sim(data = dat, theta = matrix(1, nrow = 2, ncol = 3)),
    "same number of columns as the number of parameters"
  )
})

test_that("PM_model$fit preflight checks prevent malformed runs", {
  bolus_data <- make_bolus_data()
  infusion_data <- make_infusion_data()

  mod_missing_bolus <- make_minimal_ode_model(with_bolus = FALSE, with_infusion = TRUE)
  expect_null(mod_missing_bolus$fit(data = bolus_data, cycles = 1, quiet = TRUE))

  mod_missing_infusion <- make_minimal_ode_model(with_bolus = TRUE, with_infusion = FALSE)
  expect_null(mod_missing_infusion$fit(data = infusion_data, cycles = 1, quiet = TRUE))

  mod_missing_cov <- make_minimal_ode_model(with_bolus = TRUE, with_infusion = TRUE, with_cov = TRUE)
  expect_null(mod_missing_cov$fit(data = bolus_data, cycles = 1, quiet = TRUE))

  mod_negative_cycles <- make_minimal_ode_model(with_bolus = TRUE, with_infusion = TRUE)
  expect_null(mod_negative_cycles$fit(data = bolus_data, cycles = -1, quiet = TRUE))
})
