# Integration tests for end-to-end workflows
# Verifies the full data → model → fit → result pipeline.

test_that("Full workflow: create data + model + fit check", {
  # 1. Load data
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)
  expect_equal(exData$summary()$nsub, 20)

  # 2. Create model
  mod <- PM_model$new(
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
  expect_s3_class(mod, "PM_model")

  # 3. Combine into fit and check compatibility
  fit <- PM_fit$new(data = exData, model = mod)
  expect_s3_class(fit, "PM_fit")
  expect_no_error(fit$check())
})

test_that("NPex result exploration: all components accessible", {
  data(NPex, envir = environment())

  # Sub-objects are the right types
  expect_s3_class(NPex$data, "PM_data")
  expect_s3_class(NPex$model, "PM_model")
  expect_s3_class(NPex$final, "PM_final")
  expect_s3_class(NPex$op, "PM_op")
  expect_s3_class(NPex$cycle, "PM_cycle")
  expect_s3_class(NPex$pop, "PM_pop")
  expect_s3_class(NPex$post, "PM_post")
  expect_s3_class(NPex$cov, "PM_cov")

  # Key numerical results are present and well-formed
  expect_true(is.data.frame(NPex$final$popMean))
  expect_true(ncol(NPex$final$popMean) > 0)
  expect_true(is.data.frame(NPex$cycle$objective))
  expect_true(nrow(NPex$cycle$objective) > 0)

  # OP data matches expectations
  op <- NPex$op$data
  expect_true(nrow(op) > 0)
  expect_true(all(c("obs", "pred") %in% tolower(names(op))))
})
