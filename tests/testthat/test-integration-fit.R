# Integration tests for model fitting
# Tests PM_fit backward compatibility and PM_model$fit() (the current API).

# --- PM_fit (backward compatibility) ---

test_that("PM_fit$check() validates model-data compatibility", {
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
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)

  exFit <- PM_fit$new(data = exData, model = mod)
  expect_s3_class(exFit, "PM_fit")
  expect_s3_class(exFit$model, "PM_model")
  expect_s3_class(exFit$data, "PM_data")
  expect_no_error(exFit$check())
})

# --- PM_model$fit() (current API) ---

test_that("PM_model$fit() runs NPAG and returns PM_result", {
  skip_on_cran()
  skip_if_not(is_cargo_installed(), "Cargo not installed")

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
      proportional(5, c(0.02, 0.05, -0.0002, 0))
    )
  )

  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)
  tmpdir <- withr::local_tempdir()

  res <- suppressMessages(
    mod$fit(
      data = exData,
      path = tmpdir,
      cycles = 2,
      include = 1:4,
      overwrite = TRUE
    )
  )

  # Result object
  expect_s3_class(res, "PM_result")

  # All major components present
  expect_s3_class(res$final, "PM_final")
  expect_s3_class(res$op, "PM_op")
  expect_s3_class(res$cycle, "PM_cycle")
  expect_s3_class(res$pop, "PM_pop")
  expect_s3_class(res$post, "PM_post")
  expect_s3_class(res$cov, "PM_cov")
  expect_s3_class(res$data, "PM_data")
  expect_s3_class(res$model, "PM_model")

  # Final has population statistics
  expect_true(is.data.frame(res$final$popMean))
  expect_true(ncol(res$final$popMean) > 0)
  expect_true(is.data.frame(res$final$popPoints))
  expect_true(nrow(res$final$popPoints) > 0)
  expect_equal(res$final$nsub, 4) # included 4 subjects

  # OP has observations and predictions
  expect_true(nrow(res$op$data) > 0)
  expect_true(all(c("id", "time", "obs", "pred") %in% tolower(names(res$op$data))))

  # Cycle objective function tracks convergence
  expect_true(is.data.frame(res$cycle$objective))
  expect_true(nrow(res$cycle$objective) == 2) # 2 cycles

  # AUC can be computed from results
  auc_op <- res$op$auc()
  expect_true(!is.null(auc_op))
  expect_true(is.data.frame(auc_op))
  expect_true(all(auc_op$tau > 0))
})
