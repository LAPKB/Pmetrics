# Integration tests for PM_result (NPex) and its sub-objects
# Uses the built-in NPex dataset (a pre-computed NPAG result) to verify
# that all result components are accessible and structurally correct.

test_that("NPex loads as PM_result with all components", {
  data(NPex, envir = environment())

  expect_s3_class(NPex, "PM_result")
  expect_s3_class(NPex$final, "PM_final")
  expect_s3_class(NPex$op, "PM_op")
  expect_s3_class(NPex$cycle, "PM_cycle")
  expect_s3_class(NPex$pop, "PM_pop")
  expect_s3_class(NPex$post, "PM_post")
  expect_s3_class(NPex$cov, "PM_cov")
  expect_s3_class(NPex$data, "PM_data")
  expect_s3_class(NPex$model, "PM_model")
})

# --- PM_final ---

test_that("PM_final has population and posterior statistics", {
  data(NPex, envir = environment())
  final <- NPex$final

  # Subject count
  expect_equal(final$nsub, 20)

  # Population support points
  expect_true(is.data.frame(final$popPoints))
  expect_true(nrow(final$popPoints) > 0)
  expect_true("prob" %in% names(final$popPoints))

  # Population statistics are data frames with parameter columns
  expect_true(is.data.frame(final$popMean))
  expect_true(is.data.frame(final$popSD))
  expect_true(is.data.frame(final$popMed))
  expect_true(ncol(final$popMean) > 0)

  # Covariance and correlation matrices
  expect_true(!is.null(final$popCov))
  expect_true(!is.null(final$popCor))

  # Posterior statistics
  expect_true(is.data.frame(final$postMean))
  expect_true(is.data.frame(final$postSD))
  expect_true(is.data.frame(final$postMed))
  expect_true(is.data.frame(final$shrinkage))
})

# --- PM_op ---

test_that("PM_op has observed-predicted data with correct structure", {
  data(NPex, envir = environment())
  op_data <- NPex$op$data

  expect_true(is.data.frame(op_data))
  expect_true(nrow(op_data) > 0)

  col_names <- tolower(names(op_data))
  expect_true(all(c("id", "time", "obs", "pred") %in% col_names))
})

# --- PM_cycle ---

test_that("PM_cycle tracks convergence across cycles", {
  data(NPex, envir = environment())
  obj <- NPex$cycle$objective

  expect_true(is.data.frame(obj))
  expect_true(nrow(obj) > 0)
  expect_true("cycle" %in% tolower(names(obj)))
  expect_true("neg2ll" %in% tolower(names(obj)))
})

# --- PM_pop and PM_post ---

test_that("PM_pop and PM_post have prediction data", {
  data(NPex, envir = environment())

  # Population predictions
  pop_data <- NPex$pop$data
  expect_true(is.data.frame(pop_data))
  expect_true(nrow(pop_data) > 0)
  expect_true(all(c("id", "time", "pred") %in% tolower(names(pop_data))))

  # Posterior predictions
  post_data <- NPex$post$data
  expect_true(is.data.frame(post_data))
  expect_true(nrow(post_data) > 0)
  expect_true(all(c("id", "time", "pred") %in% tolower(names(post_data))))
})

# --- PM_cov ---

test_that("PM_cov has covariate data", {
  data(NPex, envir = environment())

  cov_data <- NPex$cov$data
  expect_true(is.data.frame(cov_data))
  expect_true(nrow(cov_data) > 0)
  expect_true("id" %in% tolower(names(cov_data)))
})

# --- AUC from results ---

test_that("AUC can be computed from NPex result sub-objects", {
  data(NPex, envir = environment())

  auc_op <- NPex$op$auc()
  expect_true(is.data.frame(auc_op))
  expect_true(all(auc_op$tau > 0))

  auc_pop <- NPex$pop$auc()
  expect_true(is.data.frame(auc_pop))
  expect_true(all(auc_pop$tau > 0))

  auc_post <- NPex$post$auc()
  expect_true(is.data.frame(auc_post))
  expect_true(all(auc_post$tau > 0))
})
