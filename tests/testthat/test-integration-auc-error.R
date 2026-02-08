# Integration tests for makeErrorPoly() and makeAUC()

# --- makeErrorPoly() ---

test_that("makeErrorPoly() fits polynomials to obs/sd pairs", {
  obs <- c(0, 5, 50, 100, 250, 500, 1000)
  sd_vals <- c(1, 0.4, 4.5, 12, 34, 60, 190)

  result <- suppressWarnings(
    withr::with_pdf(tempfile(), makeErrorPoly(obs = obs, sd = sd_vals))
  )

  expect_type(result, "list")
  expect_true("first" %in% names(result))
  expect_true(length(result$first) == 2)  # C0, C1
  expect_true("second" %in% names(result))
  expect_true("third" %in% names(result))
})

# --- makeAUC() analytical accuracy ---

test_that("makeAUC() trapezoidal matches analytical for smooth curve", {
  time <- seq(0, 24, by = 0.5)
  conc <- 100 * exp(-0.1 * time)
  df <- data.frame(id = 1, time = time, out = conc)

  auc <- makeAUC(df, out ~ time)
  # Analytical: C0/k * (1 - exp(-k*24)) = 1000 * (1 - exp(-2.4)) ≈ 909.3
  expected_auc <- 100 / 0.1 * (1 - exp(-0.1 * 24))
  expect_true(abs(auc$tau - expected_auc) / expected_auc < 0.01)
})

test_that("makeAUC() include/exclude subjects", {
  df <- data.frame(
    id = c(rep(1, 5), rep(2, 5), rep(3, 5)),
    time = rep(c(0, 1, 2, 4, 8), 3),
    out = c(0, 10, 8, 5, 2, 0, 15, 12, 7, 3, 0, 20, 16, 10, 4)
  )

  auc_include <- makeAUC(df, out ~ time, include = c(1, 2))
  expect_equal(nrow(auc_include), 2)

  auc_exclude <- makeAUC(df, out ~ time, exclude = 3)
  expect_equal(nrow(auc_exclude), 2)
})

# --- makeAUC() with NPex results ---

test_that("makeAUC() works with NPex sub-objects", {
  data(NPex, envir = environment())

  auc_op <- NPex$op$auc()
  expect_true(is.data.frame(auc_op))

  auc_pop <- NPex$pop$auc()
  expect_true(is.data.frame(auc_pop))

  auc_post <- NPex$post$auc()
  expect_true(is.data.frame(auc_post))
})
