# Integration tests for PM_sim and PM_pta
# Uses the built-in simEx dataset (a pre-computed simulation result).

# --- PM_sim ---

test_that("simEx loads and has expected data components", {
  data(simEx, envir = environment())

  expect_s3_class(simEx, "PM_sim")
  expect_type(simEx$data, "list")
  expect_true(!is.null(simEx$data$obs))
  expect_true(!is.null(simEx$data$amt))
  expect_true(!is.null(simEx$data$parValues))

  obs <- simEx$data$obs
  expect_true(is.data.frame(obs))
  expect_true(nrow(obs) > 0)
})

test_that("simEx AUC calculation works", {
  data(simEx, envir = environment())

  auc <- simEx$auc()
  expect_true(!is.null(auc))
  expect_true(is.data.frame(auc$tau) || is.data.frame(auc))
})

# --- PM_pta ---

test_that("PM_pta creation from simEx", {
  data(simEx, envir = environment())
  data(mic1, envir = environment())

  target <- makePTAtarget(mic1)
  pta <- simEx$pta(
    target = target,
    success = 0.4,
    target_type = "time"
  )

  expect_s3_class(pta, "PM_pta")
  expect_true(!is.null(pta$data))
})

# --- makeAUC() ---

test_that("makeAUC() with simple data frame", {
  df <- data.frame(
    id = rep(1, 5),
    time = c(0, 1, 2, 4, 8),
    out = c(0, 10, 8, 5, 2)
  )

  auc <- makeAUC(df, out ~ time)
  expect_true(is.data.frame(auc))
  expect_true("tau" %in% names(auc))
  expect_true(auc$tau > 0)
})

test_that("makeAUC() with multiple subjects", {
  df <- data.frame(
    id = c(rep(1, 5), rep(2, 5)),
    time = rep(c(0, 1, 2, 4, 8), 2),
    out = c(0, 10, 8, 5, 2, 0, 15, 12, 7, 3)
  )

  auc <- makeAUC(df, out ~ time)
  expect_equal(nrow(auc), 2)
  expect_true(all(auc$tau > 0))
})

test_that("makeAUC() respects start and end times", {
  df <- data.frame(
    id = rep(1, 5),
    time = c(0, 1, 2, 4, 8),
    out = c(0, 10, 8, 5, 2)
  )

  auc_full <- makeAUC(df, out ~ time)
  auc_partial <- makeAUC(df, out ~ time, start = 1, end = 4)

  expect_true(auc_partial$tau < auc_full$tau)
  expect_true(auc_partial$tau > 0)
})
