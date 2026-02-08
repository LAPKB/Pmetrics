# Integration tests for helper/constructor functions
# These verify the fundamental building blocks of Pmetrics models.

# --- ab() and msd() parameter constructors ---

test_that("ab() creates correct PM_pri object", {
  p <- ab(0.1, 0.9)
  expect_s3_class(p, "PM_pri")
  expect_equal(p$min, 0.1)
  expect_equal(p$max, 0.9)
  expect_equal(p$mean, 0.5)
  expect_equal(p$sd, (0.9 - 0.1) / 6)
})

test_that("msd() creates correct PM_pri object", {
  p <- msd(5, 1)
  expect_s3_class(p, "PM_pri")
  expect_equal(p$min, 2)
  expect_equal(p$max, 8)
  expect_equal(p$mean, 5)
  expect_equal(p$sd, 1)
})

# --- interp() ---

test_that("interp() returns correct values", {
  expect_equal(interp(), 1)
  expect_equal(interp("lm"), 1)
  expect_equal(interp("none"), 0)
})

# --- proportional() and additive() error models ---

test_that("proportional() creates correct PM_err object", {
  err <- proportional(5, c(0.1, 0.15, 0, 0))
  expect_s3_class(err, "PM_err")
  expect_equal(err$type, "proportional")
  expect_equal(err$initial, 5)
  expect_equal(err$coeff, c(0.1, 0.15, 0, 0))
  expect_false(err$fixed)
})

test_that("additive() creates correct PM_err object", {
  err <- additive(2, c(0.2, 0.25, 0, 0))
  expect_s3_class(err, "PM_err")
  expect_equal(err$type, "additive")
  expect_equal(err$initial, 2)
  expect_equal(err$coeff, c(0.2, 0.25, 0, 0))
  expect_false(err$fixed)
})

# --- cor2cov() ---

test_that("cor2cov() converts correlation matrix to covariance matrix", {
  cor_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  sd_vec <- c(2, 3)
  cov_mat <- cor2cov(cor_mat, sd_vec)

  expect_equal(dim(cov_mat), c(2, 2))
  expect_equal(cov_mat[1, 1], 4)   # 2^2
  expect_equal(cov_mat[2, 2], 9)   # 3^2
  expect_equal(cov_mat[1, 2], 3)   # 0.5 * 2 * 3
  expect_equal(cov_mat[2, 1], 3)   # symmetric
})

# --- makePTAtarget() ---

test_that("makePTAtarget() creates target from data", {
  data(mic1, envir = environment())
  target <- makePTAtarget(mic1)

  expect_s3_class(target, "PMpta.targ")
  expect_s3_class(target, "data.frame")
  expect_true(nrow(target) > 0)
  expect_true("target" %in% names(target))
  expect_true("n" %in% names(target))
})
