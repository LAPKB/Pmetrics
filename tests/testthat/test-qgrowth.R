test_that("qgrowth returns full sex-by-percentile combinations", {
  out <- qgrowth(
    sex = "B",
    agemos = c(12, 24),
    percentile = c(10, 50, 90)
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2 * 2 * 3)
  expect_setequal(unique(out$sex), c("M", "F"))
  expect_setequal(unique(out$percentile), c(10, 50, 90))
  expect_setequal(out$agemos, c(12, 24))
  expect_true(all(c("wt", "ht", "bmi") %in% names(out)))
})

test_that("qgrowth keeps default shape and columns", {
  out <- qgrowth()

  expect_true(is.data.frame(out))
  expect_setequal(names(out), c("agemos", "ageyrs", "wt", "ht", "bmi", "sex", "percentile"))
  expect_true(all(out$bmi > 0))
})
