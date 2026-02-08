# Integration tests for qgrowth() and zBMI()
# Pediatric growth chart utilities.

test_that("qgrowth() returns growth data with correct structure", {
  result <- suppressWarnings(qgrowth())
  expect_true(is.data.frame(result))
  expect_true(all(c("agemos", "wt", "ht", "sex") %in% tolower(names(result))))
  # Default includes both sexes
  expect_true("M" %in% result$sex && "F" %in% result$sex)
  expect_true(all(result$wt > 0))
})

test_that("qgrowth() newborn at 50th percentile has reasonable weight", {
  result <- suppressWarnings(qgrowth(agemos = 0, percentile = 50))
  # Newborn at 50th percentile: ~3-4 kg
  expect_true(all(result$wt > 2 & result$wt < 5))
})

test_that("zBMI() returns z-score and percentile", {
  result <- zBMI(agemos = 120, sex = "M", bmi = 16.5)
  expect_type(result, "list")
  expect_true("z" %in% names(result))
  expect_true("per" %in% names(result))
  # Normal BMI for a 10-year-old male: z-score should be near 0
  expect_true(abs(result$z) < 2)
  expect_true(result$per > 0.1 && result$per < 0.9)
})

test_that("zBMI() works with weight and height instead of BMI", {
  # 10-year-old male, ~32 kg, ~137 cm
  result <- zBMI(agemos = 120, sex = "M", wt = 32, ht = 137)
  expect_true(!is.na(result$z))
  expect_true(abs(result$z) < 3)
})
