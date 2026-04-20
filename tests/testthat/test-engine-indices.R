library(Pmetrics)

test_that("decode_error_model_rows aligns observed outeq when models include leading placeholder", {
  models <- list(
    "None",
    list(Proportional = list(poly = list(c0 = 0, c1 = 0.1, c2 = 0, c3 = 0)))
  )

  decoded <- decode_error_model_rows(models, observed_outeq = c(1, 1, 1))

  expect_equal(nrow(decoded), 1)
  expect_equal(decoded$outeq, 1)
  expect_equal(decoded$type, "Proportional")
  expect_equal(decoded$c1, 0.1)
})

test_that("decode_error_model_rows keeps direct alignment when no placeholder exists", {
  models <- list(
    list(Additive = list(poly = list(c0 = 0.5, c1 = 0, c2 = 0, c3 = 0))),
    list(Proportional = list(poly = list(c0 = 0, c1 = 0.2, c2 = 0, c3 = 0)))
  )

  decoded <- decode_error_model_rows(models, observed_outeq = c(1, 2))

  expect_equal(nrow(decoded), 2)
  expect_equal(decoded$outeq, c(1, 2))
  expect_equal(decoded$type, c("Additive", "Proportional"))
  expect_equal(decoded$c0[1], 0.5)
  expect_equal(decoded$c1[2], 0.2)
})
