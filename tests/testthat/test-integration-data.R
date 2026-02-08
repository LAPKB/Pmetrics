# Integration tests for PM_data class
# Verifies data loading, validation, summarization, and round-tripping.

test_that("PM_data loads from CSV and summarizes correctly", {
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)

  expect_s3_class(exData, "PM_data")
  expect_true(is.data.frame(exData$data))
  expect_true(is.data.frame(exData$standard_data))

  summary_info <- exData$summary()
  expect_equal(summary_info$nsub, 20)
  expect_equal(summary_info$ndrug, 1)
  expect_equal(summary_info$numeqt, 1)
  expect_equal(summary_info$nobsXouteq[[1]], 139)
  expect_true("wt" %in% tolower(summary_info$covnames))
  expect_true("age" %in% tolower(summary_info$covnames))
})

test_that("PM_data save/reload round-trip preserves data", {
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)

  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile), add = TRUE)

  exData$save(tmpfile)
  expect_true(file.exists(tmpfile))

  reloaded <- PM_data$new(data = tmpfile, quiet = TRUE)
  expect_equal(nrow(reloaded$standard_data), nrow(exData$standard_data))
})

test_that("PM_data can be created from a data.frame", {
  exData <- PM_data$new(data = "ex.csv", quiet = TRUE)
  df <- exData$standard_data

  newData <- PM_data$new(data = df, quiet = TRUE)
  expect_s3_class(newData, "PM_data")
  expect_equal(nrow(newData$standard_data), nrow(exData$standard_data))
})

# --- Built-in datasets ---

test_that("Built-in datasets load correctly", {
  data(dataEx, envir = environment())
  expect_s3_class(dataEx, "PM_data")
  expect_equal(dataEx$summary()$nsub, 20)

  data(modEx, envir = environment())
  expect_s3_class(modEx, "PM_model")
  expect_true(!is.null(modEx$arg_list$pri))

  data(badData, envir = environment())
  expect_s3_class(badData, "PM_data")
})
