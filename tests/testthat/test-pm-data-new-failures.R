test_that("PM_data$new fails for missing or unreadable file paths", {
  expect_error(
    PM_data$new(data = "no_such_file_abc.csv"),
    "Unable to create.*PM_data"
  )

  expect_error(
    PM_data$new(data = "tests/testthat"),
    "Unable to create.*PM_data"
  )
})

test_that("PM_data$new fails when mandatory columns are missing", {
  complete <- data.frame(
    id = c(1, 1),
    time = c(0, 1),
    dose = c(100, NA),
    out = c(NA, 5)
  )

  expect_error(
    PM_data$new(data = complete[, c("id", "time", "dose")], validate = TRUE),
    "missing these mandatory columns: out"
  )

  expect_error(
    PM_data$new(data = complete[, c("id", "time", "out")], validate = TRUE),
    "missing these mandatory columns: dose"
  )

  expect_error(
    PM_data$new(data = complete[, c("id", "dose", "out")], validate = TRUE),
    "missing these mandatory columns: time"
  )

  expect_error(
    PM_data$new(data = complete[, c("time", "dose", "out")], validate = TRUE),
    "missing these mandatory columns: id"
  )
})

test_that("PM_data$new fails for non-tabular input when validate is TRUE", {
  expect_error(
    PM_data$new(data = 1:3, validate = TRUE),
    "missing these mandatory columns"
  )
})

test_that("PM_data$new fails when date/time cannot be parsed", {
  bad_dt <- data.frame(
    id = c(1, 1),
    date = c("notadate", "stillbad"),
    time = c("notatime", "stillbad"),
    dose = c(100, NA),
    out = c(NA, 5)
  )

  expect_error(
    suppressWarnings(PM_data$new(data = bad_dt, validate = TRUE)),
    "All dates/times failed to parse"
  )
})
