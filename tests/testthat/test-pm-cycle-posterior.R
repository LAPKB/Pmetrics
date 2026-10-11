library(Pmetrics)

test_that("a cycles = 0 run creates its cycle object", {
  skip_on_cran()
  local_exa_tmp_cleanup()
  run_root <- withr::local_tempdir(pattern = "pmetrics-cycle0-")
  model <- PM_model$new(x = modEx, compile = FALSE)

  suppressMessages(model$fit(
    data = dataEx, path = run_root, run = 1, cycles = 2,
    overwrite = TRUE, report = "none", quiet = TRUE
  ))
  run2 <- suppressMessages(model$fit(
    data = dataEx, path = run_root, run = 2, cycles = 0, prior = 1,
    overwrite = TRUE, report = "none", quiet = TRUE
  ))

  expect_length(run2$cycle$status, 1)
  expect_equal(run2$cycle$data$objective$cycle, 0)
  expect_true(all(run2$cycle$data$gamlam$cycle == 0))
})
