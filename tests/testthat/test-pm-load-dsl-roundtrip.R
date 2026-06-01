library(Pmetrics)

test_that("PM_load reopens runs from saved DSL input", {
  run_root <- tempfile("pmetrics-load-dsl-")
  dir.create(run_root, recursive = TRUE)

  model_local <- PM_model$new(x = modEx, compile = FALSE)
  run_initial <- suppressMessages(
    model_local$fit(
      data = dataEx,
      path = run_root,
      run = 1,
      overwrite = TRUE,
      report = "none",
      quiet = TRUE
    )
  )

  expect_true(inherits(run_initial, "PM_result"))

  inputs_dir <- file.path(run_root, "1", "inputs")
  dsl_path <- file.path(inputs_dir, "model.dsl")
  expect_true(file.exists(dsl_path))

  expect_no_warning(loaded <- PM_load(path = run_root, run = 1))
  expect_true(inherits(loaded, "PM_result"))
  expect_s3_class(loaded$op, "PM_op")
  expect_s3_class(loaded$pop, "PM_pop")
  expect_s3_class(loaded$post, "PM_post")
  expect_s3_class(loaded$cycle, "PM_cycle")
  expect_equal(
    trimws(as.character(loaded$model$dsl())),
    trimws(paste(readLines(dsl_path, warn = FALSE), collapse = "\n"))
  )
})

test_that("PM_sim$new works from PM_load using saved DSL input", {
  run_root <- tempfile("pmetrics-sim-dsl-")
  dir.create(run_root, recursive = TRUE)

  model_local <- PM_model$new(x = modEx, compile = FALSE)
  run_initial <- suppressMessages(
    model_local$fit(
      data = dataEx,
      path = run_root,
      run = 1,
      overwrite = TRUE,
      report = "none",
      quiet = TRUE
    )
  )

  expect_true(inherits(run_initial, "PM_result"))

  inputs_dir <- file.path(run_root, "1", "inputs")
  expect_true(file.exists(file.path(inputs_dir, "model.dsl")))

  expect_no_warning(loaded <- PM_load(path = run_root, run = 1))

  sim_from_missing <- suppressMessages(
    PM_sim$new(
      poppar = loaded$final,
      model = loaded$model,
      data = loaded$data,
      include = 1,
      nsim = 1,
      predInt = 1
    )
  )

  expect_true(inherits(sim_from_missing, "PM_sim"))
})
