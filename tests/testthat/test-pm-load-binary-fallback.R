library(Pmetrics)

# The compiled-binary workflow (.pmx files, `binary_path`) has been replaced by
# just-in-time compilation of the pharmsol DSL. These tests verify the
# equivalent behaviour: a loaded run has no binary, but the model carries (or
# regenerates) its DSL source and can be re-fit and re-simulated.

test_that("PM_load works without a compiled binary and the model can be re-fit", {
  local_exa_tmp_cleanup()
  run_root <- tempfile("pmetrics-load-fallback-")
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

  # No compiled binaries are produced; the DSL source is written instead.
  inputs_dir <- file.path(run_root, "1", "inputs")
  expect_equal(length(list.files(inputs_dir, pattern = "\\.pmx$")), 0)
  expect_true(file.exists(file.path(inputs_dir, "model.txt")))

  loaded <- PM_load(path = run_root, run = 1)
  expect_true(inherits(loaded, "PM_result"))
  expect_true(is.null(loaded$model$binary_path))
  # The loaded model carries its DSL source so it can be used without recompiling.
  expect_true(is.character(loaded$model$dsl))

  run_refit <- suppressMessages(
    loaded$model$fit(
      data = loaded$data,
      path = run_root,
      run = 2,
      overwrite = TRUE,
      report = "none",
      quiet = TRUE
    )
  )

  expect_true(inherits(run_refit, "PM_result"))
})

test_that("PM_sim$new works on a loaded run without a compiled binary", {
  local_exa_tmp_cleanup()
  run_root <- tempfile("pmetrics-sim-fallback-")
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

  loaded <- PM_load(path = run_root, run = 1)
  expect_true(is.null(loaded$model$binary_path))

  sim_from_loaded <- suppressMessages(
    PM_sim$new(
      poppar = loaded$final,
      model = loaded$model,
      data = loaded$data,
      include = 1,
      nsim = 1,
      predInt = 1
    )
  )

  expect_true(inherits(sim_from_loaded, "PM_sim"))
})
