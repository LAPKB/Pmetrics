library(Pmetrics)

test_that("PM_load tolerates missing inputs binary and fit recompiles", {
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

  inputs_dir <- file.path(run_root, "1", "inputs")
  pmx_files <- list.files(inputs_dir, pattern = "\\.pmx$", full.names = TRUE)
  expect_gt(length(pmx_files), 0)

  backup_paths <- paste0(pmx_files, ".bak")
  file.rename(pmx_files, backup_paths)

  loaded_missing <- PM_load(path = run_root, run = 1)
  expect_true(inherits(loaded_missing, "PM_result"))
  expect_true(is.null(loaded_missing$model$binary_path))

  run_recompiled <- suppressMessages(
    loaded_missing$model$fit(
      data = loaded_missing$data,
      path = run_root,
      run = 2,
      overwrite = TRUE,
      report = "none",
      quiet = TRUE
    )
  )

  expect_true(is.character(run_recompiled$model$binary_path))
  expect_true(file.exists(run_recompiled$model$binary_path))
})

test_that("PM_sim$new recompiles when PM_load has no inputs binary", {
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

  inputs_dir <- file.path(run_root, "1", "inputs")
  pmx_files <- list.files(inputs_dir, pattern = "\\.pmx$", full.names = TRUE)
  expect_gt(length(pmx_files), 0)

  file.rename(pmx_files, paste0(pmx_files, ".bak"))

  loaded_missing <- PM_load(path = run_root, run = 1)
  expect_true(is.null(loaded_missing$model$binary_path))

  sim_from_missing <- suppressMessages(
    PM_sim$new(
      poppar = loaded_missing$final,
      model = loaded_missing$model,
      data = loaded_missing$data,
      include = 1,
      nsim = 1,
      predInt = 1
    )
  )

  expect_true(inherits(sim_from_missing, "PM_sim"))
  # PM_model is R6 (reference semantics), so compile() inside PM_sim$new updates
  # loaded_missing$model$binary_path in place
  expect_true(is.character(loaded_missing$model$binary_path))
  expect_true(file.exists(loaded_missing$model$binary_path))
})
