library(Pmetrics)

write_manifest_fixture <- function(path, model) {
  fit_params <- list(
    ranges = list(ke = c(0.1, 1.0), v = c(1.0, 10.0)),
    error_models = lapply(model$model_list$err, function(x) x$flatten()),
    prior = "sobol",
    points = 20
  )
  payload <- list(
    iterations = list(list(
      cycle = 1,
      converged = FALSE,
      status = "payload",
      neg2ll = 123,
      nspp = 2,
      `gamlam.0` = 0.5,
      `ke.mean` = 2,
      `ke.median` = 2,
      `ke.sd` = 0.25,
      `v.mean` = 4,
      `v.median` = 4,
      `v.sd` = 0.5
    )),
    theta = list(
      list(ke = 2, v = 4, prob = 0.6),
      list(ke = 3, v = 5, prob = 0.4)
    ),
    posterior = list(
      list(id = "1", point = 1, ke = 2, v = 4, prob = 0.7),
      list(id = "1", point = 2, ke = 3, v = 5, prob = 0.3)
    ),
    predictions = list(list(
      id = "1",
      time = 0,
      outeq = 0,
      block = 0,
      obs = 10,
      cens = "none",
      pop_mean = 11,
      pop_median = 12,
      post_mean = 13,
      post_median = 14
    )),
    covariates = list(list(id = "1", time = 0, block = 0, cov1 = 5))
  )
  build_config <- getFromNamespace("build_fit_payload_config", "Pmetrics")

  readr::write_csv(dplyr::bind_rows(payload$theta), file.path(path, "theta.csv"))
  readr::write_csv(dplyr::bind_rows(payload$posterior), file.path(path, "posterior.csv"))
  readr::write_csv(dplyr::bind_rows(payload$iterations), file.path(path, "iterations.csv"))
  readr::write_csv(dplyr::bind_rows(payload$predictions), file.path(path, "predictions.csv"))
  readr::write_csv(dplyr::bind_rows(payload$covariates), file.path(path, "covariates.csv"))
  writeLines(
    jsonlite::toJSON(
      list(
        kind = "ok",
        schema_version = 1,
        transport = "manifest",
        config = build_config(fit_params),
        manifest = list(
          generator = list(name = "pm_rs", version = "0.1.0"),
          tables = list(
            iterations = list(path = "iterations.csv", row_count = 1, sha256 = "stub"),
            theta = list(path = "theta.csv", row_count = 2, sha256 = "stub"),
            posterior = list(path = "posterior.csv", row_count = 2, sha256 = "stub"),
            predictions = list(path = "predictions.csv", row_count = 1, sha256 = "stub"),
            covariates = list(path = "covariates.csv", row_count = 1, sha256 = "stub")
          )
        )
      ),
      auto_unbox = TRUE,
      null = "null"
    ),
    file.path(path, "fit_manifest.json")
  )
}

test_that("PM_load rebuilds runs when PMout is missing", {
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

  outputs_dir <- file.path(run_root, "1", "outputs")
  inputs_dir <- file.path(run_root, "1", "inputs")
  expect_true(file.exists(file.path(outputs_dir, "fit_payload.rds")))
  unlink(file.path(outputs_dir, "PMout.Rdata"))

  expect_false(file.exists(file.path(outputs_dir, "PMout.Rdata")))
  expect_true(file.exists(file.path(inputs_dir, "fit.rds")))

  expect_no_warning(loaded <- PM_load(path = run_root, run = 1))
  expect_true(inherits(loaded, "PM_result"))
  expect_s3_class(loaded$data, "PM_data")
  expect_s3_class(loaded$model, "PM_model")
  expect_s3_class(loaded$op, "PM_op")
  expect_equal(as.data.frame(loaded$op$data), as.data.frame(run_initial$op$data))
  expect_equal(loaded$final$data$popPoints, run_initial$final$data$popPoints)
  expect_equal(loaded$cycle$data$objective, run_initial$cycle$data$objective)
})

test_that("PM_model$fit rereads an existing run without PMout snapshot", {
  run_root <- tempfile("pmetrics-fit-fallback-")
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

  outputs_dir <- file.path(run_root, "1", "outputs")
  inputs_dir <- file.path(run_root, "1", "inputs")
  expect_true(file.exists(file.path(outputs_dir, "fit_payload.rds")))
  unlink(file.path(outputs_dir, "PMout.Rdata"))

  expect_false(file.exists(file.path(outputs_dir, "PMout.Rdata")))
  expect_true(file.exists(file.path(inputs_dir, "fit.rds")))

  reopened <- suppressMessages(
    model_local$fit(
      data = dataEx,
      path = run_root,
      run = 1,
      overwrite = FALSE,
      report = "none",
      quiet = TRUE
    )
  )

  expect_true(inherits(reopened, "PM_result"))
  expect_s3_class(reopened$op, "PM_op")
  expect_s3_class(reopened$final, "PM_final")
  expect_equal(as.data.frame(reopened$op$data), as.data.frame(run_initial$op$data))
})

test_that("PM_load rebuilds runs from fit manifest when snapshots are missing", {
  run_root <- tempfile("pmetrics-load-manifest-")
  dir.create(run_root, recursive = TRUE)

  model_local <- build_example_ode_model(compile = FALSE)
  data_local <- PM_data$new(data = "ex.csv", quiet = TRUE)
  suppressMessages(
    model_local$fit(
      data = data_local,
      path = run_root,
      run = 1,
      overwrite = TRUE,
      report = "none",
      quiet = TRUE
    )
  )

  outputs_dir <- file.path(run_root, "1", "outputs")
  expect_true(file.exists(file.path(outputs_dir, "fit_payload.rds")))

  unlink(file.path(outputs_dir, "PMout.Rdata"))
  unlink(file.path(outputs_dir, "fit_payload.rds"))
  write_manifest_fixture(outputs_dir, model_local)

  expect_false(file.exists(file.path(outputs_dir, "PMout.Rdata")))
  expect_false(file.exists(file.path(outputs_dir, "fit_payload.rds")))
  expect_true(file.exists(file.path(outputs_dir, "fit_manifest.json")))

  expect_no_warning(loaded <- PM_load(path = run_root, run = 1))
  expect_true(inherits(loaded, "PM_result"))
  expect_equal(
    unique(loaded$op$data$pred[loaded$op$data$pred.type == "pop" & loaded$op$data$icen == "mean"]),
    11
  )
  expect_equal(loaded$final$data$gridpts, 20)
})