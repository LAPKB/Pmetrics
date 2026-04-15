library(Pmetrics)

make_prior_df <- function(param_names, n = 8, seed = 42) {
  set.seed(seed)
  prior_df <- as.data.frame(
    matrix(
      stats::runif(n * length(param_names), min = 0.1, max = 1),
      nrow = n
    )
  )
  names(prior_df) <- param_names
  prior_df$prob <- rep(1 / n, n)
  prior_df
}

run_fit_with_mocked_engine <- function(prior_input) {
  model <- PM_model$new(x = NPex$model, compile = FALSE)
  model$binary_path <- tempfile(fileext = ".pmx")
  file.create(model$binary_path)

  run_root <- withr::local_tempdir(pattern = "prior-params-")
  prior_file <- file.path(run_root, "input-prior.csv")
  utils::write.csv(prior_input, prior_file, row.names = FALSE)
  prior_arg <- prior_file

  captured_prior <- NULL

  fit_result <- testthat::with_mocked_bindings(
    fit = function(model_path, data, params, output_path, kind) {
      captured_prior <<- params$prior
      fs::dir_create(output_path)
      invisible(NULL)
    },
    PM_parse = function(path) invisible(NULL),
    PM_load = function(path, file, ...) structure(list(path = path), class = c("PM_result", "R6")),
    {
      model$fit(
        data = NPex$data,
        path = run_root,
        run = 1,
        cycles = 0,
        prior = prior_arg,
        overwrite = TRUE,
        report = "none",
        quiet = TRUE
      )
    }
  )

  list(
    fit_result = fit_result,
    captured_prior = captured_prior,
    prior_csv = file.path(getwd(), "prior.csv")
  )
}

test_that("PM_model$fit accepts prior file with matching parameter names", {
  withr::local_dir(withr::local_tempdir(pattern = "prior-match-"))

  params <- tolower(names(NPex$model$model_list$pri))
  prior_df <- make_prior_df(params)

  out <- run_fit_with_mocked_engine(prior_df)

  expect_s3_class(out$fit_result, "PM_result")
  expect_equal(out$captured_prior, "prior.csv")
  expect_true(file.exists(out$prior_csv))
  expect_equal(names(utils::read.csv(out$prior_csv, check.names = FALSE)), c(params, "prob"))
})

test_that("PM_model$fit reorders prior parameter columns to match current PRI order", {
  withr::local_dir(withr::local_tempdir(pattern = "prior-reorder-"))

  params <- tolower(names(NPex$model$model_list$pri))
  reordered <- rev(params)
  prior_df <- make_prior_df(reordered)

  out <- run_fit_with_mocked_engine(prior_df)

  expect_s3_class(out$fit_result, "PM_result")
  expect_equal(out$captured_prior, "prior.csv")
  expect_equal(names(utils::read.csv(out$prior_csv, check.names = FALSE)), c(params, "prob"))
})

test_that("PM_model$fit substitutes renamed prior parameter columns when only names changed", {
  withr::local_dir(withr::local_tempdir(pattern = "prior-rename-"))

  params <- tolower(names(NPex$model$model_list$pri))
  prior_df <- make_prior_df(paste0("old_", params))

  out <- run_fit_with_mocked_engine(prior_df)

  expect_s3_class(out$fit_result, "PM_result")
  expect_equal(out$captured_prior, "prior.csv")
  expect_equal(names(utils::read.csv(out$prior_csv, check.names = FALSE)), c(params, "prob"))
})

test_that("PM_model$fit rejects prior when mismatch is not a pure substitution", {
  withr::local_dir(withr::local_tempdir(pattern = "prior-bad-"))

  params <- tolower(names(NPex$model$model_list$pri))
  prior_df <- make_prior_df(params[-1])

  out <- run_fit_with_mocked_engine(prior_df)

  expect_null(out$fit_result)
  expect_null(out$captured_prior)
})

test_that("PM_model$fit supports numeric prior run and normalizes theta.csv columns", {
  withr::local_dir(withr::local_tempdir(pattern = "prior-runnum-"))

  params <- tolower(names(NPex$model$model_list$pri))
  run_root <- withr::local_tempdir(pattern = "prior-run-root-")
  run_num <- 7
  theta_dir <- file.path(run_root, as.character(run_num), "outputs")
  fs::dir_create(theta_dir)

  theta_df <- make_prior_df(rev(params))
  utils::write.csv(theta_df, file.path(theta_dir, "theta.csv"), row.names = FALSE)

  model <- PM_model$new(x = NPex$model, compile = FALSE)
  model$binary_path <- tempfile(fileext = ".pmx")
  file.create(model$binary_path)

  captured_prior <- NULL

  fit_result <- testthat::with_mocked_bindings(
    fit = function(model_path, data, params, output_path, kind) {
      captured_prior <<- params$prior
      fs::dir_create(output_path)
      invisible(NULL)
    },
    PM_parse = function(path) invisible(NULL),
    PM_load = function(path, file, ...) structure(list(path = path), class = c("PM_result", "R6")),
    {
      model$fit(
        data = NPex$data,
        path = run_root,
        run = 1,
        cycles = 0,
        prior = run_num,
        overwrite = TRUE,
        report = "none",
        quiet = TRUE
      )
    }
  )

  expect_s3_class(fit_result, "PM_result")
  expect_equal(captured_prior, "prior.csv")
  expect_equal(names(utils::read.csv("prior.csv", check.names = FALSE)), c(params, "prob"))
})
