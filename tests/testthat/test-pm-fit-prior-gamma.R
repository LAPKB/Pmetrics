library(Pmetrics)

fit_capturing_error_models <- function(model, prior, path) {
  captured <- NULL
  testthat::with_mocked_bindings(
    fit = function(model_source, data, params, output_path, solver = NULL) {
      captured <<- params$error_models
      dir.create(output_path, recursive = TRUE)
      invisible(NULL)
    },
    PM_parse = function(path) invisible(NULL),
    PM_load = function(path, file, ...) structure(list(path = path), class = c("PM_result", "R6")),
    model$fit(
      data = NPex$data, path = path, run = 2, cycles = 0, prior = prior,
      overwrite = TRUE, report = "none", quiet = TRUE
    )
  )
  captured
}

test_that("PM_model$fit starts gamma from the final value of a prior run number", {
  run_root <- withr::local_tempdir(pattern = "prior-gamma-")
  params <- tolower(names(NPex$model$model_list$pri))
  out_dir <- file.path(run_root, "1", "outputs")
  dir.create(out_dir, recursive = TRUE)
  theta <- data.frame(matrix(0.5, nrow = 2, ncol = length(params), dimnames = list(NULL, params)), prob = 0.5)
  utils::write.csv(theta, file.path(out_dir, "theta.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(cycle = 1:2, neg2ll = c(10, 5), gamlam.0 = c(4, 2.5)),
    file.path(out_dir, "cycles.csv"),
    row.names = FALSE
  )

  model <- PM_model$new(x = NPex$model, compile = FALSE)
  err <- fit_capturing_error_models(model, 1, run_root)

  expect_equal(err[[1]]$initial, 2.5)
  # the model itself is unchanged
  expect_false(isTRUE(all.equal(model$model_list$err[[1]]$initial, 2.5)))
})

test_that("PM_model$fit starts gamma from the final value of a PM_result prior", {
  params <- tolower(names(NPex$model$model_list$pri))
  theta <- data.frame(matrix(0.5, nrow = 2, ncol = length(params), dimnames = list(NULL, params)), prob = 0.5)
  prior <- structure(
    list(
      final = list(popPoints = theta),
      cycle = list(data = list(gamlam = tibble::tibble(cycle = c(1, 2), value = c(4, 3.5), outeq = 1, type = "proportional")))
    ),
    class = "PM_result"
  )

  model <- PM_model$new(x = NPex$model, compile = FALSE)
  err <- fit_capturing_error_models(model, prior, withr::local_tempdir(pattern = "prior-gamma-res-"))

  expect_equal(err[[1]]$initial, 3.5)
})
