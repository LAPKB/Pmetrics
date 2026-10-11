library(Pmetrics)

test_that("PM_model$fit with numeric prior passes absolute paths when path is relative", {
  withr::local_dir(withr::local_tempdir(pattern = "prior-relpath-"))

  params <- tolower(names(NPex$model$model_list$pri))
  dir.create(file.path("1", "outputs"), recursive = TRUE)
  prior_df <- data.frame(matrix(0.5, nrow = 4, ncol = length(params), dimnames = list(NULL, params)), prob = 0.25)
  utils::write.csv(prior_df, file.path("1", "outputs", "theta.csv"), row.names = FALSE)

  model <- PM_model$new(x = NPex$model, compile = FALSE)

  data_found <- NULL
  captured_output <- NULL

  testthat::with_mocked_bindings(
    fit = function(model_source, data, params, output_path, solver = NULL) {
      data_found <<- file.exists(data)
      captured_output <<- output_path
      dir.create(output_path, recursive = TRUE)
      invisible(NULL)
    },
    PM_parse = function(path) invisible(NULL),
    PM_load = function(path, file, ...) structure(list(path = path), class = c("PM_result", "R6")),
    {
      model$fit(
        data = NPex$data,
        run = 2,
        cycles = 0,
        prior = 1,
        report = "none",
        quiet = TRUE
      )
    }
  )

  expect_true(data_found)
  expect_equal(captured_output, file.path(normalizePath("2"), "outputs"))
})
