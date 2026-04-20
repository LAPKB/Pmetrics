testthat::test_that("Book tutorial setup objects are available in-package", {
  testthat::expect_s3_class(dataEx, "PM_data")
  testthat::expect_s3_class(modEx, "PM_model")
  testthat::expect_s3_class(NPex, "PM_result")
  testthat::expect_s3_class(simEx, "PM_sim")
})

testthat::test_that("Book data snippets adapted: PM_data from file and from object", {
  from_file <- PM_data$new(data = "ex.csv", quiet = TRUE)
  from_obj <- PM_data$new(data = dataEx, quiet = TRUE)

  testthat::expect_s3_class(from_file, "PM_data")
  testthat::expect_s3_class(from_obj, "PM_data")
  testthat::expect_true(all(c("id", "time", "dose", "out") %in% names(from_file$data)))
  testthat::expect_gt(nrow(from_file$data), 0)
})

testthat::test_that("Book model snippets adapted: PM_model construction and mapping", {
  model_from_obj <- PM_model$new(x = modEx, compile = FALSE)
  model_rebuilt <- PM_model$new(x = model_from_obj, compile = FALSE)

  testthat::expect_s3_class(model_from_obj, "PM_model")
  testthat::expect_s3_class(model_rebuilt, "PM_model")
  testthat::expect_true(length(model_from_obj$model_list$parameters) > 0)
  testthat::expect_true("eqn" %in% names(model_from_obj$model_list))
})

testthat::test_that("Book workflow snippets adapted: fit then PM_load roundtrip", {
  testthat::skip_if_not(is_cargo_installed(), "Rust toolchain is required for model compilation.")
  local_exa_tmp_cleanup()

  run_root <- withr::local_tempdir(pattern = "book-workflow-")

  model_local <- PM_model$new(x = modEx, compile = FALSE)
  data_local <- PM_data$new(dataEx, quiet = TRUE)

  fit_result <- suppressMessages(
    model_local$fit(
      data = data_local,
      path = run_root,
      run = 1,
      overwrite = TRUE,
      report = "none",
      quiet = TRUE
    )
  )

  testthat::expect_s3_class(fit_result, "PM_result")

  loaded <- PM_load(path = run_root, run = 1)
  testthat::expect_s3_class(loaded, "PM_result")
  testthat::expect_s3_class(loaded$model, "PM_model")
})

testthat::test_that("Book simulation/PTA snippets adapted: simulate, plot, and PTA", {
  simulation <- suppressMessages(NPex$sim(nsim = 20, include = 1:3))

  testthat::expect_s3_class(simulation, "PM_sim")
  testthat::expect_gt(nrow(simulation$data$obs), 0)
  testthat::expect_no_error(simulation$plot())

  pta <- simulation$pta(
    target = list(5),
    target_type = "min",
    success = 1,
    start = 120,
    end = 144
  )

  testthat::expect_s3_class(pta, "PM_pta")
  testthat::expect_s3_class(pta$summary(), "data.frame")
  testthat::expect_no_error(pta$plot())
})

testthat::test_that("Book validation snippets adapted: non-interactive validation and summary", {
  validation <- PM_valid$new(
    NPex,
    tad = FALSE,
    binCov = "wt",
    doseC = 3,
    timeC = 3,
    nsim = 20
  )

  testthat::expect_s3_class(validation, "PM_valid")
  testthat::expect_true(!is.null(validation$npde))
  testthat::expect_true(!is.null(validation$npde_stats$time))
  testthat::expect_no_error(summary(validation))
  testthat::expect_no_error(validation$show_npde_stats(outeq = 1, tad = FALSE))
})

testthat::test_that("Book plot snippets adapted: PM_data and PM_op plot variants", {
  testthat::expect_no_error(NPex$data$plot())
  testthat::expect_no_error(NPex$data$plot(overlay = FALSE, xlim = c(119, 145)))
  testthat::expect_no_error(NPex$op$plot())
  testthat::expect_no_error(NPex$op$plot(pred.type = "pop"))
  testthat::expect_no_error(
    NPex$op$plot(
      line = list(lm = FALSE, loess = list(color = "red")),
      marker = list(symbol = 3, color = "green")
    )
  )
})

testthat::test_that("Book summary snippets adapted: S3 and R6 summaries", {
  testthat::expect_s3_class(summary(NPex$data), "summary.PM_data")
  testthat::expect_s3_class(summary(NPex$op, pred.type = "pop", icen = "mean"), "summary.PM_op")
  testthat::expect_s3_class(NPex$op$summary(pred.type = "pop", icen = "mean"), "summary.PM_op")
  testthat::expect_s3_class(NPex$final$summary(), "summary.PM_final")
  testthat::expect_s3_class(NPex$cycle$summary(), "summary.PM_cycle")
  testthat::expect_s3_class(summary(NPex$cycle), "summary.PM_cycle")
  testthat::expect_s3_class(NPex$cov$summary(icen = "mean"), "data.frame")
})

testthat::test_that("Book advanced plotting snippets adapted: final, cycle, cov, sim, valid", {
  testthat::expect_no_error(NPex$final$plot())
  testthat::expect_no_error(NPex$final$plot(line = list(color = "red")))
  testthat::expect_no_error(NPex$final$plot(ke ~ v))
  testthat::expect_no_error(NPex$cycle$plot())
  testthat::expect_no_error(NPex$cov$plot(v ~ wt))
  testthat::expect_no_error(
    NPex$cov$plot(
      ke ~ age,
      line = list(loess = FALSE, lm = list(color = "red")),
      marker = list(symbol = 3, color = "green")
    )
  )
  testthat::expect_no_error(NPex$cov$plot(v ~ wt, icen = "mean"))

  sim_local <- suppressMessages(NPex$sim(nsim = 20, include = 1:3))
  testthat::expect_no_error(sim_local$plot(include = 1))
  testthat::expect_no_error(sim_local$plot(exclude = 1))
  testthat::expect_no_error(sim_local$summary(field = "obs", include = 1))

  validation <- PM_valid$new(
    NPex,
    tad = FALSE,
    binCov = "wt",
    doseC = 3,
    timeC = 3,
    nsim = 20
  )
  testthat::expect_no_error(validation$plot(print = FALSE))
  testthat::expect_no_error(validation$plot(type = "pcvpc", print = FALSE))
  testthat::expect_no_error(validation$plot(type = "npde", print = FALSE))
  testthat::expect_no_error(validation$simdata$plot(obs = NPex$op, log = FALSE, binSize = 0.5))
})
