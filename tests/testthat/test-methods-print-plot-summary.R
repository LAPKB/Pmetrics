library(Pmetrics)

test_that("PM_data methods: print, summary, plot", {
  expect_no_error(capture.output(NPex$data$print(viewer = FALSE)))

  data_summary <- NPex$data$summary()
  expect_s3_class(data_summary, "summary.PM_data")

  expect_no_error(NPex$data$plot())
})

test_that("PM_model methods: print and plot", {
  expect_no_error(capture.output(NPex$model$print()))
  expect_no_error(NPex$model$plot())
})

test_that("PM_post methods: plot and summary", {
  expect_no_error(NPex$post$plot())

  # Current PM_post$summary() implementation forwards incorrectly to summary.PM_pop(self, ...)
  expect_error(
    NPex$post$summary(),
    "no applicable method for 'filter'"
  )

  expect_no_error(summary.PM_post(NPex$post))
})

test_that("PM_pop methods: summary and plot", {
  pop_summary <- NPex$pop$summary()
  expect_s3_class(pop_summary, "data.frame")

  expect_no_error(NPex$pop$plot())
})

test_that("PM_op methods: summary and plot", {
  op_summary <- NPex$op$summary()
  expect_s3_class(op_summary, "summary.PM_op")

  expect_no_error(NPex$op$plot())
})

test_that("PM_cycle methods: print, summary, and plot", {
  expect_no_error(capture.output(NPex$cycle$print()))

  cycle_summary <- NPex$cycle$summary()
  expect_s3_class(cycle_summary, "summary.PM_cycle")

  expect_no_error(NPex$cycle$plot())
})

test_that("PM_final methods: summary and plot", {
  final_summary <- NPex$final$summary()
  expect_s3_class(final_summary, "summary.PM_final")

  expect_no_error(NPex$final$plot())
})

test_that("PM_cov methods: print, summary, and plot", {
  expect_no_error(capture.output(NPex$cov$print()))

  cov_summary <- NPex$cov$summary()
  expect_s3_class(cov_summary, "data.frame")

  expect_no_error(NPex$cov$plot(ke ~ wt))
})

test_that("PM_sim methods: summary and plot", {
  sim_summary <- simEx$summary()
  expect_s3_class(sim_summary, "summary.PM_sim")

  expect_no_error(simEx$plot())
})

test_that("PM_pta methods: summary and plot", {
  pta <- simEx$pta(
    target = list(5),
    target_type = "min",
    success = 1,
    start = 120,
    end = 144
  )

  pta_summary <- pta$summary()
  expect_s3_class(pta_summary, "data.frame")

  expect_no_error(pta$plot())
})

test_that("PM_opt methods: print and plot", {
  expect_true(is_cargo_installed())

  opt_obj <- PM_opt$new(
    poppar = NPex,
    mmInt = c(120, 126, 132, 144),
    nsamp = 2,
    predInt = 0.5
  )

  expect_s3_class(opt_obj, "PM_opt")
  expect_no_error(capture.output(opt_obj$print()))
  expect_no_error(opt_obj$plot(print = FALSE))
})

test_that("PM_valid class guards and optional plot", {
  expect_error(
    PM_valid$new(result = "not-a-result"),
    "Please supply a PM_result object"
  )
  tmp <- NPex$clone(deep = TRUE)
  expect_no_error(
    tmp$validate(
      binCov = "wt",
      doseC = 2,
      timeC = 2,
      limits = c(0, 3),
      nsim = 20
    )
  )
  expect_s3_class(tmp$valid, "PM_valid")
  expect_no_error(tmp$valid$plot(print = FALSE))
})
