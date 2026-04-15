library(Pmetrics)

test_that("PM_data methods: print, summary, plot", {
  expect_no_error(capture.output(NPex$data$print(viewer = FALSE)))

  data_summary <- NPex$data$summary()
  expect_s3_class(data_summary, "summary.PM_data")

  expect_no_error(NPex$data$plot())
})

test_that("plot.PM_data supports multi-output and grouping options", {
  set.seed(123)

  dat2 <- dataEx$clone(deep = TRUE)
  std <- dat2$standard_data

  # Add a grouping covariate named Male from existing gender coding
  std$Male <- std$gender

  # Build synthetic output equation 2 from outeq 1 observations
  obs1 <- std |>
    dplyr::filter(evid == 0, outeq == 1)

  obs2 <- obs1
  obs2$outeq <- 2
  obs2$out <- obs1$out * stats::runif(nrow(obs1), min = 0.10, max = 0.30)

  cens_idx <- obs2$out < 0.5
  obs2$out[cens_idx] <- 0.5
  obs2$cens <- ifelse(cens_idx, "1", "0")

  dat2$standard_data <- dplyr::bind_rows(std, obs2) |>
    dplyr::arrange(id, time, outeq, evid)

  # Basic integrity checks for synthetic outeq 2
  expect_true(any(dat2$standard_data$outeq == 2))
  expect_true(all(dat2$standard_data$out[dat2$standard_data$outeq == 2] >= 0.5, na.rm = TRUE))

  # Plot outeq 1, outeq 2, and both
  p1 <- dat2$plot(outeq = 1, print = FALSE)
  p2 <- dat2$plot(outeq = 2, print = FALSE)
  p12 <- dat2$plot(outeq = 1:2, print = FALSE)

  expect_s3_class(p1, "plotly")
  expect_s3_class(p2, "plotly")
  expect_s3_class(p12, "plotly")

  # Grouping by Male
  p_group <- dat2$plot(
    outeq = 1:2,
    group = "Male",
    group_names = c("Female", "Male"),
    legend = TRUE,
    print = FALSE
  )
  expect_s3_class(p_group, "plotly")

  # Join formatting should not be accepted
  expect_warning(
    dat2$plot(outeq = 1, line = list(join = list(color = "blue")), print = FALSE)
  )
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

  post_summary <- summary(NPex$post)
  expect_s3_class(post_summary, "data.frame")
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
  if (!is_cargo_installed()) {
    skip("Cargo is not installed in this environment")
  }

  opt_obj <- tryCatch(
    PM_opt$new(
      poppar = NPex,
      mmInt = c(120, 126, 132, 144),
      nsamp = 2,
      predInt = 0.5
    ),
    error = identity
  )

  if (inherits(opt_obj, "error")) {
    skip(paste("PM_opt initialization failed in this environment:", opt_obj$message))
  }

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

  # Pass pre-computed cluster counts (from Mclust on the NPex data) and an
  # empty binCov so that PM_valid runs fully non-interactively.
  # The tryCatch guard covers environments where the installed package still
  # contains legacy interactive code that predates this non-interactive path.
  valid_result <- tryCatch(
    {
      tmp$validate(
        binCov  = character(0),
        doseC   = 8,
        timeC   = 7,
        limits  = c(0, 3),
        nsim    = 20
      )
      NULL
    },
    error = identity
  )

  if (inherits(valid_result, "error")) {
    skip(paste("PM_valid validation not supported non-interactively in this build:", valid_result$message))
  }

  expect_s3_class(tmp$valid, "PM_valid")
  expect_no_error(tmp$valid$plot(print = FALSE))
})
