test_that("make_AUC deprecates block in favor of occasion", {
  dat <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(0:2, 2),
    out = 1:6,
    occasion = rep(1:2, each = 3)
  )

  by_occasion <- make_AUC(dat, out ~ time, occasion = 2)
  lifecycle::expect_deprecated(
    make_AUC(dat, out ~ time, block = 2),
    "block"
  )
  by_block <- suppressWarnings(make_AUC(dat, out ~ time, block = 2))
  by_position <- make_AUC(dat, out ~ time, NULL, NULL, 0, Inf, "median", 1, 2)

  expect_equal(by_block, by_occasion)
  expect_equal(by_position, by_occasion)
  expect_error(
    make_AUC(dat, out ~ time, occasion = 1, block = 2),
    "Supply only.*occasion"
  )
})

test_that("plot methods deprecate block in favor of occasion", {
  plot_cases <- list(
    data = function(...) plot(NPex$data, print = FALSE, ...),
    op = function(...) plot(NPex$op, print = FALSE, ...),
    pop = function(...) plot(NPex$pop, print = FALSE, ...),
    post = function(...) plot(NPex$post, print = FALSE, ...)
  )

  for (plot_case in plot_cases) {
    expect_no_error(plot_case(occasion = 1))
    lifecycle::expect_deprecated(plot_case(block = 1), "block")
    expect_error(plot_case(occasion = 1, block = 1), "Supply only.*occasion")
  }
})

test_that("make_NCA and PM_pta deprecate block in favor of occasion", {
  nca_by_occasion <- suppressWarnings(make_NCA(NPex, occasion = 1))
  lifecycle::expect_deprecated(
    withCallingHandlers(
      make_NCA(NPex, block = 1),
      warning = function(w) {
        if (!inherits(w, "lifecycle_warning_deprecated")) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    "block"
  )
  nca_by_block <- suppressWarnings(make_NCA(NPex, block = 1))
  expect_equal(nca_by_block, nca_by_occasion)
  expect_error(
    make_NCA(NULL, occasion = 1, block = 1),
    "Supply only.*occasion"
  )

  pta_args <- list(
    simdata = simEx,
    target = list(5),
    target_type = "min",
    success = 1,
    start = 120,
    end = 144
  )
  expect_no_error(rlang::exec(PM_pta$new, !!!pta_args, occasion = 1))
  lifecycle::expect_deprecated(
    rlang::exec(PM_pta$new, !!!pta_args, block = 1),
    "block"
  )
  expect_error(
    PM_pta$new(occasion = 1, block = 1),
    "Supply only.*occasion"
  )
})

test_that("all affected public entry points expose occasion and deprecated block", {
  entry_points <- list(
    make_AUC = make_AUC,
    make_NCA = make_NCA,
    plot.PM_data = getS3method("plot", "PM_data"),
    plot.PM_op = getS3method("plot", "PM_op"),
    plot.PM_pop = getS3method("plot", "PM_pop"),
    plot.PM_post = getS3method("plot", "PM_post"),
    `PM_pta$new` = PM_pta$public_methods$initialize
  )

  for (entry_point in entry_points) {
    fmls <- formals(entry_point)
    expect_true("occasion" %in% names(fmls))
    expect_true("block" %in% names(fmls))
    expect_identical(fmls$block, quote(lifecycle::deprecated()))
  }
})

test_that("PM_upgrade converts legacy occasion columns safely", {
  old <- data.frame(id = 1:2, block = c(1L, 2L))
  lifecycle::expect_deprecated(upgraded <- PM_upgrade(old), "block")
  expect_named(upgraded, c("id", "occasion"))
  expect_identical(upgraded$occasion, old$block)

  both <- transform(old, occasion = block)
  expect_named(suppressWarnings(PM_upgrade(both)), c("id", "occasion"))

  conflicting <- transform(old, occasion = c(2L, 1L))
  expect_error(PM_upgrade(conflicting), "conflicting")
})

test_that("occasion is a reserved PMcheck header", {
  expect_error(
    PMcheck(data.frame(occasion = 1)),
    "reserved Pmetrics data header"
  )
  expect_no_error(PMcheck(dataEx$standard_data, quiet = TRUE))
})

test_that("Pmetrics data constructors upgrade legacy block columns", {
  missing_output_path <- file.path(tempdir(), "pmetrics-upgrade-no-output-files")
  constructors <- list(
    op = PM_op,
    pop = PM_pop,
    post = PM_post,
    cov = PM_cov
  )

  for (component in names(constructors)) {
    legacy <- NPex[[component]]$clone(deep = TRUE)
    names(legacy$data)[names(legacy$data) == "occasion"] <- "block"
    rebuilt <- suppressWarnings(constructors[[component]]$new(
      legacy,
      path = missing_output_path
    ))
    expect_true("occasion" %in% names(rebuilt$data), info = component)
    expect_false("block" %in% names(rebuilt$data), info = component)
  }

  legacy_data <- dataEx$clone(deep = TRUE)
  legacy_data$data$block <- legacy_data$standard_data$occasion
  rebuilt_data <- suppressWarnings(PM_data$new(legacy_data, quiet = TRUE))
  expect_true("occasion" %in% names(rebuilt_data$data))
  expect_false("block" %in% names(rebuilt_data$data))
  expect_true("occasion" %in% names(rebuilt_data$standard_data))
})
