test_that("make_AUC deprecates block in favor of occ", {
  dat <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(0:2, 2),
    out = 1:6,
    block = rep(1:2, each = 3)
  )

  by_occ <- make_AUC(dat, out ~ time, occ = 2)
  lifecycle::expect_deprecated(
    make_AUC(dat, out ~ time, block = 2),
    "block"
  )
  by_block <- suppressWarnings(make_AUC(dat, out ~ time, block = 2))
  by_position <- make_AUC(dat, out ~ time, NULL, NULL, 0, Inf, "median", 1, 2)

  expect_equal(by_block, by_occ)
  expect_equal(by_position, by_occ)
  expect_error(
    make_AUC(dat, out ~ time, occ = 1, block = 2),
    "Supply only.*occ"
  )
})

test_that("plot methods deprecate block in favor of occ", {
  plot_cases <- list(
    data = function(...) plot(NPex$data, print = FALSE, ...),
    op = function(...) plot(NPex$op, print = FALSE, ...),
    pop = function(...) plot(NPex$pop, print = FALSE, ...),
    post = function(...) plot(NPex$post, print = FALSE, ...)
  )

  for (plot_case in plot_cases) {
    expect_no_error(plot_case(occ = 1))
    lifecycle::expect_deprecated(plot_case(block = 1), "block")
    expect_error(plot_case(occ = 1, block = 1), "Supply only.*occ")
  }
})

test_that("make_NCA and PM_pta deprecate block in favor of occ", {
  nca_by_occ <- suppressWarnings(make_NCA(NPex, occ = 1))
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
  expect_equal(nca_by_block, nca_by_occ)
  expect_error(
    make_NCA(NULL, occ = 1, block = 1),
    "Supply only.*occ"
  )

  pta_args <- list(
    simdata = simEx,
    target = list(5),
    target_type = "min",
    success = 1,
    start = 120,
    end = 144
  )
  expect_no_error(rlang::exec(PM_pta$new, !!!pta_args, occ = 1))
  lifecycle::expect_deprecated(
    rlang::exec(PM_pta$new, !!!pta_args, block = 1),
    "block"
  )
  expect_error(
    PM_pta$new(occ = 1, block = 1),
    "Supply only.*occ"
  )
})

test_that("all affected public entry points expose occ and deprecated block", {
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
    expect_true("occ" %in% names(fmls))
    expect_true("block" %in% names(fmls))
    expect_identical(fmls$block, quote(lifecycle::deprecated()))
  }
})
