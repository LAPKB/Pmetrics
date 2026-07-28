test_that("report process failures are detected", {
  failed_process <- new.env(parent = emptyenv())
  failed_process$is_alive <- function() FALSE
  failed_process$get_exit_status <- function() 1L
  failed_process$read_error_lines <- function() "report app failed"
  class(failed_process) <- "process"

  expect_identical(
    Pmetrics:::.report_process_error(failed_process),
    "report app failed"
  )
})

test_that("a running report process is accepted", {
  running_process <- new.env(parent = emptyenv())
  running_process$is_alive <- function() TRUE
  class(running_process) <- "process"

  expect_null(Pmetrics:::.report_process_error(running_process))
})

test_that("an explicit HTML template does not launch the reporting app", {
  app_checked <- FALSE
  output_dir <- tempfile("pmetrics-report-test-")
  dir.create(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE))

  local_mocked_bindings(
    .report_dependency_available = function(package) {
      if (identical(package, "PmetricsReports")) {
        app_checked <<- TRUE
        stop("The reporting app should not be queried for an HTML template.")
      }
      TRUE
    },
    .package = "Pmetrics"
  )
  local_mocked_bindings(
    .render_report = function(input, output_file, ...) {
      file.create(output_file)
      output_file
    },
    .package = "Pmetrics"
  )

  expect_identical(
    PM_report(NPex, template = "plotly", path = output_dir, show = FALSE),
    1
  )
  expect_false(app_checked)
  expect_true(file.exists(file.path(output_dir, "report.html")))
})

test_that("the saved default report mode is respected", {
  app_checked <- FALSE
  output_dir <- tempfile("pmetrics-default-report-test-")
  dir.create(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE))

  local_mocked_bindings(
    getPMoptions = function(opt) {
      expect_identical(opt, "report_template")
      "plotly"
    },
    .report_dependency_available = function(package) {
      if (identical(package, "PmetricsReports")) {
        app_checked <<- TRUE
        stop("The reporting app should not be queried for the saved HTML mode.")
      }
      TRUE
    },
    .render_report = function(input, output_file, ...) {
      file.create(output_file)
      output_file
    },
    .package = "Pmetrics"
  )

  expect_identical(
    PM_report(NPex, path = output_dir, show = FALSE),
    1
  )
  expect_false(app_checked)
  expect_true(file.exists(file.path(output_dir, "report.html")))
})

test_that("an invalid report mode falls back to the saved default", {
  app_checked <- FALSE
  output_dir <- tempfile("pmetrics-fallback-report-test-")
  dir.create(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE))

  local_mocked_bindings(
    getPMoptions = function(opt) {
      expect_identical(opt, "report_template")
      "plotly"
    },
    .report_dependency_available = function(package) {
      if (identical(package, "PmetricsReports")) {
        app_checked <<- TRUE
        stop("The reporting app should not be queried for a Plotly fallback.")
      }
      TRUE
    },
    .render_report = function(input, output_file, ...) {
      file.create(output_file)
      output_file
    },
    .package = "Pmetrics"
  )

  expect_warning(
    status <- PM_report(
      NPex,
      template = "pear",
      path = output_dir,
      show = FALSE
    ),
    "Falling back to the saved default report mode \"plotly\""
  )
  expect_identical(status, 1)
  expect_false(app_checked)
  expect_true(file.exists(file.path(output_dir, "report.html")))
})

test_that("an invalid report mode respects a saved default of none", {
  local_mocked_bindings(
    getPMoptions = function(opt) "none",
    .package = "Pmetrics"
  )

  expect_warning(
    status <- PM_report(NPex, template = "pear", show = FALSE),
    "Falling back to the saved default report mode \"none\""
  )
  expect_identical(status, 0)
})

test_that("app mode launches PmetricsReports", {
  app_launched <- FALSE
  running_process <- new.env(parent = emptyenv())
  running_process$is_alive <- function() TRUE
  class(running_process) <- "process"

  local_mocked_bindings(
    .report_dependency_available = function(package) TRUE,
    .report_app_runner = function() {
      function(...) {
        app_launched <<- TRUE
        running_process
      }
    },
    .package = "Pmetrics"
  )

  expect_identical(
    PM_report(NPex, template = "app", show = FALSE),
    1
  )
  expect_true(app_launched)
})
