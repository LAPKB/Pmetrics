test_that("companion package inventory contains the supported apps", {
  expect_setequal(
    Pmetrics:::pm_companion_packages(),
    c(
      "PmetricsLitSim",
      "PmetricsModelLib",
      "PmetricsExplorer",
      "PmetricsPlot",
      "PmetricsReports"
    )
  )
  expect_false("PmetricsPTA" %in% Pmetrics:::pm_companion_packages())
})

test_that("update notifications include outdated companion packages", {
  result <- list(
    pmetrics_outdated = FALSE,
    r_outdated = FALSE,
    companions = list(
      PmetricsPlot = list(
        installed = package_version("0.0.2"),
        latest = package_version("0.0.3"),
        outdated = TRUE
      )
    )
  )

  expect_message(
    Pmetrics:::pm_notify_outdated(result),
    "PmetricsPlot 0.0.3 \\(installed: 0.0.2\\)"
  )
})

test_that("old update caches remain compatible", {
  result <- list(
    pmetrics_outdated = FALSE,
    r_outdated = FALSE
  )

  expect_no_message(Pmetrics:::pm_notify_outdated(result))
})
