testthat::skip_on_cran()

# Regression: PTA rows must follow the simulation ids a user supplied.
#
# When ids are text ("1", "2", ... "12"), everything that ordered data by id
# sorted it as text, so the regimens came out as 1, 10, 11, 12, 2, 3, ...
# PM_pta numbers its rows by regimen position, so row 4 described the regimen
# with id 12 rather than id 4. Nothing in the calculation was wrong; the results
# were attached to the wrong regimen.

skip_if_not_pmcore <- function() {
  testthat::skip_if_not_installed("Pmetrics")
}

pta_fixture <- function(n_ids = 12, nsim = 4) {
  manual_poppar <- list(
    wt = 1,
    mean = as.list(NPex$final$popMean),
    cov = diag(diag(NPex$final$popCov))
  )
  manual_limits <- tibble::tibble(
    par = c("ka", "ke", "v", "tlag1"),
    min = c(0.2, 0.01, 40, 0),
    max = c(0.8, 0.09, 110, 3)
  )

  # Log-spaced dose scales put the regimens across the target boundary, so each
  # one has a different response rather than all succeeding or all failing.
  scales <- exp(seq(log(0.15), log(0.45), length.out = n_ids))
  template_data <- dataEx$standard_data |>
    dplyr::filter(id %in% seq_len(n_ids)) |>
    dplyr::mutate(
      id = as.character(id),
      dose = dose * scales[as.integer(id)]
    )
  template <- PM_data$new(as.data.frame(template_data), quiet = TRUE)

  sim_for <- function(include) {
    suppressMessages(PM_sim$new(
      poppar = manual_poppar,
      data = template,
      model = NPex$model,
      include = include,
      nsim = nsim,
      predInt = 5,
      limits = manual_limits
    ))
  }

  list(
    ids = as.character(seq_len(n_ids)),
    sim_all = sim_for(seq_len(n_ids)),
    sim_for = sim_for
  )
}

pta_pdi <- function(sim, mic = 2) {
  suppressMessages(PM_pta$new(
    simdata = sim, target = mic, target_type = "time", success = 0.5,
    outeq = 1, free_fraction = 1, start = 0, end = Inf
  ))$data$data
}

testthat::test_that("simulated regimens follow numeric id order, not text order", {
  fx <- pta_fixture()
  observed <- unique(fx$sim_all$data$obs$id)

  testthat::expect_equal(observed, fx$ids)
  # The text order that caused the mis-association.
  testthat::expect_false(identical(observed, sort(fx$ids)))
})

testthat::test_that("each PTA row describes the regimen with the same id", {
  fx <- pta_fixture()
  rows <- pta_pdi(fx$sim_all)
  testthat::expect_equal(rows$reg_num, seq_along(fx$ids))

  # PM_pta on a single regimen gives that regimen's own result, so comparing the
  # multi-regimen rows against single-regimen runs tests the association without
  # reimplementing the PDI calculation.
  single_pdi <- lapply(fx$ids, function(id) {
    unlist(pta_pdi(fx$sim_for(id))$pdi[[1]])
  })

  fingerprints <- vapply(seq_along(fx$ids), function(i) {
    paste(round(unlist(rows$pdi[[i]]), 8), collapse = "|")
  }, character(1))
  # Guard against a vacuous pass: if every regimen returned the same vector the
  # comparison below would hold for any ordering.
  testthat::expect_gte(length(unique(fingerprints)), 4)

  for (i in seq_along(fx$ids)) {
    testthat::expect_equal(
      round(unlist(rows$pdi[[i]]), 8),
      round(single_pdi[[i]], 8),
      info = sprintf("reg_num %d should describe id %s", i, fx$ids[i])
    )
  }
})
