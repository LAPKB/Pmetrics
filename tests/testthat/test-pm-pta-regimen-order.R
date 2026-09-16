testthat::skip_on_cran()

# Regression: PTA rows must be associated with the regimen that produced them.
#
# PM_pta used to discard the regimen id and keep only the position of the
# regimen in the list of simulations, so a result could be matched to a regimen
# only by its position. Whenever the simulations came back in a different order
# than the data - which happens for any id that is not a plain integer, because
# sorting text is byte order and "10" precedes "2" - the rows described the
# wrong regimen. Every row now carries the `id` of the regimen it was calculated
# from, so the association survives any order.
#
# Note: the posterior branch of PM_sim cannot be exercised from the shipped
# data, because `NPex$postMean` is empty, so all regimens here are simulated
# from the population parameters.

pta_fixture <- function(n_ids = 12, nsim = 4,
                        shape = c("numeric", "character", "prefixed"),
                        scale_order = seq_len(n_ids)) {
  shape <- match.arg(shape)
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

  ids <- switch(shape,
    numeric = seq_len(n_ids),
    character = as.character(seq_len(n_ids)),
    prefixed = paste0("P", seq_len(n_ids))
  )

  # Log-spaced dose scales put the regimens across the target boundary, so each
  # one has a different response rather than all succeeding or all failing.
  # `scale_order` hands out the scales out of order, decoupling the response of
  # a regimen from its position so that only its id identifies it.
  scales <- exp(seq(log(0.15), log(0.45), length.out = n_ids))[scale_order]
  template_data <- dataEx$standard_data |>
    dplyr::filter(id %in% seq_len(n_ids)) |>
    # `dose` is scaled before `id` is relabeled, since it is indexed by the
    # subject number.
    dplyr::mutate(
      dose = dose * scales[as.integer(id)],
      id = ids[as.integer(id)]
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
    ids = ids, # the ids as the user supplied them
    data_ids = as.character(unique(template$standard_data$id)), # as PM_data orders them
    template = template,
    sim_all = sim_for(ids),
    sim_for = sim_for
  )
}

pta_pdi <- function(sim, mic = 2, ...) {
  suppressMessages(PM_pta$new(
    simdata = sim, target = mic, target_type = "time", success = 0.5,
    outeq = 1, free_fraction = 1, start = 0, end = Inf, ...
  ))$data$data
}

# PDI of a single regimen, keyed by the regimen id.
single_pdi <- function(fx, id) {
  unlist(pta_pdi(fx$sim_for(id))$pdi[[1]])
}

pdi_fingerprint <- function(x) paste(round(unlist(x), 8), collapse = "|")

testthat::test_that("simulated regimens follow numeric id order, not text order", {
  for (shape in c("numeric", "character")) {
    fx <- pta_fixture(shape = shape)
    observed <- as.character(unique(fx$sim_all$data$obs$id))

    testthat::expect_equal(observed, as.character(fx$ids), info = shape)
    # The text order that caused the mis-association.
    testthat::expect_false(identical(observed, sort(observed)), info = shape)
  }
})

testthat::test_that("simulated regimens follow the id order of the data", {
  for (shape in c("numeric", "character", "prefixed")) {
    fx <- pta_fixture(shape = shape)

    testthat::expect_equal(
      as.character(unique(fx$sim_all$data$obs$id)),
      fx$data_ids,
      info = shape
    )
  }
})

testthat::test_that("each PTA row carries the id of its own regimen", {
  for (shape in c("numeric", "character", "prefixed")) {
    fx <- pta_fixture(shape = shape)
    rows <- pta_pdi(fx$sim_all)
    sim_ids <- as.character(unique(fx$sim_all$data$obs$id))

    # The id is the regimen identifier itself, not a position and not a rank, so
    # it names the same regimen however the ids are written.
    testthat::expect_false(anyNA(rows$id), info = shape)
    testthat::expect_equal(rows$id, sim_ids[rows$reg_num], info = shape)
    testthat::expect_equal(rows$id, fx$data_ids[rows$reg_num], info = shape)

    # PM_pta on a single regimen gives that regimen's own result, so comparing
    # each multi-regimen row with the single-regimen run for the id it reports
    # tests the association without reimplementing the PDI calculation.
    fingerprints <- vapply(rows$pdi, pdi_fingerprint, character(1))
    # Guard against a vacuous pass: if every regimen returned the same vector
    # the comparison below would hold for any association.
    testthat::expect_gte(length(unique(fingerprints)), 4)

    for (i in seq_len(nrow(rows))) {
      testthat::expect_equal(
        round(unlist(rows$pdi[[i]]), 8),
        round(single_pdi(fx, rows$id[i]), 8),
        info = sprintf("%s: row %d reports id %s", shape, i, rows$id[i])
      )
    }
  }
})

testthat::test_that("the association does not depend on the order of the regimens", {
  # The dose scales are shuffled, so the response of each regimen is not in the
  # same order as its id: a row can only match its own regimen by id.
  fx <- pta_fixture(shape = "character", scale_order = c(5, 11, 1, 8, 12, 3, 9, 2, 7, 4, 10, 6))
  rows <- pta_pdi(fx$sim_all)
  sim_ids <- as.character(unique(fx$sim_all$data$obs$id))

  testthat::expect_equal(rows$id, sim_ids[rows$reg_num])
  # Guard against a vacuous pass, as above.
  testthat::expect_gte(length(unique(vapply(rows$pdi, pdi_fingerprint, character(1)))), 4)

  for (i in seq_len(nrow(rows))) {
    testthat::expect_equal(
      round(unlist(rows$pdi[[i]]), 8),
      round(single_pdi(fx, rows$id[i]), 8),
      info = sprintf("row %d reports id %s", i, rows$id[i])
    )
  }
})

testthat::test_that("a subset of the regimens keeps each id with its own result", {
  fx <- pta_fixture(shape = "prefixed")
  keep <- c("P7", "P2", "P11", "P4") # out of id order on purpose
  rows <- pta_pdi(fx$sim_for(keep))

  testthat::expect_setequal(rows$id, keep)
  testthat::expect_equal(rows$id, fx$data_ids[fx$data_ids %in% keep][rows$reg_num])

  for (i in seq_len(nrow(rows))) {
    testthat::expect_equal(
      round(unlist(rows$pdi[[i]]), 8),
      round(single_pdi(fx, rows$id[i]), 8),
      info = sprintf("row %d reports id %s", i, rows$id[i])
    )
  }
})

testthat::test_that("a user-supplied list of regimens is associated by its id", {
  fx <- pta_fixture(shape = "prefixed")
  obs <- fx$sim_all$data$obs
  # Deliberately not in id order, and unnamed so that the ids can only come from
  # the regimens themselves.
  order <- c("P5", "P1", "P12", "P7")
  regimens <- unname(split(obs, as.factor(obs$id))[order])
  rows <- pta_pdi(regimens)

  testthat::expect_equal(rows$id, order)
  testthat::expect_equal(rows$reg_num, seq_along(order))

  for (i in seq_along(order)) {
    testthat::expect_equal(
      round(unlist(rows$pdi[[i]]), 8),
      round(single_pdi(fx, order[i]), 8),
      info = sprintf("row %d reports id %s", i, order[i])
    )
  }
})

testthat::test_that("a PTA without simlabels is labeled with the regimen id", {
  for (shape in c("numeric", "character", "prefixed")) {
    fx <- pta_fixture(shape = shape)
    rows <- pta_pdi(fx$sim_all)

    testthat::expect_equal(rows$label, as.character(rows$id), info = shape)
    testthat::expect_equal(rows$label, as.character(fx$data_ids), info = shape)
    # Guard against a vacuous pass: the previous default was the generic text.
    testthat::expect_false(any(rows$label == paste("Regimen", rows$reg_num)), info = shape)

    summary <- suppressMessages(PM_pta$new(
      simdata = fx$sim_all, target = 2, target_type = "time", success = 0.5,
      outeq = 1, free_fraction = 1, start = 0, end = Inf
    )$summary())
    testthat::expect_equal(summary$label, summary$id, info = shape)
  }
})

testthat::test_that("id-based default labels plot", {
  # 4 regimens, since the default color palette holds at most 9.
  fx <- pta_fixture(shape = "prefixed", n_ids = 4)
  pta <- suppressMessages(PM_pta$new(
    simdata = fx$sim_all, target = c(1, 2), target_type = "time", success = 0.5,
    outeq = 1, free_fraction = 1, start = 0, end = Inf
  ))

  testthat::expect_no_error(suppressMessages(pta$plot(print = FALSE)))
})

testthat::test_that("supplied simlabels stay positional, in reg_num order", {
  fx <- pta_fixture(shape = "prefixed")
  labels <- paste0("L", seq_along(fx$ids))
  rows <- pta_pdi(fx$sim_all, simlabels = labels)

  testthat::expect_equal(rows$label, labels[rows$reg_num])
  # The label is the supplied one, while the id still identifies the regimen.
  testthat::expect_equal(rows$id, as.character(fx$data_ids)[rows$reg_num])
})

testthat::test_that("a regimen whose id is not recoverable keeps a generic label", {
  # 4 regimens, since the default color palette holds at most 9.
  fx <- pta_fixture(shape = "numeric", n_ids = 4)
  obs <- fx$sim_all$data$obs
  # Drop the id from each regimen, as an unnamed list of objects without ids.
  regimens <- unname(lapply(split(obs, as.factor(obs$id)), function(x) {
    x[, setdiff(names(x), "id"), drop = FALSE]
  }))
  # "min" is used rather than "time" because the latter arranges by id.
  pta <- suppressMessages(PM_pta$new(
    simdata = regimens, target = 2, target_type = "min", success = 1,
    outeq = 1, free_fraction = 1, start = 0, end = Inf
  ))
  rows <- pta$data$data

  testthat::expect_equal(rows$label, paste("Regimen", seq_along(regimens)))
  testthat::expect_true(all(is.na(rows$id)))
  # A missing label would break the legend, which is built from the labels.
  testthat::expect_no_error(suppressMessages(pta$plot(print = FALSE)))
})

testthat::test_that("results saved before the id column still summarise and plot", {
  pta <- suppressMessages(PM_pta$new(
    simdata = simEx, target = list(5, 10), target_type = c("min", "max"),
    success = c(1, 1), start = 120, end = 144
  ))
  # Emulate a PTA made before results carried the regimen id. Its identifiers
  # were replaced by a rank and its labels were the generic text, so the regimen
  # number is the only identifier left.
  legacy <- pta$clone(deep = TRUE)
  for (i in seq_along(legacy$data)) {
    x <- legacy$data[[i]]
    if (is.data.frame(x)) {
      x$label <- paste("Regimen", x$reg_num)
      legacy$data[[i]] <- x[, setdiff(names(x), "id"), drop = FALSE]
    }
  }
  testthat::expect_false("id" %in% names(legacy$data$data))

  summary <- suppressMessages(legacy$summary())
  testthat::expect_equal(summary$id, as.character(summary$reg_num))
  testthat::expect_equal(summary$label, paste("Regimen", summary$reg_num))
  single <- suppressMessages(legacy$summary(at = 1))
  testthat::expect_equal(single$id, as.character(single$reg_num))
  testthat::expect_no_error(suppressMessages(legacy$plot(print = FALSE)))
})
