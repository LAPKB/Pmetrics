library(Pmetrics)
library(dplyr)

sim_ids <- function(sim) {
  sort(unique(as.character(sim$data$obs$id)))
}

expect_parvalues_within_limits <- function(sim, limits_df) {
  par_values <- sim$data$parValues

  for (i in seq_len(nrow(limits_df))) {
    par_name <- limits_df$par[[i]]
    expect_true(all(par_values[[par_name]] >= limits_df$min[[i]]))
    expect_true(all(par_values[[par_name]] <= limits_df$max[[i]]))
  }
}

test_that("PM_sim handles PM_result poppar with nsim, include, exclude, split, and usePost", {
  sim_subset <- suppressMessages(
    NPex$sim(
      include = 1:3,
      exclude = 2,
      nsim = 4,
      split = FALSE
    )
  )

  expect_true(inherits(sim_subset, "PM_sim"))
  expect_equal(sim_ids(sim_subset), c("1", "3"))
  expect_equal(max(sim_subset$data$obs$nsim), 4)
  expect_equal(nrow(sim_subset$data$parValues), 4)

  sim_split <- suppressMessages(
    NPex$sim(
      include = 1:2,
      nsim = 4,
      split = TRUE
    )
  )

  expect_true(inherits(sim_split, "PM_sim"))
  expect_equal(sim_ids(sim_split), c("1", "2"))
  expect_equal(max(sim_split$data$obs$nsim), 4)

  sim_post <- suppressMessages(
    NPex$sim(
      include = 1:3,
      exclude = 2,
      nsim = 2,
      usePost = TRUE
    )
  )

  expect_true(inherits(sim_post, "PM_sim"))
  expect_equal(sim_ids(sim_post), c("1", "3"))
  expect_true(all(c("id", "nsim", names(NPex$final$popMean)) %in% names(sim_post$data$parValues)))
  expect_equal(nrow(sim_post$data$parValues), 4)
  expect_equal(nrow(sim_post$data$totalSets), 2)
})

test_that("PM_sim handles PM_result poppar with varied predInt, limits, and noise", {
  sim_pred_scalar <- suppressMessages(
    NPex$sim(
      include = 1,
      nsim = 2,
      predInt = 1
    )
  )

  expect_true(inherits(sim_pred_scalar, "PM_sim"))
  expect_true(all(c(1, 2, 3) %in% sim_pred_scalar$data$obs$time))

  sim_pred_vector <- suppressMessages(
    NPex$sim(
      include = 1,
      nsim = 2,
      predInt = c(0, 6, 2)
    )
  )

  expect_true(inherits(sim_pred_vector, "PM_sim"))
  expect_true(all(c(2, 4, 6) %in% sim_pred_vector$data$obs$time))

  sim_pred_list <- suppressMessages(
    NPex$sim(
      include = 1,
      nsim = 2,
      predInt = list(c(0, 4, 2), c(8, 10, 1))
    )
  )

  expect_true(inherits(sim_pred_list, "PM_sim"))
  expect_true(all(c(2, 4, 8, 9, 10) %in% sim_pred_list$data$obs$time))

  sim_limits_model <- suppressMessages(
    NPex$sim(
      include = 1:2,
      nsim = 3,
      limits = NA
    )
  )

  expect_true(inherits(sim_limits_model, "PM_sim"))
  expect_parvalues_within_limits(sim_limits_model, NPex$final$ab)

  mult_limits <- NPex$final$ab |>
    mutate(min = min * 0.8, max = max * 1.2)

  sim_limits_mult <- suppressMessages(
    NPex$sim(
      include = 1:2,
      nsim = 4,
      limits = c(0.8, 1.2)
    )
  )

  expect_true(inherits(sim_limits_mult, "PM_sim"))
  expect_parvalues_within_limits(sim_limits_mult, mult_limits)

  manual_limits <- tibble::tibble(
    par = c("ka", "ke", "v", "tlag1"),
    min = c(0.2, 0.01, 40, 0),
    max = c(0.8, 0.09, 110, 3)
  )

  sim_limits_manual <- suppressMessages(
    NPex$sim(
      include = 1:2,
      nsim = 4,
      limits = manual_limits
    )
  )

  expect_true(inherits(sim_limits_manual, "PM_sim"))
  expect_parvalues_within_limits(sim_limits_manual, manual_limits)

  sim_noise <- suppressMessages(
    NPex$sim(
      include = 1:2,
      nsim = 2,
      noise = list(out = list(coeff = c(0.05, 0.1)))
    )
  )

  expect_true(inherits(sim_noise, "PM_sim"))
  expect_equal(sim_ids(sim_noise), c("1", "2"))
  expect_equal(max(sim_noise$data$obs$nsim), 2)
})

test_that("PM_sim handles PM_final poppar when data and model are supplied", {
  sim_final <- suppressMessages(
    PM_sim$new(
      poppar = NPex$final,
      data = NPex$data,
      model = NPex$model,
      include = 1:2,
      nsim = 3,
      predInt = c(0, 6, 1),
      usePost = TRUE,
      limits = NA
    )
  )

  expect_true(inherits(sim_final, "PM_sim"))
  expect_equal(sim_ids(sim_final), c("1", "2"))
  expect_equal(nrow(sim_final$data$parValues), 6)
  expect_true(all(c("id", "nsim", names(NPex$final$popMean)) %in% names(sim_final$data$parValues)))
})

test_that("PM_sim handles manual-list poppar built from NPex final means and diagonal covariance", {
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

  sim_manual <- suppressMessages(
    PM_sim$new(
      poppar = manual_poppar,
      data = NPex$data,
      model = NPex$model,
      include = 1:2,
      nsim = 3,
      predInt = list(c(0, 4, 2), c(8, 10, 1)),
      limits = manual_limits
    )
  )

  expect_true(inherits(sim_manual, "PM_sim"))
  expect_equal(sim_ids(sim_manual), c("1", "2"))
  expect_equal(nrow(sim_manual$data$parValues), 3)
  expect_parvalues_within_limits(sim_manual, manual_limits)
  expect_true(all(c(2, 4, 8, 9, 10) %in% sim_manual$data$obs$time))
})

test_that("PM_sim handles data.frame poppar and ignores nsim", {
  pop_points_df <- NPex$final$popPoints |>
    as.data.frame() |>
    dplyr::select(dplyr::all_of(names(NPex$final$popMean)))

  sim_points <- suppressMessages(
    PM_sim$new(
      poppar = pop_points_df,
      data = NPex$data,
      model = NPex$model,
      include = 1,
      nsim = 2,
      predInt = 1
    )
  )

  expect_true(inherits(sim_points, "PM_sim"))
  expect_equal(sim_ids(sim_points), "1")
  expect_equal(nrow(sim_points$data$parValues), nrow(pop_points_df))
  expect_equal(max(sim_points$data$parValues$nsim), nrow(pop_points_df))
})

# Each template must be simulated from its own posterior, matched by id rather
# than by position. With `nsim = 1` the posterior mean is drawn exactly, so the
# simulated parameters show which posterior a subject was given.

posterior_params <- function(sim, id, pars) {
  p <- sim$data$parValues
  unlist(p[p$id == id, pars])
}

posterior_mean <- function(poppar, id, pars) {
  m <- poppar$postMean
  unlist(m[m$id == id, pars])
}

sim_from_posteriors <- function(poppar, data, include) {
  suppressMessages(
    PM_sim$new(
      poppar = poppar, data = data, model = NPex$model,
      include = include, nsim = 1, usePost = TRUE, quiet = TRUE
    )
  )
}

test_that("posteriors are paired by id, not by position", {
  match_posteriors <- getFromNamespace("match_posteriors", "Pmetrics")

  # posterior ids 10, 20 with the templates ordered 20, 10
  expect_equal(match_posteriors(c(20, 10), c(10, 20)), c(2, 1))
  # ids that are not 1..n, in the same order
  expect_equal(match_posteriors(c(10, 30), 10 * (1:3)), c(1, 3))

  expect_error(match_posteriors(c(1, 2), c(1, 1, 2)), "duplicate")
  expect_error(match_posteriors(c(1, 5), c(1, 2)), "No posterior mean")
})

test_that("each subject uses its own posterior when they are stored out of order", {
  pars <- names(NPex$final$popMean)
  reversed <- NPex$final$clone(deep = TRUE)
  n <- nrow(reversed$data$postMean)
  reversed$data$postMean <- reversed$data$postMean[n:1, ]
  reversed$data$postCov <- reversed$data$postCov[n:1]

  # the posteriors must differ, or the comparison below would prove nothing
  expect_false(isTRUE(all.equal(
    posterior_mean(reversed, 1, pars),
    posterior_mean(reversed, 3, pars)
  )))

  sim <- sim_from_posteriors(reversed, NPex$data, include = c(1, 3))

  for (id in c(1, 3)) {
    expect_equal(
      posterior_params(sim, id, pars),
      posterior_mean(reversed, id, pars),
      info = sprintf("subject %s should use its own posterior", id)
    )
  }

  # with no names on postCov the position is the only key available, and the two
  # structures are aligned because both were reversed
  unnamed <- reversed$clone(deep = TRUE)
  names(unnamed$data$postCov) <- NULL
  sim_unnamed <- sim_from_posteriors(unnamed, NPex$data, include = c(1, 3))
  expect_equal(
    posterior_params(sim_unnamed, 3, pars),
    posterior_mean(unnamed, 3, pars)
  )
})

test_that("subjects whose ids are not 1..n are paired by id", {
  pars <- names(NPex$final$popMean)

  relabelled <- NPex$data$standard_data
  relabelled$id <- 10 * relabelled$id
  data_10k <- PM_data$new(as.data.frame(relabelled), quiet = TRUE)

  tenths <- NPex$final$clone(deep = TRUE)
  tenths$data$postMean$id <- 10 * tenths$data$postMean$id
  names(tenths$data$postCov) <- 10 * as.numeric(names(tenths$data$postCov))

  sim <- sim_from_posteriors(tenths, data_10k, include = c(10, 30))

  for (id in c(10, 30)) {
    expect_equal(
      posterior_params(sim, id, pars),
      posterior_mean(tenths, id, pars),
      info = sprintf("subject %s should use its own posterior", id)
    )
  }
})
