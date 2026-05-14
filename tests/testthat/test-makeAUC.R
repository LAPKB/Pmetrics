trap_auc_linear <- function(df) {
  df <- df |>
  dplyr::arrange(time)
  
  if (nrow(df) < 2) {
    return(NA_real_)
  }
  
  dt <- diff(df$time)
  cc <- df$out[-1] + df$out[-nrow(df)]
  0.5 * sum(dt * cc)
}

test_that("make_AUC validates required inputs for non-PM objects", {
  expect_error(
    make_AUC(),
    "Please supply a data object"
  )
  
  expect_error(
    make_AUC(data = datasets::Theoph),
    "Please supply a formula"
  )
  
  expect_error(
    make_AUC(data = datasets::Theoph, formula = conc ~ bad_time | Subject),
    "not a variable in the data"
  )
  
  expect_error(
    make_AUC(data = datasets::Theoph, formula = bad_conc ~ Time | Subject),
    "not a variable in the data"
  )
})

test_that("make_AUC works for NPex PM_result object components", {
  expect_s3_class(NPex, "PM_result")
  
  auc_op_obj <- make_AUC(NPex$op)
  auc_op_data <- make_AUC(NPex$op$data)
  auc_pop_obj <- make_AUC(NPex$pop)
  auc_pop_data <- make_AUC(NPex$pop$data)
  auc_post_obj <- make_AUC(NPex$post)
  auc_post_data <- make_AUC(NPex$post$data)
  auc_pm_data <- make_AUC(NPex$data)
  
  expect_equal(names(auc_op_obj), c("id", "outeq", "block", "tau"))
  expect_equal(names(auc_pop_obj), c("id", "outeq", "block", "tau"))
  expect_equal(names(auc_post_obj), c("id", "outeq", "block", "tau"))
  expect_equal(names(auc_pm_data), c("id", "outeq", "block", "tau"))
  
  expect_equal(auc_op_obj, auc_op_data)
  
  expect_equal(auc_op_obj, auc_op_data)
  expect_equal(auc_pop_obj, auc_pop_data)
  expect_equal(auc_post_obj, auc_post_data)
  
  expect_equal(make_AUC(NPex$op), NPex$auc("op"))
  expect_equal(make_AUC(NPex$pop), NPex$auc("pop"))
})

test_that("make_AUC supports Theoph as external data with formula inputs", {
  ids <- sort(unique(NPex$op$data$id))
  include_ids <- ids[(c(1, 3, 4))]
  exclude_ids <- 2
  
  
  auc_window <- make_AUC(
    data = NPex$op,
    include = include_ids,
    start = 120,
    end = 132
  )
  
  expected_ids <- include_ids[!include_ids %in% exclude_ids]
  expect_equal(auc_window$id, expected_ids)
  
  manual <- NPex$op$data |>
  dplyr::filter(
    id %in% expected_ids,
    time >= 120,
    time <= 132,
    outeq == 1,
    block == 1,
    !is.na(obs)
  ) |>
  dplyr::mutate(out = obs) |>
  dplyr::group_by(id) |>
  dplyr::summarise(tau = trap_auc_linear(dplyr::pick(time, out)), .groups = "drop")
  
  joined <- dplyr::left_join(auc_window, manual, by = "id", suffix = c("_calc", "_manual"))
  expect_equal(joined$tau_calc, joined$tau_manual, tolerance = 1e-10)
})


testthat::test_that("make_AUC works with Theoph data and formula inputs", {
  theoph <- datasets::Theoph
  
  auc_grouped <- make_AUC(
    data = theoph,
    formula = conc ~ Time | Subject
  )
  
  manual_grouped <- theoph |>
  dplyr::transmute(id = Subject, time = Time, out = conc) |>
  dplyr::group_by(id) |>
  dplyr::summarise(tau = trap_auc_linear(dplyr::pick(time, out)), .groups = "drop")
  
  expect_equal(names(auc_grouped), c("Subject", "outeq", "block", "tau"))
  joined_grouped <- auc_grouped |>
  dplyr::rename(id = Subject, tau_calc = tau) |>
  dplyr::left_join(manual_grouped |> dplyr::rename(tau_manual = tau), by = "id")
  expect_equal(joined_grouped$tau_calc, joined_grouped$tau_manual, tolerance = 1e-10)
  
  theoph2 <- theoph |>
  dplyr::transmute(id = Subject, time = Time, out = conc)
  
  auc_default_group <- make_AUC(
    data = theoph2,
    formula = out ~ time
  )
  
  expect_equal(auc_default_group$tau, auc_grouped$tau, tolerance = 1e-10)
})

test_that("make_AUC addZero and method options behave as expected", {
  theoph_no_zero <- datasets::Theoph |>
  dplyr::filter(Time > 0)
  
  auc_no_zero <- make_AUC(
    data = theoph_no_zero,
    formula = conc ~ Time | Subject,
    addZero = FALSE
  )
  
  auc_add_zero <- make_AUC(
    data = theoph_no_zero,
    formula = conc ~ Time | Subject,
    addZero = TRUE
  )
  
  expect_true(any(auc_add_zero$tau > auc_no_zero$tau))
  
  auc_linear <- make_AUC(
    data = datasets::Theoph,
    formula = conc ~ Time | Subject,
    method = "linear"
  )
  
  auc_linlog <- make_AUC(
    data = datasets::Theoph,
    formula = conc ~ Time | Subject,
    method = "linlog"
  )
  
  expect_equal(nrow(auc_linear), nrow(auc_linlog))
  expect_true(any(abs(auc_linear$tau - auc_linlog$tau) > 1e-8))
})
