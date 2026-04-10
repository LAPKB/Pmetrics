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

test_that("makeAUC validates required inputs for non-PM objects", {
  expect_error(
    makeAUC(),
    "Please supply a data object"
  )

  expect_error(
    makeAUC(data = datasets::Theoph),
    "Please supply a formula"
  )

  expect_error(
    makeAUC(data = datasets::Theoph, formula = conc ~ bad_time | Subject),
    "not a variable in the data"
  )

  expect_error(
    makeAUC(data = datasets::Theoph, formula = bad_conc ~ Time | Subject),
    "not a variable in the data"
  )
})

test_that("makeAUC works for NPex PM_result object components", {
  expect_s3_class(NPex, "PM_result")

  auc_op_obj <- makeAUC(NPex$op)
  auc_op_data <- makeAUC(NPex$op$data)
  auc_pop_obj <- makeAUC(NPex$pop)
  auc_pop_data <- makeAUC(NPex$pop$data)
  auc_post_obj <- makeAUC(NPex$post)
  auc_post_data <- makeAUC(NPex$post$data)
  auc_pm_data <- makeAUC(NPex$data)

  expect_equal(names(auc_op_obj), c("id", "tau"))
  expect_equal(names(auc_pop_obj), c("id", "tau"))
  expect_equal(names(auc_post_obj), c("id", "tau"))
  expect_equal(names(auc_pm_data), c("id", "tau"))

  expect_equal(auc_op_obj, auc_op_data)
  expect_equal(auc_pop_obj, auc_pop_data)
  expect_equal(auc_post_obj, auc_post_data)

  expect_equal(
    makeAUC(NPex$op),
    NPex$auc("op")
  )
  expect_equal(
    makeAUC(NPex$pop),
    NPex$auc("pop")
  )
  expect_equal(
    makeAUC(NPex$post),
    NPex$auc("post")
  )
})

test_that("makeAUC include/exclude/start/end filtering works with NPex", {
  ids <- sort(unique(NPex$op$data$id))
  include_ids <- ids[1:4]
  exclude_ids <- include_ids[2]

  auc_window <- makeAUC(
    data = NPex$op,
    include = include_ids,
    exclude = exclude_ids,
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

test_that("makeAUC supports Theoph as external data with formula inputs", {
  theoph <- datasets::Theoph

  auc_grouped <- makeAUC(
    data = theoph,
    formula = conc ~ Time | Subject
  )

  manual_grouped <- theoph |>
    dplyr::transmute(id = Subject, time = Time, out = conc) |>
    dplyr::group_by(id) |>
    dplyr::summarise(tau = trap_auc_linear(dplyr::pick(time, out)), .groups = "drop")

  expect_equal(names(auc_grouped), c("Subject", "tau"))
  joined_grouped <- auc_grouped |>
    dplyr::rename(id = Subject, tau_calc = tau) |>
    dplyr::left_join(manual_grouped |> dplyr::rename(tau_manual = tau), by = "id")
  expect_equal(joined_grouped$tau_calc, joined_grouped$tau_manual, tolerance = 1e-10)

  theoph2 <- theoph |>
    dplyr::transmute(id = Subject, time = Time, out = conc)

  auc_default_group <- makeAUC(
    data = theoph2,
    formula = out ~ time
  )

  expect_equal(auc_default_group$tau, auc_grouped$tau, tolerance = 1e-10)
})

test_that("makeAUC addZero and method options behave as expected", {
  theoph_no_zero <- datasets::Theoph |>
    dplyr::filter(Time > 0)

  auc_no_zero <- makeAUC(
    data = theoph_no_zero,
    formula = conc ~ Time | Subject,
    addZero = FALSE
  )

  auc_add_zero <- makeAUC(
    data = theoph_no_zero,
    formula = conc ~ Time | Subject,
    addZero = TRUE
  )

  expect_true(any(auc_add_zero$tau > auc_no_zero$tau))

  auc_linear <- makeAUC(
    data = datasets::Theoph,
    formula = conc ~ Time | Subject,
    method = "linear"
  )

  auc_linlog <- makeAUC(
    data = datasets::Theoph,
    formula = conc ~ Time | Subject,
    method = "linlog"
  )

  expect_equal(nrow(auc_linear), nrow(auc_linlog))
  expect_true(any(abs(auc_linear$tau - auc_linlog$tau) > 1e-8))
})
