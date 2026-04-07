library(Pmetrics)

testthat::skip_on_cran()

testthat::skip_if_not(
  is_cargo_installed(),
  message = "Cargo is required to compile and run simulation comparisons."
)

make_sim_bolus_template_data <- function() {
  PM_data$new()$addEvent(
    id = 1,
    time = 0,
    dose = 1000
  )$addEvent(
    id = 1,
    time = 12,
    out = -1,
    validate = TRUE
  )
}

make_sim_iv_template_data <- function() {
  PM_data$new()$addEvent(
    id = 1,
    time = 0,
    dose = 1000,
    dur = 1
  )$addEvent(
    id = 1,
    time = 12,
    out = -1,
    validate = TRUE
  )
}

compare_obs <- function(sim_analytic, sim_ode, tolerance = 1e-2) {
  obs_a <- sim_analytic$data$obs |>
    dplyr::arrange(id, nsim, time, outeq)

  obs_o <- sim_ode$data$obs |>
    dplyr::arrange(id, nsim, time, outeq)

  testthat::expect_equal(obs_a$out, obs_o$out, tolerance = tolerance)

  return(invisible(TRUE))
}

make_stable_theta <- function(mod) {
  theta <- vapply(mod$model_list$pri, function(x) x$mean, numeric(1))

  rate_idx <- grepl("^(ka|ke|k[0-9]+|cl|q[0-9]*)$", tolower(names(theta)))
  theta[rate_idx] <- theta[rate_idx] + seq_len(sum(rate_idx)) * 0.05

  volume_idx <- grepl("^v[0-9]*$", tolower(names(theta)))
  theta[volume_idx] <- theta[volume_idx] + seq_len(sum(volume_idx)) * 5

  as.data.frame(as.list(theta))
}

model_names <- model_lib(show = FALSE) |>
  dplyr::pull(Name)

bolus_data <- make_sim_bolus_template_data()
iv_data <- make_sim_iv_template_data()

for (model_name in model_names) {
  testthat::test_that(paste("Simulated Analytical and ODE observations agree for", model_name), {
    dat <- if (stringr::str_detect(model_name, "bolus")) {
      bolus_data
    } else if (stringr::str_detect(model_name, "iv")) {
      iv_data
    } else {
      stop("No template data defined for model ", model_name)
    }

    mod <- build_library_model(model_name, mode = "analytical")
    theta <- make_stable_theta(mod)

    sim_analytic <- PM_sim$new(poppar = theta, model = mod, data = dat, predInt = 1)

    mod_ode <- build_library_model(model_name, mode = "ode")
    sim_ode <- PM_sim$new(poppar = theta, model = mod_ode, data = dat, predInt = 1)

    compare_obs(sim_analytic, sim_ode)
  })
}
