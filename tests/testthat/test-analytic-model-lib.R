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

build_model_from_library <- function(model_name, mode = c("analytical", "ode")) {
  mode <- match.arg(mode)

  lib_entry <- getFromNamespace("get_model_library_entry", "Pmetrics")(model_name)

  eqn_block <- if (mode == "analytical") {
    eval(parse(text = sprintf("function(){ %s }", model_name)))
  } else {
    lib_entry$arg_list$eqn
  }

  args <- list(
    pri = as.list(lib_entry$arg_list$pri),
    eqn = eqn_block,
    out = lib_entry$arg_list$out,
    err = as.list(lib_entry$arg_list$err)
  )

  if ("cov" %in% names(lib_entry$arg_list)) {
    args$cov <- lib_entry$arg_list$cov
  }

  if ("sec" %in% names(lib_entry$arg_list)) {
    args$sec <- lib_entry$arg_list$sec
  }

  if ("lag" %in% names(lib_entry$arg_list)) {
    args$lag <- lib_entry$arg_list$lag
  }

  if ("fa" %in% names(lib_entry$arg_list)) {
    args$fa <- lib_entry$arg_list$fa
  }

  if ("ini" %in% names(lib_entry$arg_list)) {
    args$ini <- lib_entry$arg_list$ini
  }

  do.call(PM_model$new, args)
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
  dplyr::pull(Name) |>
  purrr::discard(~.x %in% c("one_comp_iv_cl", "one_comp_bolus"))

for (model_name in model_names) {
  testthat::test_that(paste("Simulated Analytical and ODE observations agree for", model_name), {
    dat <- if (stringr::str_detect(model_name, "bolus")) {
      make_sim_bolus_template_data()
    } else if (stringr::str_detect(model_name, "iv")) {
      make_sim_iv_template_data()
    } else {
      stop("No template data defined for model ", model_name)
    }

    mod <- build_model_from_library(model_name, mode = "analytical")
    theta <- make_stable_theta(mod)

    sim_analytic <- PM_sim$new(poppar = theta, model = mod, data = dat, predInt = 1)

    mod_ode <- build_model_from_library(model_name, mode = "ode")
    sim_ode <- PM_sim$new(poppar = theta, model = mod_ode, data = dat, predInt = 1)

    compare_obs(sim_analytic, sim_ode)
  })
}



