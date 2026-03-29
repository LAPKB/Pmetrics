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

  lib_model <- get(model_name, mode = "function")()

  eqn_block <- if (mode == "analytical") {
    eval(parse(text = sprintf("function(){ %s }", model_name)))
  } else {
    lib_model$arg_list$eqn
  }

  args <- list(
    pri = as.list(lib_model$arg_list$pri),
    eqn = eqn_block,
    out = lib_model$arg_list$out,
    err = lib_model$arg_list$err
  )

  if ("cov" %in% names(lib_model$arg_list)) {
    args$cov <- lib_model$arg_list$cov
  }

  if ("sec" %in% names(lib_model$arg_list)) {
    args$sec <- lib_model$arg_list$sec
  }

  if ("lag" %in% names(lib_model$arg_list)) {
    args$lag <- lib_model$arg_list$lag
  }

  if ("fa" %in% names(lib_model$arg_list)) {
    args$fa <- lib_model$arg_list$fa
  }

  if ("ini" %in% names(lib_model$arg_list)) {
    args$ini <- lib_model$arg_list$ini
  }

  do.call(PM_model$new, args)
}

compare_obs <- function(sim_analytic, sim_ode, tolerance = 1e-2) {
  obs_a <- sim_analytic$data$obs |>
    dplyr::arrange(id, nsim, time, outeq)

  obs_o <- sim_ode$data$obs |>
    dplyr::arrange(id, nsim, time, outeq)

  testthat::expect_equal(nrow(obs_a), nrow(obs_o))
  testthat::expect_equal(obs_a$id, obs_o$id)
  testthat::expect_equal(obs_a$nsim, obs_o$nsim)
  testthat::expect_equal(obs_a$time, obs_o$time, tolerance = tolerance)
  testthat::expect_equal(obs_a$outeq, obs_o$outeq)
  testthat::expect_equal(obs_a$out, obs_o$out, tolerance = tolerance)

  return(invisible(TRUE))

  
}

model_names <- model_lib(show = FALSE) |>
  dplyr::pull(Name) |>
  purrr::discard(~.x %in% c("one_comp_iv_cl", "one_comp_bolus"))

for (model_name in model_names) {
  testthat::test_that(paste("Analytical and ODE simulations agree for", model_name), {
    dat <- if (stringr::str_detect(model_name, "bolus")) {
      make_sim_bolus_template_data()
    } else if (stringr::str_detect(model_name, "iv")) {
      make_sim_iv_template_data()
    } else {
      stop("No template data defined for model ", model_name)
    }

    mod <- build_model_from_library(model_name, mode = "analytical")
    theta <- mod$model_list$pri |> purrr::map_df(\(x) x$mean)

    sim_analytic <- PM_sim$new(poppar = theta, model = mod, data = dat, predInt = 1)

    mod_ode <- build_model_from_library(model_name, mode = "ode")
    sim_ode <- PM_sim$new(poppar = theta, model = mod_ode, data = dat, predInt = 1)

    compare_obs(sim_analytic, sim_ode)
  })
}


run1 <- modEx$fit(data = dataEx, path = "~/Downloads", run = 1, overwrite = TRUE)
