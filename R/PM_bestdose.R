#' @title
#' Object to contain BestDose optimization results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This object is created after a successful BestDose optimization run.
#' BestDose finds optimal dosing regimens to achieve target drug concentrations
#' or AUC values using Bayesian optimization.
#'
#' @importFrom dplyr filter left_join group_by slice_max select ungroup bind_rows arrange pull group_map mutate
#' @importFrom plotly plot_ly add_lines add_markers layout
#' @importFrom purrr list_rbind
#' @export
bd <- R6::R6Class(
    "bd",
    public = list(
        #' @field past PM_data object containing past patient data used in the optimization (if applicable)
        past = NULL,
        #' @field past_pred PM_sim object containing model predictions for the past data
        past_pred = NULL,
        #' @field future PM_data object containing future data used in the optimization
        future = NULL,
        #' @field future_pred PM_sim object containing model predictions for the future data
        future_pred = NULL,
        #' @field result List containing optimization results, including optimal doses, predictions, and objective function value
        result = NULL,
        #' @field posterior The `bd_post` object used to compute the posterior distribution (if applicable)
        posterior = NULL,
        #' @field bias_weight The bias weight (lambda) used in the optimization (if applicable)
        bias_weight = NULL,
        #' @field time_offset The time offset applied to predictions (if applicable)
        time_offset = NULL,
        #' @description
        #' Initialize a `bd` object by running a one-shot BestDose optimization.
        #' Creates the posterior internally, then optimizes. For reusing the posterior
        #' across multiple optimizations, use `bd_post$new()` followed by `$optimize()`.
        #' @param prior Prior information for the model, can be a PM_result, PM_final, or path to theta.csv
        #' @param model PM_model object or path to compiled model
        #' @param past_data PM_data object or path to CSV file with past patient data (optional)
        #' @param target PM_data object or path to CSV file with target data (optional).
        #'   Provide either `target` or `future`, not both.
        #' @param future List describing future dosing/target setup with elements:
        #'   dose (required), frequency (default 24), route (default 0), number (default 1),
        #'   target (required), target_time (default 24), covariates (optional list).
        #'   Provide either `target` or `future`, not both.
        #' @param dose_range List with 'min' and 'max' elements defining the dose search range (default: 0 to 1000)
        #' @param bias_weight Numeric between 0 and 1 indicating the weight of bias in the optimization (default: 0.5)
        #' @param target_type Character string indicating the type of target: "concentration", "auc_from_zero", or "auc_from_last_dose" (default: "concentration")
        #' @param time_offset Numeric time offset to apply to predictions (optional)
        #' @param max_cycles Maximum number of optimization cycles for computing the posterior (default: 500)
        #' @param settings List of additional settings for posterior computation (optional)
        #' @param posterior `bd_post` object to use instead of computing a new one (optional).
        #'   When provided, `prior`, `model`, `past_data`, `max_cycles`, and `settings` are ignored.
        #' @param quiet Logical indicating whether to suppress verbose simulation output (default: FALSE)
        #' @return A `bd` object containing the optimization results and associated information

        initialize = function(prior = NULL,
                              model = NULL,
                              past_data = NULL,
                              target = NULL,
                              future = NULL,
                              dose_range = list(min = 0, max = 1000),
                              bias_weight = 0.5,
                              target_type = "concentration",
                              time_offset = NULL,
                              max_cycles = 500,
                              settings = NULL,
                              posterior = NULL,
                              quiet = FALSE) {
            # Resolve target data: either `target` (file/PM_data) or `future` (inline list)
            future_data <- NULL
            if (!is.null(target) && !is.null(future)) {
                cli::cli_abort("Provide either {.arg target} or {.arg future}, not both.")
            } else if (!is.null(future)) {
                future_data <- private$.build_future_data(future)
            } else if (!is.null(target)) {
                future_data <- target
            } else {
                cli::cli_abort("Either {.arg target} or {.arg future} must be supplied.")
            }

            if (is.null(posterior)) {
                if (is.null(prior) || is.null(model)) {
                    cli::cli_abort("Both {.arg prior} and {.arg model} are required when {.arg posterior} is not provided.")
                }
                posterior <- bd_post$new(
                    prior = prior,
                    model = model,
                    past_data = past_data,
                    max_cycles = max_cycles,
                    settings = settings,
                    quiet = quiet
                )
            }

            raw <- private$.optimize(posterior, future_data, dose_range, bias_weight, target_type, time_offset, quiet)

            private$.set_result(future_data, raw, posterior, bias_weight)
            self$past <- posterior$past
            self$time_offset <- time_offset

            private$.sim_past()
            private$.sim_future()

            invisible(self)
        },

        #' @description
        #' Print summary of BestDose results
        print = function() {
            cat("BestDose Optimization Results\n")
            cat("==============================\n\n")
            dose_str <- paste(sprintf("%.2f", self$doses), collapse = ", ")
            cat(sprintf("Optimal doses: [%s] mg\n", dose_str))
            cat(sprintf("Objective function: %.10f\n", self$objf))
            cat(sprintf("ln(Objective): %.4f\n", log(self$objf)))
            cat(sprintf("Method: %s\n", self$method))
            cat(sprintf("Status: %s\n", self$status))
            if (!is.null(self$bias_weight)) {
                cat(sprintf("Bias weight (lambda): %.2f\n", self$bias_weight))
            }
            if (!is.null(self$time_offset)) {
                cat(sprintf("Time offset: %.2f\n", self$time_offset))
            }
            cat(sprintf("\nNumber of predictions: %d\n", nrow(self$result$predictions)))
            if (!is.null(self$result$auc_predictions)) {
                cat(sprintf("Number of AUC predictions: %d\n", nrow(self$result$auc_predictions)))
            }
            invisible(self)
        },
        #' @description
        #' Save results to RDS file
        #' @param filename Path to save file. Default: "bestdose_result.rds"
        save = function(filename = "bestdose_result.rds") {
            saveRDS(self, filename)
            cli::cli_alert_success("Results saved to {filename}")
            invisible(self)
        },
        #' @description
        #' Plot observed and predicted concentrations over time for both past and future data
        #' @param ... Additional arguments passed to plot.bd function
        plot = function(...) {
            plot.bd(self, ...)
        }
    ),
    private = list(
        .build_future_data = function(future) {
            if (is.null(future$dose) || is.null(future$target)) {
                cli::cli_abort(c(
                    "x" = "Future setup must include both {.code dose} and {.code target}.",
                    "i" = "Provide {.code future = list(dose = ..., target = ...)}."
                ))
            }

            # Apply defaults
            future$frequency <- future$frequency %||% 24
            future$route <- future$route %||% 0
            future$number <- future$number %||% 1
            future$target_time <- future$target_time %||% 24

            future_data <- PM_data$new()

            addl <- as.integer(future$number) - 1L
            if (addl == 0) addl <- NA
            ii <- future$frequency
            if (is.na(addl)) ii <- NA

            # Add doses
            dose_args <- list(
                id = 1, time = 0, evid = 1,
                dose = future$dose, out = NA,
                dur = future$route,
                addl = addl, ii = ii,
                validate = FALSE
            )
            do.call(future_data$addEvent, dose_args)

            # Add covariates if specified
            if (!is.null(future$covariates) && length(future$covariates) > 0) {
                cov_args <- c(list(id = 1, validate = FALSE), future$covariates)
                do.call(future_data$addEvent, cov_args)
            }

            # Add observation events at target_time after each dose
            for (i in 0:(as.integer(future$number) - 1)) {
                obs_time <- i * future$frequency + future$target_time
                obs_args <- list(
                    id = 1, time = obs_time, evid = 0,
                    dose = NA, out = future$target,
                    validate = (i == as.integer(future$number) - 1)
                )
                do.call(future_data$addEvent, obs_args)
            }

            future_data
        },
        .optimize = function(posterior, target, dose_range, bias_weight, target_type, time_offset, quiet = FALSE) {
            if (is.null(posterior$handle)) {
                cli::cli_abort(c(
                    "x" = "bd_post object is not properly initialized.",
                    "i" = "Create a new {.cls bd_post} with {.code bd_post$new()}."
                ))
            }

            if (!target_type %in% c("concentration", "auc_from_zero", "auc_from_last_dose")) {
                cli::cli_abort("{.arg target_type} must be one of: concentration, auc_from_zero, auc_from_last_dose")
            }

            if (bias_weight < 0 || bias_weight > 1) {
                cli::cli_abort("{.arg bias_weight} must be between 0 and 1")
            }

            if (is.null(dose_range$min) || is.null(dose_range$max)) {
                cli::cli_abort("{.arg dose_range} must have both 'min' and 'max' elements")
            }

            if (dose_range$min >= dose_range$max) {
                cli::cli_abort("{.arg dose_range$min} must be less than {.arg dose_range$max}")
            }

            target_data_path <- bestdose_parse_data(target)

            res <- bestdose_optimize(
                posterior$handle,
                target_data_path,
                time_offset,
                dose_range$min,
                dose_range$max,
                bias_weight,
                target_type
            )

            if (is.character(res)) {
                cli::cli_abort(res)
            }

            res
        },
        .set_result = function(future, result, posterior, bias_weight) {
            if (!is.null(future)) {
                self$future <- if (inherits(future, "PM_data")) future else PM_data$new(future, quiet = TRUE)
            }
            self$result <- result
            self$posterior <- posterior
            self$bias_weight <- bias_weight
            invisible(self)
        },
        .sim_past = function() {
            if (is.null(self$past)) {
                return(invisible(NULL))
            }
            sim1 <- PM_sim$new(
                poppar = as.data.frame(self$posterior$theta),
                model = self$posterior$model_info$model,
                data = self$past,
                predInt = 1,
                limits = NA,
                quiet = TRUE
            )
            self$past_pred <- sim1
            invisible(self)
        },
        .sim_future = function() {
            if (is.null(self$future)) {
                return(invisible(NULL))
            }
            # Replace placeholder doses with optimized values (use tail to skip past fixed doses)
            future_sim_data <- self$future$standard_data
            n_future_doses <- sum(future_sim_data$evid != 0)
            future_sim_data$dose[future_sim_data$evid != 0] <- tail(self$result$doses, n_future_doses)
            self$future$standard_data <- future_sim_data

            sim2 <- PM_sim$new(
                poppar = as.data.frame(self$posterior$theta),
                model = self$posterior$model_info$model,
                data = self$future,
                predInt = 1,
                limits = NA,
                quiet = TRUE
            )
            self$future_pred <- sim2
            invisible(self)
        }
    ),
    active = list(
        #' @field doses Optimal doses found by BestDose optimization
        doses = function() {
            self$result$doses
        },
        #' @field objf Objective function value at the optimal doses
        objf = function() {
            self$result$objf
        },
        #' @field method Optimization method used
        method = function() {
            self$result$method
        },
        #' @field status Status of the optimization run
        status = function() {
            self$result$status
        }
    )
)

#' @title
#' Compute a reusable BestDose posterior
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Use `bd_post` to compute the Bayesian posterior once from
#' prior population data and patient history, then call `$optimize()` multiple
#' times with different targets, dose ranges, or bias weights.
#'
#' @export
bd_post <- R6::R6Class(
    "bd_post",
    public = list(
        #' @field handle Memory pointer to the computed posterior (opaque to users)
        handle = NULL,
        #' @field theta Matrix of support points in the posterior distribution
        theta = NULL,
        #' @field theta_dim Dimensions of the theta matrix
        theta_dim = NULL,
        #' @field param_names Names of the parameters in the posterior
        param_names = NULL,
        #' @field posterior_weights Weights of the posterior support points
        posterior_weights = NULL,
        #' @field population_weights Weights of the population support points
        population_weights = NULL,
        #' @field past PM_data object containing past patient data used in the posterior computation (if applicable)
        past = NULL,
        #' @field model_info Information about the model used
        model_info = NULL,
        #' @field settings Settings used for the posterior computation
        settings = NULL,
        #' @description
        #' Initialize the `bd_post` object by computing the posterior distribution from the given prior, model, and past data
        #' @param prior Prior information for the model, can be a PM_result, PM_final, or path to theta.csv
        #' @param model PM_model object or path to compiled model
        #' @param past_data PM_data object or path to CSV file with past patient data (optional)
        #' @param max_cycles Maximum number of optimization cycles for computing the posterior (default: 500)
        #' @param settings List of additional settings for posterior computation (optional)
        #' @param quiet Logical indicating whether to suppress verbose simulation output (default: FALSE)
        #' @return A `bd_post` object containing the computed posterior distribution and associated information

        initialize = function(prior,
                              model,
                              past_data = NULL,
                              max_cycles = 500,
                              settings = NULL,
                              quiet = FALSE) {
            prior_path <- bestdose_parse_prior(prior)
            model_info <- bestdose_parse_model(model)
            past_data_path <- if (!is.null(past_data)) bestdose_parse_data(past_data) else NULL

            if (is.null(settings)) {
                model_for_settings <- if (!is.null(model_info$model)) model_info$model else model
                settings <- bestdose_default_settings(prior, model_for_settings, max_cycles = max_cycles)
            }

            prep <- bestdose_prepare(
                model_path = model_info$path,
                prior_path = prior_path,
                past_data_path = past_data_path,
                params = settings,
                kind = model_info$kind
            )

            if (is.character(prep)) {
                cli::cli_abort(prep)
            }

            dim <- as.integer(prep$theta_dim)
            theta_matrix <- matrix(prep$theta_values, nrow = dim[1], ncol = dim[2])
            colnames(theta_matrix) <- prep$param_names

            self$handle <- prep$handle
            self$theta <- theta_matrix
            self$theta_dim <- dim
            self$param_names <- prep$param_names
            self$posterior_weights <- prep$posterior_weights
            self$population_weights <- prep$population_weights
            self$past <- if (!is.null(past_data)) PM_data$new(past_data, quiet = TRUE) else NULL
            self$model_info <- model_info
            self$settings <- settings
            private$.quiet <- quiet

            cli::cli_alert_success("BestDose posterior computed with {dim[1]} support points")
        },
        #' @description
        #' Run optimization and return a `bd` result object
        #' @param target PM_data object or path to CSV file with target data
        #' @param dose_range List with 'min' and 'max' elements defining the dose search range (default: 0 to 1000)
        #' @param bias_weight Numeric between 0 and 1 indicating the weight of bias in the optimization (default: 0.5)
        #' @param target_type Character string indicating the type of target: "concentration", "auc_from_zero", or "auc_from_last_dose" (default: "concentration")
        #' @param time_offset Numeric time offset to apply to predictions (optional)
        #' @param quiet Logical indicating whether to suppress verbose simulation output. If NULL, uses the quiet setting from posterior computation (default: NULL)
        #' @return A `bd` object containing the optimization results
        optimize = function(target,
                            dose_range = list(min = 0, max = 1000),
                            bias_weight = 0.5,
                            target_type = "concentration",
                            time_offset = NULL,
                            quiet = NULL) {
            if (is.null(quiet)) quiet <- private$.quiet
            bd$new(
                target = target,
                dose_range = dose_range,
                bias_weight = bias_weight,
                target_type = target_type,
                time_offset = time_offset,
                posterior = self,
                quiet = quiet
            )
        }
    ),
    private = list(
        .quiet = FALSE,
        finalize = function() {
            self$handle <- NULL
        }
    )
)

#' @export
bd$load <- function(filename = "bestdose_result.rds") {
    if (!file.exists(filename)) {
        cli::cli_abort("File not found: {filename}")
    }
    readRDS(filename)
}


############ HELPER FUNCTIONS ############

bestdose_parse_prior <- function(prior) {
    if (inherits(prior, "PM_result")) {
        theta_path <- file.path(prior$rundir, "outputs", "theta.csv")
        if (!file.exists(theta_path)) {
            cli::cli_abort("theta.csv not found in PM_result outputs")
        }
        theta_path
    } else if (inherits(prior, "PM_final")) {
        temp_path <- tempfile(fileext = ".csv")
        bestdose_write_prior_csv(prior, temp_path)
        temp_path
    } else if (is.character(prior)) {
        if (!file.exists(prior)) {
            cli::cli_abort("Prior file not found: {prior}")
        }
        prior
    } else {
        cli::cli_abort("prior must be PM_result, PM_final, or path to theta.csv")
    }
}

bestdose_write_prior_csv <- function(prior, path) {
    df <- as.data.frame(prior$popPoints)
    df$prob <- prior$popProb
    write.csv(df, path, row.names = FALSE, quote = FALSE)
}

bestdose_parse_model <- function(model) {
    if (inherits(model, "PM_model")) {
        compiled_path <- model$binary_path
        if (is.null(compiled_path) || !file.exists(compiled_path)) {
            cli::cli_abort("Model must be compiled first. Use model$compile()")
        }

        kind <- if (!is.null(model$model_list$analytical) && model$model_list$analytical) {
            "analytical"
        } else {
            "ode"
        }

        list(path = compiled_path, kind = kind, model = model)
    } else if (is.character(model)) {
        if (!file.exists(model)) {
            cli::cli_abort("Model file not found: {model}")
        }
        kind <- if (grepl("analytical", model, ignore.case = TRUE)) {
            "analytical"
        } else {
            "ode"
        }
        list(path = model, kind = kind, model = NULL)
    } else {
        cli::cli_abort("model must be PM_model or path to compiled model")
    }
}

bestdose_parse_data <- function(data) {
    if (inherits(data, "PM_data")) {
        temp_path <- tempfile(fileext = ".csv")
        write.csv(data$standard_data, temp_path, row.names = FALSE, quote = FALSE)
        temp_path
    } else if (is.character(data)) {
        if (!file.exists(data)) {
            cli::cli_abort("Data file not found: {data}")
        }
        data
    } else {
        cli::cli_abort("data must be PM_data or path to CSV file")
    }
}

bestdose_default_settings <- function(prior, model, max_cycles = 500) {
    param_ranges <- lapply(model$model_list$pri, function(x) {
        c(x$min, x$max)
    })
    names(param_ranges) <- tolower(names(param_ranges))

    list(
        algorithm = "NPAG",
        ranges = param_ranges,
        error_models = lapply(model$model_list$err, function(x) x$flatten()),
        max_cycles = max_cycles,
        points = 2028,
        seed = 22,
        prior = "prior.csv",
        idelta = 0.25,
        tad = 0.0
    )
}


#' Plot BestDose predictions
#'
#' @description
#' Plot observed and predicted concentrations over time for both past and future data,
#' with options to include/exclude specific subjects, apply a multiplier to the
#' concentrations, and customize the plot appearance. The top 5 posterior support
#' points are highlighted with distinct colors.
#'
#' @param x A `bd` object containing the best dose predictions.
#' @param include Vector of subject IDs to include in the plot. If missing, all subjects will be included.
#' @param exclude Vector of subject IDs to exclude from the plot. If missing, no subjects will be excluded.
#' @param mult Numeric multiplier to apply to the concentrations (default: 1, no scaling).
#' @param outeq Numeric value of outeq to filter observations for plotting (default: 1).
#' @param quiet Logical indicating whether to suppress messages (default: FALSE).
#' @param legend Logical indicating whether to display a legend (default: TRUE).
#' @param log Logical indicating whether to use a logarithmic scale for the y-axis (default: FALSE).
#' @param grid Logical indicating whether to display a grid (default: FALSE).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param title Title of the plot.
#' @param xlim Limits for the x-axis.
#' @param ylim Limits for the y-axis.
#' @param print Logical indicating whether to print the plot (default: TRUE).
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return Invisibly returns a plotly object.
#'
#' @export
#' @method plot bd
plot.bd <- function(x, include, exclude, mult = 1, outeq = 1,
                    quiet = FALSE, legend = TRUE, log = FALSE,
                    grid = FALSE, xlab = "Time", ylab = "Concentration",
                    title = NULL, xlim = NULL, ylim = NULL, print = TRUE, ...) {
    if (!inherits(x, "bd")) {
        stop("Object must be of class 'bd'")
    }

    # Check if we have future predictions (past is optional)
    if (is.null(x$future_pred)) {
        cli::cli_abort(c(
            "x" = "Future predictions are required for plotting.",
            "i" = "BestDose object must have {.code future_pred}."
        ))
    }

    # Get starting date/time for x-axis datetime conversion
    if (!is.null(x$past) && !is.null(x$past$data)) {
        start_row <- x$past$data[1, ]
        start_date_str <- as.character(start_row$date[1])
        start_time_str <- as.character(start_row$time[1])
        start_datetime <- as.POSIXct(
            paste(start_date_str, start_time_str),
            format = "%m/%d/%y %H:%M:%S",
            tz = "UTC"
        )
    } else {
        now <- Sys.time()
        next_hour <- ceiling(as.numeric(format(now, "%H")))
        if (next_hour > 23) next_hour <- 0
        start_datetime <- as.POSIXct(
            paste0(format(now, "%Y-%m-%d"), " ", sprintf("%02d", next_hour), ":00:00"),
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
        )
        if (start_datetime <= now) {
            start_datetime <- start_datetime + 3600
        }
    }

    # Extract observation data from both simulations
    past_obs <- NULL
    if (!is.null(x$past_pred)) {
        past_obs <- x$past_pred$data$obs |>
            dplyr::filter(outeq == !!outeq)
    }

    future_obs <- x$future_pred$data$obs |>
        dplyr::filter(outeq == !!outeq)

    # Handle include/exclude
    if (missing(include)) {
        if (!is.null(past_obs)) {
            include <- unique(c(past_obs$id, future_obs$id))
        } else {
            include <- unique(future_obs$id)
        }
    }
    if (missing(exclude)) exclude <- NA

    if (!is.null(past_obs)) {
        past_obs <- past_obs |> includeExclude(include, exclude)
    }
    future_obs <- future_obs |> includeExclude(include, exclude)

    # Get the last time in past_obs for each id (if past exists)
    last_past_times <- NULL
    if (!is.null(past_obs) && nrow(past_obs) > 0) {
        last_past_times <- past_obs |>
            dplyr::group_by(id, nsim) |>
            dplyr::slice_max(time, n = 1) |>
            dplyr::select(id, nsim, last_time = time) |>
            dplyr::ungroup()
    }

    # Adjust future times by adding to the last past time (if past exists)
    if (!is.null(last_past_times) && nrow(last_past_times) > 0) {
        future_obs_adjusted <- future_obs |>
            dplyr::left_join(last_past_times, by = c("id", "nsim")) |>
            dplyr::mutate(time = time + last_time) |>
            dplyr::select(-last_time)
    } else {
        future_obs_adjusted <- future_obs
    }

    # Combine past and future observations
    if (!is.null(past_obs) && nrow(past_obs) > 0) {
        combined_obs <- dplyr::bind_rows(
            past_obs |> dplyr::mutate(source = "past"),
            future_obs_adjusted |> dplyr::mutate(source = "future")
        ) |>
            dplyr::arrange(id, nsim, time) |>
            dplyr::mutate(datetime = start_datetime + time * 3600)
    } else {
        combined_obs <- future_obs_adjusted |>
            dplyr::mutate(source = "future") |>
            dplyr::arrange(id, nsim, time) |>
            dplyr::mutate(datetime = start_datetime + time * 3600)
    }

    # Apply multiplier
    combined_obs$out <- combined_obs$out * mult

    # Handle log scale
    if (log) {
        if (any(combined_obs$out <= 0, na.rm = TRUE)) {
            if (!quiet) {
                cat("Values <= 0 omitted from log plot.\n")
            }
            combined_obs$out[combined_obs$out <= 0] <- NA
        }
    }

    # Remove NA values
    combined_obs <- combined_obs |> dplyr::filter(!is.na(out))

    # Extract actual observations from past and future data
    past_data_obs <- NULL
    future_data_obs <- NULL
    past_doses <- NULL
    future_doses <- NULL

    if (!is.null(x$past)) {
        past_data_obs <- x$past$standard_data |>
            dplyr::filter(evid == 0, outeq == !!outeq) |>
            dplyr::select(id, time, out) |>
            includeExclude(include, exclude)

        past_data_obs$out <- past_data_obs$out * mult

        if (log && any(past_data_obs$out <= 0, na.rm = TRUE)) {
            past_data_obs$out[past_data_obs$out <= 0] <- NA
        }

        past_data_obs <- past_data_obs |> dplyr::filter(!is.na(out))

        past_doses <- x$past$standard_data |>
            dplyr::filter(evid == 1) |>
            dplyr::select(id, time, dur, dose) |>
            includeExclude(include, exclude)
    }

    if (!is.null(x$future)) {
        future_data_obs <- x$future$standard_data |>
            dplyr::filter(evid == 0, outeq == !!outeq) |>
            dplyr::select(id, time, out) |>
            includeExclude(include, exclude)

        # Adjust future times if past data exists
        if (!is.null(x$past)) {
            last_times <- x$past$standard_data |>
                dplyr::filter(evid == 0) |>
                dplyr::group_by(id) |>
                dplyr::slice_max(time, n = 1) |>
                dplyr::select(id, last_time = time) |>
                dplyr::ungroup() |>
                includeExclude(include, exclude)

            future_data_obs <- future_data_obs |>
                dplyr::left_join(last_times, by = "id") |>
                dplyr::mutate(time = time + last_time) |>
                dplyr::select(-last_time)
        }

        future_data_obs$out <- future_data_obs$out * mult

        if (log && any(future_data_obs$out <= 0, na.rm = TRUE)) {
            future_data_obs$out[future_data_obs$out <= 0] <- NA
        }

        future_data_obs <- future_data_obs |> dplyr::filter(!is.na(out))

        future_doses <- x$future$standard_data |>
            dplyr::filter(evid == 1) |>
            dplyr::select(id, time, dur, dose) |>
            includeExclude(include, exclude)

        # Adjust future dose times if past data exists
        if (!is.null(x$past)) {
            last_times_dose <- x$past$standard_data |>
                dplyr::group_by(id) |>
                dplyr::slice_max(time, n = 1) |>
                dplyr::select(id, last_time = time) |>
                dplyr::ungroup() |>
                includeExclude(include, exclude)

            future_doses <- future_doses |>
                dplyr::left_join(last_times_dose, by = "id") |>
                dplyr::mutate(time = time + last_time) |>
                dplyr::select(-last_time)
        }
    }

    # Initialize plot
    p <- plotly::plot_ly()

    # Get posterior weights and robustly map them to nsim values
    posterior_weights <- as.numeric(x$posterior$posterior_weights)
    top5_colors <- c("#FF0000", "#0000FF", "#008000", "#800080", "#FFA500")

    unique_nsim <- sort(unique(combined_obs$nsim))
    nsim_keys <- as.character(unique_nsim)

    sim_index_by_nsim <- setNames(seq_along(unique_nsim), nsim_keys)

    weight_by_nsim <- setNames(rep(NA_real_, length(nsim_keys)), nsim_keys)
    for (k in nsim_keys) {
        idx <- sim_index_by_nsim[[k]]
        if (!is.na(idx) && idx >= 1 && idx <= length(posterior_weights)) {
            weight_by_nsim[[k]] <- posterior_weights[idx]
        }
    }

    # Identify top 5 nsim keys by weight (largest to smallest)
    ranked_keys <- names(sort(weight_by_nsim, decreasing = TRUE, na.last = NA))
    top_keys <- ranked_keys[seq_len(min(5, length(ranked_keys)))]

    color_by_nsim <- setNames(rep("#cccccc", length(nsim_keys)), nsim_keys)
    legend_name_by_nsim <- setNames(rep(NA_character_, length(nsim_keys)), nsim_keys)
    for (i in seq_along(top_keys)) {
        k <- top_keys[i]
        color_by_nsim[[k]] <- top5_colors[i]
        legend_name_by_nsim[[k]] <- sprintf("%.4e", weight_by_nsim[[k]])
    }

    legend_added <- setNames(rep(FALSE, length(nsim_keys)), nsim_keys)
    id_values <- unique(combined_obs$id)
    non_top_keys <- setdiff(nsim_keys, top_keys)

    # 1) Plot non-top lines first (light grey, no legend)
    for (nsim_key in non_top_keys) {
        weight_val <- weight_by_nsim[[nsim_key]]
        for (id_val in id_values) {
            this_sim <- combined_obs |>
                dplyr::filter(id == id_val, as.character(nsim) == nsim_key) |>
                dplyr::arrange(datetime)

            if (nrow(this_sim) > 0) {
                p <- p |>
                    plotly::add_lines(
                        x = ~datetime, y = ~out, data = this_sim,
                        line = list(color = "#cccccc", width = 0.8),
                        opacity = 0.6,
                        name = NULL,
                        legendgroup = NULL,
                        hovertemplate = sprintf(
                            "Time: %%{x}<br>Out: %%{y}<br>Weight: %s<extra></extra>",
                            ifelse(is.na(weight_val), "NA", sprintf("%.4e", weight_val))
                        ),
                        showlegend = FALSE
                    )
            }
        }
    }

    # 2) Plot top lines second (colored, legend in descending weight order)
    for (nsim_key in top_keys) {
        line_color <- color_by_nsim[[nsim_key]]
        legend_name <- legend_name_by_nsim[[nsim_key]]
        weight_val <- weight_by_nsim[[nsim_key]]

        for (id_val in id_values) {
            this_sim <- combined_obs |>
                dplyr::filter(id == id_val, as.character(nsim) == nsim_key) |>
                dplyr::arrange(datetime)

            if (nrow(this_sim) > 0) {
                show_in_legend <- !legend_added[[nsim_key]] && legend
                if (show_in_legend) {
                    legend_added[[nsim_key]] <- TRUE
                }
                legend_rank <- match(nsim_key, top_keys)

                p <- p |>
                    plotly::add_lines(
                        x = ~datetime, y = ~out, data = this_sim,
                        opacity = 0.6,
                        line = list(color = line_color, width = 2),
                        name = legend_name,
                        legendgroup = legend_name,
                        legendrank = legend_rank,
                        hovertemplate = sprintf(
                            "Time: %%{x}<br>Out: %%{y}<br>Weight: %s<extra></extra>",
                            ifelse(is.na(weight_val), "NA", sprintf("%.4e", weight_val))
                        ),
                        showlegend = show_in_legend
                    )
            }
        }
    }

    # Add past data observations as markers
    if (!is.null(past_data_obs) && nrow(past_data_obs) > 0) {
        past_data_obs$datetime <- start_datetime + past_data_obs$time * 3600
        p <- p |>
            plotly::add_markers(
                x = ~datetime, y = ~out, data = past_data_obs,
                marker = list(color = "black", size = 8, symbol = "circle"),
                name = "Past Observations",
                hovertemplate = "Time: %{x}<br>Out: %{y}<extra></extra>",
                showlegend = FALSE
            )
    }

    # Add future data observations as markers
    if (!is.null(future_data_obs) && nrow(future_data_obs) > 0) {
        future_data_obs$datetime <- start_datetime + future_data_obs$time * 3600
        p <- p |>
            plotly::add_markers(
                x = ~datetime, y = ~out, data = future_data_obs,
                marker = list(color = "red", size = 8, symbol = "circle"),
                name = "Future Observations",
                hovertemplate = "Time: %{x}<br>Out: %{y}<extra></extra>",
                showlegend = FALSE
            )
    }

    # Add dose indicators as markers/segments directly on the plot
    # Past doses (black triangles)
    if (!is.null(past_doses) && nrow(past_doses) > 0) {
        y_min <- min(combined_obs$out, na.rm = TRUE)

        for (i in 1:nrow(past_doses)) {
            dose_time <- past_doses$time[i]
            dose_datetime <- start_datetime + dose_time * 3600
            dose_dur <- past_doses$dur[i]
            dose_amt <- past_doses$dose[i]

            dur_text <- if (is.na(dose_dur) || dose_dur == 0) "Bolus" else sprintf("Duration: %.1f", dose_dur)
            p <- p |>
                plotly::add_markers(
                    x = dose_datetime,
                    y = y_min,
                    marker = list(
                        symbol = "triangle-up",
                        size = 10,
                        color = "black",
                        line = list(color = "black", width = 2)
                    ),
                    customdata = list(list(dose_amt, dur_text)),
                    hovertemplate = "Time: %{x}<br>Dose: %{customdata[0]}<br>%{customdata[1]}<extra></extra>",
                    showlegend = FALSE
                )

            # If infusion, add marker at end time too
            if (!is.na(dose_dur) && dose_dur > 0) {
                p <- p |>
                    plotly::add_markers(
                        x = dose_datetime + dose_dur * 3600,
                        y = y_min,
                        marker = list(
                            symbol = "triangle-down",
                            size = 10,
                            color = "black",
                            line = list(color = "black", width = 2)
                        ),
                        text = "End infusion",
                        hovertemplate = "Time: %{x}<br>%{text}<extra></extra>",
                        showlegend = FALSE
                    )
            }
        }
    }

    # Future doses (red triangles)
    if (!is.null(future_doses) && nrow(future_doses) > 0) {
        y_min <- min(combined_obs$out, na.rm = TRUE)

        for (i in 1:nrow(future_doses)) {
            dose_time <- future_doses$time[i]
            dose_datetime <- start_datetime + dose_time * 3600
            dose_dur <- future_doses$dur[i]
            dose_amt <- future_doses$dose[i]

            dur_text <- if (is.na(dose_dur) || dose_dur == 0) "Bolus" else sprintf("Duration: %.1f", dose_dur)
            p <- p |>
                plotly::add_markers(
                    x = dose_datetime,
                    y = y_min,
                    marker = list(
                        symbol = "triangle-up",
                        size = 10,
                        color = "red",
                        line = list(color = "red", width = 2)
                    ),
                    customdata = list(list(dose_amt, dur_text)),
                    hovertemplate = "Time: %{x}<br>Dose: %{customdata[0]}<br>%{customdata[1]}<extra></extra>",
                    showlegend = FALSE
                )

            if (!is.na(dose_dur) && dose_dur > 0) {
                p <- p |>
                    plotly::add_markers(
                        x = dose_datetime + dose_dur * 3600,
                        y = y_min,
                        marker = list(
                            symbol = "triangle-down",
                            size = 10,
                            color = "red",
                            line = list(color = "red", width = 2)
                        ),
                        text = "End infusion",
                        hovertemplate = "Time: %{x}<br>%{text}<extra></extra>",
                        showlegend = FALSE
                    )
            }
        }
    }

    # Set axis labels and layout
    xlab_val <- if (missing(xlab)) "Time" else xlab
    ylab_val <- if (missing(ylab)) "Concentration" else ylab
    title_val <- if (missing(title)) "BestDose Predictions: Past and Future" else title

    p$x$layout$title <- title_val
    p$x$layout$xaxis <- list(
        title = xlab_val,
        zeroline = FALSE,
        showgrid = grid
    )
    p$x$layout$yaxis <- list(
        title = ylab_val,
        type = if (log) "log" else "linear",
        zeroline = FALSE,
        showgrid = grid
    )
    p$x$layout$hovermode <- "closest"
    p$x$layout$showlegend <- legend
    p$x$layout$legend <- list(traceorder = "normal")

    if (!missing(xlim)) {
        p$x$layout$xaxis$range <- xlim
    }
    if (!missing(ylim)) {
        p$x$layout$yaxis$range <- ylim
    }

    if (print) print(p)
    return(invisible(p))
}
