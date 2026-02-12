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

bestdose_default_settings <- function(prior, model) {
    if (inherits(prior, "PM_result")) {
        return(prior$settings)
    }

    param_ranges <- lapply(model$model_list$pri, function(x) {
        c(x$min, x$max)
    })
    names(param_ranges) <- tolower(names(param_ranges))

    list(
        algorithm = "NPAG",
        ranges = param_ranges,
        error_models = list(
            list(
                initial = 0.0,
                type = "additive",
                coeff = c(0.0, 0.2, 0.0, 0.0)
            )
        ),
        max_cycles = 500,
        points = 2028,
        seed = 22,
        prior = "prior.csv",
        idelta = 0.25,
        tad = 0.0
    )
}

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
#' @export
PM_bestdose <- R6::R6Class(
    "PM_bestdose",
    public = list(
        result = NULL,
        problem = NULL,
        bias_weight = NULL,
        initialize = function(prior = NULL,
                              model = NULL,
                              past_data = NULL,
                              target = NULL,
                              dose_range = list(min = 0, max = 1000),
                              bias_weight = 0.5,
                              target_type = "concentration",
                              time_offset = NULL,
                              settings = NULL,
                              result = NULL,
                              problem_obj = NULL,
                              bias_override = NULL) {
            if (!is.null(result)) {
                private$.set_result(result, problem_obj, bias_override)
                return(invisible(self))
            }

            if (is.null(target)) {
                cli::cli_abort("target must be supplied when computing a new BestDose result")
            }

            problem <- PM_bestdose_problem$new(
                prior = prior,
                model = model,
                past_data = past_data,
                target = target,
                dose_range = dose_range,
                bias_weight = bias_weight,
                target_type = target_type,
                time_offset = time_offset,
                settings = settings
            )

            raw <- problem$optimize_raw(bias_weight = bias_weight)
            private$.set_result(raw$result, problem, raw$bias_weight)
            invisible(self)
        },

        #' @description
        #' Print summary of BestDose results
        print = function() {
            cat("BestDose Optimization Results\n")
            cat("==============================\n\n")
            cat(sprintf("Optimal doses: [%.2f, %.2f] mg\n", self$get_doses()[1], self$get_doses()[2]))
            cat(sprintf("Objective function: %.10f\n", self$get_objf()))
            cat(sprintf("ln(Objective): %.4f\n", log(self$get_objf())))
            cat(sprintf("Method: %s\n", self$get_method()))
            cat(sprintf("Status: %s\n", self$get_status()))
            if (!is.null(self$bias_weight)) {
                cat(sprintf("Bias weight (lambda): %.2f\n", self$bias_weight))
            }
            cat(sprintf("\nNumber of predictions: %d\n", nrow(self$result$predictions)))
            if (!is.null(self$result$auc_predictions)) {
                cat(sprintf("Number of AUC predictions: %d\n", nrow(self$result$auc_predictions)))
            }
            invisible(self)
        },

        #' @description
        #' Get optimal dose values
        #' @return Numeric vector of optimal doses
        get_doses = function() {
            self$result$doses
        },

        #' @description
        #' Get concentration-time predictions
        #' @return Data frame with predictions
        get_predictions = function() {
            self$result$predictions
        },

        #' @description
        #' Get AUC predictions (if available)
        #' @return Data frame with AUC predictions or NULL
        get_auc_predictions = function() {
            self$result$auc_predictions
        },

        #' @description
        #' Get objective function value
        #' @return Numeric objective function value
        get_objf = function() {
            self$result$objf
        },

        #' @description
        #' Get optimization status
        #' @return Character string with status
        get_status = function() {
            self$result$status
        },

        #' @description
        #' Get optimization method used
        #' @return Character string: "posterior" or "uniform"
        get_method = function() {
            self$result$method
        },

        #' @description
        #' Save results to RDS file
        #' @param filename Path to save file. Default: "bestdose_result.rds"
        save = function(filename = "bestdose_result.rds") {
            saveRDS(self, filename)
            cli::cli_alert_success("Results saved to {filename}")
            invisible(self)
        }
    ),
    private = list(
        .set_result = function(result, problem, bias_weight) {
            self$result <- result
            self$problem <- problem
            self$bias_weight <- bias_weight
        }
    )
)

#' @title
#' Prepare a reusable BestDose optimization problem
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Use `PM_bestdose_problem` to mirror the Rust workflow: compute posterior
#' support points once, inspect them in R, and solve for multiple bias weights
#' without repeating the expensive initialization step.
#'
#' @export
PM_bestdose_problem <- R6::R6Class(
    "PM_bestdose_problem",
    public = list(
        handle = NULL,
        theta = NULL,
        theta_dim = NULL,
        param_names = NULL,
        posterior_weights = NULL,
        population_weights = NULL,
        bias_weight = NULL,
        target_type = NULL,
        dose_range = NULL,
        model_info = NULL,
        settings = NULL,
        initialize = function(prior,
                              model,
                              past_data = NULL,
                              target,
                              dose_range = list(min = 0, max = 1000),
                              bias_weight = 0.5,
                              target_type = "concentration",
                              time_offset = NULL,
                              settings = NULL) {
            if (!target_type %in% c("concentration", "auc_from_zero", "auc_from_last_dose")) {
                cli::cli_abort("target_type must be one of: concentration, auc_from_zero, auc_from_last_dose")
            }

            if (bias_weight < 0 || bias_weight > 1) {
                cli::cli_abort("bias_weight must be between 0 and 1")
            }

            if (is.null(dose_range$min) || is.null(dose_range$max)) {
                cli::cli_abort("dose_range must have both 'min' and 'max' elements")
            }

            if (dose_range$min >= dose_range$max) {
                cli::cli_abort("dose_range$min must be less than dose_range$max")
            }

            prior_path <- bestdose_parse_prior(prior)
            model_info <- bestdose_parse_model(model)
            past_data_path <- if (!is.null(past_data)) bestdose_parse_data(past_data) else NULL
            target_data_path <- bestdose_parse_data(target)

            if (is.null(settings)) {
                model_for_settings <- if (!is.null(model_info$model)) model_info$model else model
                settings <- bestdose_default_settings(prior, model_for_settings)
            }

            prep <- bestdose_prepare(
                model_path = model_info$path,
                prior_path = prior_path,
                past_data_path = past_data_path,
                target_data_path = target_data_path,
                time_offset = time_offset,
                dose_min = dose_range$min,
                dose_max = dose_range$max,
                bias_weight = bias_weight,
                target_type = target_type,
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
            self$bias_weight <- prep$bias_weight
            self$target_type <- prep$target_type
            self$dose_range <- dose_range
            self$model_info <- model_info
            self$settings <- settings

            cli::cli_alert_success("BestDose problem prepared with %d support points", dim[1])
        },
        finalize = function() {
            self$handle <- NULL
        },

        #' @description
        #' Run optimization and return raw list (doses, objf, predictions)
        optimize_raw = function(bias_weight = NULL) {
            private$.run_optimize(bias_weight)
        },

        #' @description
        #' Run optimization and return a `PM_bestdose` result object
        optimize = function(bias_weight = NULL) {
            raw <- self$optimize_raw(bias_weight)
            PM_bestdose$new(
                result = raw$result,
                problem_obj = self,
                bias_override = raw$bias_weight
            )
        }
    ),
    private = list(
        .run_optimize = function(bias_weight) {
            if (is.null(self$handle)) {
                cli::cli_abort("BestDose problem handle has been released")
            }

            bw <- if (is.null(bias_weight)) self$bias_weight else bias_weight

            if (bw < 0 || bw > 1) {
                cli::cli_abort("bias_weight must be between 0 and 1")
            }

            res <- bestdose_optimize(self$handle, bw)
            if (is.character(res)) {
                cli::cli_abort(res)
            }

            list(result = res, bias_weight = bw)
        }
    )
)

#' @export
PM_bestdose$load <- function(filename = "bestdose_result.rds") {
    if (!file.exists(filename)) {
        cli::cli_abort("File not found: {filename}")
    }
    readRDS(filename)
}
