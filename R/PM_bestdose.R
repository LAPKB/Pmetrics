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
bd <- R6::R6Class(
    "bd",
    public = list(
        #' @field result List containing optimization results, including optimal doses, predictions, and objective function value
        result = NULL,
        #' @field posterior The `bd_post` object used to compute the posterior distribution (if applicable)
        posterior = NULL,
        #' @field bias_weight The bias weight (lambda) used in the optimization (if applicable)
        bias_weight = NULL,
        #' @description
        #' Initialize a `bd` object by running optimization or loading from a previous result
        #' @param prior Prior information for the model, can be a PM_result, PM_final, or path to theta.csv
        #' @param model PM_model object or path to compiled model
        #' @param past_data PM_data object or path to CSV file with past patient data (optional)
        #' @param target PM_data object or path to CSV file with target data (required for new optimization)
        #' @param dose_range List with 'min' and 'max' elements defining the dose search range (default: 0 to 1000)
        #' @param bias_weight Numeric between 0 and 1 indicating the weight of bias in the optimization (default: 0.5)
        #' @param target_type Character string indicating the type of target: "concentration", "auc_from_zero", or "auc_from_last_dose" (default: "concentration")
        #' @param time_offset Numeric time offset to apply to predictions (optional)
        #' @param max_cycles Maximum number of optimization cycles for computing the posterior (default: 500)
        #' @param settings List of additional settings for posterior computation (optional)
        #' @param result Raw result list from optimization to initialize from (optional)
        #' @param posterior_obj `bd_post` object to associate with this result (optional, used when initializing from a previous result)
        #' @param bias_override Numeric bias weight to override the one in the result (optional, used when initializing from a previous result)
        #' @return A `bd` object containing the optimization results and associated information

        initialize = function(prior = NULL,
                              model = NULL,
                              past_data = NULL,
                              target = NULL,
                              dose_range = list(min = 0, max = 1000),
                              bias_weight = 0.5,
                              target_type = "concentration",
                              time_offset = NULL,
                              max_cycles = 500,
                              settings = NULL,
                              result = NULL,
                              posterior_obj = NULL,
                              bias_override = NULL) {
            if (!is.null(result)) {
                private$.set_result(result, posterior_obj, bias_override)
                return(invisible(self))
            }

            if (is.null(target)) {
                cli::cli_abort("target must be supplied when computing a new BestDose result")
            }

            posterior <- bd_post$new(
                prior = prior,
                model = model,
                past_data = past_data,
                max_cycles = max_cycles,
                settings = settings
            )

            raw <- posterior$optimize(
                target = target,
                dose_range = dose_range,
                bias_weight = bias_weight,
                target_type = target_type,
                time_offset = time_offset
            )
            private$.set_result(raw$result, posterior, raw$bias_weight)
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
        .set_result = function(result, posterior, bias_weight) {
            self$result <- result
            self$posterior <- posterior
            self$bias_weight <- bias_weight
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
        #' @return A `bd_post` object containing the computed posterior distribution and associated information
    
        initialize = function(prior,
                              model,
                              past_data = NULL,
                              max_cycles = 500,
                              settings = NULL) {
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
            self$model_info <- model_info
            self$settings <- settings

            cli::cli_alert_success("BestDose posterior computed with {dim[1]} support points")
        },
        #' @description
        #' Run optimization and return a `bd` result object
        #' @param target PM_data object or path to CSV file with target data
        #' @param dose_range List with 'min' and 'max' elements defining the dose search range (default: 0 to 1000)
        #' @param bias_weight Numeric between 0 and 1 indicating the weight of bias in the optimization (default: 0.5)
        #' @param target_type Character string indicating the type of target: "concentration", "auc_from_zero", or "auc_from_last_dose" (default: "concentration")
        #' @param time_offset Numeric time offset to apply to predictions (optional)
        #' @return A `bd` object containing the optimization results for the given target and settings
        optimize = function(target,
                            dose_range = list(min = 0, max = 1000),
                            bias_weight = 0.5,
                            target_type = "concentration",
                            time_offset = NULL) {
            raw <- private$.run_optimize(target, dose_range, bias_weight, target_type, time_offset)
            bd$new(
                result = raw$result,
                posterior_obj = self,
                bias_override = raw$bias_weight
            )
        }
    ),
    private = list(
        .run_optimize = function(target, dose_range, bias_weight, target_type, time_offset) {
            if (is.null(self$handle)) {
                cli::cli_abort(c( x = "bd_post object is not properly initialized",
                "i" = "Re-run `bd$new()`."   ))
            }

            if (!target_type %in% c("concentration", "auc_from_zero", "auc_from_last_dose")) {
                cli::cli_abort(" {.arg target_type} must be one of: concentration, auc_from_zero, auc_from_last_dose")
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
                self$handle,
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

            list(result = res, bias_weight = bias_weight)
        },
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