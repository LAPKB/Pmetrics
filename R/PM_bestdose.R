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
#' @importFrom plotly plot_ly add_lines add_markers add_shape layout
#' @importFrom purrr list_rbind
#' @export
bd <- R6::R6Class(
    "bd",
    public = list(
        #' @field past PM_data object containing past patient data used in the optimization (if applicable)
        past = NULL,
        #' @field past_pred PM_sim object containing model predictions for the past data
        past_pred = NULL,
        #' @field target PM_data object containing target data used in the optimization
        target = NULL,
        #' @field target_pred PM_sim object containing model predictions for the target data
        target_pred = NULL,
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
        #' @param posterior `bd_post` object to associate with this result (optional, used when initializing from a previous result)
        #' @param bias_override Numeric bias weight to override the one in the result (optional, used when initializing from a previous result)
        #' @return A `bd` object containing the optimization results and associated information
        
        initialize = function(
            prior = NULL,
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
            posterior = NULL,
            bias_override = NULL) {
                if (!is.null(result)) {
                    private$.set_result(result, posterior, bias_override)
                    return(invisible(self))
                }
                
                if (is.null(target)) {
                    cli::cli_abort("Target must be supplied when computing a new BestDose.")
                }
                
                if (is.null(posterior)) {
                    posterior <- bd_post$new(
                        prior = prior,
                        model = model,
                        past_data = past_data,
                        max_cycles = max_cycles,
                        settings = settings
                    )
                }
                
                raw <- posterior$optimize(
                    target = target,
                    dose_range = dose_range,
                    bias_weight = bias_weight,
                    target_type = target_type,
                    time_offset = time_offset
                )
       
                self$past <- posterior$past # always defined or NULL
                
                private$.set_result(target, raw$result, posterior, raw$bias_weight)
                private$.sim_past()
                private$.sim_target()
                
                invisible(self)
        },
            
            #' @description
            #' Print summary of BestDose results
            print = function() {
                cat("BestDose Optimization Results\n")
                cat("==============================\n\n")
                cat(sprintf("Optimal doses: [%.2f, %.2f] mg\n", self$doses[1], self$doses[2]))
                cat(sprintf("Objective function: %.10f\n", self$objf))
                cat(sprintf("ln(Objective): %.4f\n", log(self$objf)))
                cat(sprintf("Method: %s\n", self$method))
                cat(sprintf("Status: %s\n", self$status))
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
            #' Save results to RDS file
            #' @param filename Path to save file. Default: "bestdose_result.rds"
            save = function(filename = "bestdose_result.rds") {
                saveRDS(self, filename)
                cli::cli_alert_success("Results saved to {filename}")
                invisible(self)
            }
            
        ),
        private = list(
            .set_result = function(target, result, posterior, bias_weight) {
                
                self$target <- PM_data$new(target, quiet = TRUE) 
                self$result <- result
                self$posterior <- posterior
                self$bias_weight <- bias_weight
                return(invisible(self))
            },
            .sim_past = function(){
                # simulate concentrations in the past if available based on posterior support points and weights
                if (is.null(self$past)) {
                    return(invisible(NULL)) 
                }
                # use the $sim function in the model to simualtete concentrations at the past time points based on the posterior support points and weights
                sim1 <- PM_sim$new(
                    poppar = as.data.frame(self$posterior$theta),
                    model = self$posterior$model_info$model,
                    data = self$past,
                    predInt = 1,
                    limits = NA,
                    quiet = TRUE
                )
                self$past_pred <- sim1
                return(invisible(self))
            },
            .sim_target = function(){
                # simulate concentrations in the future
                if (is.null(self$target)) {
                    return(invisible(NULL)) 
                }
                # replace the doses in the target with the optimal doses found by BestDose optimization
                target_sim_data <- self$target$standard_data
                target_sim_data$dose[target_sim_data$evid != 0] <- self$result$doses # replace doses with optimal doses for all dosing events
                self$target$standard_data <- target_sim_data
                

                sim2 <- PM_sim$new(
                    poppar = as.data.frame(self$posterior$theta),
                    model = self$posterior$model_info$model,
                    data = self$target,
                    predInt = 1,
                    limits = NA,
                    quiet = TRUE
                )
                self$target_pred <- sim2
                return(invisible(self))
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
                    self$past <- if(!is.null(past_data)) PM_data$new(past_data, quiet = TRUE) else NULL
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
                optimize = function(
                    target,
                    dose_range = list(min = 0, max = 1000),
                    bias_weight = 0.5,
                    target_type = "concentration",
                    time_offset = NULL) {
                        raw <- private$.run_optimize(target, dose_range, bias_weight, target_type, time_offset)
                        return(list(
                            result = raw$result,
                            posterior_obj = self,
                            bias_override = raw$bias_weight
                        ))
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


#' @export
plot.bd <- function(x, include, exclude, mult = 1, outeq = 1, 
                    quiet = FALSE, legend = FALSE, log = TRUE, 
                    grid = FALSE, xlab, ylab, title, xlim, ylim, print = TRUE, ...) {
    if (!inherits(x, "bd")) {
        stop("Object must be of class 'bd'")
    }
    
    # Check if we have target predictions (past is optional)
    if (is.null(x$target_pred)) {
        cli::cli_abort(c(
            "x" = "Target predictions are required for plotting.",
            "i" = "BestDose object must have {.code target_pred}."
        ))
    }
    
    # Get starting date/time for x-axis datetime conversion
    if (!is.null(x$past) && !is.null(x$past$data)) {
        # Extract start date and time from past data
        start_row <- x$past$data[1, ]
        start_date_str <- as.character(start_row$date[1])
        start_time_str <- as.character(start_row$time[1])
        start_datetime <- as.POSIXct(
            paste(start_date_str, start_time_str),
            format = "%m/%d/%y %H:%M:%S",
            tz = "UTC"
        )
    } else {
        # Use today's date and closest hour from now
        now <- Sys.time()
        next_hour <- ceiling(as.numeric(format(now, "%H")))
        if (next_hour > 23) next_hour <- 0
        start_datetime <- as.POSIXct(
            paste0(format(now, "%Y-%m-%d"), " ", sprintf("%02d", next_hour), ":00:00"),
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
        )
        # If the ceiling hour is in the past, use it; otherwise use next hour
        if (start_datetime <= now) {
            start_datetime <- start_datetime + 3600  # Add 1 hour
        }
    }
    
    # Extract observation data from both simulations
    past_obs <- NULL
    if (!is.null(x$past_pred)) {
        past_obs <- x$past_pred$data$obs |>
            dplyr::filter(outeq == !!outeq)
    }
    
    target_obs <- x$target_pred$data$obs |>
        dplyr::filter(outeq == !!outeq)
    
    # Handle include/exclude
    if (missing(include)) {
        if (!is.null(past_obs)) {
            include <- unique(c(past_obs$id, target_obs$id))
        } else {
            include <- unique(target_obs$id)
        }
    }
    if (missing(exclude)) exclude <- NA
    
    if (!is.null(past_obs)) {
        past_obs <- past_obs |> includeExclude(include, exclude)
    }
    target_obs <- target_obs |> includeExclude(include, exclude)
    
    # Get the last time in past_obs for each id (if past exists)
    last_past_times <- NULL
    if (!is.null(past_obs) && nrow(past_obs) > 0) {
        last_past_times <- past_obs |>
            dplyr::group_by(id, nsim) |>
            dplyr::slice_max(time, n = 1) |>
            dplyr::select(id, nsim, last_time = time) |>
            dplyr::ungroup()
    }
    
    # Adjust target times by adding to the last past time (if past exists)
    if (!is.null(last_past_times) && nrow(last_past_times) > 0) {
        target_obs_adjusted <- target_obs |>
            dplyr::left_join(last_past_times, by = c("id", "nsim")) |>
            dplyr::mutate(time = time + last_time) |>
            dplyr::select(-last_time)
    } else {
        target_obs_adjusted <- target_obs
    }
    
    # Combine past and target observations
    if (!is.null(past_obs) && nrow(past_obs) > 0) {
        combined_obs <- dplyr::bind_rows(
            past_obs |> dplyr::mutate(source = "past"),
            target_obs_adjusted |> dplyr::mutate(source = "target")
        ) |>
            dplyr::arrange(id, nsim, time) |>
            dplyr::mutate(datetime = start_datetime + time * 3600)  # Convert hours to seconds
    } else {
        combined_obs <- target_obs_adjusted |>
            dplyr::mutate(source = "target") |>
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
    
    # Extract actual observations from past and target data
    past_data_obs <- NULL
    target_data_obs <- NULL
    past_doses <- NULL
    target_doses <- NULL
    
    if (!is.null(x$past)) {
        past_data_obs <- x$past$standard_data |>
            dplyr::filter(evid == 0, outeq == !!outeq) |>
            dplyr::select(id, time, out) |>
            includeExclude(include, exclude)
        
        # Apply multiplier
        past_data_obs$out <- past_data_obs$out * mult
        
        # Handle log scale
        if (log && any(past_data_obs$out <= 0, na.rm = TRUE)) {
            past_data_obs$out[past_data_obs$out <= 0] <- NA
        }
        
        past_data_obs <- past_data_obs |> dplyr::filter(!is.na(out))
        
        # Extract dose events (evid == 1)
        past_doses <- x$past$standard_data |>
            dplyr::filter(evid == 1) |>
            dplyr::select(id, time, dur, dose) |>
            includeExclude(include, exclude)
    }
    
    if (!is.null(x$target)) {
        target_data_obs <- x$target$standard_data |>
            dplyr::filter(evid == 0, outeq == !!outeq) |>
            dplyr::select(id, time, out) |>
            includeExclude(include, exclude)
        
        # Get last time from past data for each id to adjust target times
        if (!is.null(x$past)) {
            last_times <- x$past$standard_data |>
                dplyr::filter(evid == 0) |>
                dplyr::group_by(id) |>
                dplyr::slice_max(time, n = 1) |>
                dplyr::select(id, last_time = time) |>
                dplyr::ungroup() |>
                includeExclude(include, exclude)
            
            target_data_obs <- target_data_obs |>
                dplyr::left_join(last_times, by = "id") |>
                dplyr::mutate(time = time + last_time) |>
                dplyr::select(-last_time)
        }
        
        # Apply multiplier
        target_data_obs$out <- target_data_obs$out * mult
        
        # Handle log scale
        if (log && any(target_data_obs$out <= 0, na.rm = TRUE)) {
            target_data_obs$out[target_data_obs$out <= 0] <- NA
        }
        
        target_data_obs <- target_data_obs |> dplyr::filter(!is.na(out))
        
        # Extract dose events (evid == 1)
        target_doses <- x$target$standard_data |>
            dplyr::filter(evid == 1) |>
            dplyr::select(id, time, dur, dose) |>
            includeExclude(include, exclude)
        
        # Adjust target dose times if past data exists
        if (!is.null(x$past)) {
            last_times_dose <- x$past$standard_data |>
                dplyr::group_by(id) |>
                dplyr::slice_max(time, n = 1) |>
                dplyr::select(id, last_time = time) |>
                dplyr::ungroup() |>
                includeExclude(include, exclude)
            
            target_doses <- target_doses |>
                dplyr::left_join(last_times_dose, by = "id") |>
                dplyr::mutate(time = time + last_time) |>
                dplyr::select(-last_time)
        }
    }
    
    # Initialize plot
    p <- plotly::plot_ly()
    
    # Plot all individual simulation curves grouped by id and nsim
    for (id_val in unique(combined_obs$id)) {
        for (nsim_val in unique(combined_obs$nsim)) {
            this_sim <- combined_obs |>
                dplyr::filter(id == id_val, nsim == nsim_val) |>
                dplyr::arrange(datetime)
            
            if (nrow(this_sim) > 0) {
                p <- p |>
                    plotly::add_lines(
                        x = ~datetime, y = ~out, data = this_sim,
                        line = list(color = "dodgerblue", width = 1),
                        hovertemplate = "Time: %{x}<br>Out: %{y}<extra></extra>",
                        showlegend = FALSE
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
                showlegend = legend
            )
    }
    
    # Add target data observations as markers
    if (!is.null(target_data_obs) && nrow(target_data_obs) > 0) {
        target_data_obs$datetime <- start_datetime + target_data_obs$time * 3600
        p <- p |>
            plotly::add_markers(
                x = ~datetime, y = ~out, data = target_data_obs,
                marker = list(color = "red", size = 8, symbol = "circle"),
                name = "Target Observations",
                hovertemplate = "Time: %{x}<br>Out: %{y}<extra></extra>",
                showlegend = legend
            )
    }
    
    # Add dose indicators as markers/segments directly on the plot
    # Past doses (black)
    if (!is.null(past_doses) && nrow(past_doses) > 0) {
        # Get y-axis minimum for positioning
        y_min <- min(combined_obs$out, na.rm = TRUE)
        
        # Create data frame for dose segments
        for (i in 1:nrow(past_doses)) {
            dose_time <- past_doses$time[i]
            dose_datetime <- start_datetime + dose_time * 3600
            dose_dur <- past_doses$dur[i]
            dose_amt <- past_doses$dose[i]
            
            # Add upward arrow marker at dose time
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
    
    # Target doses (red)
    if (!is.null(target_doses) && nrow(target_doses) > 0) {
        # Get y-axis minimum for positioning
        y_min <- min(combined_obs$out, na.rm = TRUE)
        
        # Create data frame for dose segments
        for (i in 1:nrow(target_doses)) {
            dose_time <- target_doses$time[i]
            dose_datetime <- start_datetime + dose_time * 3600
            dose_dur <- target_doses$dur[i]
            dose_amt <- target_doses$dose[i]
            
            # Add upward arrow marker at dose time
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
            
            # If infusion, add marker at end time too
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
    
    # Set axis labels
    xlab_val <- if (missing(xlab)) "Time" else xlab
    ylab_val <- if (missing(ylab)) "Concentration" else ylab
    title_val <- if (missing(title)) "BestDose Predictions: Past and Target" else title
    
    # Prepare shapes for vertical dividing lines and background shading
    shapes_list <- list()
    annotations_list <- list()
    
    # Create layout
    layout_list <- list(
        title = list(text = title_val, font = list(size = 16)),
        xaxis = list(title = xlab_val, zeroline = FALSE, showgrid = grid),
        yaxis = list(
            title = ylab_val,
            type = if (log) "log" else "linear",
            zeroline = FALSE,
            showgrid = grid
        ),
        hovermode = "closest",
        showlegend = legend,
        shapes = shapes_list,
        annotations = annotations_list
    )
    
    # Set axis limits if provided
    if (!missing(xlim)) {
        layout_list$xaxis$range <- xlim
    }
    if (!missing(ylim)) {
        layout_list$yaxis$range <- ylim
    }
    
    # Apply layout
    p <- p |> plotly::layout(layout_list)
    
    if (print) print(p)
    return(invisible(p))
}
