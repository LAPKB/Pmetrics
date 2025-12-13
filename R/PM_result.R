# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ---------------------------------------------------------------


#' @title Results of a Pmetrics run
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object contains all of the results after a Pmetrics runs. It is created
#' by using the [PM_load] function.
#'
#' @details After a run completes, results are stored on your hard drive. They are loaded
#' back into R with [PM_load] to create the [PM_result] object, which contains both
#' the results and functions to analyze or plot the result.
#'
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_result <- R6::R6Class(
  "PM_result",
  public <- list(
    #' @field pop A [PM_pop] object
    pop = NULL,
    #' @field post A [PM_post] object
    post = NULL,
    #' @field final A [PM_final] object
    final = NULL,
    #' @field cycle A [PM_cycle] object
    cycle = NULL,
    #' @field op A [PM_op] object
    op = NULL,
    #' @field cov A [PM_cov] object
    cov = NULL,
    #' @field data [PM_data] object representing the original .csv data file used in the run. The predictions contained in the `$data` fields from `$pop` and `$post` will be added to this [PM_data] object to permit easy addition of such predictions to raw data plots. See [plot.PM_data] for more details.
    data = NULL,
    #' @field model text string representing the original model file used in the run
    model = NULL,
    #' @field errfile Name of error file if it exists
    errfile = NULL,
    #' @field success Boolean if successful run
    success = NULL,
    #' @field valid If the `$validate` method has been executed after a run,
    #' this field will contain the information required to plot
    #' visual predictive checks and normalized prediction
    #' error discrepancies via the npde code developed by Comets et al. Use the
    #' `$save` method on the augmented `PM_result` object to save it with the
    #' new validation results.
    valid = NULL,
    #' @field opt_samp If the `$opt` method has been executed after a run, this
    #' field will contain a [PM_opt] object which has optimal sampling times
    #' and methods to plot them.
    #' Use the `$save` method on the augmented `PM_result` object to save it with the
    #' new optimal sampling results.
    opt_samp = NULL,
    
    #' @description
    #' Create new object populated with data from previous run
    #' @details
    #' Creation of new `PM_result` objects is via [PM_load].
    #' @param out The parsed output from [PM_load], which is
    #' automatically generated. This is not a user-modifiable.
    #' @param path include `r template("path")`.
    #' @param quiet Quietly validate. Default is `FALSE`.
    initialize = function(out, path = ".", quiet = TRUE) {
      # the following were saved as R6 objects
      purrr::walk(
        c("pop", "post", "final", "cycle", "op", "cov", "data", "model", "valid"),
        \(x){
          self[[x]] <- NULL
          if (!is.null(out[[x]])) { # if the object is loaded...
            if (!inherits(out[[x]], "R6")) { # older save
              cli::cli_abort(c("x" = "The object was saved in an older format. Please re-run the analysis."))
            } else {
              if(x == "model"){
                args <- list(x  = out[[x]], compile = FALSE)
              } else { 
                args <- list(out[[x]], path = path, quiet = TRUE)
              }
              self[[x]] <- do.call(get(paste0("PM_", x))$new, args = args) # was saved in R6 format, but remake to update if needed
            }
          }
        }
      )
      
      # these are diagnostics, not R6
      self$errfile <- out$errfile
      self$success <- out$success
      
      # add the pop/post data to data
      if (is.null(self$data$pop) | is.null(self$data$post)) {
        self$data <- PM_data$new(self$data$data, quiet = TRUE)
        self$data$pop <- self$pop$data
        self$data$post <- self$post$data
      }
      
      return(self)
    },
    #' @description
    #' Fit the model to the data
    #' #' @details
    #' This method is used to fit the model in the [PM_result] object to data. 
    #' It calls the `$fit` method of the model stored in the `model` field.
    #' @param data Optional data to fit. If not provided, the data stored in the
    #' `data` field of the [PM_result] object will be used. This can be useful to
    #' continue a prior run that did not converge before the maximum number of cycles,
    #' e.g. `run2 <- run1$fit(cycles = 10000, prior = 1)`
    #' @param ... Additional arguments passed to the model's `$fit` method.
    #' @return Returns an invisible [PM_result].
    #' @export
    #' 
    fit = function(data, ...){
      if (missing(data)) {
        data <- self$data
      }
      res <- self$model$fit(data = data, ...)
      return(invisible(res))
    },
    
    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param ... Plot-specific arguments
    plot = function(type, ...) {
      if (is.null(type)) {
        cli::cli_abort(c("x" = "Please provide the type of plot."))
      } else {
        self[[type]]$plot(...)
      }
    },
    
    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param ... Summary-specific arguments
    summary = function(type, ...) {
      if (is.null(type)) {
        cli::cli_abort(c("x" = "Please provide the type of summary you want to obtain"))
      } else {
        self[[type]]$summary(...)
      }
    },
    
    #' @description
    #' AUC generic function based on type
    #' @param src Source of AUC, one of "op", "pop", "post", or "sim"
    #' @param ... Summary-specific arguments
    auc = function(src, ...) {
      if (!src %in% c("op", "pop", "post", "sim")) {
        cli::cli_abort(c("x" = "{.fn makeAUC} is defined only for {.cls PM_op}, {.cls PM_pop}, {.cls PM_post}, and {.cls PM_sim} objects."))
      }
      self[[src]]$auc(...)
    },
    
    #' @description
    #' Perform non-compartmental analysis
    #' @details
    #' See [makeNCA].
    #' @param ... Arguments passed to [makeNCA].
    nca = function(...) {
      make_NCA(self, ...)
    },
    #' @description Re-generate the report
    #' @param ... Parameters passed to [PM_report]. In particular, pay attention to `path`.
    report = function(...) {
      PM_report(self, ...)
    },
    #' @description
    #' Calls [PM_sim]. Default is to use the `$final`, `$model`, and `$data` objects
    #' within the [PM_result]. It is common to supply a different `data` template.
    #' Occasionally it is necessary to use a different `model` with the `$final` field,
    #' or vice versa. If all three are different, use `PM_sim$new()` instead.
    #' @param ...  Parameters passed to [PM_sim]. If using the `$final`, `$model`, and
    #' `$data` fields, it is not necessary to specify these. Alternates for any of these
    #' should be specified. Other parameters for [PM_sim] should be passed as named
    #' arguments, e.g. `$sim(include = 1:2, predInt = 1, limits = NA)`.
    sim = function(...) {
      dots <- list(...)
      
      dots$poppar <- self # send the PM_result object as poppar
      
      
      # if (!"data" %in% names(dots)) {
      #   dots$data <- self$data
      # }
      
      # if (!"model" %in% names(dots)) {
      #   dots$model <- self$model
      # }
      
      # store copy of the final object
      bk_final <- self$final$clone()
      sim <- do.call(PM_sim$new, dots)
      self$final <- bk_final
      return(sim)
    },
    
    #' @description
    #' Save the current PM_result object to an .Rdata file.
    #' @details
    #' This is useful if you have updated the result in some way, for example you
    #' have run the `$validate()` method on the `PM_result` object, which returns
    #' an internal simulation based validation as a new `valid` field. To save this
    #' validation, use this `$save` method. Note that unless a `file` name is provided,
    #' the changes will overwrite the
    #' previous run results, although unchanged items will be preserved. This is the
    #' usual workflow. However, a custom file name may be useful to share the run
    #' results with someone.
    #'
    #' The saved object is an .Rdata file. When loaded, it should be assigned to an R
    #' object, e.g. `run2 <- PM_result$new("filename")`. An equivalent statement would
    #' be `run2 <- PM_load(file = "filename")`.
    #' @param run The run output folder number to save the revised result. If missing,
    #' will save in the current working directory. For example, if folder "1" is in
    #' your current working directory, specify `run = 1` to save the result to the "outputs"
    #' subfolder of the "1" folder.
    #' @param file Custom file name. Default is "PMout.Rdata". If `run` is not specified, `file`
    #' should be the full path and filename. 
    save = function(run, file = "PMout.Rdata") {
      if (missing(run)) {
        cli::cli_inform(c(
          "i" = "No {.code run} argument was provided.",
          " " = "Saving the results as {.code file} in the current working directory."
        ))
        outputfolder <- getwd()
      } else {
        if (is.na(suppressWarnings(as.numeric(run)))) {
          cli::cli_abort(c("x" = "The {.code run} argument is not numeric. Do you need to say {.code file = }? See help for {.fn PM_result}."))
        }
        outputfolder <- paste0(run, "/outputs")
        if (!file.exists(outputfolder)) {
          cli::cli_abort(c("x" = "{outputfolder} does not exist in the current working directory."))
        }
      }
      PMout <- list(
        pop = self$pop$data, post = self$post$data,
        final = self$final$data, cycle = self$cycle$data,
        op = self$op$data, cov = self$cov$data, data = self$data$data,
        model = self$model, errfile = self$errfile,
        success = self$success,
        valid = self$valid
      )
      save(PMout, file = paste0(outputfolder, "/", file))
    },
    
    #' @description
    #' Validate the result by internal simulation methods.
    #' @param ... Arguments passed to [PM_valid].
    validate = function(...) {
      self$valid <- PM_valid$new(self, ...)
      cli::cli_inform(c(
        "i" = "Validation results were stored in the {.code $valid} field.",
        " " = "Use {.code $save()} method on your run results to save the validation results.",
        " " = "For example, if your results are in {.code my_run}, use {.code my_run$save(1)} to save back to the outputs folder of run 1."
      ))
      return(invisible(self))
      
    },
    
    #' @description
    #' Conduct stepwise linear regression of Bayesian posterior parameter values
    #' and covariates.
    #' @param ... Arguments passed to [PM_step].
    step = function(...) {
      PM_step(self$cov$data, ...)
    },
    
    #' @description
    #' Calculate optimal sampling times.
    #'
    #' Method to compute optimal sampling times.
    #' @details
    #' See [PM_opt] for details.
    #'
    #' @param ... Parameters to pass to [PM_opt].
    opt = function(...) {
      self$opt_samp <- tryCatch(PM_opt$new(self, ...), error = function(e) {
        cat(crayon::red("Error:"), e$message, "\n")
      })
      return(invisible(self))
    },
    
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load prior results saved with the `$save` method.
    #' Replaced by [PM_load].
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "PM_result$load() is deprecated. Please use PM_load() instead.")
    },
    #' @description
    #' Continue fitting the model to the data using the final population points
    #' from the prior run as the prior for the new run.
    #' @details
    #' This method is useful if a prior run did not converge before the maximum
    #' number of cycles. It uses the final population points from the prior run
    #' as the prior for a new run. The data and model are the same as in the prior run.
    continue = function(...) {
      self$model$fit(data = self$data, prior = self$final$popPoints, ...)
    }
  ) # end public
) # end PM_result

#' @keywords internal
#' @name PM_result
#' @export
PM_result$load <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "Please use PM_load() instead. ?PM_load for details.")
}









# LOAD --------------------------------------------------------------------
#' @title Load Pmetrics NPAG or IT2B output
#' @description
#' `r lifecycle::badge("stable")`
#' Loads all the data from a prior Pmetrics run.
#' @details
#' A combination of `run`, `path`, and `file` are used to locate the results.
#' * If `run` is provided, it is assumed that the results are in a subfolder
#' `/outputs` of the folder named by `run` within the `path` folder.
#' * If `run` is missing, the results are assumed to be in the `path` folder.
#' * The `file` name is the name of the Rdata file containing the results.
#' Default is "PMout.Rdata", which is created by Pmetrics after a run.
#' * If both `run` and `path` are missing, the current working directory is used
#' for `path`.
#' * If both `run` and `file` are missing, the current working directory is used
#' for `path` and "PMout.Rdata" is used for `file`.
#' * If both `path` and `file` are missing, the current working directory is used
#' for `path` and `run` is required.
#' * If all three are missing, the current working directory is used for `path`
#' and "PMout.Rdata" is used for `file`.
#'
#' @param run The numerical value of the folder in `path` containing run results
#' @param path include `r template("path")`.
#' @param file Default is "PMout.Rdata", which is created after a Pmetrics run,
#' but it could also be the name of an .Rdata file created by running the
#' `$save` method for a [PM_result] object.
#' @return An R6 [PM_result].
#' @examples
#' \dontrun{
#' run1 <- PM_load(1) 
#' # loads from ./1/outputs/PMout.Rdata, where "." is the current working directory
#' 
#' run2 <- PM_load(2, path = "Pmetrics/MyRuns") 
#' # loads from Pmetrics/MyRuns/2/outputs/PMout.Rdata
#' 
#' run3 <- PM_load(path = "Pmetrics/MyRuns/3", file = "MyResults.Rdata") 
#' # loads from Pmetrics/MyRuns/3/MyResults.Rdata
#' 
#' run4 <- PM_load(file = "Pmetrics/MyRuns/4/outputs/PMout.Rdata") 
#' # loads from Pmetrics/MyRuns/4/outputs/PMout.Rdata
#' 
#' run5 <- PM_load() 
#' # loads from ./PMout.Rdata
#' }
#' 
#' @author Michael Neely and Julian Otalvaro
#' @seealso [PM_final],
#' [PM_cycle], [PM_op], [PM_cov],
#' [PM_pop], [PM_post]
#' @export


PM_load <- function(run, path = ".", file = "PMout.Rdata") {
  # internal function
  output2List <- function(Out) {
    result <- list()
    for (i in 1:length(Out)) {
      aux_list <- list(Out[[i]])
      names(aux_list) <- names(Out)[i]
      result <- append(result, aux_list)
    }
    
    return(result)
  }
  
  found <- "" # initialize
  
  if (!missing(run)) {
    filepath <- file.path(path, run, "outputs", file)
  } else {
    filepath <- file.path(path, file)
  } 
  
  if (file.exists(filepath)) { found <- filepath }
  
  if (found != "") {
    result <- output2List(Out = get(load(found)))
    rebuild <- PM_result$new(result, path = dirname(found), quiet = TRUE)
    return(rebuild)
  } else {
    cli::cli_abort(c("x" = "No Pmetrics output file found in {.path {path}}."))
  }
}


# internal update function
update <- function(res, found) {
  return(invisible(NULL)) # remove when cycle.csv updated
  msg <- NULL
  # CYCLE
  if (!is.null(res$cycle)) {
    dat <- res$cycle
    if (
      !tibble::is_tibble(dat$gamlam) # version prior to 2.2, add next update via or join
    ) {
      # start conversion
      n_cyc <- nrow(dat$mean)
      n_out <- max(res$op$outeq)
      dat$gamlam <- dat$gamlam %>% select(starts_with("add")|starts_with("prop"))
      if (ncol(gamlam) == 1 & n_out > 1) {
        gamlam <- cbind(gamlam, replicate((n_out - 1), gamlam[, 1]))
      }
      gamlam <- gamlam %>%
      pivot_longer(
        cols = everything(),
        values_to = "value", names_to = c("type", "outeq"), 
        names_sep = "\\."
      ) %>%
      mutate(cycle = rep(1:n_cyc, each = n_out)) %>%
      select(cycle, value, outeq, type) %>% arrange(cycle, outeq)
      if (is.matrix(dat$mean)) { # old fortran format, but not rust format
        dat$mean <- tibble::tibble(cycle = 1:n_cyc) %>%
        dplyr::bind_cols(tidyr::as_tibble(dat$mean))
        dat$median <- tibble::tibble(cycle = 1:n_cyc) %>%
        dplyr::bind_cols(tidyr::as_tibble(dat$median))
        dat$sd <- tibble::tibble(cycle = 1:n_cyc) %>%
        dplyr::bind_cols(tidyr::as_tibble(dat$sd))
      }
      msg <- c(msg, "cycle")
      res$cycle <- dat
    }
  }
  
  ####### DONE PROCESSING, INFORM #########
  if (!is.null(msg)) {
    cat(
      crayon::blue("NOTE: "),
      "The",
      crayon::green(dplyr::case_when(
        length(msg) == 1 ~ msg,
        length(msg) == 2 ~ paste(msg, collapse = " and "),
        length(msg) > 2 ~ paste(msg, collapse = ", ")
      )[1]),
      ifelse(length(msg) > 1, "fields", "field"),
      "in your PM_result object",
      ifelse(length(msg) > 1, "have", "has"),
      "been updated",
      "to the most current format.",
      "\n\n",
      crayon::blue("1"), "Save the updates\n",
      crayon::blue("2"), "Do not save updates\n "
    )
    flush.console()
    ans <- readline(" ")
    if (ans == 1) {
      temp <- PM_result$new(res)
      temp$save(file = found)
      cat("Results saved\n")
    }
  }
  
  return(res)
}
