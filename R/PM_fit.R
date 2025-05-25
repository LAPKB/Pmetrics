# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


# R6 ----------------------------------------------------------------------


#' @title
#' Object to define and run a model and data in Pmetrics
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `PM_fit` objects comprise a `PM_data` and `PM_model` object ready for analysis
#'
#' @details
#' Data and model objects can be previously created as [PM_data] or [PM_model] objects,
#' or created on the fly when making a new PM_fit object. PM_fit objects contain
#' methods to cross-check data and model objects for compatibility, as well as to
#' run the analysis.
#' @importFrom stringr str_glue
#' @export

PM_fit <- R6::R6Class(
  "PM_fit",
  public = list(
    #' @field data [PM_data] object
    data = NULL,
    #' @field model [PM_model] object
    model = NULL,
    #' @field result Hold the results of the run.
    result = NULL,
    
    
    #' @description Create or read a fit of a model to data
    #' @details
    #' This function will fit the model contained in the `model` field to the data
    #' in the `data` field, using the engine specified by the `engine` argument and
    #' modified by other arguments. If all function arguments are default,
    #' the simplest execution of this method is
    #' `$run()`.
    #' @param run Specify the run number of the output folder.  Default if missing is the next available number.
    #' @param data Either the name of a  [PM_data()]
    #' object in memory or the quoted name of a Pmetrics
    #' data file in the current working directory, which will crate a [PM_data()]
    #' object on the fly. However, if created on the fly, this object
    #' will not be available to other
    #' methods or other instances of [PM_fit].
    #' @param model Similarly, this is either the name of a [PM_model()]
    #' object in memory or the quoted name of a Pmetrics text model file
    #' in the current working directory. Again, if created on the fly,
    #' the object will not be available to other
    #' methods or other instances of [PM_fit].
    #' @param include Vector of subject id values in the data file to include in the analysis.
    #' The default (missing) is all.
    #' @param exclude A vector of subject IDs to exclude in the analysis, e.g. `c(4,6:14,16:20)`
    #    #' @param ode Ordinary Differential Equation solver log tolerance or stiffness.
    #    Default is -4, i.e. 0.0001.  Higher values will result in faster
    #    #' runs, but parameter estimates may not be as accurate.
    #    #' @param tol Tolerance for convergence of NPAG.  Smaller numbers make it harder to converge.
    #    #' Default value is 0.01.
    #    #' @param salt Vector of salt fractions for each drug in the data file, default is 1 for each drug.  This is not the same as bioavailability.
    #' @param cycles Number of cycles to run. Default is 100.
    #' @param prior The distribution for the initial support points, which can be
    #' one of several options.
    #' * The default is "sobol", which is a semi-random distribution. This is the distribution
    #' typically used when fitting a new model to the data. An example of this is
    #' on our [website](https://www.lapk.org/images/sobol_3d_plot.html).
    #'
    #' The following all specify non-random, informative prior distributions. They
    #' are useful for either continuing a previous
    #' run which did not converge or for fitting a model to new data, whether to simply
    #' calculate Bayesian posteriors with `cycles = 0` or to revise the model to a new
    #' covergence with the new data.
    #' * The name of a suitable [PM_result] object from a prior run loaded with [PM_load].
    #' This starts from the non-uniform, informative distribution obtained at the end of a prior NPAG run.
    #' Example: `run1 <- PM_load(1); fit1$run(prior = run1)`.
    #'
    #' * A character string with the filename of a csv file containing a prior distribution with
    #' format as for 'theta.csv' in the output folder of a prior run: column headers are parameter
    #' names, and rows are the support point values. A final column with probabilities
    #' for each support point is not necessary, but if present will be ignored, as these
    #' probabilities are calculated by the engine. Note that the parameter names must match the
    #' names of the primary variables in the model. Example: `fit1$run(prior = "mytheta.csv")`.
    #' * The number of a previous run with `theta.csv` in the output folder which will be read
    #' as for the filename option above. Example: `fit1$run(prior = 2)`.
    #' * A data frame obtained from reading an approriate file, such that the data frame
    #' is in the required format described in the filename option above. Example:
    #' `mytheta <- read_csv("mytheta.csv"); fit1$run(prior = mytheta)`.
    #'
    #' @param density0 The proportion of the volume of the model parameter
    #' hyperspace used to calculate the initial number of support points if one of
    #' the semi-random, uniform distributions are selected in the `prior` argument
    #' above. The initial points are
    #' spread through that hyperspace and begin the search for the optimal
    #' parameter value distribution (support points) in the population.
    #' The volume of the parameter space is the product of the ranges for all parameters.
    #' **The minimum number of points is 100, but the default is 0.01 x volume.**
    #' For example if using two parameters `Ke` and `V`, with ranges of \[0, 5\] and \[10, 100\],
    #' the volume is (5 - 0) x (100 - 10) = 450 The default value of `density0` is 0.01, so the initial
    #' number of support points will be 0.01 x 450 = 4.5, increased to the nearest integer,
    #' which is 5. However, this is < 100, so the initial number of support points will be 100.
    #' The greater the initial number of points, the less chance of
    #' missing the globally maximally likely parameter value distribution,
    #' but the slower the run.
    #' @param seed Seed used if `prior = "sobol"`. Ignored otherwise.
    #' @param intern Run NPAG in the R console without a batch script.  Default is TRUE.
    #    #' @param quiet Boolean operator controlling whether a model summary report is given.  Default is `TRUE`.
    #' @param overwrite Boolean operator to overwrite existing run result folders.  Default is `FALSE`.
    #    #' @param nocheck Suppress the automatic checking of the data file with [PM_data].  Default is `FALSE`.
    #    #' @param parallel Run NPAG in parallel.  Default is `NA`, which will be set to `TRUE` for models that use
    #    #' differential equations, and `FALSE` for algebraic/explicit models.  The majority of the benefit for parallelization comes
    #    #' in the first cycle, with a speed-up of approximately 80\% of the number of available cores on your machine, e.g. an 8-core machine
    #    #' will speed up the first cycle by 0.8 * 8 = 6.4-fold.  Subsequent cycles approach about 50\%, e.g. 4-fold increase on an 8-core
    #    #' machine.  Overall speed up for a run will therefore depend on the number of cycles run and the number of cores.
    #' @param algorithm The algorithm to use for the run.  Default is "NPAG". Alternatives: "NPOD".
    #' @param report If missing, the default Pmetrics report template as specified in [getPMoptions]
    #' is used. Otherwise can be "plotly", "ggplot", or "none".
    #'
    #' @return A successful run will result in creation of a new folder in the working
    #' directory with the results inside the folder.
    #'
    #' @author Michael Neely
    #' @export
    
    initialize = function(
    run = NULL,
    data = NULL,
    model = NULL,
    include = NULL, exclude = NULL,
    cycles = 100,
    prior = "sobol",
    density0 = 0.01,
    seed = 23,
    overwrite = FALSE,
    algorithm = "NPAG",
    report = getPMoptions("report_template")) {
      
      cwd <- getwd()
      intern <- TRUE # always true until (if) rust can run separately from R
      
      # Check the status of run
      if(is.null(run)){
        cli::cli_abort(c(
          "x" = "Please specify a {.arg run} number.",
          ">" = "If {.arg run} does not exist, it will be created to hold the results of the run.",
          ">" = "If {.arg run} exists, and {.arg overwrite} is {.val FALSE}, the prior run will be loaded.",
          "i" = "Please see {.fn Pmetrics::PM_fit} for details."
        ))
      }
      
      if (!is.numeric(run)) {
        cli::cli_abort(c("x" = " {.arg run} must be numeric."))
      } else {
        run_folder <- as.character(run)
      }
      
      if (file.exists(run_folder)){
        if (overwrite) {
          cli::cli_inform(c(
            "i" = "Overwriting prior run in {run_folder}."
          ))
          unlink(run_folder, recursive = TRUE)
          dir.create(run_folder)
          load_prior_run <- FALSE
        } else {
          cli::cli_inform(c(
            "i" = "Loading results from {run_folder}."
          ))
          load_prior_run <- TRUE
        }
      } else {
        cli::cli_inform(c(
          "i" = "Creating new run in {run_folder}."
        ))
        dir.create(run_folder)
        load_prior_run <- FALSE
      }
      
      # Load prior run because folder exists and overwrite is FALSE
      if(load_prior_run){
        self$result <- PM_load(run_folder)
        self$model <- self$result$model
        self$data <- self$result$data
        return(self)
      } else { # Create new run because folder doesn't exist or overwrite is TRUE
        
        if (is.null(data)) {
          cli::cli_abort(c(
            "x" = "The {.code data} argument is required.",
            ">" = "{.emph Preferred}: a {.cls PM_data} object will be used directly.",
            ">" = "Alternative: a character string specifying a data file name.",
            
            "i" = "Please see {.fn Pmetrics::PM_fit} for details."
          ))
        }
        if (is.null(model)) {
          cli::cli_abort(c(
            "x" = "The {.code model} argument is required.",
            ">" = "{.emph Preferred}: a {.cls PM_model} object will be used directly.",
            ">" = "Alternative: a character string specifying a model file name.",
            "i" = "Please see {.fn Pmetrics::PM_fit} for details."
          ))
        }
        
        if (is.character(data)) {
          data <- PM_data$new(data)
        }
        if (is.character(model)) {
          model <- PM_model$new(model)
        }
        if (!inherits(data, "PM_data")) {
          cli::cli_abort(c("x" = "{.code data} must be a {.cls PM_data} object"))
        }
        if (!inherits(model, "PM_model")) {
          cli::cli_abort(c("x" = "{.code model} must be a {.cls PM_model} object"))
        }
        
        self$data <- data
        self$model <- model
        # check if compiled and if not, do so
        self$model$compile()
        
        setwd(run_folder)
        algorithm <- tolower(algorithm)
        
        
        #### Include or exclude subjects ####
        if (is.null(include)) include <- unique(self$data$standard_data$id)
        if (is.null(exclude)) exclude <- NA
        data_filtered <- self$data$standard_data %>% includeExclude(include, exclude)
        
        if (nrow(data_filtered) == 0) {
          cli::cli_abort("x" = "No subjects remain after filtering.")
          setwd(cwd)
          return(invisible(NULL))
        }
        
        
        #### Save objects ####
        self$data <- PM_data$new(data_filtered, quiet = TRUE)
        self$data$write("gendata.csv", header = FALSE)
        
        save(self, file = "fit.Rdata")
        
        # Get ranges and calculate points
        
        ranges <- lapply(self$model$model_list$pri, function(x) {
          c(x$min, x$max)
        })
        names(ranges) <- tolower(names(ranges))
        
        # Set initial grid points (only applies for sobol)
        
        vol <- prod(sapply(ranges, function(x) x[2] - x[1]))
        points <- max(ceiling(density0 * vol), 100) # at least 100 points
        
        # set prior
        if (prior != "sobol") {
          if (is.numeric(prior)) { # prior specified as a run number
            if (!file.exists(glue::glue(prior,"/outputs/theta.csv"))) {
              cli::cli_abort(c(
                "x" = "Error: {.arg prior} file does not exist.",
                "i" = "Check the file path."
              ))
            }
            file.copy(glue::glue(prior,"/outputs/theta.csv"), "theta.csv")
            prior <- "theta.csv"
          } else if (is.character(prior)) { # prior specified as a filename
            if (!file.exists(prior)) {
              cli::cli_abort(c(
                "x" = "Error: {.arg prior} file does not exist.",
                "i" = "Check the file path."
              ))
            }
            file.copy(prior, overwrite = TRUE) # ensure in current working directory
          } else {
            cli::cli_abort(c(
              "x" = "Error: {.arg prior} must be a numeric run number or character filename.",
              "i" = "Check the value."
            ))
          }
        } else {
          prior <- "sobol"
        }
        
        if (intern) {
          ### CALL RUST
          out_path <- file.path(getwd(), "outputs")
          
          rlang::try_fetch(
            fit(
              self$model$binary_path,
              "gendata.csv",
              list(
                ranges = ranges,
                algorithm = algorithm,
                gamlam = c(self$model$model_list$out$Y1$err$model$additive, self$model$model_list$out$Y1$err$model$proportional),
                error_type = c("additive", "proportional")[1 + is.null(self$model$model_list$out$Y1$err$model$additive)],
                error_coefficients = t(sapply(self$model$model_list$out, function(x) {
                  y <- x$err$assay$coefficients
                  if (length(y) < 6) {
                    y <- c(y, 0, 0)
                  }
                  y
                })), # matrix numeqt x 6
                max_cycles = cycles,
                prior = prior,
                ind_points = points,
                seed = seed
              ), out_path
            ),
            error = function(e) {
              cli::cli_warn("Unable to create {.cls PM_result} object", parent = e)
              return(NULL)
            }
          )
          
          PM_parse("outputs")
          self$result <- PM_load(file = "PMout.Rdata")
          PM_report(self$result, outfile = "report.html", template = report)
        } else {
          cli::cli_abort(c(
            "x" = "Error: Currently, the rust engine only supports internal runs.",
            "i" = "This is a temporary limitation."
          ))
        }
        setwd(cwd)
        return(self)
        
        
      } # end new run else condition
      
      
      
      
    }, # end initialize
    
    #' @description
    #' Save the current PM_fit object to a .rds file.
    #' @param file_name Name of the file to be created. The
    #' default is "PMfit.rds".
    save = function(file_name = "PMfit.rds") {
      saveRDS(self, file_name)
    },
    
    #' @description
    #' `PM_fit` objects contain a `save` method which invokes [saveRDS] to write
    #' the object to the hard drive as an .rds file. This is the corresponding load
    #' function.
    #' @param file_name Name of the file to be read, the default is "PMfit.rds".
    #' @return A `PM_fit` object.
    load = function(file_name) {
      return(invisible())
    },
    #' @description
    #' Checks for errors in data and model objects and agreement between them.
    check = function() {
      if (inherits(self$model, "PM_model_list")) {
        cat(sprintf("Checking...\n"))
        file_name <- random_name()
        self$model$write(file_name)
        Pmetrics::PMcheck(self$data$standard_data, file_name)
        system(sprintf("rm %s", file_name))
      }
    }
  ),
  private = list(
    binary_path = NULL,
    setup_rust_execution = function() {
      # check if compiled and if not, do so
      self$model$compile()
    }
  ) # end private
) # end PM_fit



#' @export
PM_fit$load <- function(file_name = "PMfit.rds") {
  readRDS(file_name)
}

.logical_to_rust <- function(bool) {
  if (!is.logical(bool)) {
    stop("This functions expects a logical value")
  }
  
  rust_logical <- ifelse(bool, "true", "false")
}
