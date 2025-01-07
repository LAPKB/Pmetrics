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
    #' @field backend Backend used for calculations; default is value in PMoptions.
    backend = NULL,

    #' @description
    #' Create a new object
    #' @param data Either the name of a  [PM_data]
    #' object in memory or the quoted name of a Pmetrics
    #' data file in the current working directory, which will crate a [PM_data]
    #' object on the fly. However, if created on the fly, this object
    #' will not be available to other
    #' methods or other instances of [PM_fit].
    #' @param model Similarly, this is either the name of a [PM_model]
    #' object in memory or the quoted name of a Pmetrics text model file
    #' in the current working directory. Again, if created on the fly,
    #' the object will not be available to other
    #' methods or other instances of [PM_fit].
    #' @param backend Backend used for calculations; default is value in PMoptions.
    #' @param ... Other parameters passed to [PM_data] or [PM_model] if created
    #' from a filename
    initialize = function(data = data, model = model, backend = getPMoptions()$backend, ...) {
      if (is.character(data)) {
        data <- PM_data$new(data, ...)
      }
      if (is.character(model)) {
        model <- PM_model$new(model, ...)
      }
      if (!inherits(data, "PM_data")) {
        cli::cli_abort(c("x" = "{.code data} must be a {.cls PM_data} object"))
      }
      if (!inherits(model, "PM_model")) {
        cli::cli_abort(c("x" = "{.code model} must be a {.cls PM_model} object"))
      }
      self$data <- data
      self$model <- model
      self$backend <- backend

      if (backend == "rust") {
        private$setup_rust_execution()
      }
    },
    #' @description Fit the model to the data
    #' @details
    #' This function will fit the model contained in the `model` field to the data
    #' in the `data` field, using the engine specified by the `engine` argument and 
    #' modified by other arguments. If all function arguments are default, 
    #' the simplest execution of this method is
    #' `$run()`.  
    #' @param run Specify the run number of the output folder.  Default if missing is the next available number.
    #' @param include Vector of subject id values in the data file to include in the analysis.  The default (missing) is all.
    #' @param exclude A vector of subject IDs to exclude in the analysis, e.g. c(4,6:14,16:20)
#    #' @param ode Ordinary Differential Equation solver log tolerance or stiffness.  Default is -4, i.e. 0.0001.  Higher values will result in faster
#    #' runs, but parameter estimates may not be as accurate.
#    #' @param tol Tolerance for convergence of NPAG.  Smaller numbers make it harder to converge.
#    #' Default value is 0.01.
#    #' @param salt Vector of salt fractions for each drug in the data file, default is 1 for each drug.  This is not the same as bioavailability.
    #' @param cycles Number of cycles to run. Default is 100.
    #' @param indpts Index of starting grid point number.  Default is missing, which allows NPAG to choose depending on the number of random parameters:
#    #' 1 or 2 = index of 1; 3 = 3; 4 = 4, 5 = 6,
#    #' 6 or more is 10+number of multiples for each parameter greater than 5, e.g. 6 = 101; 7 = 102, up to 108 for 13 or more parameters.
#    #' @param icen Summary of parameter distributions to be used to calculate predictions in HTML report.  Default is "median", but could be "mean".
#    #' Predictions based on both summaries will be available in objects loaded by [PM_load].
#    #' @param aucint Maintained for backwards compatibility and not used currently. Interval for AUC calculations.  Default is 24 hours if the number of intervals is not greater than 48; otherwise it defaults
#    #' to the interval which allows for <= 48 intervals.
#    #' @param idelta Interval in 1/60 time unit, typically minutes, for predictions at times other than observations.  Default is 12.
    #' @param prior Either "uniform" (default) or the name of a suitable [PM_result] object from a prior run loaded with [PM_load].
    #' A `prior` may be specified if the user wishes to
    #' start from a non-uniform prior distribution for the NPAG run. This is useful for continuing a previous
    #' run which did not converge, e.g., `fit1$run(prior = run1)`, assuming `run1` is a [PM_result] object
    #' that was loaded with [PM_load].  
    #' @param intern Run NPAG in the R console without a batch script.  Default is TRUE.
#    #' @param quiet Boolean operator controlling whether a model summary report is given.  Default is `TRUE`.
    #' @param overwrite Boolean operator to overwrite existing run result folders.  Default is `FALSE`.
#    #' @param nocheck Suppress the automatic checking of the data file with [PM_data].  Default is `FALSE`.
#    #' @param parallel Run NPAG in parallel.  Default is `NA`, which will be set to `TRUE` for models that use
#    #' differential equations, and `FALSE` for algebraic/explicit models.  The majority of the benefit for parallelization comes
#    #' in the first cycle, with a speed-up of approximately 80\% of the number of available cores on your machine, e.g. an 8-core machine
#    #' will speed up the first cycle by 0.8 * 8 = 6.4-fold.  Subsequent cycles approach about 50\%, e.g. 4-fold increase on an 8-core
#    #' machine.  Overall speed up for a run will therefore depend on the number of cycles run and the number of cores.
    #' @param engine The engine to use for the run.  Default is "NPAG". Alternatives: "NPOD".
    #' @param sampler The pseudo-random sampler to use for the initial distribution of grid points when 
    #' `prior = "uniform"`. 
    #' @param report If missing, the default Pmetrics report template as specified in [getPMoptions]
    #' is used. Otherwise can be "plotly", "ggplot", or "none".
    #' @param artifacts Default is `TRUE`.  Set to `FALSE` to suppress creating the `etc` folder. This folder
    #' will contain all the compilation artifacts created during the compilation and run steps.
    #' 
    #' @return A successful run will result in creation of a new folder in the working
    #' directory with the results inside the folder.  
    #'
    #' @author Michael Neely
    #' @export

    run = function(run = NULL,
                   include = NULL, exclude = NULL, 
                   #ode, tol, salt, 
                   cycles = 100,
                   indpts = NULL, 
                   #icen, aucint,
                   #idelta, 
                   prior = "uniform", 
                   #xdev, search,
                   #auto, 
                   intern = TRUE, 
                   #quiet, 
                   overwrite = FALSE, 
                   #nocheck, parallel, batch,
                   engine = "NPAG",
                   sampler = "sobol",
                   report = getPMoptions("report_template"),
                   artifacts = TRUE) {
      
      cwd <- getwd()

      
      # make new output directory
      if (is.null(run)) {
        olddir <- list.dirs(recursive = FALSE)
        olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
        olddir <- sub("^\\./", "", olddir)
        if (length(olddir) > 0) {
          newdir <- as.character(max(as.numeric(olddir)) + 1)
        } else {
          newdir <- "1"
        }
      } else {
        if (!is.numeric(run)) {
          cli::cli_abort(c("x" = " {.arg run} must be numeric."))
        } else {
          newdir <- as.character(run)
        }
      }
      
      if (file.exists(newdir)) {
        if (overwrite) {
          unlink(newdir, recursive = TRUE)
        } else {
          cli::cli_abort(c("x" = "A directory named '{newdir}' exists already.",
          "i" = "Set {.arg overwrite = TRUE} to overwrite it."))
        }
      }
      dir.create(newdir)
      setwd(newdir)
      
      engine <- tolower(engine)

      if  (getPMoptions()$backend != "rust") {
        setwd(cwd)
        cli::cli_abort(c(
          "x" = "Error: unsupported backend.",
          "i" = "See help for {.fn setPMoptions}"
        ))
      }
      
     

      ### Move temp folder to ect/PMcore ###
      # check if temp folder exist, create if not
      if (artifacts) {
        # if (!dir.exists("etc")) {
        #   dir.create("etc")
        # }
        # system(sprintf("cp -R %s etc/PMcore", getPMoptions()$rust_template))
        self$model$write_rust("model.txt")
      }
      
      #### Include or exclude subjects ####
      if (is.null(include)) include <- unique(self$data$standard_data$id)
      if (is.null(exclude)) exclude <- NA
      data_filtered <- self$data$standard_data %>% includeExclude(include, exclude)
      
      if (nrow(data_filtered) == 0) {
        cli::cli_abort("x" = "No subjects remain after filtering.")
        setwd(cwd)
        return(invisible(NULL))
      }
      
      
      # #### Determine indpts #####
      # num_ran_param <- purrr::map(self$model$model_list$pri, \(x)
      # is.null(x$fixed)) %>%
      #   unlist() %>%
      #   sum()
      
      # indpts <- ifelse(length(arglist$indpts) == 0, num_ran_param, arglist$indpts)
      # # convert index into number of grid points
      # arglist$num_gridpoints <- dplyr::case_when(
      #   indpts == 1 ~ 2129,
      #   indpts == 2 ~ 5003,
      #   indpts == 3 ~ 10007,
      #   indpts == 4 ~ 20011,
      #   indpts == 5 ~ 40009,
      #   indpts == 6 ~ 80021,
      #   indpts > 6 ~ 80021 + (min(indpts, 16) - 6) * 80021
      # ) %>% format(scientific = FALSE)
      
      
      # arglist$num_indpts <- (2**num_ran_param) * arglist$indpts
      # arglist$num_indpts <- format(arglist$num_indpts, scientific = FALSE)
      # arglist$num_indpts <- 2129 # TO-DO: Remove this line
      
      # #### Format cycles #####
      # arglist$cycles <- format(arglist$cycles, scientific = FALSE)
      
      # ### Prior ###
      # if (arglist$prior != "uniform") {
      #   prior <- arglist$prior
      #   if (inherits(prior, "PM_result")) { # PM_result
      #     write.csv(prior$final$popPoints, "theta.csv")
      #     prior <- "theta.csv"
      #   } else if (!is.na(suppressWarnings(as.numeric(prior)))) { # numeric
      #     prior <- paste(prior, "outputs/theta.csv", sep = "/")
      #     if (!file.exists(prior)) {
      #       cat(paste0(crayon::red("Error: "), crayon::blue(prior), " does not exist.\n"))
      #       setwd(cwd)
      #       return(invisible(nullfile()))
      #     }
      #   } else if (!file.exists(prior)) { # if file doesn't exist, error; otherwise prior will be an existing filename
      #     cat(paste0(crayon::red("Error: "), crayon::blue(prior), " is not in the current working directory:\n", crayon::blue(getwd(), "\n")))
      #     setwd(cwd)
      #     return(invisible(NULL))
      #   }
      # }
      
      # ### Sampler ###
      # arglist$sampler <- tolower(arglist$sampler)
      # if (!arglist$sampler %in% c("sobol", "osat")) {
      #   cat(paste0(
      #     crayon::red("Error: "), " Only \"sobol\" or \"osat\" are valid for the ",
      #     crayon::blue("sampler"), " argument.\n"
      #   ))
      #   setwd(cwd)
      #   return(invisible(NULL))
      # }
      
      # #### Verify engine ###
      # arglist$engine <- toupper(arglist$engine)
      # if (!arglist$engine %in% c("NPAG", "NPOD")) {
      #   cat(paste0(
      #     crayon::red("Error: "), " Only \"NPAG\" or \"NPOD\" are valid for the ",
      #     crayon::blue("engine"), " argument.\n"
      #   ))
      #   setwd(cwd)
      #   return(invisible(NULL))
      # }
      
      
      # #### Other arguments ####
      # arglist$use_tui <- "false" # TO-DO: Convert TRUE -> "true", vice versa.
      # arglist$cache <- "true"
      # arglist$seed <- 347
      
      
      #### Save objects ####
      self$data <- PM_data$new(data_filtered, quiet = TRUE)
      self$data$write("gendata.csv", header = FALSE)
      save(self, file = "fit.Rdata")
      
      # #### Parameter info ####
      # pars <- list(
      #   random = "[random]",
      #   fixed = "[fixed]",
      #   constant = "[constant]"
      # )
      
      # temp <- lapply(seq_along(names(self$model$model_list$pri)), function(i) {
      #   pri <- self$model$model_list$pri[i]
      #   name <- names(pri)
      #   pri <- self$model$model_list$pri[[i]]
      
      #   # Constant parameter
      #   if (pri$constant) {
      #     value <- format(pri$fixed, scientific = FALSE, nsmall = 1)
      #     str <- paste0(name, " = ", value)
      #     pars$constant <<- paste(pars$constant, str, sep = "\n")
      #   }
      
      #   # Fixed parameter
      #   if (!pri$constant & !is.null(pri$fixed)) {
      #     value <- format(pri$fixed, scientific = FALSE, nsmall = 1)
      #     str <- paste0(name, " = ", value)
      #     pars$fixed <<- paste(pars$fixed, str, sep = "\n")
      #   }
      
      #   # Random parameter
      #   if (!pri$constant & is.null(pri$fixed)) {
      #     min <- format(pri$min, scientific = FALSE, nsmall = 1)
      #     max <- format(pri$max, scientific = FALSE, nsmall = 1)
      #     str <- paste0(name, " = [", min, ",", max, "]")
      #     pars$random <<- paste(pars$random, str, sep = "\n")
      #   }
      
      #   return()
      # })
      
      # arglist$parameter_block <- paste0(unlist(pars), collapse = "\n")
      # arglist$poly_coeff <-
      #   self$model$model_list$out$Y1$err$assay$coefficients %>%
      #   paste0(collapse = ",")
      
      # if (!is.null(self$model$model_list$out$Y1$err$model$proportional)) {
      #   arglist$error_class <- "proportional"
      #   arglist$lamgam <- self$model$model_list$out$Y1$err$model$proportional
      # } else if (!is.null(self$model$model_list$out$Y1$err$model$additive)) {
      #   arglist$error_class <- "additive"
      #   arglist$lamgam <- self$model$model_list$out$Y1$err$model$additive
      # } else {
      #   cli::cli_abort(c(
      #     "x" = "Error model is misspecified.",
      #     "i" = "Choose {.code proportional} or {.code additive}."
      #   ))
      # }
      
      # #### Generate config.toml #####
      # toml_template <- stringr::str_glue(
      #   # "[paths]",
      #   # "data = \"gendata.csv\"",
      #   # "log = \"run.log\"",
      #   # "prior = \"{prior}\"",
      #   "[config]",
      #   "cycles = {cycles}",
      #   "algorithm = \"{engine}\"",
      #   # "engine = \"{engine}\"",
      #   # "init_points = {num_gridpoints}",
      #   # "seed = {seed}",
      #   # "sampler = \"{sampler}\"",
      #   # "tui = {use_tui}",
      #   # "output = true",
      #   # "cache = {cache}",
      #   "{parameter_block}",
      #   "[error]",
      #   "value = {lamgam}",
      #   "class = \"{error_class}\"",
      #   "poly = [{poly_coeff}]",
      #   .envir = arglist,
      #   .sep = "\n"
      # )
      
      # writeLines(text = toml_template, con = "config.toml")
      
      # # check if the file exists
      # file.copy(private$binary_path, "NPcore")
      ranges <- lapply(self$model$model_list$pri, function(x) {
        c(x$min,x$max)
      })
      names(ranges) <- tolower(names(ranges))
      if (intern) {
        
        ### CALL RUST
        out_path <- file.path(getwd(), "outputs")
        
        rlang::try_fetch(fit(self$model$binary_path, "gendata.csv", ranges, out_path),
                         error = function(e) {
                           cli::cli_warn("Unable to create {.cls PM_result} object", parent = e)
                           return(NULL)
                         }
        )
        
        PM_parse("outputs")
        res <- PM_load(file = "PMout.Rdata")
        PM_report(res, outfile = "report.html", template = report)
      } else {
        cli::cli_abort(c(
          "x" = "Error: Currently, the rust engine only supports internal runs.",
          "i" = "This is a temporary limitation."
        ))

      }
      setwd(cwd)
    },
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
      
      # if (is.null(self$model$binary_path)) {
      #   self$model$compile()
      #   if (!file.exists(self$model$binary_path)) {
      #     cli::cli_abort(c(
      #       "x" = "Rust has not been built.",
      #       "i" = "Make sure the model was built correcly when calling {.fn PM_model$new}"
      #     ))
      #   }
      # }
      # cwd <- getwd()
      # self$model$write_rust()

      # # # copy model and config to the template project
      # # if (length(getPMoptions()$rust_template) == 0) {
      # #   # PMbuild has not been executed
      # #   PMbuild()
      # # }
      # system(sprintf("mv main.rs %s/src/main.rs", getPMoptions()$rust_template))
      # # compile the template folder
      # setwd(getPMoptions()$rust_template)
      # system("cargo fmt")
      # system("cargo build --release")
      # if (!file.exists("target/release/template")) {
      #   setwd(cwd)
      #   cli::cli_abort(c("x" = "Error: Compilation failed"))
      # }
      # compiled_binary_path <- paste0(getwd(), "/target/release/template")
      # # Save the binary somewhere
      # setwd(tempdir())
      # olddir <- list.dirs(recursive = F)
      # olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
      # olddir <- sub("^\\./", "", olddir)
      # if (length(olddir) > 0) {
      #   newdir <- as.character(max(as.numeric(olddir)) + 1)
      # } else {
      #   newdir <- "1"
      # }
      # dir.create(newdir)
      # setwd(newdir)
      # cat("\nMoving compiled binary to a temp folder\n")
      # system(sprintf("cp %s NPcore", compiled_binary_path))
      # private$binary_path <- paste0(getwd(), "/NPcore")
      # setwd(cwd)
      # # TODO: I think it might be necessary to implement a re_build() method
      # # In case the binary the temp folder gets deleted
    }
  )
)



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
