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
    #' @field arglist Arguments passed to rust engine
    arglist = NULL,
    #' @field backend Backend used for calculations; default is value in PMoptions.
    backend = NULL,
    
    #' @description
    #' Create a new object
    #' @param data Either the name of a  [PM_data]
    #' object in memory or the quoted name of a Pmetrics
    #' data file in the current working directory, which will crate a `PM_data`
    #' object on the fly. However, if created on the fly, this object
    #' will not be available to other
    #' methods or other instances of `PM_fit`.
    #' @param model Similarly, this is either the name of a [PM_model]
    #' object in memory or the quoted name of a Pmetrics text model file
    #' in the current working directory. Again, if created on the fly,
    #' the object will not be available to other
    #' methods or other instances of `PM_fit`.
    #' @param backend Backend used for calculations; default is value in PMoptions.
    #' @param ... Other parameters passed to `PM_data` or `PM_model` if created
    #' from a filename
    initialize = function(data = data, model = model, backend = getPMoptions()$backend, ...) {
      if (is.character(data)) {
        data <- PM_data$new(data, ...)
      }
      if (is.character(model)) {
        model <- PM_model$new(model, ...)
      }
      if(!inherits(data, "PM_data")){
        cli::cli_abort(c("x"="{.code data} must be a {.cls PM_data} object"))
      }
      if(!inherits(model, "PM_model")){
        cli::cli_abort(c("x"="{.code model} must be a {.cls PM_model} object"))
      }
      self$data <- data
      self$model <- model
      self$backend <- backend
      
      if (backend == "rust") {
        private$setup_rust_execution()
      }
    },
    #' @description Fit the model to the data
    #' @param ... Other arguments passed to [NPrun]
    #' @param engine "NPAG" (default) or "IT2B"
    #' @param rundir This argument specifies an *existing* folder that will store the run inside.
    #' @param backend Backend used for calculations; default is value in PMoptions.
    
    run = function(..., engine = "NPAG", rundir = getwd(), backend = getPMoptions()$backend) {
      wd <- getwd()
      if (!dir.exists(rundir)) {
        cli::cli_abort(c("x"="You have specified a directory that does not exist, please create it first."))
      }
      setwd(rundir)
      engine <- tolower(engine)
      
      if (self$backend == "fortran") {
        if (inherits(self$model, "PM_model_legacy")) {
          cat(sprintf("Runing Legacy"))
          if (engine == "npag") {
            NPrun(self$model$legacy_file_path, self$data$standard_data, ...)
          } else if (engine == "it2b") {
            ITrun(self$model$legacy_file_path, self$data$standard_data, ...)
          } else {
            endNicely(paste0("Unknown engine: ", engine, ". \n"))
          }
        } else if (inherits(self$model, "PM_model_list")) {
          engine <- tolower(engine)
          model_path <- self$model$write(engine = engine)
          cat(sprintf("Creating model file at: %s\n", model_path))
          if (engine == "npag") {
            NPrun(model_path, self$data$standard_data, ...)
          } else {
            ITrun(model_path, self$data$standard_data, ...)
          }
        }
      } else if (getPMoptions()$backend == "rust") {
        private$run_rust(..., engine = engine)
      } else {
        setwd(wd)
        cli::cli_abort(c("x"="Error: unsupported backend.",
                         "i"="See help for {.fn setPMoptions}"))
        
      }
      
      setwd(wd)
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
    run_rust = function(..., engine) {
      arglist <- list(...)
      cwd <- getwd()
      
      # set defaults then update with any supplied arguments
      arglist_default <- list(
        indpts = NULL,
        run = NULL,
        overwrite = FALSE,
        artifacts = TRUE,
        engine = "NPAG",
        include = NULL,
        exclude = NULL,
        cycles = 100,
        prior = "uniform", 
        sampler = "sobol",
        intern = TRUE
      )
      arglist <- modifyList(arglist_default, arglist)
      
      # Create a new directory for this run if needed
      run <- format(arglist$run, scientific = FALSE)
      if(run == "NULL"){ #run was not specified
        olddir <- list.dirs(recursive = F)
        olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
        olddir <- sub("^\\./", "", olddir)
        if (length(olddir) > 0) {
          newdir <- as.character(max(as.numeric(olddir)) + 1)
        } else {
          newdir <- "1"
        }
      } else { #run was specified
        if(dir.exists(run) & !arglist$overwrite){ #abort if the folder exists and overwrite is FALSE
          cat(paste0(crayon::red("Error: "), "The \"", run, "\" folder already exists. Use ", crayon::blue("overwrite = TRUE"), " to replace."))
          setwd(cwd)
          return(invisible(NULL))
        } else {
          unlink(run)
        }
        newdir <- run
      }
      
      dir.create(newdir, recursive = TRUE)
      setwd(newdir)
      
      ### Move temp folder to ect/PMcore ###
      # check if temp folder exist, create if not
      if (arglist$artifacts) {
        if (!dir.exists("etc")) {
          dir.create("etc")
        }
        system(sprintf("cp -R %s etc/PMcore", getPMoptions()$rust_template))
      }
      
      #### Include or exclude subjects ####
      
      data_filtered <- self$data$standard_data
      if (length(arglist$include)>0) {
        data_filtered <- data_filtered %>%
          filter(id %in% arglist$include)
      }
      
      if (length(arglist$exclude)>0) {
        data_filtered <- data_filtered %>%
          filter(!id %in% arglist$exclude)
      }
      
      if(nrow(data_filtered) == 0){
        cat(paste0(crayon::red("Error: "), "No subjects remain after filtering."))
        setwd(cwd)
        return(invisible(NULL))
      }
      
      data_new <- PM_data$new(data_filtered, quiet = TRUE)
      data_new$write("gendata.csv", header = FALSE)
      
      #### Determine indpts #####
      num_ran_param <- purrr::map(self$model$model_list$pri, \(x) 
                                  is.null(x$fixed)
      ) %>%
        unlist() %>%
        sum()
      
      indpts <- ifelse(length(arglist$indpts)==0, num_ran_param, arglist$indpts)
      #convert index into number of grid points
      arglist$num_gridpoints <- dplyr::case_when(
        indpts == 1 ~ 2129,
        indpts == 2 ~ 5003,
        indpts == 3 ~ 10007,
        indpts == 4 ~ 20011,
        indpts == 5 ~ 40009,
        indpts == 6 ~ 80021,
        indpts > 6 ~ 80021 + (min(indpts, 16) - 6) * 80021
      ) %>% format(scientific = FALSE)
      
      
      # arglist$num_indpts <- (2**num_ran_param) * arglist$indpts
      # arglist$num_indpts <- format(arglist$num_indpts, scientific = FALSE)
      # arglist$num_indpts <- 2129 # TO-DO: Remove this line
      
      #### Format cycles #####
      arglist$cycles <- format(arglist$cycles, scientific = FALSE)
      
      ### Prior ###
      if(arglist$prior != "uniform"){
        prior <- arglist$prior
        if (inherits(prior, "PM_result")){ #PM_result
          write.csv(prior$final$popPoints, "theta.csv")
          prior <- "theta.csv"
        } else if (!is.na(suppressWarnings(as.numeric(prior)))){ #numeric
          prior <- paste(prior, "outputs/theta.csv", sep = "/" )
          if (!file.exists(prior)){
            cat(paste0(crayon::red("Error: "), crayon::blue(prior), " does not exist.\n"))
            setwd(cwd)
            return(invisible(nullfile()))
          }
        } else if (!file.exists(prior)){ #if file doesn't exist, error; otherwise prior will be an existing filename
          cat(paste0(crayon::red("Error: "), crayon::blue(prior), " is not in the current working directory:\n", crayon::blue(getwd(),"\n")))
          setwd(cwd)
          return(invisible(NULL))
        }
      }
      
      ### Sampler ###
      arglist$sampler <- tolower(arglist$sampler)
      if (!arglist$sampler %in% c("sobol", "osat")){
        cat(paste0(crayon::red("Error: "), " Only \"sobol\" or \"osat\" are valid for the ",
                   crayon::blue("sampler")," argument.\n"))
        setwd(cwd)
        return(invisible(NULL))
      }
      
      #### Verify engine ###
      arglist$engine <- toupper(arglist$engine)
      if (!arglist$engine %in% c("NPAG", "NPOD")) {
        cat(paste0(crayon::red("Error: "), " Only \"NPAG\" or \"NPOD\" are valid for the ",
                   crayon::blue("engine")," argument.\n"))
        setwd(cwd)
        return(invisible(NULL))
      }
      
      
      #### Other arguments ####
      arglist$use_tui <- "false" # TO-DO: Convert TRUE -> "true", vice versa.
      arglist$cache <- "true"
      arglist$seed <- 347
      
      
      #### Save PM_fit ####
      self$data <- PM_data$new(data_filtered, quiet = TRUE)
      self$arglist <- arglist
      save(self, file = "fit.Rdata")
      
      #### Parameter info ####
      pars <- list(
        random = "[random]",
        fixed = "[fixed]",
        constant = "[constant]"
      )
      
      temp <- lapply(seq_along(names(self$model$model_list$pri)), function(i) {
        pri <- self$model$model_list$pri[i]
        name <- names(pri)
        pri <- self$model$model_list$pri[[i]]
        
        # Constant parameter
        if (pri$constant) {
          value <- format(pri$fixed, scientific = FALSE, nsmall = 1)
          str <- paste0(name, " = ", value)
          pars$constant <<- paste(pars$constant, str, sep = "\n")
        }
        
        # Fixed parameter
        if (!pri$constant & !is.null(pri$fixed)) {
          value <- format(pri$fixed, scientific = FALSE, nsmall = 1)
          str <- paste0(name, " = ", value)
          pars$fixed <<- paste(pars$fixed, str, sep = "\n")
        }
        
        # Random parameter
        if (!pri$constant & is.null(pri$fixed)) {
          min <- format(pri$min, scientific = FALSE, nsmall = 1)
          max <- format(pri$max, scientific = FALSE, nsmall = 1)
          str <- paste0(name, " = [", min, ",", max, "]")
          pars$random <<- paste(pars$random, str, sep = "\n")
        }
        
        return()
      })
      
      arglist$parameter_block <- paste0(unlist(pars), collapse = "\n")
      arglist$poly_coeff <-
        self$model$model_list$out$Y1$err$assay$coefficients %>%
        paste0(collapse = ",")
      
      if (!is.null(self$model$model_list$out$Y1$err$model$proportional)) {
        arglist$error_class <- "proportional"
        arglist$lamgam <- self$model$model_list$out$Y1$err$model$proportional
      } else if (!is.null(self$model$model_list$out$Y1$err$model$additive)) {
        arglist$error_class <- "additive"
        arglist$lamgam <- self$model$model_list$out$Y1$err$model$additive
      } else {
        cli::cli_abort(c("x"="Error model is misspecified.",
                         "i"="Choose {.code proportional} or {.code additive}."))
      }
      
      #### Generate config.toml #####
      toml_template <- stringr::str_glue(
        # "[paths]",
        # "data = \"gendata.csv\"",
        # "log = \"run.log\"",
        # "prior = \"{prior}\"",
        "[config]",
        "cycles = {cycles}",
        "algorithm = \"{engine}\"",
        # "engine = \"{engine}\"",
        # "init_points = {num_gridpoints}",
        # "seed = {seed}",
        # "sampler = \"{sampler}\"",
        # "tui = {use_tui}",
        # "output = true",
        # "cache = {cache}",
        "{parameter_block}",
        "[error]",
        "value = {lamgam}",
        "class = \"{error_class}\"",
        "poly = [{poly_coeff}]",
        .envir = arglist,
        .sep = "\n"
      )
      
      writeLines(text = toml_template, con = "config.toml")
      
      # check if the file exists
      file.copy(private$binary_path, "NPcore")
      if (arglist$intern) {
        system2("./NPcore", wait = TRUE)
        PM_parse("outputs")
        res <- PM_load(file = "outputs/PMout.Rdata")
        PM_report(res, outfile = "report.html", template = "plotly")
      } else {
        system2("./NPcore", args = "&")
        # TODO: The code to generate the report is missing here
      }
      setwd(cwd)
    },
    setup_rust_execution = function() {
      if (!file.exists(getPMoptions()$rust_template)) {
        cli::cli_abort(c("x"="Rust has not been built.", 
                         "i" = "Execute {.fn PMbuild} for this Pmetrics installation."))
      }
      cwd <- getwd()
      self$model$write_rust()
      
      # # copy model and config to the template project
      # if (length(getPMoptions()$rust_template) == 0) {
      #   # PMbuild has not been executed
      #   PMbuild()
      # }
      system(sprintf("mv main.rs %s/src/main.rs", getPMoptions()$rust_template))
      # compile the template folder
      setwd(getPMoptions()$rust_template)
      system("cargo fmt")
      system("cargo build --release")
      if (!file.exists("target/release/template")) {
        setwd(cwd)
        cli::cli_abort(c("x"="Error: Compilation failed"))
      }
      compiled_binary_path <- paste0(getwd(), "/target/release/template")
      # Save the binary somewhere
      setwd(tempdir())
      olddir <- list.dirs(recursive = F)
      olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
      olddir <- sub("^\\./", "", olddir)
      if (length(olddir) > 0) {
        newdir <- as.character(max(as.numeric(olddir)) + 1)
      } else {
        newdir <- "1"
      }
      dir.create(newdir)
      setwd(newdir)
      cat("\nMoving compiled binary to a temp folder\n")
      system(sprintf("cp %s NPcore", compiled_binary_path))
      private$binary_path <- paste0(getwd(), "/NPcore")
      setwd(cwd)
      # TODO: I think it might be necessary to implement a re_build() method
      # In case the binary the temp folder gets deleted
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
