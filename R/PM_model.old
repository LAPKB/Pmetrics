# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


# R6 ----------------------------------------------------------------------


#' @title
#' Defines the PM_model class
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' PM_model objects contain the variables, covariates, equations and error models
#' necessary to run a population analysis.
#'
#' @details
#' PM_model objects are one of two fundamental objects in Pmetrics, along with
#' [PM_data()] objects. Defining a PM_model allows for fitting it to the data
#' via the `$fit()` method to conduct a
#' population analysis, i.e. estimating the probability distribution of model equation
#' paramter values in the population. The PM_model object is created using the
#' [build_model()] function, by defining a list of lists
#' directly in R, or by reading a model text file. See the vignette on models
#' for details.
#'
#' @export
PM_model <- R6::R6Class("PM_Vmodel",
  public = list(
    #' @description
    #' Build a new PM_model from a variety of inputs.
    #' @param model This can be a quoted name of a model text file in the
    #' working directory which will be read and passed to Fortran engines.
    #' It can be a list of lists that defines the model directly in R. It
    #' can also be a [PM_model] object, which will simply rebuild it. See the user
    #' manual for more help on directly defining models in R.
    #' @param ... Not currently used.

    # the following functions are dummy to permit documentation
    new = function(model, ...) {
      return(invisible())
    },
    #' @description
    #' This is the main method to run a population analysis.
    #' @details
    #' As of Pmetrics 3.0.0, models contain compiled code to fit
    #' the model equations to the data, optimizing the parameter
    #' value probability distributions in the population to
    #' maximize their likelihood, or more precisely, minimize
    #' the objective function, which is -2*log-likelihood.
    #'
    #' The `$fit` method is the means of running that compiled
    #' code to conduct to fitting procedure. At a minimum, it requires
    #' a [PM_data] object, which can be created with
    #' [PM_data$new()]. There are a number of additional arguments
    #' to control the fitting procedure, such as the number of cycles
    #' to run, the initial number of support points,
    #' and the algorithm to use, among others.
    #'
    #' The `$fit` method is the descendant of the legacy
    #' [NPrun()] function, which is maintained as a wrapper to `$fit`
    #' for backwards compatibility.
    #'
    #' @param data Either the name of a  [PM_data]
    #' object in memory or the quoted name of a Pmetrics
    #' data file in the current working directory, which will crate a [PM_data]
    #' object on the fly. However, if created on the fly, this object
    #' will not be available to other
    #' methods or other instances of [PM_fit].
    #' @param run Specify the run number of the output folder.  Default if missing is the next available number.
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
    #' For example if using two parameters `Ke` and `V`, with ranges of \[0, 5\] and \[10, 100\],
    #' the volume is (5 - 0) x (100 - 10) = 450 The default value of `density0` is 0.01, so the initial
    #' number of support points will be 0.01 x 450 = 4.5, increased to the nearest integer,
    #' which is 5. The greater the initial number of points, the less chance of
    #' missing the globally maximally likely parameter value distribution,
    #' but the slower the run.
    #'
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
    #' @param artifacts Default is `TRUE`.  Set to `FALSE` to suppress creating the `etc` folder. This folder
    #' will contain all the compilation artifacts created during the compilation and run steps.
    #'
    #' @return A successful run will result in creation of a new folder in the working
    #' directory with the results inside the folder.
    #'
    #' @author Michael Neely
    #' @export
    fit = function(data = NULL, run = NULL, include = NULL,
                   exclude = NULL, cycles = 100, prior = "sobol",
                   density0 = 0.01, seed = 23, overwrite = FALSE,
                   algorithm = "NPAG", report = getPMoptions("report_template")) {
      return(invisible())
    },
    #' @description
    #' Simulates multiple scenarios using the provided data and parameter values.
    #'
    #' @param data A `PM_data` object containing the data for the simulation.
    #' @param theta A matrix of numeric values representing the parameter values for the simulation.
    #'
    #' @details
    #' This function simulates multiple scenarios using the provided data and parameter values.
    #' It requires the data to be a `PM_data` object and the parameter values to be a numeric matrix.
    #' The number of columns in the parameter matrix must match the number of parameters in the model.
    #' The function writes the data to a temporary CSV file and uses the Rust backend to perform the simulation.
    #' If the model is not already compiled, it will be compiled before the simulation.
    #'
    #' @return A data frame with the following columns: id, time, out, outeq, state, state_index, spp_index.
    #'
    #' @examples
    #' \dontrun{
    #' data <- PM_data$new(...)
    #' theta <- matrix(c(1.0, 20.0, 2.0, 70.0), nrow = 2, byrow = TRUE)
    #' result <- model$simulate_all(data, theta)
    #' }
    #'
    #' @export
    simulate_all = function(data, theta) {
      return(invisible())
    },
    #' @description
    #' Retrieves the list of model parameters from the compiled version of the model.
    #'
    #' @details
    #' This function returns a list of the model parameters in the compiled version of the model.
    #' It only works with the Rust backend. If the backend is not set to "rust", an error will be thrown.
    #'
    #' @return A list of model parameters.
    #'
    #' @examples
    #' \dontrun{
    #' model$parameters()
    #' }
    #'
    #' @export
    parameters = function() {
      return(invisible())
    },
    #' @description
    #' Print a model object to the console in readable format
    #' @param ... Not used currently.
    print = function(...) {
      return(invisible())
    },
    #' @description
    #' Update selected elements of a model object
    #' @param changes_list The named list containing elements and values to update.
    #' Because R6 objects are mini-environments, using typical
    #' R notation to copy an object like mod2 <- mod1 can lead to unexpected
    #' results since this syntax simply creates a copied object in the same
    #' environment. Therefore updating any one object (e.g., mod1 or mod2)
    #' will update the other. To avoid this behavior, use the $clone() function
    #' first if you want to create a copied, yet independent new model.
    #' @examples
    #' \dontrun{
    #' mod2 <- modEx$clone() #create an independent copy of modEx called mod2
    #' mod2$update(list(
    #'   pri = list(
    #'    Ke = ab(0, 1), #change the range
    #'    V = NULL, #this deletes the variable
    #'    V0 = ab(10, 100) #add a new variable
    #'   ),
    #'   sec = "V = V0 * WT" #add a new secondary equation
    #' ))
    #' #note that they are different now
    #' mod2
    #' modEx
    #' }
    update = function(changes_list) {
      return(invisible())
    },
    #' @description Write a `PM_model` object to a text file
    #' @param model_path Full name of the file to be created, including the path
    #' relative to the current working directory
    #' @param engine Currently only "npag".
    #' @examples
    #' \dontrun{
    #' modEx$write("model.txt")
    #' },
    write = function(model_path = "genmodel.txt", engine = "npag") {
      return(invisible())
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_model].
    #' @param ... Arguments passed to [plot.PM_model]
    plot = function(...) {
      return(invisible())
    }
  )
)

#' @export
##### This function creates a new model depending on input given

PM_model$new <- function(model, ...) {
  # print(model)
  # Now we have multiple options for the model:
  # The model can be a...
  # String -> legacy run
  # List
  # PM_model object
  if (is.character(model) && length(model) == 1) {
    model <- PM_model_file$new(model)
  } else if (is.list(model)) {
    model <- PM_model_list$new(model)
  } else if (inherits(model, "PM_model")) {
    # if not compiled, do that; otherwise model is already a PM_model
    if (is.null(model$binary_path) || !file.exists(model$binary_path)) {
      model <- PM_model_list$new(model$model_list)
    }
  } else {
    cli::cli_abort(c("x" = "Non supported model type: {typeof(model)}"))
  }
  if (getPMoptions()$backend == "rust") {
    model$compile()
  }
  return(model)
}





# PM_model_list -----------------------------------------------------------
# This creates the model from a model_list object
#
PM_model_list <- R6::R6Class("PM_model_list",
  inherit = PM_Vmodel,
  public = list(
    model_list = NULL,
    binary_path = NULL,
    initialize = function(model_list) {
      # guarantees primary keys are lowercase and max first 3 characters
      orig_names <- names(model_list)
      names(model_list) <- private$lower3(names(model_list))
      model_blocks <- names(model_list)
      if (!identical(model_blocks, orig_names)) cli::cli_inform(c("i" = "Model block names standardized to 3 lowercase characters.\n"))

      # checks for minimal model requirements
      if (!"pri" %in% model_blocks) cli::cli_abort(c("x" = "Model must have a PRImary block."))
      if (!"out" %in% model_blocks) cli::cli_abort(c("x" = "Model must have an OUTput block."))
      n_out <- length(names(model_list$out))
      for (i in 1:n_out) {
        out_names <- private$lower3(names(model_list$out[[i]]))
        names(model_list$out[[i]]) <- out_names
        if (!"err" %in% out_names) {
          cli::cli_abort(c("x" = "Ensure all outputs have an ERRor block."))
        }
        if (!"model" %in% names(model_list$out[[i]]$err) ||
          !"assay" %in% names(model_list$out[[i]]$err)) {
          cli::cli_abort(c("x" = "ERRor blocks need {.code model} and {.code assay} components."))
        }
        if (!"proportional" %in% names(model_list$out[[i]]$err$model) ||
          !"additive" %in% names(model_list$out[[i]]$err$model)) {
          cli::cli_abort(c("x" = "ERRor model block must be either {.code proportional} or {.code additive}."))
        }
      }

      ### check template/equation blocks

      # TEMPLATE
      tem <- model_list$tem %>% tolower()

      # EQUATIONS
      eqs <- model_list$eqn %>% tolower()

      # no equations found
      if (length(eqs) == 0) {
        if (length(tem) > 0 && tem != "ode") { # found template, so get equations from model library
          model_list$eqn <- model_lib(name = tem, show = FALSE) # these are only for plotting purposes
        } else { # no equations or template, so try to parse like old Pmetrics and look for key variable names
          key_vars <- c("ka", "ke", "v", "kcp", "kpc", "cl", "vc", "q", "vp")
          pri <- names(model_list$pri)
          found_pri_keys <- key_vars %in% tolower(pri)

          if (!is.null(model_list$sec)) {
            found_sec_keys <- purrr::map_lgl(key_vars, \(x) {
              any(stringr::str_detect(
                model_list$sec,
                stringr::regex(x, ignore_case = TRUE)
              ))
            })
          } else {
            found_sec_keys <- rep(NA, 9)
          }

          found_keys <- key_vars[found_pri_keys | found_sec_keys] %>% na.exclude()

          if (length(found_keys) > 0) { # we found key variable names

            model_list$tem <- tem <- dplyr::case_when(
              all(found_keys %in% c("ke", "v")) ~ "one_comp_iv",
              all(found_keys %in% c("cl", "v")) ~ "one_comp_iv_cl",
              all(found_keys %in% c("ka", "ke", "v")) ~ "two_comp_bolus",
              all(found_keys %in% c("ka", "cl", "v")) ~ "two_comp_bolus_cl",
              all(found_keys %in% c("ke", "v", "kcp", "kpc")) ~ "two_comp_iv",
              all(found_keys %in% c("cl", "vc", "q", "vp")) ~ "two_comp_iv_cl",
              all(found_keys %in% c("ka", "ke", "v", "kcp", "kpc")) ~ "three_comp_bolus",
              all(found_keys %in% c("ka", "cl", "vc", "q", "vp")) ~ "three_comp_bolus_cl",
              .default = "error"
            )
          }

          # if we didn't find any keys or match a template, then we need to abort
          if (length(found_keys) == 0 || tem == "error") {
            cli::cli_abort(c(
              "x" = "Provide a valid {.code tem} or an {.code eqn} block to define the model equations.",
              "i" = "See help for {.fn PM_model}."
            ))
          }

          # we found a template, then we need to get the equations from the model library
          model_list$eqn <- model_lib(name = tem, show = FALSE)
        }
      } else { # length of equations > 0
        if (length(tem) == 0) { # equations present, but no template
          model_list$tem <- tem <- "ode"
        }
        # otherwise don't need to do anything since model_list$tem is already set
      }

      self$model_list <- private$order(model_list)
    },
    write = function(model_path = "genmodel.txt", engine = "npag") {
      engine <- tolower(engine)
      keys <- names(self$model_list)
      lines <- c()
      for (i in 1:length(keys)) {
        lines <- private$write_block(lines, keys[i], self$model_list[[i]], engine)
      }
      fileConn <- file(model_path)
      writeLines(lines, fileConn)
      close(fileConn)

      return(model_path)
    },
    write_rust = function(file_name = "parsed_model.txt") {
      model_file <- system.file("Rust/template.rs", package = "Pmetrics")
      content <- readr::read_file(model_file)

      # PRIMARY
      constant_parameter <- c()
      random_parameter <- c()
      for (i in 1:length(self$model_list$pri)) {
        if (self$model_list$pri[[i]]$constant) {
          constant_parameter <- c(constant_parameter, names(self$model_list$pri)[[i]])
        } else {
          random_parameter <- c(random_parameter, names(self$model_list$pri)[[i]])
        }
      }

      params <- c()
      for (key in random_parameter) {
        params <- append(params, sprintf("%s", tolower(key)))
      }
      content <- gsub("</params>", params %>% paste(collapse = ","), content)

      constant <- c()
      for (key in constant_parameter) {
        constant <- append(constant, sprintf("let %s = %s;", tolower(key), private$rust_up(self$model_list$pri[key][[1]]$fixed)))
      }
      content <- gsub("</constant>", constant %>% paste(collapse = "\n"), content)

      # COVARIATE
      covs <- c()
      for (key in tolower(purrr::map_chr(self$model_list$cov, \(x) x$covariate))) {
        covs <- append(covs, sprintf("%s", key))
      }
      content <- gsub("</covs>", covs %>% paste(collapse = ","), content)

      # SECONDARY
      sec <- self$model_list$sec %>% purrr::map(function(l) {
        l <- private$rust_up(l) # convert fortran/R to rust
        if (stringr::str_detect(l, regex("if|else|[{}]", ignore_case = TRUE))) {
          return(l) # return the corrected line
        } else {
          # contruct the variable declaration
          splitted <- stringr::str_split(l, "=")[[1]]
          lhs <- splitted[1] %>% tolower()
          rhs <- splitted[2] %>% tolower()
          return(paste0(lhs, " = ", rhs, ";\n"))
        }
      }) # end line by line mapping of sec
      content <- gsub("</sec>", sec %>% paste(collapse = ""), content)

      # TEMPLATE
      tem <- self$model_list$tem %>% tolower()
      # content <- gsub("</tem>", tem, content)

      # EQUATIONS
      eqs <- self$model_list$eqn %>% tolower()

      # count the number of equations by looking for xp() or dx[]
      eqs <- tolower(self$model_list$eqn)
      neqs <- sum(sapply(stringr::str_extract_all(eqs, "xp\\(\\d+\\)|dx\\[\\d+\\]"), function(x) length(x) > 0))
      content <- gsub("</neqs>", neqs, content)


      eqs <- eqs %>%
        stringr::str_replace_all("[\\(\\[](\\d+)[\\)\\]]", function(a) {
          paste0("[", as.integer(substring(a, 2, 2)) - 1, "]")
        }) %>%
        stringr::str_replace_all("xp", "dx") %>%
        purrr::map(\(l) private$rust_up(l)) %>%
        trimws() %>%
        paste(collapse = ";\n") %>%
        paste0(";")
      content <- gsub("</eqn>", eqs, content)

      # LAG
      lag <- ""
      for (line in self$model_list$lag %>% tolower()) {
        match <- stringr::str_match(line, "tlag[\\(\\[](\\d+)[\\)\\]]\\s*=\\s*(\\w+)")
        lag <- append(lag, sprintf("%i=>%s,", strtoi(match[2]) - 1, private$rust_up(match[3])))
      }
      content <- gsub("</lag>", lag %>% paste0(collapse = ""), content)

      # FA
      fa <- ""
      for (line in self$model_list$fa %>% tolower()) {
        match <- stringr::str_match(line, "fa[\\(\\[]\\d+)[\\)\\]]\\s*=\\s*(\\w+)")
        fa <- append(fa, sprintf("%i=>%s,", strtoi(match[2]), match[3]))
      }
      fa <- fa %>% purrr::map(\(l) private$rust_up(l))
      content <- gsub("</fa>", fa %>% paste0(collapse = ""), content)

      # INITIAL CONDITIONS
      init <- self$model_list$ini %>%
        stringr::str_split("\n") %>%
        unlist() %>%
        stringr::str_trim() %>%
        purrr::discard(~ .x == "") %>%
        purrr::map(function(x) {
          aux <- x %>%
            tolower() %>%
            stringr::str_replace_all("[\\(\\[](\\d+)[\\)\\]]", function(a) {
              paste0("[", as.integer(substring(a, 2, 2)) - 1, "]")
            }) %>%
            stringr::str_split("=")
          lhs <- aux[[1]][1]
          rhs <- aux[[1]][2]
          paste0(lhs, "=", private$rust_up(rhs), ";")
        }) %>%
        unlist() %>%
        paste0(collapse = "\n")
      content <- gsub("</init>", init, content)

      # OUTPUTS
      out_eqs <- ""
      for (key in names(self$model_list$out)) {
        rhs <- self$model_list$out[[key]]$val %>%
          tolower() %>%
          stringr::str_replace_all("[\\(\\[](\\d+)[\\)\\]]", function(a) {
            paste0("[", as.integer(substring(a, 2, 2)) - 1, "]")
          }) %>%
          purrr::map(\(l) private$rust_up(l))
        number <- as.numeric(stringr::str_extract(key, "\\d+"))
        key <- paste0(tolower(stringr::str_sub(key, 1, 1)), "[", number - 1, "]")
        out_eqs <- append(out_eqs, sprintf("%s = %s;\n", key, rhs))
      }
      content <- gsub("</out_eqs>", out_eqs %>% paste(collapse = ""), content)

      n_out <- length(self$model_list$out)
      content <- gsub("</nouteqs>", n_out, content)


      self$model_list <- private$order(self$model_list)
      # browser()
      readr::write_file(content, file_name)
    },
    update = function(changes_list) {
      keys <- names(changes_list)
      if (!private$lower3(keys) %in% c("pri", "sec", "tem", "dif", "eqn", "ini", "cov", "lag", "bol", "out", "err", "fa", "ext")) {
        cli::cli_abort(c(
          "x" = "Invalid block name: {keys}",
          "i" = "See help for {.fn PM_model}."
        ))
      }
      self$model_list <- modifyList(self$model_list, changes_list)
    },
    compile = function() {
      if (getPMoptions()$backend != "rust") {
        cli::cli_abort(c("x" = "This function can only be used with the rust backend."))
      }

      if (!is.null(self$binary_path)) {
        if (file.exists(self$binary_path)) {
          return()
        }
      }


      temp_model <- file.path(tempdir(), "temp_model.txt")
      self$write_rust(temp_model)
      model_path <- tempfile(pattern = "model_", fileext = ".pmx")
      tryCatch(
        {
          compile_model(
            temp_model,
            model_path, self$get_primary()
          )
          self$binary_path <- model_path
        },
        error = function(e) {
          cli::cli_abort(c("x" = "Model compilation failed: {e$message}"))
        }
      )
      file.remove(temp_model)
    },
    fit = function(data = NULL, run = NULL, include = NULL,
                   exclude = NULL, cycles = 100, prior = "sobol",
                   density0 = 0.01, seed = 23, overwrite = FALSE,
                   algorithm = "NPAG", report = getPMoptions("report_template")) {
      if (is.null(data)) {
        cli::cli_abort(c("x" = " {.arg data} must be specified."))
      }

      if (is.null(self$model_list)) {
        cli::cli_abort(c("x" = "Model is malformed."))
      }

      if (is.character(data)) {
        data <- PM_data$new(data)
      }

      if (!inherits(data, "PM_data")) {
        data <- tryCatch(
          {
            PM_data$new(data)
          },
          error = function(e) {
            cli::cli_abort(c(
              "x" = "{.code data} must be a {.cls PM_data} object or an appropriate data frame.",
              "i" = "See help for {.fn Pmetrics::PM_data}."
            ))
          }
        )
      }

      #### checks

      # covariates
      dataCov <- tolower(getCov(data)$covnames)
      modelCov <- tolower(sapply(self$model_list$cov, function(x) x$covariate))
      if (length(modelCov) == 0) {
        modelCov <- NA
      }
      if (!all(is.na(dataCov)) && !all(is.na(modelCov))) { # if there are covariates
        if (!identical(dataCov, modelCov)) { # if not identical, abort
          msg <- glue::glue("Model covariates: {paste(modelCov, collapse = ', ')}; Data covariates: {paste(dataCov, collapse = ', ')}")
          cli::cli_abort(c(
            "x" = "Error: Covariates in data and model do not match.",
            "i" = msg
          ))
        }
      }

      # output equations

      if (!is.null(data$standard_data$outeq)) {
        dataOut <- max(data$standard_data$outeq, na.rm = TRUE)
      } else {
        dataOut <- 1
      }

      modelOut <- length(self$model_list$out)
      if (dataOut != modelOut) {
        cli::cli_abort(c(
          "x" = "Error: Number of output equations in data and model do not match.",
          "i" = "Check the number of output equations in the data and model."
        ))
      }

      # check if model compiled and if not, do so
      self$compile()


      cwd <- getwd()
      intern <- TRUE # always true until (if) rust can run separately from R


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
          cli::cli_inform(c(
            "i" = "Overwriting the prior run in folder '{newdir}'."
          ))
        } else {
          cli::cli_inform(c(
            "x" = "The prior run from '{newdir}' was read.",
            " " = "Set {.arg overwrite} to {.val TRUE} to overwrite prior run in '{newdir}'."
          ))
          return(invisible(PM_load(newdir)))
        }
      }

      dir.create(newdir)
      setwd(newdir)

      algorithm <- tolower(algorithm)

      if (getPMoptions()$backend != "rust") {
        setwd(cwd)
        cli::cli_abort(c(
          "x" = "Error: unsupported backend.",
          "i" = "See help for {.fn setPMoptions}"
        ))
      }

      #### Include or exclude subjects ####
      if (is.null(include)) include <- unique(data$standard_data$id)
      if (is.null(exclude)) exclude <- NA
      data_filtered <- data$standard_data %>% includeExclude(include, exclude)

      if (nrow(data_filtered) == 0) {
        cli::cli_abort("x" = "No subjects remain after filtering.")
        setwd(cwd)
        return(invisible(NULL))
      }


      #### Save objects ####
      PM_data$new(data_filtered, quiet = TRUE)$write("gendata.csv", header = FALSE)
      save(self, file = "fit.Rdata")

      # Get ranges and calculate points

      ranges <- lapply(self$model_list$pri, function(x) {
        c(x$min, x$max)
      })
      names(ranges) <- tolower(names(ranges))

      # Set initial grid points (only applies for sobol)

      vol <- prod(sapply(ranges, function(x) x[2] - x[1]))
      points <- max(ceiling(density0 * vol), 100) # at least 100 points



      # set prior
      if (prior != "sobol") {
        if (is.numeric(prior)) { # prior specified as a run number
          if (!file.exists(glue::glue(prior, "/outputs/theta.csv"))) {
            cli::cli_abort(c(
              "x" = "Error: {.arg prior} file does not exist.",
              "i" = "Check the file path."
            ))
          }
          file.copy(glue::glue(prior, "/outputs/theta.csv"), "theta.csv")
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
          fit_model( # defined in extendr-wrappers.R
            self$binary_path,
            "gendata.csv",
            list(
              ranges = ranges,
              algorithm = algorithm,
              gamlam = c(self$model_list$out$Y1$err$model$additive, self$model_list$out$Y1$err$model$proportional),
              error_type = c("additive", "proportional")[1 + is.null(self$model_list$out$Y1$err$model$additive)],
              error_coefficients = t(sapply(self$model_list$out, function(x) {
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
            setwd(cwd)
            return(NULL)
          }
        )

        PM_parse("outputs")
        res <- PM_load(file = "PMout.Rdata")
        PM_report(res, outfile = "report.html", template = report)
        setwd(cwd)
        return(invisible(res))
      } else {
        cli::cli_abort(c(
          "x" = "Error: Currently, the rust engine only supports internal runs.",
          "i" = "This is a temporary limitation."
        ))
      }
    }, # end fit

    simulate_all = function(data, theta) {
      if (!inherits(data, "PM_data")) {
        cli::cli_abort(c("x" = "Data must be a PM_data object."))
      }
      if (!is.matrix(theta)) {
        cli::cli_abort(c("x" = "theta must be a matrix."))
      }
      if (!is.numeric(theta)) {
        cli::cli_abort(c("x" = "theta must be a matrix of numeric values."))
      }
      if (ncol(theta) != length(self$parameters())) {
        cli::cli_abort(c("x" = "theta must have the same number of columns as the number of parameters."))
      }

      temp_csv <- tempfile(fileext = ".csv")
      data$write(temp_csv, header = FALSE)
      if (getPMoptions()$backend == "rust") {
        if (is.null(self$binary_path)) {
          self$compile()
          if (is.null(self$binary_path)) {
            cli::cli_abort(c("x" = "Model must be compiled before simulating."))
          }
        }
        sim <- simulate_all(temp_csv, self$binary_path, theta)
      } else {
        cli::cli_abort(c("x" = "This function can only be used with the rust backend."))
      }
      return(sim)
    },
    parameters = function() {
      if (getPMoptions()$backend != "rust") {
        cli::cli_abort(c("x" = "This function can only be used with the rust backend."))
      }
      model_parameters(self$binary_path)
    } # end parameters method
  ), # end public list
  private = list(
    # converts R to rust
    rust_up = function(.l) {
      ### TEMPORARY TO REMOVE BOLUS[X] UNTIL RUST HANDLES THIS KEYWORD
      if (stringr::str_detect(.l, stringr::regex("bolus[\\[\\(]\\d+[\\]\\)]", ignore_case = TRUE))) {
        .l <- stringr::str_replace(.l, stringr::regex("bolus[\\[\\(]\\d+[\\]\\)]", ignore_case = TRUE), "")
      }



      ###
      # sequentially modify for operators
      pattern1 <- "(\\((?:[^)(]+|(?1))*+\\))"
      # this pattern recursively finds nested parentheses
      # and returns contents of outer
      for (x in c("abs", "exp", "ln", "log10", "log", "sqrt")) {
        .l <- gsub("dlog10", "log10", .l)
        .l <- gsub(
          pattern = paste0("(?<!\\.)", x, pattern1), # add negative look behind to exclude .fn()
          replacement = paste0("\\1\\.", x, "\\(\\)"),
          x = .l,
          perl = TRUE
        )
      }

      .l <- gsub("log\\(", "ln\\(", .l) # log in R and Fortran is ln in Rust

      # deal with exponents: **, ^, and exp(). Very tricky. This code was made by chatGPT after about 10 tries.
      exponents <- function(eq) {
        # Remove whitespace
        eq <- gsub("\\s+", "", eq)

        # Step 1: Replace exp(...) with placeholders (using a loop)
        exp_counter <- 0
        exp_map <- list()

        extract_exp <- function(text) {
          pattern <- "exp\\(([^()]+(?:\\([^()]*\\)[^()]*)*)\\)"
          m <- regexpr(pattern, text, perl = TRUE)
          while (m[1] != -1) {
            match_start <- m[1]
            match_len <- attr(m, "match.length")
            matched <- substr(text, match_start, match_start + match_len - 1)
            inner <- sub("exp\\((.*)\\)", "\\1", matched)
            exp_counter <<- exp_counter + 1
            placeholder <- paste0("__EXP", exp_counter, "__")
            exp_map[[placeholder]] <<- paste0("(", inner, ").exp()")
            text <- paste0(
              substr(text, 1, match_start - 1),
              placeholder,
              substr(text, match_start + match_len, nchar(text))
            )
            m <- regexpr(pattern, text, perl = TRUE)
          }
          return(text)
        }

        eq <- extract_exp(eq)

        # Step 2: Replace ** and ^ with .powf()
        eq <- gsub("(\\([^()]+\\)|[A-Za-z0-9_\\.]+)\\*\\*\\(?([^\\)\\+\\*/\\-]+)\\)?",
          "\\1.powf(\\2)", eq,
          perl = TRUE
        )
        eq <- gsub("(\\([^()]+\\)|[A-Za-z0-9_\\.]+)\\^(\\([^\\)]+\\)|[A-Za-z0-9_\\.]+)",
          "\\1.powf(\\2)", eq,
          perl = TRUE
        )

        # Step 3: Restore exp placeholders
        for (ph in names(exp_map)) {
          eq <- gsub_fixed(ph, exp_map[[ph]], eq)
        }

        return(eq)
      }

      # Helper functions for literal substitution
      sub_fixed <- function(pattern, replacement, x) {
        pos <- regexpr(pattern, x, fixed = TRUE)[1]
        if (pos == -1) {
          return(x)
        }
        paste0(substr(x, 1, pos - 1), replacement, substr(x, pos + nchar(pattern), nchar(x)))
      }

      gsub_fixed <- function(pattern, replacement, x) {
        while (grepl(pattern, x, fixed = TRUE)) {
          x <- sub_fixed(pattern, replacement, x)
        }
        x
      }

      # pattern2 <- "\\*{2}\\(([^)]+)\\)|\\*{2}([\\d.]+)|\\^\\(([^)]+)\\)|\\^([\\d.]+)"
      # replace2 <- "\\.powf\\(\\1\\2\\) " # will use first match if complex, second if simple
      # .l <- gsub(pattern2, replace2, .l, perl = TRUE)

      .l <- exponents(.l)

      # deal with integers, exclude [x] and alphax
      pattern3 <- "(?<![\\.\\w\\[])(\\d+)(?![\\.\\]\\d])"
      replace3 <- "\\1\\.0"
      .l <- gsub(pattern3, replace3, .l, perl = TRUE)

      # deal with if statements
      if_fix <- function(code, .l) {
        if (is.na(.l)) {
          return(.l)
        }
        if (code == "if" | code == "else if") {
          pattern <- paste0("^&*", code, "(\\((?:[^)(]+|(?1))*+\\))")
          n_found <- regexpr(pattern = pattern, text = .l, perl = TRUE)
          if (attr(n_found, "match.length") > -1) { # found something
            found <- regmatches(x = .l, m = n_found)
            repl <- paste(gsub("[()]", " ", regmatches(x = .l, m = n_found)), "{")
            .l <- gsub(pattern = found, replacement = repl, x = .l, fixed = TRUE)
            if (grepl("then", .l, ignore.case = TRUE)) { # remove 'then'
              .l <- paste(gsub(pattern = "then", replacement = "", x = .l, ignore.case = TRUE), "\n")
            } else { # single line if
              .l <- paste(.l, "}\n")
            }
          }
        }
        if (code == "else") {
          .l <- gsub(
            pattern = "^&*else",
            replacement = "\\} else \\{\n",
            x = .l, ignore.case = TRUE
          )
        }
        if (code == "end if") {
          .l <- gsub(
            pattern = "^&*end if",
            replacement = "}\n",
            x = .l, ignore.case = TRUE
          )
        }
        return(.l)
      } # end if_fix function

      # fix if and if-else blocks
      for (i in c("if", "else if", "else", "end if")) {
        .l <- if_fix(i, .l)
      }

      # deal with secondary equations, which don't have xp or dx
      .l2 <- stringr::str_replace_all(.l, "\\s*", "") # eliminate any spaces to make pattern matching easier
      pattern4 <- "^[^=]*(?<!(xp|dx)(\\(|\\[)\\d{1,2}(\\)|\\]))=.*" # match everything left of "=" that doesn't have xp or dx followed by () or [] with one or two digits
      if (!is.na(.l2) && stringr::str_detect(.l2, stringr::regex(pattern4, ignore_case = TRUE))) {
        .l <- paste0("let ", .l)
      }

      return(.l)
    }, # end rust_up function
    lower3 = function(chr) {
      purrr::map_chr(chr, function(x) {
        substr(tolower(x), 1, 3)
      })
    },

    # writes blocks in format for .txt file
    write_block = function(lines, key, block, engine) {
      if (private$lower3(key) == "fa") {
        key <- "f"
      }


      # print the block name
      lines <- append(lines, sprintf("#%s", key))

      # add content depending on the block
      if (private$lower3(key) == "pri") {
        i <- 1
        for (param in names(block)) {
          lines <- append(
            lines,
            if (is.numeric(block[[i]])) {
              sprintf("%s, %f", param, block[[i]])
            } else {
              sprintf("%s, %s", param, block[[i]]$print_to("ab", engine))
            }
          )
          i <- i + 1
        }
      } else if (private$lower3(key) == "cov") {
        for (i in 1:length(block)) {
          lines <- append(
            lines,
            if (block[[i]]$constant) {
              sprintf("%s!", block[[i]]$covariate)
            } else {
              sprintf("%s", block[[i]]$covariate)
            }
          )
        }
      } else if (private$lower3(key) %in% c("bol", "ext")) {
        if (!is.null(names(block))) {
          cli::cli_abort(c("x" = "The {key} block should be quoted equations"))
        }
        for (i in 1:length(block)) {
          lines <- append(lines, sprintf("%s", block[[i]]))
        }
      } else if (private$lower3(key) == "sec") {
        names <- names(block)
        for (i in 1:length(block)) {
          key <- toupper(names[i])
          lines <- append(
            lines,
            if (is.null(names[i]) || nchar(names[i]) == 0) {
              sprintf("%s", block[[i]])
            } else {
              sprintf("%s = %s", key, block[[i]][1])
            }
          )
        }
      } else if (private$lower3(key) == "lag") {
        names <- names(block)
        for (i in 1:length(block)) {
          key <- toupper(names[i])
          lines <- append(
            lines,
            if (is.null(names[i]) || nchar(names[i]) == 0) { # not named list
              if (stringr::str_starts(block[[i]], "&")) { # add on statement
                block[[i]]
              } else {
                # grab right side of equation if there
                rhs <- stringr::str_split(block[[i]][1], "=")[[1]][2]
                if (!is.na(rhs)) {
                  rhs <- stringr::str_replace_all(rhs, " ", "")
                } else { # no "=" detected
                  cli::cli_abort(c("x" = "No equation detected for lag expression: {block[[i]][1]}"))
                }
                lhs <- stringr::str_split(block[[i]][1], "=")[[1]][1]
                eqn <- stringr::str_extract(lhs, "\\d+")
                if (is.na(eqn)) { # no number in lhs
                  cli::cli_abort(c("x" = "No equation number detected for lag expression: {block[[i]][1]}"))
                }
                sprintf("TLAG[%s] = %s", eqn, rhs)
              }
            } else { # named list
              eqn <- stringr::str_extract(names[i], "\\d+") # standardize
              sprintf("TLAG[%s] = %s", eqn, block[[i]][1])
            }
          )
        }
      } else if (private$lower3(key) == "ini") {
        names <- names(block)
        for (i in 1:length(block)) {
          key <- toupper(names[i])
          lines <- append(
            lines,
            if (is.null(names[i]) || nchar(names[i]) == 0) { # not named list
              if (stringr::str_starts(block[[i]], "&")) { # add on statement
                block[[i]]
              } else { # grab right side of equation if there
                rhs <- stringr::str_split(block[[i]][1], "=")[[1]][2]
                if (!is.na(rhs)) {
                  rhs <- stringr::str_replace_all(rhs, " ", "")
                } else { # no "=" detected
                  cli::cli_abort(c("x" = "No equation detected for initial conditions: {block[[i]][1]}"))
                }
                lhs <- stringr::str_split(block[[i]][1], "=")[[1]][1]
                eqn <- stringr::str_extract(lhs, "\\d+")
                if (is.na(eqn)) { # no number in lhs
                  cli::cli_abort(c("x" = "No equation number detected for initial conditions: {block[[i]][1]}"))
                }
                sprintf("X[%s] = %s", eqn, rhs)
              }
            } else { # named list
              eqn <- stringr::str_extract(names[i], "\\d+") # standardize
              sprintf("X[%s] = %s", eqn, block[[i]][1])
            }
          )
        }
      } else if (private$lower3(key) == "f") {
        names <- names(block)
        for (i in 1:length(block)) {
          key <- toupper(names[i])
          lines <- append(
            lines,
            if (is.null(names[i]) || nchar(names[i]) == 0) { # not named list
              if (stringr::str_starts(block[[i]], "&")) { # add on statement
                block[[i]]
              } else { # grab right side of equation if there
                rhs <- stringr::str_split(block[[i]][1], "=")[[1]][2]
                if (!is.na(rhs)) {
                  rhs <- stringr::str_replace_all(rhs, " ", "")
                } else { # no "=" detected
                  cli::cli_abort(c("x" = "No equation detected for bioavailability: {block[[i]][1]}"))
                }
                lhs <- stringr::str_split(block[[i]][1], "=")[[1]][1]
                eqn <- stringr::str_extract(lhs, "\\d+")
                if (is.na(eqn)) { # no number in lhs
                  cli::cli_abort(c("x" = "No equation number detected for bioavailabilty: {block[[i]][1]}"))
                }
                sprintf("FA[%s] = %s", eqn, rhs)
              }
            } else { # named list
              eqn <- stringr::str_extract(names[i], "\\d+") # standardize
              sprintf("FA[%s] = %s", eqn, block[[i]][1])
            }
          )
        }
      } else if (private$lower3(key) == "tem") {
        lines <- append(lines, block)
      } else if (private$lower3(key) == "dif" | private$lower3(key) == "eqn") {
        # names <- names(block)
        for (i in 1:length(block)) {
          # key <- toupper(names[i])
          lines <- append(
            lines,
            block[[i]]
            # if (is.null(names[i]) || nchar(names[i]) == 0) { # not named list
            #   # grab right side of equation if there
            #   rhs <- stringr::str_split(block[[i]][1], "=")[[1]][2]
            #   if (!is.na(rhs)) {
            #     rhs <- stringr::str_replace_all(rhs, " ", "")
            #   } else { # no "=" detected
            #     cli::cli_abort(c("x" = sprintf("Error: No differential equation(s) detected for: %s", block[[i]][1])))
            #   }
            #   lhs <- stringr::str_split(block[[i]][1], "=")[[1]][1]
            #   eqn <- stringr::str_extract(lhs, "\\d+")
            #   if (is.na(eqn)) { # no number in lhs
            #     cli::cli_abort(c("x" = sprintf("Error: No differential equation number detected for: %s", block[[i]][1])))
            #   }
            #   sprintf("XP(%s) = %s", eqn, rhs)
            # } else { # named list
            #   eqn <- stringr::str_extract(names[i], "\\d+") # standardize
            #   sprintf("XP(%s) = %s", eqn, block[[i]][1])
            # }
          )
        }
      } else if (private$lower3(key) == "out") {
        i <- 1 # keep track of the first outeq
        err_lines <- "#err"
        for (param in names(block)) {
          if (nchar(param) != 2 && nchar(param) != 0) {
            cli::cli_abort(c("x" = "Name output lists as {.code Y1}, {.code Y2}, etc."))
          }
          key <- toupper(names(block)[i])
          lines <- append(
            lines,
            if (nchar(param) == 2) {
              sprintf("%s[%s]=%s", substr(key, 1, 1), substr(key, 2, 2), block[[i]][1])
            } else {
              sprintf("%s", block[[i]][1])
            }
          )
          err_block <- block[[i]]$err
          if (i == 1) {
            err_lines <- append(err_lines, err_block$model$print_to("ab", engine))
          }
          err_lines <- append(err_lines, err_block$assay$print_to("ab", engine))

          i <- i + 1
        }
        lines <- append(lines, "")
        lines <- append(lines, err_lines)
      } else {
        cli::cli_abort(c("x" = "Unsupported block named: {key}"))
      }
      lines <- append(lines, "")
      return(lines)
    },
    order = function(model_list) {
      model_list <- model_list[match(c("pri", "cov", "sec", "bol", "lag", "fa", "ini", "tem", "dif", "eqn", "out", "ext"), names(model_list))]
      model_list[which(is.na(names(model_list)))] <- NULL
      return(model_list)
    }
  ) # end private
)
