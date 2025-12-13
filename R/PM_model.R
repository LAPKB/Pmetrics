# make new map() method that calls fit with algorithm = "POSTPROB" and other modifications as noted in fit()
# add map() to PM_result also, which call the method on the PM_model object

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
#' a model building app (coming soon), by defining a list
#' directly in R, or by reading a model text file. When reading a model text file, 
#' the list code is generated and copied to the clipboard for pasting in to scripts.
#' Model files will be deprecated in future versions of Pmetrics.  
#'
#' **Some notes on the example at the end of this help page:**
#'
#' * It's a complete example of a three compartment model with delayed absorption.
#' * We show the method of defining the model first and embedding the `PM_model$new()` within
#' a `dontrun` block to avoid automatic compilation.
#' * Since this model can also be solved analytically with algebra, we could have used
#'  `eqn = function(){three_comp_bolus}`.
#' @examples
#'
#' mod_list <- list(
#'   pri = list(
#'     CL = ab(10, 200),
#'     V0 = ab(0, 100),
#'     ka = ab(0, 3),
#'     k23 = ab(0, 5),
#'     k32 = ab(0, 5),
#'     lag1 = ab(0, 2)
#'   ),
#'   cov = list(
#'     wt = interp()
#'   ),
#'   sec = function() {
#'     V <- V0 * (wt / 70)
#'     ke <- CL / V # define here to make eqn simpler
#'   },
#'   eqn = function() {
#'     dx[1] <- -ka * x[1]
#'     dx[2] <- rateiv[1] + ka * x[1] - (ke + k23) * x[2] + k32 * x[3]
#'     dx[3] <- k23 * x[2] - k32 * x[3]
#'     dx[4] <- x[1] / V
#'   },
#'   lag = function() {
#'     tlag[1] <- lag1
#'   },
#'   out = function() {
#'     y[1] <- x[1] / V
#'     y[2] <- x[4] # AUC, not fitted to any data, not required
#'   },
#'   err = list(
#'     proportional(2, c(0.1, 0.15, 0, 0)) # only applies to y[1]
#'   )
#' )
#'
#' \dontrun{
#' mod <- PM_model$new(mod_list)
#' }
#'
#' @export
PM_model <- R6::R6Class(
  "PM_model",
  public = list(
    #' @field model_list A list containing the model components built by translating
    #' the original arguments into Rust
    model_list = NULL,
    #' @field arg_list A list containing the original arguments passed to the model
    arg_list = NULL,
    #' @field binary_path The full path and filename of the compiled model
    binary_path = NULL,
    #' @description
    #' This is the method to create a new `PM_model` object. 
    #'
    #' The first argument allows creation of a model from a variety of pre-existing
    #' sources, and if used, all the subsequent arguments will be ignored. If a model
    #' is defined on the fly, the arguments form the building blocks. Blocks are of two types:
    #'
    #' * **Lists** define *primary parameters*, *covariates*,
    #' and *error models*. These
    #' portions of the model have specific and defined creator functions and no additional
    #' R code is permissible. They take this form:
    #'     ```
    #'     block_name = list(
    #'       var1 = creator(),
    #'       var2 = creator()
    #'     )
    #'     ```
    #'     Note the comma separating the creator functions, "`c(`" to open the vector and "`)`" to close the vector.
    #'     Names are case-insensitive and are converted to lowercase for Rust.
    #' * **Functions** define the other parts of the model, including *secondary (global)
    #' equations*, *model equations* (e.g. ODEs), *lag time*, *bioavailability*, *initial conditions*,
    #' and *outputs*. These parts of the model are defined as R functions without arguments,
    #' but whose body contains any permissible R code.
    #'     ```
    #'     block_name = function() {
    #'
    #'      # any valid R code
    #'      # can use primary or secondary parameters and covariates
    #'      # lines are not separated by commas
    #'
    #'     }
    #'     ```
    #'     Note the absence of arguments between the "`()`", the opening curly brace "`{`" to start
    #'     the function body and the closing curly brace "`}`" to end the body.
    #'     Again, all R code will be converted to lowercase prior to translation into Rust.
    #'
    #' **Important:** All models must have `pri`, `eqn`, `out`, and `err` blocks.
    #'
    #' @param x An optional argument, but if specified, all the subsequent
    #' arguments will be ignored. `x` creates a `PM_model` from
    #' existing appropriate input, which can be one of the following:
    #'
    #' * Quoted name of a model text file in the
    #' working directory which will be read and passed to Rust engine. **Note:** Model
    #' text files are being deprecated in future versions of Pmetrics.
    #' 
    #' * List that defines the model directly in R. This will be in the same format as if
    #' all the subsequent arguments were used. For example:
    #'     ```
    #'     mod_list <- list(
    #'      pri = list(...),
    #'      eqn = function(){...},
    #'      out = function(){...},
    #'      err = c(...)
    #'     )
    #'     mod <- PM_model$new(mod_list)
    #'     ```
    #' * `PM_model` object, which will simply rebuild it, e.g. carrying on the
    #' prior example: `PM_model$new(mod)`
    #'
    #' See the user manual [PM_manual()] for more help on directly defining models in R.
    #' @param pri The first of the arguments used if `x` is not specified. This is
    #' a named list of primary parameters, which are the model parameters that
    #' are estimated in the population analysis. They are specified
    #' by one of two creator functions: [ab()] or [msd()]. For example,
    #' ```
    #' pri = list(
    #'   Ke = ab(0, 5),
    #'   V = msd(100, 10)
    #' )
    #' ```
    #' The [ab()] creator specifies the
    #' initial range `[a, b]` of the parameter, while the [msd()] creator specifies
    #' the initial mean and standard deviation of the parameter.
    #' @param cov A list whose names are some or all of the covariates in the data file.
    #' Unlike prior versions of Pmetrics, as of 3.0.0, they do not have to be listed in the same order
    #' as in the data file, and they do not need to be all present.
    #' **Only those covariates you wish to use in model equations or analyze for relationships to model parameters need to be declared here.**
    #' Values for each element in the covariate vector are the [interp()] creator function to declare
    #' how each covariate is interpolated between entries in the data. The default argument
    #' for [interp()] is "lm" which means that values will be linearly interpolated
    #' between entries, like the R linear model function [stats::lm()]. The alternative is "none",
    #' which holds the covariate value the same as the previous entry until it changes,
    #' i.e., a carry-forward strategy.
    #'
    #' For example:
    #' ```
    #' cov = list(
    #'   wt = interp(), # will be linear by default
    #'   visit = interp("none")
    #' )
    #' ```
    #' Note that `wt = interp()` is equivalent to `wt = interp("lm")`, since "lm" is the default.
    #' @param sec A function defining the secondary (global) equations in the model. Values
    #' are not estimated for these equations but they are available to every other block in the model.
    #' For example:
    #' ```
    #' sec = function() {
    #'   V = V0 * (wt/70)
    #' }
    #' ```
    #' Note that the function
    #' must be defined with no arguments between the parentheses,
    #' and the body **must be in R syntax**. Any number of lines and R code, e.g.
    #' `if` - `else` statements, etc. are permissible.
    #' @param eqn A function defining the model equations. The function must have no arguments.
    #' The body of the function may contain three kinds of equations, written in R syntax.
    #'
    #' * **Implicit equations** referenced by calling the name of a Pmetrics model library object
    #' detailed in [model_lib()]. The Pmetrics model library contains a number of template models
    #' solved analytically (algebraically) and may include user-defined models. For example, to use
    #' a two-compartment model with intavenous input:
    #'     ```
    #'     eqn = function(){
    #'      two_comp_iv
    #'     }
    #'     ```
    #'     Required parameters for library models are listed for each model and must be
    #'     included in model blocks exactly as named. For example, in a one-compartment model
    #'     `Ke` is a required parameter. Thus, if `Ke` is a function of a covariate called "crcl",
    #'     here is a code snippet illustrating the inclusion of the required parameter.
    #'     ```
    #'     mod <- PM_model$new(
    #'       pri = list(
    #'         ke0 = ab(0, 5),
    #'         v = ab(0, 100)
    #'       ),
    #'       cov = list(
    #'         crcl = interp()
    #'       ),
    #'       eqn = function(){
    #'         one_comp
    #'         ke = ke0 * crcl # the required parameter, ke, is defined
    #'       },
    #'       ... # more model blocks, including out, err
    #'     )
    #'     ```
    #' * **Explicit equations** are ordinary differential equations that directly define a model.
    #' Use the following notation in equations:
    #'   - `dx[i]` for the change in amount with respect to time (i.e., \eqn{dx/dt}),
    #' where `i` is the compartment number,
    #'   - `x[i]` for the compartment amount, where `i` is the compartment number.
    #'   - `rateiv[j]` for the infusion rate of input `j`, where `j` is the input number
    #' in the data corresponding to doses for that input.
    #'   - Bolus doses are indicated by `DUR = 0` for dose events in the
    #' data. Currently only one bolus input is allowed, which goes into compartment 1
    #' and is not modifiable. It does not appear in the differential equations.
    #'
    #'     For example,
    #'     ```
    #'     eqn = function() {
    #'      dx[1] = -ka * x[1]
    #'      dx[2] = rateiv[1] + ka * x[1] - ke * x[2]
    #'     }
    #'     ```
    #' * **Additional equations** in R code can be defined in this block, which are similar to
    #' the `sec` block, but will only be available within the `eqn` block as opposed
    #' to global availability when defined in `sec`. They can be added to either
    #' @param lag A function defining the lag time (delayed absorption) for the bolus input.
    #' The function must have no arguments,
    #' and the equations must be defined
    #' in R syntax The equations must be defined in the form of
    #' `lag[i] = par`, where `lag[i]` is the lag for drug (input) `i` and
    #' `par` is the lag parameter used in the `pri` block.
    #'
    #' For example, if
    #' `antacid` is a covariate in the data file, and `lag1` is a primary parameter,
    #' this code could be used to model delayed absorption if an antacid is present.
    #' ```
    #' lag = function() {
    #'   lag[1] = if(antacid == 1) lag1 else 0
    #' }
    #' ```
    #' As for `eqn`, additional equations in R code can be defined in this block,
    #' but will only be available within the `lag` block.
    #' @param fa A function defining the bioavailability (fraction absorbed) equations,
    #' similar to `lag`.
    #'
    #' Example:
    #' ```
    #' fa = function() {
    #'   fa[1] = if(antacid == 1) fa1 else 1
    #' }
    #' ```
    #' As for `eqn`, additional equations in R code can be defined in this block,
    #' but will only be available within the `fa` block.
    #' @param ini A function defining the initial conditions for a compartment
    #' in the model. Structure is similar to `lag` and `fa`.
    #'
    #' Example:
    #' ```
    #' ini = function() {
    #'   x[2] = init2 * V
    #' }
    #' ```
    #' This sets the initial amount of drug in compartment 2 to the value
    #' of a covariate `init2` multiplied by the volume of the compartment,
    #' `V`, assuming `V` is either a primary parameter or defined in the
    #' `sec` block.
    #'
    #' As for `eqn`, additional equations in R code can be defined in this block,
    #' but will only be available within the `ini` block.
    #' @param out A function defining the output equations, which are the predictions
    #' from the model. The function must have no arguments,
    #' and the equations for predictions must be defined
    #' in R syntax.
    #'
    #' Use the following notation in equations:
    #'
    #' * `y[i]` for the predicted value, where `i` is the output equation number,
    #' typically corresponding to an observation with `outeq = i` in the data, but not
    #' always (see **Note** below).
    #' * `x[j]` for the compartment amount, where `j` is the compartment number.
    #'
    #' As with all function blocks, secondary equations are permitted,
    #' but will be specific to the `out` block.
    #'
    #' For example,
    #' ```
    #' out = function() {
    #'   V = V0 * wt # only needed if not included in sec block
    #'   y[1] = x[1]/V
    #'   #Vp and Vm must be defined in pri or sec blocks
    #'   y[2] = x[2]/Vp
    #'   y[3] = x[3]/Vm
    #' }
    #' ```
    #' This assumes `V`, `Vp`, and `Vm` are either primary parameters or defined in the
    #' `sec` block.
    #'
    #' **Note** that as of Pmetrics 3.0.0, you can have more output equations
    #' than values for `outeq` in the data. This is not possible with prior
    #' versions of Pmetrics. Outputs without corresponding observations
    #' are not used in the fitting, but do generate predictions.
    #' For example, this snippet is part of a model that calculates AUC:
    #' ```
    #' eqn = function(){
    #'   dx[1] = -ka * x[1]
    #'   dx[2] = rateiv[1] + ka * x[1] - ke * x[2]
    #'   dx[3] = x[2] - x[3]
    #'   dx[4] = x[1] / v
    #' },
    #' out = function(){
    #'   y[1] = x[1]/v
    #'   y[2] = x[4]
    #' },
    #' err = list(
    #'  proportional(2, c(0.1, 0.15, 0, 0))
    #' )
    #' ```
    #' If the data only contain observations for `y[1]`, i.e. the concentration
    #' of drug in the plasma compartment with `outeq = 1`, the model will
    #' use that information to optimize the parameter values, but will also
    #' generate predictions for `y[2]`, which is the AUC of the drug in compartment 1,
    #' even though there is no `outeq = 2` in the data. There is only one `err`
    #' equation since there is only one source of observations: plasma concentration.
    #' AUC (`y[2]`) is not fitted to any observations; it is a calculation based on
    #' the model state, given the optimized parameter values. It's not required, but
    #' shown here for illustrative purposes.
    #'
    #' @param err An unammed vector of error models for each of the output equations
    #' with observations, i.e. those that have an `outeq` number associated with them in
    #' the data.
    #' Each error model is defined by the [proportional()] creator or
    #' the [additive()] creator, relative to the observation error.
    #' For example, if there are three output equations corresponding to three
    #' sources of observations in the data, the error models
    #' could be defined as:
    #' ```
    #' err = list(
    #'   proportional(2, c(0.1, 0.15, 0, 0)),
    #'   proportional(3, c(0.05, 0.1, 0, 0)),
    #'   additive(1, c(0.2, 0.25, 0, 0))
    #' )
    #' ```
    #' This defines the first two output equations to have proportional error
    #' with initial values of 2 and 3, respectively, and the third output equation
    #' to have additive error with initial value of 1. Each output is measured by
    #' a different assay with different error characteristics.
    #'
    #' If all the output equations have the same error model, you can
    #' simply use a single error model embedded in [replicate()] , e.g.,
    #' for 3 outputs with the same error model:
    #' ```
    #' err = list(
    #'   replicate(3, proportional(2, c(0.1, 0.15, 0, 0)))
    #' )
    #' ```
    #' @param ... Not currently used.
    initialize = function(x = NULL,
      pri = NULL,
      cov = NULL,
      sec = NULL,
      eqn = NULL,
      lag = NULL,
      fa = NULL,
      ini = NULL,
      out = NULL,
      err = NULL,
      ...) {
        # Store the original function arguments
        self$arg_list <- list(
          # x = x,
          pri = pri,
          cov = cov,
          sec = sec,
          eqn = eqn,
          lag = lag,
          fa = fa,
          ini = ini,
          out = out,
          err = err
        )
        
        if (!is.null(x)) {
          model_sections <- c("pri", "cov", "sec", "eqn", "lag", "fa", "ini", "out", "err")
          if (is.character(x) && length(x) == 1) { # x is a filename
            if (!file.exists(x)) {
              cli::cli_abort(c(
                "x" = "File {.file {x}} does not exist.",
                "i" = "Current directory: {getwd()}"
              ))
            }
            self$arg_list <- private$R6fromFile(x) # read file and populate fields
            cli::cli_inform(c("i" = "{.strong Note:} Model files will be deprecated in future versions of Pmetrics."))
            self$copy() # copy to clipboard
            
          } else if (is.list(x)) { # x is a list in R
            purrr::walk(model_sections, \(s) {
              if (s %in% names(x)) {
                self$arg_list[[s]] <- x[[s]]
              }
            })
          } else if (inherits(x, "PM_model")) { # x is a PM_model object
            if (!"arg_list" %in% names(x)) {
              cli::cli_abort(c(
                "x" = "You have supplied an older {.code PM_model} format.",
                "i" = "Please see for {.help Pmetrics::PM_model()} to remake it."
              ))
            }
            
            purrr::walk(model_sections, \(s) {
              if (s %in% names(x$arg_list)) {
                self$arg_list[[s]] <- x$arg_list[[s]]
              }
            })
            self$arg_list$x <- NULL
          } else {
            cli::cli_abort(c(
              "x" = "Non supported input for {.arg x}: {typeof(x)}",
              "i" = "It must be a filename, list, or current {.code PM_model} object."
            ))
          }
        } else { # x is NULL, check if other arguments are NULL
          named_args <- list(
            pri = pri,
            cov = cov,
            sec = sec,
            eqn = eqn,
            lag = lag,
            fa = fa,
            ini = ini,
            out = out,
            err = err
          )
          other_args <- list(...)
          all_args <- c(named_args, other_args)
          if (all(sapply(all_args, is.null))) { # everything is NULL
            self <- build_model() # launch the shiny app
            return(invisible(self))
          }
        } # no, some arguments were not NULL, so keep going
        
        msg <- NULL
        
        # check for reserved variable names
        conflict_vars <- reserved_name_conflicts(self$arg_list)
        if (length(conflict_vars) > 0) {
          msg <- "The following {?is a/are} reserved name{?s} and cannot be used as {?a variable or covariate/variables or covariates} in the model: {.var {conflict_vars}}."
        }
        
        # Primary parameters must be provided
        if (is.null(self$arg_list$pri)) {
          msg <- c(msg, "Primary parameters are missing.")
        }
        
        
        # Either an ODE-based model or an analytical model must be provided in eqn
        if (is.null(self$arg_list$eqn)) {
          msg <- c(msg, "No equations or template provided. Please provide either a template (see {.help model_lib}) or differential equations.")
        }
        
        
        
        # Get model template name if present (NA if absent) and set type
        model_template <- get_found_model(self$arg_list$eqn) # function defined below, returns 0 if not found, -1 if error
        
        # change logic; need to accomodate library models that are ODEs
        if (length(model_template) > 1 && model_template$analytical) {
          type <- "Analytical"
        } else {
          if (model_template == -1) {
            # length was 1, value 0
            msg <- c(msg, "A maximum of one model template can be included in a model.")
          }
          
          # length was 1, value 0
          type <- "ODE"
        }
        
        # Number of equations
        n_eqn <- if (type == "Analytical") {
          model_template$ncomp
        } else {
          get_assignments(self$arg_list$eqn, "dx")
        }
        n_out <- get_assignments(self$arg_list$out, "y")
        
        ## Get the names of the parameters
        parameters <- tolower(names(self$arg_list$pri))
        covariates <- tolower(names(self$arg_list$cov))
        ## check to make sure required parameters present if Analytical
        if (type == "Analytical") {
          # look in pri, sec, eqn, lag, fa, ini, out blocks for required parameters
          required_parameters <- tolower(model_template$parameters)
          pri_list <- map_lgl(required_parameters, \(x){
            if (x %in% parameters) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          })
          
          if (length(covariates) > 0) {
            cov_list <- map_lgl(required_parameters, \(x){
              if (x %in% covariates) {
                return(TRUE)
              } else {
                return(FALSE)
              }
            })
          } else {
            cov_list <- rep(FALSE, length(required_parameters))
          }
          
          if (!is.null(self$arg_list$sec)) {
            sec_list <- map_lgl(required_parameters, \(x){
              any(stringr::str_detect(tolower(func_to_char(self$arg_list$sec)), x))
            })
          } else {
            sec_list <- rep(FALSE, length(required_parameters))
          }
          
          eqn_list <- map_lgl(required_parameters, \(x){
            any(
              stringr::str_detect(
                stringr::str_remove_all(tolower(func_to_char(self$arg_list$eqn)), "\\s+"), # string
                paste0(x, "(?=(<-|=))")
              ) # pattern
            )
          })
          
          if (!is.null(self$arg_lag)) {
            lag_list <- map_lgl(required_parameters, \(x){
              any(
                stringr::str_detect(
                  stringr::str_remove_all(tolower(func_to_char(self$arg_list$lag)), "\\s+"), # string
                  paste0(x, "(?=(<-|=))")
                ) # pattern
              )
            })
          } else {
            lag_list <- rep(FALSE, length(required_parameters))
          }
          
          if (!is.null(self$arg_fa)) {
            lag_list <- map_lgl(required_parameters, \(x){
              any(
                stringr::str_detect(
                  stringr::str_remove_all(tolower(func_to_char(self$arg_list$fa)), "\\s+"), # string
                  paste0(x, "(?=(<-|=))")
                ) # pattern
              )
            })
          } else {
            fa_list <- rep(FALSE, length(required_parameters))
          }
          
          if (!is.null(self$arg_ini)) {
            ini_list <- map_lgl(required_parameters, \(x){
              any(
                stringr::str_detect(
                  stringr::str_remove_all(tolower(func_to_char(self$arg_list$ini)), "\\s+"), # string
                  paste0(x, "(?=(<-|=))")
                ) # pattern
              )
            })
          } else {
            ini_list <- rep(FALSE, length(required_parameters))
          }
          
          out_list <- map_lgl(required_parameters, \(x){
            any(
              stringr::str_detect(
                stringr::str_remove_all(tolower(func_to_char(self$arg_list$out)), "\\s+"), # string
                paste0(x, "(?=(<-|=))")
              ) # pattern
            )
          })
          
          all_lists <- bind_rows(
            tibble::tibble(
              parameter = required_parameters,
              pri = pri_list,
              cov = cov_list,
              sec = sec_list,
              eqn = eqn_list,
              lag = lag_list,
              fa = fa_list,
              ini = ini_list,
              out = out_list
            )
          ) %>% mutate(ok = purrr::pmap_lgl(across(pri:out), any))
          
          
          if (any(!all_lists$ok)) {
            missing <- all_lists$parameter[!all_lists$ok]
            msg <- c(
              msg,
              "The following parameters are required for the {.code {model_template$name}} model template but are missing: {missing}",
              "They should be defined in one of the model blocks, likely {.code pri}, {.code sec}, {.code eqn}, or {.code out}.",
              "Parameters defined in {.code pri} and {.code sec} are available to all blocks.",
              "Parameters defined in other blocks are only available to that block."
            )
          }
        } # end parameter checks for Analytical model
        
        
        # if Analytical, need to combine sec and eqn
        if (type == "Analytical") {
          # shell function
          sec_eqn <- function() {}
          # define the body of the shell function
          body(sec_eqn) <- suppressWarnings(as.call(c(
            quote(`{`),
            as.list(body(self$arg_list$eqn))[-1], # remove outer `{` of f1
            as.list(body(self$arg_list$sec))[-1] # remove outer `{` of f2
          )))
          
          # this will include template and equations in both sec and eqn
        }
        
        # sec
        # still needed for analytic, because these equations will be used
        # in other blocks
        
        if (!is.null(self$arg_list$sec)) {
          sec <- transpile_sec(self$arg_list$sec)
        } else {
          sec <- ""
        }
        
        # eqn
        if (type == "ODE") {
          eqn <- transpile_ode_eqn(self$arg_list$eqn, parameters, covariates, sec)
        } else if (type == "Analytical") {
          eqn <- transpile_analytic_eqn(sec_eqn, parameters, covariates)
        }
        
        # fa
        if (!is.null(self$arg_list$fa)) {
          fa <- transpile_fa(self$arg_list$fa, parameters, covariates, sec)
        } else {
          fa <- empty_fa()
        }
        
        # lag
        if (!is.null(self$arg_list$lag)) {
          lag <- transpile_lag(self$arg_list$lag, parameters, covariates, sec)
        } else {
          lag <- empty_lag()
        }
        
        # ini
        if (!is.null(self$arg_list$ini)) {
          ini <- transpile_ini(self$arg_list$ini, parameters, covariates, sec)
        } else {
          ini <- empty_ini()
        }
        
        # out
        if (!is.null(self$arg_list$out)) {
          out <- transpile_out(self$arg_list$out, parameters, covariates, sec)
        } else {
          out <- empty_out()
        }
        
        # err
        if (is.null(self$arg_list$err)) {
          msg <- c(msg, "Error model is missing and required.")
        }
        
        # ensure length err matches length outeqs
        if (length(self$arg_list$err) != n_out) {
          msg <- c(msg, "There must be one error model for each output equation.")
        }
        err <- self$arg_list$err
        
        # name
        name <- if (type == "Analytical") {
          model_template$name
        } else {
          "user"
        }
        
        # build the model list of rust components
        model_list <- list(
          pri = self$arg_list$pri,
          eqn = eqn,
          sec = sec,
          lag = lag,
          fa = fa,
          ini = ini,
          out = out,
          n_eqn = n_eqn,
          n_out = n_out,
          parameters = parameters,
          covariates = covariates,
          err = err,
          name = name
        )
        # make everything lower case if a character vector
        self$model_list <- purrr::map(model_list, \(x) {
          if (is.character(x)) {
            tolower(x)
          } else {
            x
          }
        })
        
        # this one needs to be capital
        self$model_list$type <- type
        
        
        # Abort if errors
        if (length(msg) > 0) {
          cli::cli_alert_danger("{.strong PM_model$new() aborted due to {length(msg)} error{?s}:}")
          purrr::walk(msg, \(m) cli::cli_bullets(c("*" = m)))
          return(invisible(NULL))
        }
        
        
        extra_args <- list(...)
        if (!is.null(purrr::pluck(extra_args, "compile"))) {
          if (extra_args$compile) {
            self$compile()
          }
        } else { # default is to compile
          self$compile()
        }
      },
      
      #' @description
      #' Print the model summary.
      #' @details
      #' This method prints a summary of the model.
      #' @param ... Not used.
      print = function(...) {
        cli::cli_div(theme = list(
          span.eqs = list(color = navy())
        ))
        
        cli::cli_h1("Model summary")
        
        cli::cli_h3(text = "Primary Parameters")
        # pars = self$model_list$parameters
        # cli::cli_text("{.eqs {pars}}")
        
        self$arg_list$pri %>%
        purrr::imap(\(x, y) cli::cli_text("{.strong {y}}: [{.strong {x$min}}, {.strong {x$max}}], {.emph ~N({round(x$mean,2)}}, {.emph {round(x$sd,2)})}")) %>%
        invisible() # to suppress NULL
        
        
        if (!is.null(self$model_list$covariates)) {
          cli::cli_h3(text = "Covariates")
          
          cov_list <- paste0(
            self$model_list$covariates,
            ifelse(self$arg_list$cov == 1, "", " (no interpolation)")
          )
          
          cli::cli_text("{.eqs {cov_list}}")
        }
        
        if (!is.null(self$arg_list$sec)) {
          cli::cli_h3(text = "Secondary (Global) Equations")
          eqs <- func_to_char(self$arg_list$sec) # function in PMutitlities
          for (i in eqs) {
            cli::cli_text("{.eqs {i}}")
          }
        }
        
        if (!is.null(self$arg_list$tem)) {
          cli::cli_h3(text = "Analytical Model")
          cli::cli_text("{.eqs {self$arg_list$tem$name}})")
        }
        
        if (!is.null(self$arg_list$eqn)) {
          cli::cli_h3(text = "Primary Equations")
          eqs <- func_to_char(self$arg_list$eqn) # function in PMutitlities
          for (i in eqs) {
            cli::cli_text("{.eqs {i}}")
          }
        }
        
        if (!is.null(self$arg_list$lag)) {
          cli::cli_h3(text = "Lag Time")
          eqs <- func_to_char(self$arg_list$lag) # function in PMutitlities
          for (i in eqs) {
            cli::cli_text("{.eqs {i}}")
          }
        }
        
        if (!is.null(self$arg_list$fa)) {
          cli::cli_h3(text = "Bioavailability (Fraction Absorbed)")
          eqs <- func_to_char(self$arg_list$fa) # function in PMutitlities
          for (i in eqs) {
            cli::cli_text("{.eqs {i}}")
          }
        }
        
        if (!is.null(self$arg_list$ini)) {
          cli::cli_h3(text = "Initial Conditions")
          eqs <- func_to_char(self$arg_list$ini) # function in PMutitlities
          for (i in eqs) {
            cli::cli_text("{.eqs {i}}")
          }
        }
        
        cli::cli_h3(text = "Outputs")
        outs <- func_to_char(self$arg_list$out)
        for (i in outs) {
          cli::cli_text("{.eqs {i}}")
        }
        
        cli::cli_h3(text = "Error Model")
        for (i in self$model_list$err) {
          if (i$fixed) {
            cli::cli_text("{.strong {tools::toTitleCase(i$type)}}, with fixed value of {.val {i$initial}} and coefficients {.val {i$coeff}}.")
          } else {
            cli::cli_text("{.strong {tools::toTitleCase(i$type)}}, with initial value of {.val {i$initial}} and coefficients {.val {i$coeff}}.")
          }
        }
        cli::cli_end()
        
        invisible(self)
      },
      #' @description
      #' Plot the model.
      #' @details
      #' This method plots the model using the
      #' [plot.PM_model()] function.
      #' @param ... Additional arguments passed to the plot function.
      plot = function(...) {
        tryCatch(
          plot.PM_model(self, ...),
          error = function(e) {
            cat(crayon::red("Error:"), e$message, "\n")
          }
        )
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
      #' The `$fit()` method is the means of running that compiled
      #' code to conduct to fitting procedure. At a minimum, it requires
      #' a [PM_data] object, which can be created with
      #' `PM_data$new()`. There are a number of additional arguments
      #' to control the fitting procedure, such as the number of cycles
      #' to run, the initial number of support points,
      #' and the algorithm to use, among others.
      #'
      #' The `$fit()` method is the descendant of the legacy
      #' [NPrun] function, which is maintained as a wrapper to `$fit()`
      #' for backwards compatibility.
      #'
      #' @param data Either the name of a  [PM_data]
      #' object in memory or the quoted filename (with or without a path) of a Pmetrics
      #' data file. If the path is not specified, the file is assumed to be in the current working directory,
      #' unless the `path` argument below is also specified as a global option for the fit.
      #' The file will be used to create a [PM_data]
      #' object on the fly. However, if created on the fly, this object
      #' will not be available to other
      #' methods or other instances of `$fit()`.
      #' @param path Optional full path or relative path from current working directory
      #' to the folder where `data` and `model` are located if specified as filenames without
      #' their own paths,
      #' and where the output will be saved. Default is the current working directory.
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
      #' @param points The number of initial support points if one of
      #' the semi-random, uniform distributions are selected in the `prior` argument
      #' above. Default is 100. The initial points are
      #' spread through the hyperspace defined by the random parameter ranges
      #' and begin the search for the optimal
      #' parameter value distribution (support points) in the population.
      #' If there are fewer than 2 points per unit range for any parameter,
      #' Pmetrics will suggest the minimum number of points that should be tried.
      #' The greater the initial number of points, the less chance of
      #' missing the globally maximally likely parameter value distribution,
      #' but the slower the run.
      #'
      #' @param idelta How often to generate posterior predictions in units of time.
      #' Default is 0.1, which means a prediction is generated every 0.1 hours (6 minutes)
      #' if the unit of time is hours. Predictions are made at this interval until the time
      #' of the last event (dose or observation) or until `tad` if that value is greater
      #' than the time of the last dose or observation in the data.
      #'
      #' @param tad Length of time after the last dose event to add additional predictions
      #' at frequency `idelta`.  Default is 0, which means no additional predictions
      #' beyond the last dose, assuming the dose is the last event. . If the
      #' last observation in the data is after `tad`, then a prediction will be generated at
      #' time = `tad` after the last dose
      #'
      #' @param seed Seed used if `prior = "sobol"`. Ignored otherwise.
      #' @param intern Run NPAG in the R console without a batch script.  Default is TRUE.
      #    #' @param quiet Boolean operator controlling whether a model summary report is given.  Default is `TRUE`.
      #' @param overwrite Boolean operator to overwrite existing run result folders.  Default is `FALSE`.
      #    #' @param nocheck Suppress the automatic checking of the data file with [PM_data].  Default is `FALSE`.
      #    #' @param parallel Run NPAG in parallel.  Default is `NA`, which will be set to `TRUE` for models that use
      #    #' differential equations, and `FALSE` for analytical/explicit models.  The majority of the benefit for parallelization comes
      #    #' in the first cycle, with a speed-up of approximately 80\% of the number of available cores on your machine, e.g. an 8-core machine
      #    #' will speed up the first cycle by 0.8 * 8 = 6.4-fold.  Subsequent cycles approach about 50\%, e.g. 4-fold increase on an 8-core
      #    #' machine.  Overall speed up for a run will therefore depend on the number of cycles run and the number of cores.
      #' @param algorithm The algorithm to use for the run.  Default is "NPAG" for the **N**on-**P**arametric **A**daptive **G**rid. Alternatives: "NPOD".
      #' @param report If missing, the default Pmetrics report template as specified in [getPMoptions]
      #' is used. Otherwise can be "plotly", "ggplot", or "none".
      #' @return A successful run will result in creation of a new folder in the working
      #' directory with the results inside the folder.
      #'
      #' @author Michael Neely
      #' @export
      fit = function(data = NULL,
        path = ".",
        run = NULL,
        include = NULL,
        exclude = NULL,
        cycles = 100,
        prior = "sobol",
        points = 100,
        idelta = 0.1,
        tad = 0,
        seed = 23,
        overwrite = FALSE,
        algorithm = "NPAG", # POSTPROB for posteriors, select when cycles = 0, allow for "NPOD"
        report = getPMoptions("report_template")) {
          msg <- NULL # status message at end of run
          run_error <- 0
          
          path <- stringr::str_replace(path, "/$", "") # remove trailing /
          
          if (is.null(data)) {
            msg <- c(msg, " {.arg data} must be specified.")
            run_error <- run_error + 1
          }
          
          if (is.null(self$model_list)) {
            msg <- c(msg, "Model is malformed.")
            run_error <- run_error + 1
          }
          
          if (is.character(data)) {
            # create PM_data object from file
            data <- PM_data$new(normalizePath(file.path(path, data), mustWork = FALSE))
          }
          
          if (!inherits(data, "PM_data")) {
            data <- tryCatch(
              {
                PM_data$new(data)
              },
              error = function(e) {
                -1
              }
            )
            
            if (!inherits(data, "PM_data")) {
              msg <- c(msg, "{.arg data} must be a {.cls PM_data} object or an appropriate data frame.")
              run_error <- run_error + 1
            }
          }
          
          #### checks
          
          # bolus and infusions
          if (self$model_list$type == "ODE") { # only need to check these for ODE models
            bolus <- unique(data$standard_data$input[data$standard_data$dur == 0]) %>% purrr::discard(~ is.na(.x))
            infusion <- unique(data$standard_data$input[data$standard_data$dur > 0]) %>% purrr::discard(~ is.na(.x))
            if (length(bolus) > 0) {
              missing_bolus <- bolus[!stringr::str_detect(self$model_list$eqn, paste0("b\\[", bolus - 1))]
              if (length(missing_bolus) > 0) {
                msg <- c(msg, "Bolus input(s) {paste(missing_bolus, collapse = ', ')} {?is/are} missing from the model equations. Use {.code b[{missing_bolus}]} or {.code bolus[{missing_bolus}]}, for example, to represent bolus inputs in the equations.")
                run_error <- run_error + 1
              }
            }
            if (length(infusion) > 0) {
              missing_infusion <- infusion[!stringr::str_detect(self$model_list$eqn, paste0("rateiv\\[", infusion - 1))]
              if (length(missing_infusion) > 0) {
                msg <- c(msg, "Infusion input(s) {paste(missing_infusion, collapse = ', ')} {?is/are} missing from the model equations. Use {.code r[{missing_infusion}]} or {.code rateiv[{missing_infusion}]} , for example, to represent infusion inputs in the equations.")
                run_error <- run_error + 1
              }
            }
          }
          
          # covariates
          modelCov <- self$model_list$cov
          if (length(modelCov) > 0) {
            dataCov <- tolower(getCov(data)$covnames)
            missingCov <- modelCov[!modelCov %in% dataCov]
            if (length(missingCov) > 0) { # if not identical, abort
              msg <- c(msg, "{.arg {modelCov}} {?is/are} missing from the data.")
              run_error <- run_error + 1
            }
          }
          
          # cycles
          # if programmer is a crazy Norwegian....
          if (cycles < 0) {
            msg <- c(msg, "Error: {.arg cycles} must be 0 or greater.")
            run_error <- run_error + 1
          }
          
          # output equations
          if (!is.null(data$standard_data$outeq)) {
            dataOut <- max(data$standard_data$outeq, na.rm = TRUE)
          } else {
            dataOut <- 1
          }
          modelOut <- self$model_list$n_out
          
          
          #### Algorithm ####
          algorithm <- toupper(algorithm)
          if (cycles == 0) {
            if (length(prior) ==1 && prior == "sobol") {
              msg <- c(msg, "Cannot use {.code prior = 'sobol'} with {.code cycles = 0}.")
              run_error <- run_error + 1
            }
            algorithm <- "POSTPROB"
          } else {
            if (!(algorithm %in% c("NPAG", "NPOD"))) {
              msg <- c(msg, "Unsupported algorithm. Supported algorithms are 'NPAG' and 'NPOD'.")
              run_error <- run_error + 1
            }
          }
          if (algorithm == "POSTPROB" && cycles > 0) {
            msg <- c(msg, "Warning: {.code algorithm = 'POSTPROB'} is used with {.code cycles = 0}. {.code cycles} set to 0.")
            cycles <- 0
          }
          
          
          
          # if (getPMoptions()$backend != "rust") {
          #   cli::cli_abort(c("x" = "Error: unsupported backend.", "i" = "See help for {.fn setPMoptions}"))
          # }
          
          #### Include or exclude subjects ####
          if (is.null(include)) {
            include <- unique(data$standard_data$id)
          }
          if (is.null(exclude)) {
            exclude <- NA
          }
          data_filtered <- data$standard_data %>% includeExclude(include, exclude)
          
          if (nrow(data_filtered) == 0) {
            msg <- c(msg, "No subjects remained after filtering.")
            run_error <- run_error + 1
          }
          
          # set prior
          if (length(prior)==1 && prior != "sobol") {
            if (is.numeric(prior)) {
              # prior specified as a run number
              if (!file.exists(glue::glue("{path}/{prior}/outputs/theta.csv"))) {
                msg <- c(msg, "{.arg prior} file does not exist.", "i" = "Check the file path.")
                run_error <- run_error + 1
              }
              file.copy(glue::glue("{path}/{prior}/outputs/theta.csv"), "prior.csv", overwrite = TRUE)
              prior <- "prior.csv"
            } else if (length(prior)==1 && is.character(prior)) {
              # prior specified as a filename
              if (!file.exists(prior)) {
                msg <- c(msg, "{.arg prior} file does not exist.")
                run_error <- run_error + 1
              }
              file.copy(prior, "prior.csv", overwrite = TRUE) # ensure in current working directory
            } else if (is.data.frame(prior)) {
              # prior specified as a data frame
              if (!all(c("prob", self$model_list$parameters) %in% names(prior))) {
                msg <- c(msg, "{.arg prior} data frame must contain columns for parameters and probabilities.")
                run_error <- run_error + 1
              }
              prior <- prior %>% dplyr::select(all_of(self$model_list$parameters), prob)
              write.csv(prior, "prior.csv", row.names = FALSE)
            } else {
              msg <- c(msg, "{.arg prior} must be a numeric run number or character filename.")
              run_error <- run_error + 1
            }
          } else {
            prior <- "sobol"
          }
          
          #### Abort if errors before creating new folder ####
          if (run_error > 0) {
            cli::cli_alert_danger("{.strong PM_model$fit() aborted due to {run_error} error{?s}:}")
            purrr::walk(msg, \(m) cli::cli_bullets(c("*" = m)))
            return(invisible(NULL))
          }
          #### Continue with fit ####
          
          # check if model compiled and if not, do so
          self$compile()
          
          intern <- TRUE # always true until (if) rust can run separately from R
          
          
          # make new output directory
          
          if (is.null(run)) {
            olddir <- list.dirs(path, recursive = FALSE)
            olddir <- olddir[grep("^\\./[[:digit:]]+", olddir)]
            olddir <- sub("^\\./", "", olddir)
            if (length(olddir) > 0) {
              run <- as.character(max(as.numeric(olddir)) + 1)
            } else {
              run <- "1"
            }
          } else {
            if (!is.numeric(run)) {
              msg <- c(msg, "{.arg run} must be numeric, so was ignored.")
            }
          }
          
          path_run <- normalizePath(file.path(path, run), mustWork = FALSE)
          
          if (file.exists(path_run)) {
            if (overwrite) {
              unlink(path_run, recursive = TRUE)
              msg <- c(msg, "The previous run in folder '{path_run}' was overwritten.")
            } else {
              cli::cli_inform(
                c("i" = "The previous run from '{path_run}' was read.", " " = "Set {.arg overwrite} to {.val TRUE} to overwrite prior run in '{path_run}'.")
              )
              return(invisible(PM_load(file = normalizePath(file.path(path_run, "PMout.Rdata"), mustWork = FALSE))))
            }
          }
          
          fs::dir_create(path_run)
          
          #### Save input objects ####
          fs::dir_create(normalizePath(file.path(path_run, "inputs"), mustWork = FALSE))
          PM_data$new(data_filtered, quiet = TRUE)$save(normalizePath(file.path(path_run, "inputs", "gendata.csv"), mustWork = FALSE), header = FALSE)
          saveRDS(list(data = data, model = self), file = normalizePath(file.path(path_run, "inputs", "fit.rds"), mustWork = FALSE))
          file.copy(self$binary_path, normalizePath(file.path(path_run, "inputs"), mustWork = FALSE))
          
          # Get ranges and calculate points
          ranges <- lapply(self$model_list$pri, function(x) {
            c(x$min, x$max)
          })
          
          names(ranges) <- tolower(names(ranges))
          # Set initial grid points (only applies for sobol)
          marginal_densities <- sapply(ranges, function(x) {
            points / (x[2] - x[1])
          })
          if (any(marginal_densities < 2)) {
            increase_to <- round(points * (max(2 / marginal_densities)), 0)
            msg <- c(msg, "Recommend increasing {.arg points} to at least {increase_to} to ensure adequate coverage of parameter space.")
          }
          
          
          if (intern) {
            ### CALL RUST
            out_path <- normalizePath(file.path(path_run, "outputs"), mustWork = FALSE)
            msg <- c(msg, "Run results were saved in folder '{.path {out_path}}'")
            rlang::try_fetch(
              fit(
                # defined in extendr-wrappers.R
                model_path = normalizePath(self$binary_path),
                data = normalizePath(file.path(path_run, "inputs", "gendata.csv")),
                params = list(
                  ranges = ranges, # not important but needed for POSTPROB
                  algorithm = algorithm,
                  error_models = lapply(self$model_list$err, function(x) x$flatten()),
                  idelta = idelta,
                  tad = tad,
                  max_cycles = cycles, # will be hardcoded in Rust to 0 for POSTPROB
                  prior = prior, # needs warning if missing and algorithm = POSTPROB
                  points = points, # only relevant for sobol prior
                  seed = seed
                ),
                output_path = out_path,
                kind = tolower(self$model_list$type)
              ),
              error = function(e) {
                cli::cli_warn("Unable to create {.cls PM_result} object", parent = e)
                return(NULL)
              }
            )
            
            PM_parse(path = out_path)
            res <- PM_load(path = normalizePath(out_path), file = "PMout.Rdata")
            if (report != "none") {
              valid_report <- tryCatch(
                PM_report(res, path = normalizePath(out_path), template = report, quiet = TRUE),
                error = function(e) {
                  -1
                }
              )
              if (valid_report == 1) {
                msg <- c(msg, "Report generated with {report} template.")
                # if(tolower(algorithm) == "postprob") {this_alg <- "map"} else {this_alg <- "fit"}
                msg <- c(msg, "If assigned to a variable, e.g. {.code run{run} <-}, results are available in {.code run{run}}.")
              } else {
                msg <- c(msg, "Report could not be generated.")
              }
            }
            
            
            if (length(msg) > 1) {
              cli::cli_h1("Notes:")
              cli::cli_ul()
              purrr::walk(msg[-1], ~ cli::cli_li(.x))
              cli::cli_end()
            }
            return(invisible(res))
          } else {
            cli::cli_abort(
              c("x" = "Error: Currently, the rust engine only supports internal runs.", "i" = "This is a temporary limitation.")
            )
          }
        }, # end fit method
        
        #' @description
        #' Calculate posteriors from a fitted model.
        #' #' @details
        #' This method calculates posteriors from a compiled model. It is not necessary to have
        #' run the model first, but it is necessary to have an informative prior distribution.
        #' This prior will typically be the result of a previous run, but may also be a file
        #' containing support points, with each column named as a parameter in the model plus a final column
        #' for probability.  Each row contains values for the parameters and the associated probability for
        #' those parameter values.  The file can be saved as a csv file.
        #'
        #' To calculate the posteriors, `map()` calls the `fit()` method with the `cycles` argument set to 0
        #' and the `algorithm` argument set to "POSTPROB". If `data` are not provided as an argument to
        #' `map()`, the model's `data` field is used instead. If `data` is provided, it must be a
        #' [PM_data] object or the pathname of a file which can be loaded as a [PM_data] object.
        #' @param ... Arguments passed to the `fit` method. Note that the `cycles` argument is set to 0,
        #' and the `algorithm` argument is set to "POSTPROB" automatically.
        map = function(...) {
          # browser()
          args <- list(...)
          
          if (!is.null(purrr::pluck(args, "cycles")) && purrr::pluck(args, "cycles") != 0) {
            cli::cli_inform(c("i" = "{.arg cycles} set to 0 for posteriors"))
          }
          args$cycles <- 0 # ensure cycles is set to 0
          
          
          if (!is.null(purrr::pluck(args, "algorithm")) && purrr::pluck(args, "algorithm") != "POSTPROB") {
            cli::cli_inform(c("i" = "{.arg algorithm} set to POSTPROB for posteriors"))
          }
          args$algorithm <- "POSTPROB" # ensure algorithm is set to POSTPROB
          
          
          if (is.null(purrr::pluck(args, "data"))) {
            cli::cli_abort(c("x" = "Data must be specified for posteriors."))
          }
          
          if (is.null(purrr::pluck(args, "prior")) || purrr::pluck(args, "prior") == "sobol") {
            cli::cli_abort(c(
              "x" = "Please specify a non-uniform prior for posteriors.",
              " " = "This can be a prior run number or the name of a file with support points."
            ))
          }
          
          do.call(self$fit, args)
        },
        #' @description
        #' Simulate data from the model using a set of parameter values.
        #' @details
        #' This method simulates output from the model using a set of parameter values
        #' provided in the `theta` matrix and the template for dosing/observations in
        #' the `data` object.
        #' @param data A [PM_data] object containing the dosing and observation information.
        #' @param theta A matrix of parameter values to use for the simulation.
        #' The `theta` matrix should have the same number of columns as the number of primary parameters in the model.
        #' Each row of `theta` represents a different set of parameter values.
        #'
        sim = function(data, theta) {
          if (!inherits(data, "PM_data")) {
            cli::cli_abort(c("x" = "Data must be a PM_data object."))
          }
          if (!is.matrix(theta)) {
            cli::cli_abort(c("x" = "theta must be a matrix."))
          }
          if (!is.numeric(theta)) {
            cli::cli_abort(c("x" = "theta must be a matrix of numeric values."))
          }
          if (ncol(theta) != length(private$get_primary())) {
            cli::cli_abort(c("x" = "theta must have the same number of columns as the number of parameters."))
          }
          
          
          temp_csv <- tempfile(fileext = ".csv")
          data$save(temp_csv, header = FALSE)
          if (getPMoptions()$backend == "rust") {
            if (is.null(self$binary_path)) {
              self$compile()
              if (is.null(self$binary_path)) {
                cli::cli_abort(c("x" = "Model must be compiled before simulating."))
              }
            }
            sim <- simulate_all(temp_csv, self$binary_path, theta, kind = tolower(self$model_list$type))
          } else {
            cli::cli_abort(c("x" = "This function can only be used with the rust backend."))
          }
          return(sim)
        },
        #' @description
        #' Compile the model to a binary file.
        #' @details
        #' This method write the model to a Rust file in a temporary path,
        #' updates the `binary_path` field for the model, and compiles that
        #' file to a binary file that can be used for fitting or simulation.
        #' If the model is already compiled, the method does nothing.
        #'
        compile = function() {
          if (!is.null(self$binary_path) && file.exists(self$binary_path)) {
            # model is compiled
            return(invisible(NULL))
          }
          
          model_path <- file.path(tempdir(), "model.rs")
          private$write_model_to_rust(model_path)
          output_path <- tempfile(pattern = "model_", fileext = ".pmx")
          cli::cli_inform(c("i" = "Compiling model..."))
          # path inside Pmetrics package
          template_path <- if (Sys.getenv("env") == "Development") { temporary_path() } else { system.file(package = "Pmetrics")}
          if (file.access(template_path, 0) == -1 | file.access(template_path, 2) == -1){
            cli::cli_abort(c("x" = "Template path {.path {template_path}} does not exist or is not writable.",
            "i" = "Please set the template path with {.fn setPMoptions} (choose {.emph Compile Options}), to an existing, writable folder."
          ))
        } 
        if (Sys.getenv("env") == "Development") {cat("Using template path:", template_path, "\n")}
        tryCatch(
          {
            compile_model(model_path, output_path, private$get_primary(), template_path, kind = tolower(self$model_list$type))
            self$binary_path <- output_path
          },
          error = function(e) {
            cli::cli_abort(
              c("x" = "Model compilation failed: {e$message}", "i" = "Please check the model file and try again.")
            )
          }
        )
        
        return(invisible(self))
      }, # end compile method
      #' @description
      #' Copy model code to clipboard.
      #' @details
      #' This method copies the R code to create the model to the clipboard.
      #' This is useful for saving the model code in a script, as model files
      #' will be deprecated in future versions of Pmetrics.
      copy = function() {
        arg_list <- self$arg_list

        # pri
        pri <- c(
          "  pri = list(\n",
          
          purrr::map_chr(names(arg_list$pri), \(i) {
            sprintf("    %s = ab(%.3f, %.3f)", i, arg_list$pri[[i]]$min, arg_list$pri[[i]]$max)
          }) %>% paste(collapse = ",\n"),
          "\n  ),"
        )
        # cov
        if ("cov" %in% names(arg_list)) {
          cov <- c(
            "\n  cov = list(\n",
            purrr::map_chr(names(arg_list$cov), \(i) {
              sprintf("    %s = interp(%s)", i, ifelse(arg_list$cov[[i]] == 0, "\"none\"", ""))
            }) %>% paste(collapse = ",\n"),
            "\n  ),"
          )
        } else {
          cov <- NULL
        }
        # sec
        if (!is.null(arg_list$sec)) {
          sec <- c(
            "\n  sec = ",
            paste0(deparse(arg_list$sec), collapse = "\n  "),
            ","
          )
        } else {
          sec <- NULL
        }
        # fa
        if (!is.null(arg_list$fa)) {
          fa <- c(
            "\n  fa = ",
            paste0(deparse(arg_list$fa), collapse = "\n  "),
            ","
          )
        } else {
          fa <- NULL
        }
        # ini
        if (!is.null(arg_list$ini)) {
          ini <- c(
            "\n  ini = ",
            paste0(deparse(arg_list$ini), collapse = "\n  "),
            ","
          )
        } else {
          ini <- NULL
        }
        # lag
        if (!is.null(arg_list$lag)) {
          lag <- c(
            "\n  lag = ",
            paste0(deparse(arg_list$lag), collapse = "\n  "),
            ","
          )
        } else {
          lag <- NULL
        }
        # eqn
        if (!is.null(arg_list$eqn)) {
          eqn <- c(
            "\n  eqn = ",
            paste0(deparse(arg_list$eqn), collapse = "\n  "),
            ","
          )
        } else {
          eqn <- NULL
        }
        # out
        out <- c(
          "\n  out = ",
          paste0(deparse(arg_list$out), collapse = "\n  "),
          ","
        )
        # err
        err <- c(
          "\n  err = list(\n",
          purrr::map_chr((arg_list$err), \(i) {
            sprintf("    %s(%i, c(%.1f, %.1f, %.1f, %.1f)%s)", 
            i$type,
            i$initial,
            ifelse(length(i$coeff) >= 1, i$coeff[1], 0),
            ifelse(length(i$coeff) >= 2, i$coeff[2], 0),
            ifelse(length(i$coeff) >= 3, i$coeff[3], 0),
            ifelse(length(i$coeff) >= 4, i$coeff[4], 0),
            ifelse(i$fixed, ", fixed = TRUE", "")
          )
        }) %>% paste(collapse = ",\n"),
        "\n  )"
      )
      
      model_copy <- c(
        "mod <- PM_model$new(\n",
        paste0(c(pri, cov, sec, fa, ini, lag, eqn, out, err), collapse = ""),
        "\n)"
      )
      cli::cli_inform(c(
        ">" = "Model code copied to clipboard.",
        ">" = "Paste the code into your script for future use, renaming the assigned variable if needed."))
      if (requireNamespace("clipr", quietly = TRUE)) {
        clipr::write_clip(model_copy)
      } else {
        cli::cli_inform(c("i" = "Please install the {.pkg clipr} package to enable clipboard functionality."))
        cat(model_copy, sep = "\n")
      }
      return(invisible(self))
      
      } # end copy
      
    ), # end public list
    private = list(
      # read file
      R6fromFile = function(file) {
        msg <- ""
        blocks <- parseBlocks(file) # this function is in PMutilities
        # check for reserved variable names
        conflicts <- reserved_name_conflicts(blocks)
        if (length(conflicts) > 0) {
          msg <- glue::glue("The following are reserved names and cannot be used as variables in a model: {paste(conflicts, collapse = ', ')}")
          return(list(status = -1, msg = msg))
        }
        
        if (length(grep(";", blocks$primVar)) > 0) {
          # using ';' as separator
          sep <- ";"
        } else {
          if (length(grep(",", blocks$primVar)) > 0) {
            # using ',' as separator
            sep <- ","
          } else {
            return(list(status = -1, msg = "\nPrimary variables should be defined as 'var,lower_val,upper_val' or 'var,fixed_val'.\n"))
          }
        }
        
        # build arg_list
        arg_list <- list()
        # this function makes pri for PM_model
        arg_list$pri <- sapply(strsplit(blocks$primVar, sep), function(x) {
          # find out if constrained to be positive
          const_pos <- any(grepl("\\+", x))
          if (const_pos) {
            x <- gsub("\\+", "", x)
            cli::cli_inform(c(
              "i" = "Truncating variables to positive ranges is not required for NPAG/NPOD",
              " " = "This may be updated as parametric algorithms come online, but will be ignored for now."
            ))
          }
          
          # find out if constant
          const_var <- any(grepl("!", x))
          if (const_var) {
            x <- gsub("!", "", x)
            cli::cli_abort(c("x" = "Constants should be defined in the appropriate block, not #PRI."))
          }
          
          values <- as.numeric(x[-1])
          
          if (length(x[-1]) == 1) { # fixed
            cli::cli_abort(c(
              "x" = "Fixed but unknown are no longer supported.",
              "i" = "If necessary, fit them as random and then use a fixed value in subsequent runs."
            ))
          } else { # range
            thisItem <- list(ab(values[1], values[2]))
          }
          names(thisItem) <- x[1]
          thisItem
        }) # end sapply
        
        # covariates
        covar <- blocks$covar
        const_covar <- grepl("!", covar) # returns boolean vector, length = ncov
        covar <- gsub("!", "", covar) # remove "!"
        covar_list <- tolower(covar)
        
        # add to arg_list
        arg_list$cov <- purrr::map_vec(const_covar, \(x){
          type <- ifelse(!x, "lm", "none")
          interp(type)
        }) %>%
        purrr::set_names(covar_list)
        
        
        # extra
        # if (blocks$extra[1] != "") {
        #   arg_list$ext <- blocks$extra
        # } else {
        #   arg_list$extra <- NULL
        # }
        
        # secondary variables
        if (blocks$secVar[1] != "") {
          arg_list$sec <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$secVar, collapse = '\n  ')}\n}}")))
        } else {
          arg_list$sec <- NULL
        }
        
        # bioavailability
        if (blocks$f[1] != "") {
          arg_list$fa <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$f, collapse = '\n  ')}\n}}")))
        } else {
          arg_list$fa <- NULL
        }
        
        # bolus
        if (blocks$bol[1] != "") {
          cli::cli_inform(c(
            "i" = "The bolus block is no longer used as of Pmetrics 3.0.0.",
            " " = "Indicate bolus inputs as {.code B[x]} in equations, where {.code x} is the input number."
          ))
        }
        
        # initial conditions
        if (blocks$ini[1] != "") {
          arg_list$ini <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$ini, collapse = '\n  ')}\n}}")))
        } else {
          arg_list$ini <- NULL
        }
        
        # lag time
        if (blocks$lag[1] != "") {
          arg_list$lag <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$lag, collapse = '\n  ')}\n}}")))
        } else {
          arg_list$lag <- NULL
        }
        
        # differential equations - legacy
        if (!is.null(blocks$diffeq) && blocks$diffeq[1] != "") {
          cli::cli_inform(c(
            "i" = "The #DIFFEQ block is no longer used as of Pmetrics 3.0.0.",
            " " = "The block is now called #EQN for more general equations.",
            " " = "Equations have been moved to the {.code eqn} element."
          ))
          arg_list$eqn <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$diffeq, collapse = '\n  ')}\n}}")))
        } else {
          arg_list$eqn <- NULL
        }
        
        # model equations - will eventually replace diffeq above
        if (blocks$eqn[1] != "") {
          arg_list$eqn <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$eqn, collapse = '\n  ')}\n}}")))
        } else {
          arg_list$eqn <- NULL
        }
        
        # out/err
        n_outputLines <- length(blocks$output)
        outputLines <- grep("y\\([[:digit:]]+\\)|y\\[[[:digit:]]+\\]", blocks$output)
        if (length(outputLines) == 0) {
          return(list(status = -1, msg = "\nYou must have at least one output equation of the form 'Y[1] = ...'\n"))
        }
        
        
        arg_list$out <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$out, collapse = '\n  ')}\n}}")))
        
        err <- tolower(gsub("[[:space:]]", "", blocks$error))
        # process constant gamma/lambda
        err_type <- c("additive", "proportional")[1 + grepl("^g", err[1])]
        const_gamlam <- grepl("!", err[1])
        gamlam_value <- as.numeric(stringr::str_match(err[1], "\\d+\\.?\\d*"))
        # process constant coefficients
        const_coeff <- grepl("!", err[-1]) # returns boolean vector, length = nout
        err <- gsub("!", "", err) # remove "!"
        
        coeff_fxns <- err[-1] %>%
        purrr::imap(\(x, idx) {
          glue::glue("{err_type}({gamlam_value}, c({x}), {const_coeff[{idx}]})")
        }) %>%
        unlist()
        
        arg_list$err <- eval(parse(text = glue::glue("c(\n{paste({coeff_fxns}, collapse = ',\n')}\n)")))
        
        cat(msg)
        flush.console()
        return(arg_list)
      }, # end R6fromFile
  
      write_model_to_rust = function(file_path = "main.rs") {
        # Check if model_list is not NULL
        if (is.null(self$model_list)) {
          cli::cli_abort(c("x" = "Model list is empty.", "i" = "Please provide a valid model list."))
        }
        
        if (self$model_list$type %in% c("Analytical", "ODE")) {
          placeholders <- c("eqn", "lag", "fa", "ini", "out", "n_eqn", "n_out")
          base <- paste0(
            "#[allow(unused_mut)]\nequation::",
            self$model_list$type,
            "::new(\n",
            paste("<", placeholders[1:5], ">", sep = "", collapse = ",\n "),
            ",\n (",
            paste("<", placeholders[6:7], ">", sep = "", collapse = ", "),
            "),\n)"
          )
        } else {
          cli::cli_abort(c("x" = "Invalid model type.", "i" = "Please provide a valid model type."))
        }
        
        
        # Replace placeholders in the base string with actual values from model_list
        base <- placeholders %>%
        purrr::reduce(\(x, y) stringr::str_replace(x, stringr::str_c("<", y, ">"), as.character(self$model_list[[y]])), .init = base)
        # Write the model to a file
        writeLines(base, file_path)
      },
      from_file = function(file_path) {
        self$model_list <- private$makeR6model(model_filename)
      },
      get_primary = function() {
        return(tolower(self$model_list$parameters))
      }  
  ) # end private
) # end R6Class PM_model

##### These functions create various model components

#' @title Additive error model
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Create an additive (lambda) error model
#' @param initial Initial value for lambda
#' @param coeff Vector of coefficients defining assay error polynomial
#' @param fixed Estimate if `FALSE` (default).
#' @export
additive <- function(initial, coeff, fixed = FALSE) {
  PM_err$new(type = "additive", initial = initial, coeff = coeff, fixed = fixed)
}



#' @title Proportional error model
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Create an proportional (gamma) error model
#' @param initial Initial value for gamma
#' @param coeff Vector of coefficients defining assay error polynomial
#' @param fixed Estimate if `FALSE` (default).
#' @export
proportional <- function(initial, coeff, fixed = FALSE) {
  PM_err$new(type = "proportional", initial = initial, coeff = coeff, fixed = fixed)
}

PM_err <- R6::R6Class(
  "PM_err",
  public = list(
    #' @field type Type of error model, either "additive" or "proportional".
    type = NULL,
    #' @field initial Initial value for the error model.
    initial = NULL,
    #' @field coeff Coefficients for the assay error polynomial.
    coeff = NULL,
    #' @field fixed If `TRUE`, the error model is fixed and not estimated.
    fixed = NULL,
    initialize = function(type, initial, coeff, fixed) {
      self$type <- type
      self$initial <- initial
      self$coeff <- coeff
      self$fixed <- fixed
    },
    print = function() {
      if (self$fixed) {
        cli::cli_text("{.strong {tools::toTitleCase(self$type)}}, with fixed value of {.emph {self$initial}} and coefficients {.emph {paste(self$coeff, collapse = ', ')}}.")
      } else {
        cli::cli_text("{.strong {tools::toTitleCase(self$type)}}, with initial value of {.emph {self$initial}} and coefficients {.emph {paste(self$coeff, collapse = ', ')}}.")
      }
    },
    flatten = function() {
      list(initial = self$initial, coeff = self$coeff, type = self$type, fixed = self$fixed)
    }
  )
)

#' @title Primary parameter values
#' @description
#' `r lifecycle::badge("experimental")`
#' Define primary model parameter object.
#' This is used internally by the `PM_model` class.
#' @keywords internal
PM_pri <- R6::R6Class(
  "PM_pri",
  public = list(
    #' @field min Minimum value of the range.
    min = NULL,
    #' @field max Maximum value of the range.
    max = NULL,
    #' @field mean Mean value of the range, calculated as (min + max) / 2.
    mean = NULL,
    #' @field sd Standard deviation of the range, calculated as (max - min) / 6.
    sd = NULL,
    #' @description
    #' Initialize a new range object.
    #' @param min Minimum value of the range.
    #' @param max Maximum value of the range.
    initialize = function(min, max) {
      self$min <- min
      self$max <- max
      self$mean <- (min + max) / 2
      self$sd <- (max - min) / 6
    },
    #' @description
    #' Print the range.
    print = function() {
      cli::cli_text("[{.strong {self$min}}, {.strong {self$max}}], {.emph ~N({round(self$mean,2)}}, {.emph {round(self$sd,2)})}")
    }
  )
)


#' @title Initial range for primary parameter values
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Define primary model parameter initial values as range. For nonparametric,
#' this range will be absolutely respected. For parametric, the range serves
#' to define the mean (midpoint) and standard deviation (1/6 of the range) of the
#' initial parameter value distribution.
#' @param min Minimum value.
#' @param max Maximum value.
#' @export
ab <- function(min, max) {
  PM_pri$new(min, max)
}



#' @title Initial mean/SD for primary parameter values
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Define primary model parameter initial values as mean and standard
#' deviation, which translate to a range. The mean serves as the midpoint
#' of the range, with 3 standard deviations above and below the mean to define
#' the min and max of the range. For nonparametric,
#' this range will be absolutely respected. For parametric,
#' values can be estimated beyond the range.
#' @param mean Initial mean.
#' @param sd Initial standard deviation.
#' @export
msd <- function(mean, sd) {
  min <- mean - 3 * sd
  max <- mean + 3 * sd
  if (min < 0) {
    cli::cli_warn(c(
      "i" = "Negative minimum value for primary parameter range.",
      " " = "This may not be appropriate for your model."
    ))
  }
  PM_pri$new(min, max)
}



#' @title Model covariate declaration
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Declare whether covariates in the data are to have
#' interpolation between values or not.
#' @param type If `type = "lm"` (the default) or `type = "linear"`,
#' the covariate value will be
#' linearly interpolated between values when fitting the model to the data.
#' in a model list `cov` item. To fix covariate values to the value at the
#' last time point, set `type = "none"`.
#' @return A value of 1 for "lm" and 0 for "none", which will be passed to Rust.
#' @examples
#' \dontrun{
#' cov <- c(
#'   wt = interp(), # same as interp("lm") or interp("linear")
#'   visit = interp("none")
#' )
#' }
#' @export
interp <- function(type = "lm") {
  if (!type %in% c("lm", "linear", "none")) {
    cli::cli_abort(c(
      "x" = "{type} is not a valid covariate interpolation type.",
      "i" = "See help for {.help PM_model()}."
    ))
  }
  if (type %in% c("lm", "linear")) {
    return(1)
  } else {
    return(0)
  }
}




# PLOT --------------------------------------------------------------------

#' @title Plot PM_model objects
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots a [PM_model] based on differential equations using network plots from tidygraph and ggraph packages.
#'
#' @details
#' This accepts a [PM_model] object and creates a network plot where nodes are compartments
#' and edges are arrows connecting compartments.
#' @method plot PM_model
#' @param x The name of an [PM_model] object.
#' @param marker Controls the characteristics of the compartments (nodes).
#' It can be boolean or a list.
#' `TRUE` will plot the compartments with default characteristics.
#' `FALSE` will suppress compartment plotting.
#' If a list, can control some marker characteristics, including overriding defaults.
#' These include:
#' \itemize{
#' \item{`color`} Marker color (default: dodgerblue).
#' \item{`opacity`} Ranging between 0 (fully transparent) to 1 (fully opaque). Default is 0.5.
#' \item{`size`} Relative size of boxes, ranging from 0 to 1.  Default is 0.25.
#' \item{`line`} A list of  additional attributes governing the outline for filled shapes, most commonly
#' color (default: black) and width (default: 0.5).
#' }
#' <br>
#' <br>
#' Example: `marker = list(color = "red", opacity = 0.8, line = list(color = "black", width = 1))`
#' @param line Controls characteristics of arrows (edges).
#' `TRUE` will plot default lines. `FALSE` will suppress lines.
#' If a list, can control some line characteristics, including overriding defaults.
#' These include:
#' \itemize{
#' \item{`color`} Line color (default: black)
#' \item{`width`} Thickness in points (default: 1).
#' }
#' <br>
#' <br>
#' Example: `line = list(color = "red", width = 2)`
#' @param explicit A data frame or tibble containing two columns named `from` and `to`
#' to add additional connecting arrows to the plot indicating transfer between
#' compartments. For each row, the `from` column contains the compartment number of the arrow origin, and the
#' `to` column contains the compartment number of the arrow destination. Use 0 to indicate
#' a destination to the external sink. e.g., `explicit = data.frame(from = 3, to = 0)`
#' @param implicit Similar to `explicit`, used to add dashed connecting arrows
#' to the plot indicating implicit transfer between
#' compartments. For each row, the `from` column contains the compartment number of the arrow origin, and the
#' `to` column contains the compartment number of the arrow destination. Use 0 to indicate
#' a destination to the external sink. e.g., `implicit = data.frame(from = 2, to = 4)`
#' @param print If `TRUE`, will print the object and return it. If `FALSE`, will only return the object.
#' @param ... Not used.
#' @return A plot object of the model.
#' @author Markus Hovd, Julian Otalvaro, Michael Neely
#' @seealso [PM_model], [ggraph::ggraph()], [ggplot2::ggplot()]
#' @export
#' @examples
#' \dontrun{
#' NPex$model$plot()
#' }
#' @family PMplots

plot.PM_model <- function(x,
  marker = TRUE,
  line = TRUE,
  explicit,
  implicit,
  print = TRUE,
  ...) {
    model <- x
    marker <- if (is.list(marker) || marker) {
      amendMarker(marker,
        default = list(
          color = "dodgerblue",
          size = 0.25,
          line = list(width = 0.5)
        )
      )
    } else {
      FALSE
    }
    line <- if (is.list(line) || line) {
      amendLine(line, default = list(color = "black"))
    } else {
      FALSE
    }
    
    if (inherits(model, "PM_lib")) {
      eqns <- model$arg_list$eqn
      outs <- model$arg_list$out
    } else if (inherits(model, "PM_model")) {
      if (model$model_list$name == "user") {
        eqns <- model$arg_list$eqn
        outs <- model$arg_list$out
      } else {
        eqns <- get(model$model_list$name)$arg_list$eqn
        outs <- get(model$model_list$name)$arg_list$out
      }
    } else {
      cli::cli_abort(c(
        "x" = "Unknown model type to plot."
      ))
    }
    
    eqns <- func_to_char(eqns)
    outs <- func_to_char(outs)
    
    
    # filter any equations that are not diffeq or outputs
    
    eqns <- eqns %>%
    map(
      purrr::keep,
      stringr::str_detect,
      stringr::regex("dX\\[\\d+\\]|XP\\(\\d+\\)", ignore_case = TRUE)
    ) %>%
    unlist()
    
    outs <- outs %>%
    map(
      purrr::keep,
      stringr::str_detect,
      stringr::regex("Y\\[\\d+\\]", ignore_case = TRUE)
    ) %>%
    unlist()
    
    
    
    
    
    #### INTERNAL FUNCTIONS
    # Parse the function body
    parse_equations <- function(func) {
      body_expr <- body(func)
      equations <- list()
      
      # Handle single expression or block
      if (is.call(body_expr) && body_expr[[1]] == "{") {
        # Multiple statements in braces
        for (i in 2:length(body_expr)) {
          eq <- body_expr[[i]]
          if (is.call(eq) && length(eq) == 3 && as.character(eq[[1]]) %in% c("=", "<-")) {
            equations <- append(equations, list(eq))
          }
        }
      } else if (is.call(body_expr) && length(body_expr) == 3 &&
      as.character(body_expr[[1]]) %in% c("=", "<-")) {
        # Single assignment
        equations <- list(body_expr)
      }
      
      return(equations)
    }
    
    
    ##### Handle distributions
    # Recursively distribute products over sums in a single expression or equation.
    # - Works symbolically (no evaluation).
    # - Handles unary minus.
    # - If given an assignment (= or <-), only the RHS is expanded.
    # Fully distribute products over sums and flatten subtraction.
    # If given an assignment (= or <-), only the RHS is expanded.
    expand_distribute <- function(expr) {
      op_of <- function(e) if (is.call(e)) as.character(e[[1]]) else ""
      
      # Build a product call (no eval)
      make_prod <- function(a, b) as.call(list(as.name("*"), a, b))
      
      # Fold a list of factors into a product
      fold_prod <- function(factors) Reduce(make_prod, factors)
      
      # Rebuild a (flattened) sum from signed terms
      build_sum <- function(terms) {
        if (length(terms) == 0) {
          return(0)
        }
        mk <- function(sign, e) if (sign == -1) as.call(list(as.name("-"), e)) else e
        out <- mk(terms[[1]]$sign, terms[[1]]$expr)
        if (length(terms) == 1) {
          return(out)
        }
        for (k in 2:length(terms)) {
          tk <- terms[[k]]
          out <- if (tk$sign == 1) {
            as.call(list(as.name("+"), out, tk$expr))
          } else {
            as.call(list(as.name("-"), out, tk$expr))
          }
        }
        out
      }
      
      # Core: return a flat list of signed terms {sign=±1, expr=LANG}
      expand_terms <- function(e, sign = 1) {
        # atoms
        if (!is.call(e)) {
          return(list(list(sign = sign, expr = e)))
        }
        
        op <- op_of(e)
        
        # parentheses
        if (op == "(") {
          return(expand_terms(e[[2]], sign))
        }
        
        # assignment: expand RHS only, rebuild later
        if (op %in% c("=", "<-")) {
          rhs_terms <- expand_terms(e[[3]], +1)
          rhs_exp <- build_sum(rhs_terms)
          return(list(list(sign = +1, expr = as.call(list(as.name(op), e[[2]], rhs_exp)))))
        }
        
        # addition
        if (op == "+") {
          return(c(
            expand_terms(e[[2]], sign),
            expand_terms(e[[3]], sign)
          ))
        }
        
        # subtraction (binary or unary)
        if (op == "-") {
          if (length(e) == 3) {
            return(c(
              expand_terms(e[[2]], sign),
              expand_terms(e[[3]], -sign)
            ))
          } else {
            return(expand_terms(e[[2]], -sign)) # unary minus
          }
        }
        
        # multiplication: distribute across additive factors
        if (op == "*") {
          # expand each factor into its additive term list
          args <- as.list(e)[-1]
          expanded_factors <- lapply(args, function(a) expand_terms(a, +1))
          
          # start with neutral element (sign=+1, expr=1)
          combos <- list(list(sign = +1, expr = 1))
          for (f_terms in expanded_factors) {
            newc <- list()
            for (c1 in combos) {
              for (t2 in f_terms) {
                s <- c1$sign * t2$sign
                # build product (avoid multiplying by 1 syntactically where possible)
                e1 <- c1$expr
                e2 <- t2$expr
                prod_expr <-
                if (is.numeric(e1) && length(e1) == 1 && e1 == 1) {
                  e2
                } else if (is.numeric(e2) && length(e2) == 1 && e2 == 1) {
                  e1
                } else if (is.numeric(e1) && length(e1) == 1 && e1 == -1) {
                  as.call(list(as.name("-"), e2))
                } else if (is.numeric(e2) && length(e2) == 1 && e2 == -1) {
                  as.call(list(as.name("-"), e1))
                } else {
                  make_prod(e1, e2)
                }
                newc[[length(newc) + 1]] <- list(sign = s, expr = prod_expr)
              }
            }
            combos <- newc
          }
          # apply the incoming sign to all combos
          for (i in seq_along(combos)) combos[[i]]$sign <- sign * combos[[i]]$sign
          return(combos)
        }
        
        # other calls: expand children but treat as atomic w.r.t. addition
        args <- as.list(e)
        args[-1] <- lapply(args[-1], function(a) build_sum(expand_terms(a, +1)))
        list(list(sign = sign, expr = as.call(args)))
      }
      
      # If it's an assignment, expand_terms already rebuilt it as a single term.
      # Otherwise, build the flattened sum.
      terms <- expand_terms(expr, +1)
      
      # Special case: a single rebuilt assignment
      if (length(terms) == 1 && is.call(terms[[1]]$expr) &&
      op_of(terms[[1]]$expr) %in% c("=", "<-")) {
        return(terms[[1]]$expr)
      }
      
      build_sum(terms)
    }
    
    # Parse output equations
    parse_output_equations <- function(equations) {
      # if (is.null(func)) return(list())
      
      # equations <- parse_equations(func)
      outputs <- list()
      
      for (eq in equations) {
        lhs <- eq[[2]]
        rhs <- eq[[3]]
        
        # Extract output number from y[i]
        if (is.call(lhs) && as.character(lhs[[1]]) == "[" &&
        length(lhs) >= 3 && as.character(lhs[[2]]) == "y") {
          output_num <- as.numeric(as.character(lhs[[3]]))
          
          # Convert RHS to string representation
          rhs_str <- deparse(rhs, width.cutoff = 500)
          
          # Find which compartment this output refers to
          comp_ref <- extract_x_pattern(rhs)
          if (is.null(comp_ref)) {
            # Look deeper in the expression for x[i] patterns
            comp_ref <- find_x_in_expression(rhs)
          }
          
          outputs <- append(outputs, list(list(
            output_num = output_num,
            equation = rhs_str,
            compartment = comp_ref
          )))
        }
      }
      
      return(outputs)
    }
    
    # Find x[i] pattern in any expression
    find_x_in_expression <- function(expr) {
      if (is.call(expr)) {
        # Check current expression
        x_idx <- extract_x_pattern(expr)
        if (!is.null(x_idx)) {
          return(x_idx)
        }
        
        # Recursively check sub-expressions
        for (i in 1:length(expr)) {
          if (i > 1) { # Skip the function name
            x_idx <- find_x_in_expression(expr[[i]])
            if (!is.null(x_idx)) {
              return(x_idx)
            }
          }
        }
      }
      return(NULL)
    }
    
    # Parse terms from right-hand side recursively
    parse_rhs_terms <- function(rhs_expr) {
      terms <- list()
      
      # Recursively extract terms and track sign
      extract_terms <- function(expr, current_sign = "+") {
        if (is.call(expr)) {
          op <- as.character(expr[[1]])
          
          if (op == "+") {
            extract_terms(expr[[2]], current_sign)
            extract_terms(expr[[3]], current_sign)
          } else if (op == "-") {
            if (length(expr) == 3) {
              # Binary subtraction: a - b
              extract_terms(expr[[2]], current_sign)
              extract_terms(expr[[3]], ifelse(current_sign == "+", "-", "+"))
            } else {
              # Unary minus: -a
              extract_terms(expr[[2]], ifelse(current_sign == "+", "-", "+"))
            }
          } else if (op == "*") {
            # Look for x[i] and collect coefficient(s)
            vars <- lapply(expr[-1], extract_x_pattern)
            if (any(!sapply(vars, is.null))) {
              xi_index <- which(!sapply(vars, is.null))
              x_part <- expr[[xi_index + 1]]
              coeff_parts <- expr[-c(1, xi_index + 1)]
              coeff_str <- paste(sapply(coeff_parts, deparse), collapse = "*")
              terms <<- append(terms, list(list(expr = x_part, coeff = coeff_str, sign = current_sign)))
            } else {
              # No x[i], maybe just a numeric or unrelated variable
              terms <<- append(terms, list(list(expr = expr, coeff = NULL, sign = current_sign)))
            }
          } else {
            # Some other operation; treat as atomic for now
            terms <<- append(terms, list(list(expr = expr, coeff = NULL, sign = current_sign)))
          }
        } else {
          # Symbol or constant
          terms <<- append(terms, list(list(expr = expr, coeff = NULL, sign = current_sign)))
        }
      }
      
      extract_terms(rhs_expr)
      
      return(terms)
    }
    
    # Extract x[i] pattern from expression
    extract_x_pattern <- function(expr) {
      if (is.call(expr) && as.character(expr[[1]]) == "[" &&
      length(expr) == 3 && as.character(expr[[2]]) == "x") {
        return(as.numeric(as.character(expr[[3]])))
      }
      return(NULL)
    }
    
    # Extract compartment connections
    extract_connections <- function(equations) {
      compartments <- c()
      all_terms <- list()
      
      # First pass: collect signed terms per compartment
      for (eq in equations) {
        lhs <- eq[[2]]
        rhs <- eq[[3]]
        
        if (is.call(lhs) && as.character(lhs[[1]]) == "[" &&
        length(lhs) >= 3 && as.character(lhs[[2]]) == "dx") {
          comp_num <- as.numeric(as.character(lhs[[3]]))
          compartments <- unique(c(compartments, comp_num))
          
          # dist_terms <- distribute_product(rhs)
          # terms <- parse_rhs_terms(dist_terms)
          terms <- parse_rhs_terms(rhs)
          
          for (term in terms) {
            expr <- term$expr
            sign <- term$sign
            coeff <- term$coeff
            
            x_index <- extract_x_pattern(expr)
            if (!is.null(x_index)) {
              all_terms <- append(all_terms, list(list(
                comp = comp_num,
                sign = sign,
                coeff = coeff,
                x_index = x_index
              )))
            }
          }
        }
      }
      
      # Second pass: match positive and negative terms
      used <- logical(length(all_terms))
      connections <- list()
      
      for (i in seq_along(all_terms)) {
        ti <- all_terms[[i]]
        if (used[i] || ti$sign != "-") next
        
        match_found <- FALSE
        for (j in seq_along(all_terms)) {
          tj <- all_terms[[j]]
          if (used[j] || tj$sign != "+") next
          
          # Match by coeff and x_index
          if (identical(ti$coeff, tj$coeff) && ti$x_index == tj$x_index) {
            connections <- append(connections, list(list(
              from = ti$comp,
              to = tj$comp,
              coeff = ti$coeff
            )))
            used[i] <- TRUE
            used[j] <- TRUE
            match_found <- TRUE
            break
          }
        }
        
        # If no match, it's elimination
        if (!match_found) {
          connections <- append(connections, list(list(
            from = ti$comp,
            to = 0,
            coeff = ti$coeff
          )))
          used[i] <- TRUE
        }
      }
      
      return(list(connections = connections, compartments = sort(compartments)))
    }
    
    
    
    # Modify layout logic to use circular positioning
    create_plot <- function(connections, compartments, outputs) {       
      box_width <- 1.2
      box_height <- 0.8
      
      n_comp <- length(compartments)
      if (n_comp == 0) {
        plot.new()
        title(main = "No compartments detected")
        return()
      }
      
      # Circular layout
      radius <- 4
      angles <- seq(0, 2 * pi, length.out = n_comp + 1)[-(n_comp + 1)]
      angles <- angles - angles[which(compartments == 1)] + pi / 2
      x_pos <- radius * cos(angles)
      y_pos <- radius * sin(angles)
      layout_df <- data.frame(compartment = compartments, x = x_pos, y = y_pos)
      
      # Elimination
      elim_comps <- unique(sapply(connections, function(c) if (c$to == 0) c$from else NULL))
      elim_comps <- elim_comps[!sapply(elim_comps, is.null)]
      
      arrow_segments <- list()
      arrow_heads <- list()
      labels <- list()
      label_tracker <- list()
      
      # Bidirectional detection
      pair_keys <- data.frame(
        original = sapply(connections, function(c) paste(c(c$from, c$to), collapse = "-")),
        sorted = sapply(connections, function(c) paste(sort(c(c$from, c$to)), collapse = "-"))
      )
      dup_table <- table(pair_keys$sorted)
      duplicates <- pair_keys$original[which(pair_keys$sorted %in% names(dup_table[dup_table > 1]))]
      
      for (conn in connections) {
        from <- as.numeric(conn$from)
        to <- as.numeric(conn$to)
        if (to == 0) next
        
        from_pos <- layout_df %>% filter(compartment == from)
        to_pos <- layout_df %>% filter(compartment == to)
        
        key <- paste(sort(c(from, to)), collapse = "-")
        offset <- if (key %in% duplicates) 0.25 else 0
        
        dx <- to_pos$x - from_pos$x
        dy <- to_pos$y - from_pos$y
        len <- sqrt(dx^2 + dy^2)
        norm_dx <- dx / len
        norm_dy <- dy / len
        perp_x <- -norm_dy
        perp_y <- norm_dx
        
        # Adjust start/end for box edges
        edge_dx <- box_width / 2 * norm_dx
        edge_dy <- box_height / 2 * norm_dy
        
        x1 <- from_pos$x + offset * perp_x + edge_dx
        y1 <- from_pos$y + offset * perp_y + edge_dy
        x2 <- to_pos$x + offset * perp_x - edge_dx
        y2 <- to_pos$y + offset * perp_y - edge_dy
        
        arrow_segments[[length(arrow_segments) + 1]] <- data.frame(
          x = x1, y = y1, xend = x2, yend = y2, color = "black"
        )
        
        # Arrowhead at 2/3
        frac <- 2 / 3
        xm <- x1 + frac * (x2 - x1)
        ym <- y1 + frac * (y2 - y1)
        perp_x_head <- -norm_dy * 0.10
        perp_y_head <- norm_dx * 0.10
        
        arrow_heads[[length(arrow_heads) + 1]] <- data.frame(
          x = c(xm - perp_x_head, xm + perp_x_head, xm + norm_dx * 0.3),
          y = c(ym - perp_y_head, ym + perp_y_head, ym + norm_dy * 0.3),
          group = paste0("arrow", length(arrow_heads) + 1),
          fill = "black"
        )
        
        if (!is.null(conn$coeff)) {
          key_xy <- paste(round((x1 + x2) / 2, 2), round((y1 + y2) / 2, 2))
          if (is.null(label_tracker[[key_xy]])) label_tracker[[key_xy]] <- 0
          vertical_offset <- 0.25 * label_tracker[[key_xy]]
          label_tracker[[key_xy]] <- label_tracker[[key_xy]] + 1
          
          mx <- (x1 + x2) / 2
          my <- (y1 + y2) / 2 - vertical_offset
          
          labels[[length(labels) + 1]] <- data.frame(
            x = mx, y = my, label = conn$coeff,
            color = "white", text_color = "black"
          )
        }
      }
      
      seg_df <- bind_rows(arrow_segments)
      head_df <- bind_rows(arrow_heads)
      label_df <- bind_rows(labels)
      
      elim_triangles <- layout_df %>%
      filter(compartment %in% elim_comps) %>%
      mutate(x = x - 0.4, y = y + 0.2)
      
      p <- ggplot()
      
      if (nrow(seg_df) > 0) { # we have connections
        p <- p + geom_segment(
          data = seg_df,
          aes(x = x, y = y, xend = xend, yend = yend, color = color),
          linewidth = 0.7, show.legend = FALSE
        ) +
        geom_polygon(
          data = head_df,
          aes(x = x, y = y, group = group, fill = fill),
          color = NA, show.legend = FALSE
        )
      }
      
      p <- p + geom_rect(
        data = layout_df,
        aes(
          xmin = x - box_width / 2, xmax = x + box_width / 2,
          ymin = y - box_height / 2, ymax = y + box_height / 2
        ),
        fill = "grey80", color = "black"
      ) +
      
      geom_label(
        data = layout_df,
        aes(x = x, y = y + 0.15, label = compartment), fill = NA,
        color = "black", fontface = "bold", size = 7, label.size = NA
      ) +
      
      geom_point(
        data = elim_triangles,
        aes(x = x, y = y),
        color = "black", shape = 2, size = 4
      )
      
      if (nrow(label_df) > 0) {
        p <- p + geom_label(
          data = label_df,
          aes(x = x, y = y, label = label),
          fill = label_df$color,
          color = label_df$text_color,
          fontface = "bold",
          size = 4,
          show.legend = FALSE,
          label.size = NA
        )
      }
      if (length(outputs) > 0) {
        out_df <- bind_rows(lapply(outputs, function(out) {
          comp <- out$compartment
          if (is.null(comp)) return(data.frame(x = NA, y = NA, label = NA))
          txt <- paste0("y[", out$output_num, "]")
          pos <- layout_df %>% filter(compartment == comp)
          data.frame(x = pos$x, y = pos$y - 0.2, label = txt)
        }))
        if (any(is.na(out_df$x))){
          missing_out <- as.character(which(is.na(out_df$x)))
          cli::cli_warn(c("!" = "{?This/These} output equation{?s} did not contain a parsable compartment number on the right side of the equation and {?was/were} not plotted: {missing_out}."))
          out_df <- out_df %>% filter(!is.na(x))
        } else {
          p <- p + geom_label(
            data = out_df,
            aes(x = x, y = y, label = label),
            color = "black",
            fill = NA,
            fontface = "bold",
            size = 3,
            label.size = 0
          )
        }
      }
      
      p <- p +
      coord_fixed() +
      xlim(range(layout_df$x) + c(-1.5, 1.5)) +
      ylim(range(layout_df$y) + c(-1.5, 1.5)) +
      theme_void() +
      ggtitle("Structural model") +
      scale_color_identity() +
      scale_fill_identity()
      
      return(p)
    }
    
    ##### FUNCTION CALLS
    
    # equations <- parse_equations(this_model)
    # Expand and distribute equations
    
    expanded_equations <- purrr::map(parse(text = tolower(eqns)), expand_distribute)
    outputs <- parse_output_equations(as.list(parse(text = tolower(outs))))
    out_comp <- map_chr(outputs, function(o) if (!is.null(o$compartment)) {as.character(o$compartment)} else {"unknown"})
    result <- extract_connections(expanded_equations)
    elim_count <- sum(sapply(result$connections, function(c) c$to == 0))
    elim_coeff <- map_chr(result$connections, function(c) if (c$to == 0) c$coeff else NA) %>% keep(~ !is.na(.))
    
    cli::cli_h1("Model elements")
    cli::cli_text("{length(result$compartments)} compartments")
    cli::cli_text("{length(result$connections)} connections, of which {elim_count} {?is an elimination/are eliminations}: {elim_coeff}")
    cli::cli_text("{length(outputs)} output{?s} in compartment{?s} {unique(out_comp)}")
    
    
    p <- create_plot(result$connections, result$compartments, outputs)
    if (print) print(p)
    
    return(
      invisible(list(
        p = p,
        connections = result$connections,
        compartments = result$compartments,
        outputs = outputs
      ))
    )
  }
  