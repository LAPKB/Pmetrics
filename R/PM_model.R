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
#' [build_model()] function, by defining a list
#' directly in R, or by reading a model text file. See the vignette on models
#' for details.
#'
#' **Some notes on the example at the end of this help page:**
#'
#' * It's a complete example of a three compartment model with delayed absorption.
#' * We show the method of defining the model first and embedding the `PM_model$new()` within
#' a `donttest` block to avoid automatic compilation. 
#' * Since this model can also be solved analytically with algebra, we could have used
#'  `eqn = function(){three_comp_bolus}`. 
#' @examples 
#' 
#' mod_list <- list(
#'   pri = c(
#'      CL = ab(10, 200),
#'      V0 = ab(0, 100),
#'      ka = ab(0, 3),
#'      k23 = ab(0, 5),
#'      k32 = ab(0, 5),
#'      lag1 = ab(0, 2)
#'    ),
#'    cov = c(
#'      wt = interp()
#'    ),
#'    sec = function() {
#'      V = V0 * (wt/70)
#'      ke = CL/V # define here to make eqn simpler
#'    },
#'    eqn = function() {
#'      dx[1] = -ka * x[1]
#'      dx[2] = rateiv[1] + ka * x[1] - (ke + k23) * x[2] + k32 * x[3]
#'      dx[3] = k23 * x[2] - k32 * x[3]
#'      dx[4] = x[1] / V
#'    },
#'    lag = function() {
#'      tlag[1] = lag1
#'    },
#'    out = function() {
#'      y[1] = x[1]/V
#'      y[2] = x[4] # AUC, not fitted to any data, not required
#'    },
#'    err = c(
#'      proportional(2, c(0.1, 0.15, 0, 0)) # only applies to y[1]
#'    )
#'  )
#'
#' \donttest{
#'    mod <- PM_model$new(mod_list)
#'  } 
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
    #' This is the method to create a new `PM_model` object. If all arguments are `NULL`,
    #' e.g. `mod <- PM_model$new()` the model builder shiny app will launch by a call to [build_model()],
    #' which will return the model object upon exit.
    #' 
    #' 
    #' Otherwise, the first parameter allows creation of a model from a variety of pre-existing
    #' sources, and if used, all the subsequent arguments will be ignored. If a model
    #' is defined on the fly, the arguments form the building blocks. Blocks are of two types:
    #' 
    #' * **Vectors** define *primary parameters*, *covariates*,
    #' and *error models*. These 
    #' portions of the model have specific and defined creator functions and no additional
    #' R code is permissible. They take this form:
    #'     ```
    #'     block_name = c( 
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
    #' working directory which will be read and passed to Rust engine.
    #' * List that defines the model directly in R. This will be in the same format as if
    #' all the subsequent arguments were used. For example:
    #'     ```
    #'     mod_list <- list(
    #'      pri = c(...),
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
    #' a named vector of primary parameters, which are the model parameters that
    #' are estimated in the population analysis. They are specified
    #' by one of two creator functions: [ab()] or [msd()]. For example,
    #' ```
    #' pri = c(
    #'   Ke = ab(0, 5),
    #'   V = msd(100, 10)
    #')
    #' ```
    #' The [ab()] creator specifies the
    #' initial range `[a, b]` of the parameter, while the [msd()] creator specifies
    #' the initial mean and standard deviation of the parameter.
    #' @param cov A vector whose names are the same as the covariates in the data file,
    #' and whose values are the [interp()] creator function to declare
    #' how each covariate is interpolated between entries in the data. The default argument
    #' for [interp()] is "lm" which means that values will be linearly interpolated
    #' between entries, like the R linear model function [stats::lm()]. The alternative is "none",
    #' which holds the covariate value the same as the previous entry until it changes,
    #' i.e., a carry-forward strategy.
    #'
    #' For example:
    #' ```
    #' cov = c(
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
    #'       pri = c(
    #'         ke0 = ab(0, 5),
    #'         v = ab(0, 100)
    #'       ),
    #'       cov = c(
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
    #' `tlag[i] = par`, where `tlag[i]` is the lag for drug (input) `i` and
    #' `par` is the lag parameter used in the `pri` block. 
    #'
    #' For example, if
    #' `antacid` is a covariate in the data file, and `lag1` is a primary parameter,
    #' this code could be used to model delayed absorption if an antacid is present.
    #' ```
    #' lag = function() {
    #'   tlag[1] = if(antacid == 1) lag1 else 0
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
    #' err = c(
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
    #' err = c(
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
    #' err = c(
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
          x = x,
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
        
        if(!is.null(x)){
          model_sections <- c("pri", "cov", "sec", "eqn", "lag", "fa", "ini", "out", "err")
          if (is.character(x) && length(x) == 1) { # x is a filename
            if (!file.exists(x)) {
              cli::cli_abort(c("x" = "File {.file {x}} does not exist.",
              "i" = "Current directory: {getwd()}"))
            }
            self$arg_list <- private$R6fromFile(x) # read file and populate fields
            
            
          } else if (is.list(x)) { # x is a list in R
            purrr::walk(model_sections, \(s) {
              if (s %in% names(x)) {
                self$arg_list[[s]] <- x[[s]]
              }
            })
            
          } else if (inherits(x, "PM_model")) { # x is a PM_model object
            if(!"arg_list" %in% names(x)) {
              cli::cli_abort(c("x" = "You have supplied an older {.code PM_model} format.",
              "i" = "Please see for {.help Pmetrics::PM_model()} to remake it."))
            }
            
            purrr::walk(model_sections, \(s) {
              if (s %in% names(x$arg_list)) {
                self$arg_list[[s]] <- x$arg_list[[s]]
              }
            })
            
          } else {
            cli::cli_abort(c("x" = "Non supported input for {.arg x}: {typeof(x)}",
            "i" = "It must be a filename, list, or current {.code PM_model} object."))
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
            err = err)
            other_args <- list(...)
            all_args <- c(named_args, other_args)
            if (all(sapply(all_args, is.null))) { # everything is NULL
              self <- build_model() # launch the shiny app
              return(invisible(self))
            }
          } # no, some arguments were not NULL, so keep going
          
          
          # Primary parameters must be provided
          if (is.null(self$arg_list$pri)) {
            cli::cli_abort(
              c("x" = "Primary parameters are missing.", "i" = "Please provide a list of primary parameters.")
            )
          }
          
          
          # Either an ODE-based model or an analytical model must be provided in eqn
          if (is.null(self$arg_list$eqn)){
            cli::cli_abort(c("x" = "No equations or template provided.", 
            "i" = "Please provide either a template (see {.help model_lib()}) or differential equations using {.code eqn}."))
          }
          
          
          
          # Get model template name if present (NA if absent) and set type
          model_template <- get_found_model(self$arg_list$eqn) #function defined below, returns 0 if not found, -1 if error
          
          #change logic; need to accomodate library models that are ODEs
          if (length(model_template) > 1 && model_template$analytical) {
            type <- "Analytical"
          } else {
            if(model_template == -1){
              # length was 1, value 0
              cli::cli_abort(c(
                "x" = "You have included  more than one model template.",
                "i" = "A maximum of one model template can be included in a model."
              ))
            }
            
            # length was 1, value 0
            type <- "ODE"
          }
          
          # Number of equations
          n_eqn <- if (type == "Analytical"){ model_template$ncomp } else {get_assignments(self$arg_list$eqn, "dx")}
          n_out <- get_assignments(self$arg_list$out, "y")
          
          ## Get the names of the parameters
          parameters <- tolower(names(self$arg_list$pri))
          covariates <- tolower(names(self$arg_list$cov))
          ## check to make sure required parameters present if Analytical
          if (type == "Analytical"){
            # look in pri, sec, eqn, lag, fa, ini, out blocks for required parameters
            required_parameters <- tolower(model_template$parameters)
            pri_list <- map_lgl(required_parameters, \(x){
              if (x %in% parameters){
                return(TRUE)
              } else { return(FALSE)}
            })
            
            if(length(covariates)>0){
              cov_list <- map_lgl(required_parameters, \(x){
                if (x %in% covariates){
                  return(TRUE)
                } else { return(FALSE)}
              })
            } else {
              cov_list <- rep(FALSE, length(required_parameters))
            }
            
            if(!is.null(self$arg_list$sec)){
              sec_list <- map_lgl(required_parameters, \(x){
                stringr::str_detect(tolower(func_to_char(self$arg_list$sec)), x)
              })
            } else {
              sec_list <- rep(FALSE, length(required_parameters))
            }
            
            eqn_list <- map_lgl(required_parameters, \(x){
              any(stringr::str_detect(
                stringr::str_remove_all(tolower(func_to_char(self$arg_list$eqn)), "\\s+"), # string
                paste0(x,"(?=(<-|=))")) # pattern
              )
            })
            
            if(!is.null(self$arg_lag)){
              lag_list <- map_lgl(required_parameters, \(x){
                any(stringr::str_detect(
                  stringr::str_remove_all(tolower(func_to_char(self$arg_list$lag)), "\\s+"), # string
                  paste0(x,"(?=(<-|=))")) # pattern
                )
              })
            } else {
              lag_list <- rep(FALSE, length(required_parameters))
            }
            
            if(!is.null(self$arg_fa)){
              lag_list <- map_lgl(required_parameters, \(x){
                any(stringr::str_detect(
                  stringr::str_remove_all(tolower(func_to_char(self$arg_list$fa)), "\\s+"), # string
                  paste0(x,"(?=(<-|=))")) # pattern
                )
              })
            } else {
              fa_list <- rep(FALSE, length(required_parameters))
            }
            
            if(!is.null(self$arg_ini)){
              ini_list <- map_lgl(required_parameters, \(x){
                any(stringr::str_detect(
                  stringr::str_remove_all(tolower(func_to_char(self$arg_list$ini)), "\\s+"), # string
                  paste0(x,"(?=(<-|=))")) # pattern
                )
              })
            } else {
              ini_list <- rep(FALSE, length(required_parameters))
            }
            
            out_list <- map_lgl(required_parameters, \(x){
              any(stringr::str_detect(
                stringr::str_remove_all(tolower(func_to_char(self$arg_list$out)), "\\s+"), # string
                paste0(x,"(?=(<-|=))")) # pattern
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
              cli::cli_abort(
                c("x" = "The following parameters are required for the {.code {model_template}} model template but are missing: {missing}",
                "i" = "They should be defined in one of the model blocks, likely {.code pri}, {.code sec}, {.code eqn}, or {.code out}.",
                " " = "Parameters defined in {.code pri} and {.code sec} are available to all blocks.",
                " " = "Parameters defined in other blocks are only available to that block."))
              }
            } # end parameter checks for Analytical model
            
            
            # if Analytical, need to combine sec and eqn
            if (type == "Analytical"){
              # shell function
              sec_eqn <- function() {} 
              # define the body of the shell function
              body(sec_eqn) <- suppressWarnings(as.call(c(
                quote(`{`),
                as.list(body(self$arg_list$eqn))[-1],  # remove outer `{` of f1
                as.list(body(self$arg_list$sec))[-1]   # remove outer `{` of f2
              )))
              
              # this will include template and equations in both sec and eqn
            }
            
            # sec
            # still needed for analytic, because these equations will be used
            # in other blocks
            if (!is.null(self$arg_list$sec)){
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
            if(is.null(self$arg_list$err)) {
              cli::cli_abort(
                c("x" = "Error model is missing and required.", 
                "i" = "Please see help for {.help PM_model()}.")
              )
            }
            
            #ensure length err matches length outeqs
            if (length(self$arg_list$err) != n_out) {
              cli::cli_abort(
                c("x" = "There must be one error model for each output equation.", 
                "i" = "Please check the error model.")
              )
            }
            err <- self$arg_list$err
            
            # name
            name <- if(type == "Analytical") { model_template$name } else { "user" }
            
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
            #make everything lower case if a character vector
            self$model_list <- purrr::map(model_list, \(x) {
              if (is.character(x)) {
                tolower(x)
              } else {
                x
              }
            })
            
            # this one needs to be capital
            self$model_list$type <- type
            
            
            extra_args <- list(...)
            if (!is.null(purrr::pluck(extra_args, "compile"))){
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
            pars = self$model_list$parameters
            cli::cli_text("{.eqs {pars}}")
            
            
            if (!is.null(self$model_list$covariates)) {
              cli::cli_h3(text = "Covariates")
              
              cov_list <- paste0(self$model_list$covariates, 
                ifelse(self$arg_list$cov==1, "", "(no interpolation)"))
                
                cli::cli_text("{.eqs {cov_list}}")
              }
              
              if(!is.null(self$arg_list$sec)){
                cli::cli_h3(text = "Secondary (Global) Equations")
                eqs <- func_to_char(self$arg_list$sec) #function in PMutitlities
                for (i in eqs) {
                  cli::cli_text("{.eqs {i}}")
                }
              }
              
              if(!is.null(self$arg_list$tem)){
                cli::cli_h3(text = "Analytical Model")
                cli::cli_text("{.eqs {self$arg_list$tem$name}})")
              }
              
              if(!is.null(self$arg_list$eqn)){
                cli::cli_h3(text = "Primary Equations")
                eqs <- func_to_char(self$arg_list$eqn) #function in PMutitlities
                for (i in eqs) {
                  cli::cli_text("{.eqs {i}}")
                }
              }
              
              if(!is.null(self$arg_list$lag)){
                cli::cli_h3(text = "Lag Time")
                eqs <- func_to_char(self$arg_list$lag) #function in PMutitlities
                for (i in eqs) {
                  cli::cli_text("{.eqs {i}}")
                }
              }
              
              if(!is.null(self$arg_list$fa)){
                cli::cli_h3(text = "Bioavailability (Fraction Absorbed)")
                eqs <- func_to_char(self$arg_list$fa) #function in PMutitlities
                for (i in eqs) {
                  cli::cli_text("{.eqs {i}}")
                }
              }
              
              if(!is.null(self$arg_list$ini)){
                cli::cli_h3(text = "Initial Conditions")
                eqs <- func_to_char(self$arg_list$ini) #function in PMutitlities
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
            #' @param algorithm The algorithm to use for the run.  Default is "NPAG". Alternatives: "NPOD".
            #' @param report If missing, the default Pmetrics report template as specified in [getPMoptions]
            #' is used. Otherwise can be "plotly", "ggplot", or "none".
            #' @return A successful run will result in creation of a new folder in the working
            #' directory with the results inside the folder.
            #'
            #' @author Michael Neely
            #' @export
            fit = function(data = NULL,
              run = NULL,
              include = NULL,
              exclude = NULL,
              cycles = 100,
              prior = "sobol",
              density0 = 0.01,
              idelta = 0.1,
              tad = 0,
              seed = 23,
              overwrite = FALSE,
              algorithm = "NPAG", #POSTPROB for posteriors
              report = getPMoptions("report_template")) {
                
                msg <- "" # status message at end of run
                
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
                  data <- tryCatch({
                    PM_data$new(data)
                  }, error = function(e) {
                    cli::cli_abort(
                      c("x" = "{.code data} must be a {.cls PM_data} object or an appropriate data frame.", "i" = "See help for {.fn Pmetrics::PM_data}.")
                    )
                  })
                }
                
                #### checks
                
                # covariates
                dataCov <- tolower(getCov(data)$covnames)
                modelCov <- tolower(self$model_list$covariates)
                if (length(modelCov) == 0) {
                  modelCov <- NA
                }
                if (!all(is.na(dataCov)) &&
                !all(is.na(modelCov))) {
                  # if there are covariates
                  if (!identical(sort(dataCov), sort(modelCov))) {
                    # if not identical, abort
                    msg <- glue::glue(
                      "Model covariates: {paste(modelCov, collapse = ', ')}; Data covariates: {paste(dataCov, collapse = ', ')}"
                    )
                    cli::cli_abort(c("x" = "Error: Covariates in data and model do not match.", "i" = msg))
                  }
                }
                
                # output equations
                
                if (!is.null(data$standard_data$outeq)) {
                  dataOut <- max(data$standard_data$outeq, na.rm = TRUE)
                } else {
                  dataOut <- 1
                }
                
                modelOut <- self$model_list$n_out
                # if (dataOut != modelOut) {
                #   cli::cli_abort(
                #     c("x" = "Number of output equations in data and model do not match.", "i" = "Check the number of output equations in the data and model.")
                #   )
                # }
                
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
                    msg <- c(msg, "The previous run in folder '{newdir}' was overwritten.")
                    #cli::cli_inform(c("i" = "Overwriting the previous run in folder '{newdir}'."))
                  } else {
                    cli::cli_inform(
                      c("i" = "The previous run from '{newdir}' was read.", " " = "Set {.arg overwrite} to {.val TRUE} to overwrite prior run in '{newdir}'.")
                    )
                    return(invisible(PM_load(newdir)))
                  }
                }
                
                dir.create(newdir)
                setwd(newdir)
                
                algorithm <- tolower(algorithm)
                
                if (getPMoptions()$backend != "rust") {
                  setwd(cwd)
                  cli::cli_abort(c("x" = "Error: unsupported backend.", "i" = "See help for {.fn setPMoptions}"))
                }
                
                #### Include or exclude subjects ####
                if (is.null(include)) {
                  include <- unique(data$standard_data$id)
                }
                if (is.null(exclude)) {
                  exclude <- NA
                }
                data_filtered <- data$standard_data %>% includeExclude(include, exclude)
                
                if (nrow(data_filtered) == 0) {
                  cli::cli_abort("x" = "No subjects remain after filtering.")
                  setwd(cwd)
                  return(invisible(NULL))
                }
                
                
                #### Save objects ####
                PM_data$new(data_filtered, quiet = TRUE)$save("gendata.csv", header = FALSE)
                #save(self, file = "fit.Rdata")
                this_fit <- PM_fit$new(data = data, model = self)
                save(this_fit, file = "fit.Rdata")
                
                # Get ranges and calculate points
                ranges <- lapply(self$model_list$pri, function(x) {
                  c(x$min, x$max)
                })
                
                names(ranges) <- tolower(names(ranges))
                
                # Set initial grid points (only applies for sobol)
                vol <- prod(sapply(ranges, function(x) {
                  x[2] - x[1]
                }))
                points <- max(ceiling(density0 * vol), 100) # at least 100 points
                
                
                
                # set prior
                if (prior != "sobol") {
                  if (is.numeric(prior)) {
                    # prior specified as a run number
                    if (!file.exists(glue::glue("../{prior}/outputs/theta.csv"))) {
                      cli::cli_abort(c("x" = "Error: {.arg prior} file does not exist.", "i" = "Check the file path."))
                    }
                    file.copy(glue::glue("../{prior}/outputs/theta.csv"), "prior.csv", overwrite = TRUE)
                    prior <- "prior.csv"
                  } else if (is.character(prior)) {
                    # prior specified as a filename
                    if (!file.exists(prior)) {
                      cli::cli_abort(c("x" = "Error: {.arg prior} file does not exist.", "i" = "Check the file path."))
                    }
                    file.copy(prior, "prior.csv", overwrite = TRUE) # ensure in current working directory
                  } else if (is.data.frame(piror)){
                    # prior specified as a data frame
                    if (!all(c("prob", self$model_list$parameters) %in% names(prior))) {
                      cli::cli_abort(c("x" = "Error: {.arg prior} data frame must contain columns for parameters and probabilities.", "i" = "Check the data frame."))
                    }
                    prior <- prior %>% dplyr::select(all_of(self$model_list$parameters), prob)
                    write.csv(prior, "prior.csv", row.names = FALSE)
                    
                  } else {
                    cli::cli_abort(
                      c("x" = "Error: {.arg prior} must be a numeric run number or character filename.", "i" = "Check the value.")
                    )
                  }
                } else {
                  prior <- "sobol"
                }
                
                # get BLQ
                if (is.null(data$blq)) {
                  blq <- rep(NA, dataOut)
                } else {
                  if (length(data$blq) != dataOut) {
                    cli::cli_abort(c("x" = "Error: Number of BLQ values does not match number of output equations.", "i" = "Check the BLQ values."))
                  }
                  blq <- data$blq
                }

                cat("data$blq: ",data$blq, "\n")
                cat("BLQ: ",blq, "\n")
                
                if (intern) {
                  ### CALL RUST
                  out_path <- file.path(getwd(), "outputs")
                  msg <- c(msg, "Run results were saved in folder '{.path {out_path}}'")
                  rlang::try_fetch(
                    fit(
                      # defined in extendr-wrappers.R
                      model_path = self$binary_path,
                      data = "gendata.csv",
                      params = list(
                        ranges = ranges, #not important but needed for POSTPROB
                        algorithm = algorithm,
                        error_models = lapply(self$model_list$err, function(x) x$flatten()),
                        idelta = idelta,
                        tad = tad,
                        blq = blq, # BLQ values 
                        max_cycles = cycles, #will be hardcoded in Rust to 0 for POSTPROB
                        prior = prior,
                        points = points, # only relevant for sobol prior
                        seed = seed
                      ),
                      output_path = out_path,
                      kind = tolower(self$model_list$type)
                    ),
                    error = function(e) {
                      cli::cli_warn("Unable to create {.cls PM_result} object", parent = e)
                      setwd(cwd)
                      return(NULL)
                    }
                  )
                  
                  PM_parse("outputs")
                  res <- PM_load(file = "PMout.Rdata")
                  PM_report(res, outfile = "report.html", template = report, quiet = TRUE)
                  msg <- c(msg, "Report generated with {report} template.")
                  if(tolower(algorithm) == "postprob") {this_alg <- "map"} else {this_alg <- "fit"}
                  msg <- c(msg, "If assigned to a variable, e.g. {.code run{newdir} <-}, results of {.fn {this_alg}} are available in {.code run{newdir}}.")
                  setwd(cwd)
                  if(length(msg)>1) {
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
                #browser()
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
                  cli::cli_abort(c("x" = "Please specify a non-uniform prior for posteriors.",
                  " " = "This can be a prior run number or the name of a file with support points."))
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
                
                tryCatch({
                  compile_model(model_path , output_path, private$get_primary(), kind = tolower(self$model_list$type))
                  self$binary_path <- output_path
                }, error = function(e) {
                  cli::cli_abort(
                    c("x" = "Model compilation failed: {e$message}", "i" = "Please check the model file and try again.")
                  )
                })
                file.remove(model_path) # remove temporary model file
              },
              #' @description
              #' Update the model by launching the model editor.
              #' 
              
              update = function() {
                new_mod <- build_model(self, update = TRUE)
                
                self$arg_list <- new_mod
                #self$compile()
              }
              
            ),  # end public list
            private = list(
              R6fromFile = function(file) {
                msg <- ""
                blocks <- parseBlocks(file) # this function is in PMutilities
                # check for reserved variable names
                reserved <- c(
                  "t",
                  "x",
                  "dx",
                  "p",
                  "rateiv",
                  "cov",
                  "y"
                )
                conflict <- c(match(tolower(blocks$primVar), reserved, nomatch = -99), match(tolower(blocks$secVar), reserved, nomatch = -99), match(tolower(blocks$covar), reserved, nomatch = -99))
                nconflict <- sum(conflict != -99)
                if (nconflict > 0) {
                  msg <- paste("\n", paste(paste("'", reserved[conflict[conflict != -99]], "'", sep = ""), collapse = ", "), " ", c("is a", "are")[1 + as.numeric(nconflict > 1)], " reserved ", c("name", "names")[1 + as.numeric(nconflict > 1)], ", regardless of case.\nPlease choose non-reserved parameter/covariate names.\n", sep = "")
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
                    cli::cli_inform(c("i" = "Truncating variables to positive ranges is not required for NPAG/NPOD",
                    " " = "This may be updated as parametric algorithms come online, but will be ignored for now."))
                  } 
                  
                  # find out if constant
                  const_var <- any(grepl("!", x))
                  if (const_var) {
                    x <- gsub("!", "", x)
                    cli::cli_abort(c("x" = "Constants should be defined in the appropriate block, not #PRI."))
                  }
                  
                  values <- as.numeric(x[-1])
                  
                  if (length(x[-1]) == 1) { # fixed
                    cli::cli_abort(c("x" = "Fixed but unknown are no longer supported.",
                    "i" = "If necessary, fit them as random and then use a fixed value in subsequent runs."))
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
                    " " = "Indicate bolus inputs as {.code B[x]} in equations, where {.code x} is the input number."))
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
                      " " = "Equations have been moved to the {.code eqn} element."))
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
                    # otherLines <- (1:n_outputLines)[!(1:n_outputLines) %in% outputLines] # find other lines
                    # if (length(otherLines) > 0) {
                    #   arg_list$sec <- c(arg_list$sec, blocks$output[otherLines]) # append to #sec block
                    # }
                    # output <- blocks$output[outputLines]
                    # remParen <- stringr::str_replace(blocks$output, regex("Y(?:\\[|\\()(\\d+)(?:\\]|\\))", ignore_case = TRUE), "Y\\1")
                    # diffeq <- stringr::str_split(remParen, "\\s*=\\s*")
                    # diffList <- sapply(diffeq, function(x) x[2])
                    # num_out <- length(diffList)
                    
                    arg_list$out <- eval(parse(text = glue::glue("function() {{\n  {paste(blocks$out, collapse = '\n  ')}\n}}")))
                    
                    err <- tolower(gsub("[[:space:]]", "", blocks$error))
                    # process constant gamma/lambda
                    err_type <- c("additive", "proportional")[1+grepl("^g", err[1])]
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
                    
                    if (self$model_list$type %in% c("Analytical", "ODE")){
                      placeholders <- c("eqn", "lag", "fa", "ini", "out", "n_eqn", "n_out")
                      base <- paste0("equation::", 
                      self$model_list$type, 
                      "::new(\n", 
                      paste("<", placeholders[1:5], ">", sep = "", collapse = ",\n "),
                      ",\n (",
                      paste("<", placeholders[6:7], ">", sep = "", collapse = ", "),
                      "),\n)")
                      
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
                    #self$content <- readChar(model_filename, file.info(model_filename)$size)
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
                  print = function(){
                    if (self$fixed) {
                      cli::cli_text("{.strong {tools::toTitleCase(self$type)}}, with fixed value of {.emph {self$initial}} and coefficients {.emph {paste(self$coeff, collapse = ', ')}}.")
                    } else {
                      cli::cli_text("{.strong {tools::toTitleCase(self$type)}}, with initial value of {.emph {self$initial}} and coefficients {.emph {paste(self$coeff, collapse = ', ')}}.")
                    }
                  },
                  
                  flatten = function(){
                    list(initial = self$initial, coeff = self$coeff, type = self$type, fixed = self$fixed)
                  }
                )
              )
              
              #' @title Primary parameter values
              #' @description
              #' `r lifecycle::badge("experimental")`
              #' Define primary model parameter object.
              #' This is used internally by the `PM_model` class.
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
                    self$min = min
                    self$max = max
                    self$mean = (min + max) / 2
                    self$sd = (max - min) / 6
                  },
                  #' @description
                  #' Print the range.
                  print = function(){
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
                if(min< 0) {
                  cli::cli_warn(c("i" = "Negative minimum value for primary parameter range.",
                  " " = "This may not be appropriate for your model."))
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
              #' cov = c(
              #'   wt = interp() # same as interp("lm") or interp("linear")
              #'   visit = interp("none")
              #' )
              #' }
              #' @export
              interp <- function(type = "lm") {
                if (!type %in% c("lm", "linear", "none")) {
                  cli::cli_abort(c("x" = "{type} is not a valid covariate interpolation type.", 
                  "i" = "See help for {.help PM_model()}."))
                }
                if (type %in% c("lm", "linear")) {
                  return(1)
                } else {
                  return(0)
                }
              }
              
              #' @title List of Pmetrics model library model names
              #' @description
              #' `r lifecycle::badge("stable")`
              #' Returns a list of names of all Pmetrics model library objects in the global environment.
              #' @export
              mod_lib_names <- function(){
                ls(envir = .GlobalEnv) %>% purrr::map(\(x) x[inherits(get(x), "PM_lib")]) %>% purrr::discard(\(x) length(x) == 0) 
              }
              
              # returns model from detected template, 0 if none, and -1 if more than one
              get_found_model <- function(fun){
                eqns <- as.list(body(fun)[-1])
                found <- map(eqns, \(x) deparse(x) %in% mod_lib_names()) %>% unlist()
                
                if(sum(found) > 1){
                  cli::cli_inform(c(
                    "x" = "Multiple library model templates detected",
                    "i" = "Maximum of one library model template allowed."
                  ))
                  return(-1)
                } 
                
                if(sum(found) == 0){
                  return(0)
                }
                
                found_model_name <- eqns[[which(found)]] %>% deparse()
                if(length(found_model_name)>0) {
                  found_model <- get(found_model_name)
                } else {
                  found_model <- 0
                }
                return(found_model)
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
                      ))
                    } else {
                      FALSE
                    }
                    line <- if (is.list(line) || line) {
                      amendLine(line, default = list(color = "black"))
                    } else {
                      FALSE
                    }
                    
                    if(inherits(model, "PM_lib")){
                      eqns <- func_to_char(model$arg_list$eqn)
                    } else if(inherits(model, "PM_model")){
                      eqns <- if(model$model_list$name == "user") { 
                        func_to_char(model$arg_list$eqn) 
                      } else {  
                        func_to_char(get(model$model_list$name)$arg_list$eqn)
                      }
                      
                    } else {
                      cli::cli_abort(c(
                        "x" = "Unknown model type to plot."
                      ))
                    }
                    
                    # filter any equations that are not diffeq and make everything capital
                    #this_model <- model$model_list$eqn %>%
                    
                    this_model <- eqns %>%
                    map(
                      purrr::keep,
                      stringr::str_detect,
                      stringr::regex("dX\\[\\d+\\]|XP\\(\\d+\\)", ignore_case = TRUE)
                    ) %>%
                    unlist()
                    
                    tree <- parse(text = this_model)
                    if (length(tree) == 0) {
                      cli::cli_abort(
                        c("x" = "No differential equations detected. Use {.code dX[i]} for changes and {.code X[i]} for amounts (case insensitive).")
                      )
                    }
                    index <- 0
                    
                    parse_arrows <- function(tree, arrows = list()) {
                      if (length(tree) == 3) {
                        op <- tree[[1]]
                        lhs <- tree[[2]]
                        rhs <- tree[[3]]
                      } else if (length(tree[[1]]) == 3) {
                        op <- tree[[1]][[1]]
                        lhs <- tree[[1]][[2]]
                        rhs <- tree[[1]][[3]]
                      } else {
                        return(arrows)
                      }
                      
                      # check for distributions
                      if (length(lhs) > 1 && lhs[[1]] == "(") {
                        # expand distribution
                        nterms <- length(lhs[[2]])
                        lhs <- parse(text = paste(
                          sapply(2:nterms, function(x) {
                            as.character(lhs[[2]][[x]])
                          }),
                          as.character(op),
                          deparse(rhs),
                          collapse = paste0(" ", as.character(lhs[[2]][[1]]), " ")
                        ))[[1]]
                        rhs <- ""
                      }
                      
                      if (length(rhs) > 1 && rhs[[1]] == "(") {
                        # expand distribution
                        nterms <- length(rhs[[2]])
                        rhs <- parse(text = paste(
                          deparse(lhs),
                          as.character(op),
                          sapply(2:nterms, function(x) {
                            as.character(rhs[[2]][[x]])
                          }),
                          collapse = paste0(" ", as.character(rhs[[2]][[1]]), " ")
                        ))[[1]]
                        lhs <- ""
                      }
                      
                      
                      l <- if (length(lhs) == 1) {
                        lhs
                      } else if (lhs[[1]] == "[") {
                        lhs[[2]]
                      } else if (is.call(lhs) & length(lhs) == 3) {
                        lhs[[3]]
                      } else {
                        lhs[[1]]
                      }
                      r <- if (length(rhs) == 1) {
                        rhs
                      } else if (rhs[[1]] == "[") {
                        rhs[[2]]
                      } else if (is.call(rhs) & length(rhs) == 3) {
                        rhs[[3]]
                      } else {
                        rhs[[1]]
                      }
                      
                      
                      if (l == "x" || r == "x" || l == "X" || r == "X") {
                        arrows <- append(arrows, tree)
                        return(arrows)
                      }
                      
                      index <<- index + 1
                      if (is.call(lhs)) {
                        arrows <- parse_arrows(lhs, arrows)
                      }
                      
                      if (is.call(rhs)) {
                        arrows <- parse_arrows(rhs, arrows)
                      }
                      
                      return(arrows)
                    }
                    
                    parse_inputs <- function(input, itree) {
                      itree <- paste(itree, collapse = "")
                      if (grepl(input, itree, ignore.case = TRUE)) {
                        type <- toupper(substr(input, 1, 1))
                        number <- stringr::str_extract(itree, regex(paste0(input, "(\\(|\\[)\\d+(\\)|\\])"), ignore_case = TRUE)) %>%
                        stringr::str_extract("\\d+")
                        return(paste0(type, number))
                      } else {
                        return("")
                      }
                    }
                    
                    # process each compartment/equation
                    parse_tree <- function(tree) {
                      nodes <- list()
                      if (inherits(tree, "expression")) {
                        for (itree in tree) {
                          op <- itree[[1]]
                          lhs <- itree[[2]]
                          rhs <- itree[[3]]
                          if (op == "=") {
                            if (lhs[[1]] == "[") {
                              lhs <- lhs[-1]
                            }
                            nodes <- append(nodes, list(
                              node = list(
                                node = as.character(lhs),
                                arrows = as.character(parse_arrows(rhs)),
                                bolus = parse_inputs("b", deparse(itree)),
                                rateiv = parse_inputs("r", deparse(itree))
                              )
                            ))
                          } else {
                            # only one equation
                            as.character(parse_arrows(tree))
                          }
                        }
                      }
                      return(nodes)
                    }
                    
                    res <- parse_tree(tree)
                    
                    # clean up
                    swap_if_needed <- function(obj) {
                      if (grepl("X\\[", obj[1], ignore.case = TRUE)) {
                        return(paste(obj[2], obj[1], sep = " * "))
                      } else {
                        return(paste(obj[1], obj[2], sep = " * "))
                      }
                    }
                    # clean up
                    
                    # remove hanging arrows without "*"
                    res <- purrr::map(res, function(x) {
                      list(
                        node = x$node,
                        arrows = x$arrows[grepl("\\*", x$arrows)],
                        bolus = x$bolus,
                        rateiv = x$rateiv
                      )
                    }) %>%
                    # ensure unique arrows in each node
                    purrr::map(function(x) {
                      list(
                        node = x$node,
                        arrows = unique(x$arrows),
                        bolus = x$bolus,
                        rateiv = x$rateiv
                      )
                    }) %>%
                    # ensure X terms come second
                    purrr::map(function(x) {
                      list(
                        node = x$node,
                        arrows = unlist(purrr::map(
                          x$arrows, ~ swap_if_needed(stringr::str_split_1(.x, " \\* "))
                        )),
                        bolus = x$bolus,
                        rateiv = x$rateiv
                      )
                    })
                    
                    layout <- res %>%
                    lapply(., function(node) {
                      data.frame(
                        node = paste(node$node, collapse = ""),
                        arrow = node$arrow,
                        bolus = node$bolus,
                        rateiv = node$rateiv
                      )
                    }) %>%
                    bind_rows() %>%
                    dplyr::mutate(from = stringr::str_replace(node, stringr::regex("XP|dX", ignore_case = TRUE), "")) %>%
                    dplyr::mutate(to = stringr::str_extract(string = arrow, pattern = "\\((\\d+)\\)|\\[(\\d+)\\]")) %>%
                    dplyr::mutate(to = stringr::str_remove(to, pattern = "\\(|\\[")) %>%
                    dplyr::mutate(to = stringr::str_remove(to, pattern = "\\)|\\]")) %>%
                    dplyr::mutate(to = ifelse(from == to, "", to)) %>%
                    dplyr::mutate(arrow = stringr::str_remove(string = arrow, pattern = "\\X\\((\\d+)\\)|\\X\\[(\\d+)\\]")) %>%
                    dplyr::mutate(arrow = stringr::str_remove_all(string = arrow, pattern = " ")) %>%
                    dplyr::mutate(arrow = stringr::str_remove_all(string = arrow, pattern = "^\\*|\\*\\w*$")) %>%
                    dplyr::mutate(arrow = stringr::str_remove_all(string = arrow, pattern = "^\\-|\\-\\w*$")) %>%
                    dplyr::relocate(node, arrow, to, from) %>%
                    dplyr::rename(to = from, from = to)
                    
                    # pause to define inputs
                    input_cmt <- layout %>%
                    dplyr::select(to, bolus, rateiv) %>%
                    dplyr::filter(bolus != "" | rateiv != "") %>%
                    dplyr::distinct() %>%
                    tidyr::pivot_longer(c(bolus, rateiv), names_to = "type", values_to = "input") %>%
                    dplyr::select(-type) %>%
                    dplyr::filter(input != "") %>%
                    dplyr::rename(cmt = to)
                    
                    # resume layout
                    layout <- layout %>%
                    dplyr::select(-bolus, -rateiv) %>%
                    dplyr::group_by(arrow) %>%
                    dplyr::filter(dplyr::n() == 1 | dplyr::n() > 1 & from != "") %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(from = ifelse(from == "", to, from)) %>%
                    dplyr::mutate(to = ifelse(from == to, "", to)) %>%
                    dplyr::mutate(to = ifelse(to == "", as.numeric(max(
                      c(to, from), na.rm = T
                    )) + 1, to)) %>%
                    dplyr::mutate(to = ifelse(is.na(to), as.numeric(max(
                      c(to, from), na.rm = T
                    )) + 1, to)) %>%
                    dplyr::filter(arrow != "") %>%
                    dplyr::mutate(across(everything(), as.character)) %>%
                    dplyr::mutate(node = stringr::str_extract(node, "\\d+")) %>%
                    dplyr::distinct(node, from, to) %>%
                    dplyr::mutate(implicit = FALSE)
                    
                    # outputs
                    if (inherits(model, c("PM_model", "PM_lib"))) {
                      cmts <- map_chr(func_to_char(model$arg_list$out), \(x) stringr::str_extract(x, "X\\[(\\d+)\\]", group = 1))
                      output_cmt <- dplyr::tibble(out = paste0("Y", seq_along(cmts)), cmt = cmts)
                    } else {
                      output_cmt <- dplyr::tibble(out = "", cmt = "1")
                    }
                    
                    # add explicit arrows from user
                    if (!missing(explicit)) {
                      max_to <- max(as.numeric(layout$to))
                      
                      if (!all(names(explicit) %in% c("from", "to"))) {
                        cli::cli_abort(
                          c("x" = "{.code explicit} should be a data frame with names {.code from} and {.code to}")
                        )
                      }
                      imp <- explicit %>%
                      dplyr::mutate(to = ifelse(to == 0, max_to, to)) %>%
                      dplyr::mutate(node = from, implicit = FALSE) %>%
                      dplyr::relocate(node, from, to, implicit) %>%
                      dplyr::mutate(across(c(node, from, to), as.character))
                      
                      layout <- dplyr::bind_rows(layout, imp)
                    }
                    
                    # add implicit arrows from user
                    if (!missing(implicit)) {
                      max_to <- max(as.numeric(layout$to))
                      
                      if (!all(names(implicit) %in% c("from", "to"))) {
                        cli::cli_abort(
                          c("x" = "{.code implicit} should be a data frame with names {.code from} and {.code to}")
                        )
                      }
                      imp <- implicit %>%
                      dplyr::mutate(to = ifelse(to == 0, max_to, to)) %>%
                      dplyr::mutate(node = from, implicit = TRUE) %>%
                      dplyr::relocate(node, from, to, implicit) %>%
                      dplyr::mutate(across(c(node, from, to), as.character))
                      
                      layout <- dplyr::bind_rows(layout, imp)
                    }
                    
                    
                    graph <- tidygraph::as_tbl_graph(layout) %>%
                    dplyr::mutate(cmt = c(unique(layout$from), 0)) %>%
                    dplyr::mutate(position = ifelse(cmt == 0, "outside", "inside")) %>%
                    dplyr::left_join(input_cmt, by = "cmt") %>%
                    dplyr::mutate(input = ifelse(is.na(input), "", input)) %>%
                    dplyr::left_join(output_cmt, by = "cmt") %>%
                    dplyr::mutate(out = ifelse(is.na(out), "", out))
                    
                    
                    
                    
                    g <- ggraph::ggraph(graph, layout = "tree")
                    if (!is.logical(marker)) {
                      # will only be logical if FALSE
                      g <- g +
                      ggraph::geom_node_tile(
                        aes(fill = position, linetype = position),
                        color = marker$line$color,
                        lwd = marker$line$width,
                        width = marker$size,
                        height = marker$size,
                        alpha = marker$opacity
                      ) +
                      ggraph::geom_node_text(
                        aes(label = input),
                        nudge_x = .07,
                        nudge_y = .05,
                        color = "white"
                      ) +
                      ggraph::geom_node_text(
                        aes(label = out),
                        nudge_x = -.07,
                        nudge_y = .05,
                        color = "black"
                      ) +
                      ggplot2::scale_fill_manual(values = c(marker$color, "grey80"))
                    }
                    if (!is.logical(line)) {
                      # will only be logical if FALSE
                      g <- g +
                      ggraph::geom_edge_fan(
                        aes(linetype = as.numeric(implicit) + 1),
                        arrow = grid::arrow(
                          angle = 15,
                          type = "closed",
                          length = grid::unit(6, "mm")
                        ),
                        end_cap = ggraph::circle(3, "mm"),
                        start_cap = ggraph::circle(4, "mm"),
                        angle_calc = "across",
                        edge_color = line$color,
                        label_push = grid::unit(-4, "mm"),
                        edge_width = line$width
                      )
                    }
                    g <- g +
                    ggraph::geom_node_label(aes(label = cmt), position = "identity") +
                    ggraph::theme_graph() +
                    ggplot2::theme(legend.position = "none")
                    if (print) print(g)
                    return(invisible(g))
                  }
                  
                  
                  # MODEL LIBRARY -----------------------------------------------------------
                  
                  #' @title Pharmacokinetic model library
                  #' @description
                  #' `r lifecycle::badge("stable")`
                  #'
                  #' This function provides a list of available pharmacokinetic models.
                  #' @param name The name of the model to display. If `NULL`, the entire list is displayed.
                  #' @param show If `TRUE`, the model is displayed in the console. If `FALSE`, the model is only returned as a tibble.
                  #' @return If `name` is not `NULL`, a tibble with the model equations; otherwise the
                  #' function returns `NULL` and only displays the entire library in tabular format.
                  #' @author Michael Neely
                  #' @seealso [PM_model]
                  #' @export
                  #' @examples
                  #' \dontrun{
                  #' model_lib()
                  #' model_lib("one_comp_iv")
                  #' }
                  model_lib <- function(name = NULL, show = TRUE) {
                    mod_table <- matrix(
                      c(
                        "one_comp_iv",
                        "advan1\nadvan1-trans1",
                        "One compartment IV input, Ke",
                        "1 = Central",
                        "Ke, V",
                        "one_comp_iv_cl",
                        "advan1\nadvan1-trans2",
                        "One compartment IV input, CL",
                        "1 = Central",
                        "CL, V",
                        "two_comp_bolus",
                        "advan2\nadvan2-trans1",
                        "Two compartment bolus input, Ke",
                        "1 = Bolus\n2 = Central",
                        "Ka, Ke, V",
                        "two_comp_bolus_cl",
                        "advan2\nadvan2-trans2",
                        "Two compartment bolus input, CL",
                        "1 = Bolus\n2 = Central",
                        "Ka, CL, V",
                        "two_comp_iv",
                        "advan3\nadvan3-trans1",
                        "Two compartment IV input, Ke",
                        "1 = Central\n2 = Peripheral",
                        "Ke, V, KCP, KPC",
                        "two_comp_iv_cl",
                        "advan3\nadvan3-trans4",
                        "Two compartment IV input, CL",
                        "1 = Central\n2 = Peripheral",
                        "CL, V1, Q, V2",
                        "three_comp_bolus",
                        "advan4\nadvan4-trans1",
                        "Three compartment bolus input, Ke",
                        "1 = Bolus\n2 = Central\n3 = Peripheral",
                        "Ka, Ke, V, KCP, KPC",
                        "three_comp_bolus_cl",
                        "advan4\nadvan4-trans4",
                        "Three compartment bolus input, CL",
                        "1 = Bolus\n2 = Central\n3 = Peripheral",
                        "Ka, CL, V2, Q, V3"
                      ),
                      ncol = 5,
                      byrow = TRUE
                    ) %>%
                    as.data.frame() %>%
                    stats::setNames(c(
                      "Primary Name",
                      "Alt Names",
                      "Description",
                      "Compartments",
                      "Parameters"
                    )) %>%
                    tibble::as_tibble()
                    
                    
                    mod_table$ODE <- list(
                      list("dX[1] = RATEIV[1] - Ke*X[1]"),
                      list("dX[1] = RATEIV[1] - CL/V*X[1]"),
                      list(
                        "dX[1] = BOLUS[1] - Ka*X[1]",
                        "dX[2] = RATEIV[1] + Ka*X[1] - Ke*X[2]"
                      ),
                      list(
                        "dX[1] = BOLUS[1] - Ka*X[1]",
                        "dX[2] = RATEIV[1] + Ka*X[1] - CL/V*X[2]"
                      ),
                      list(
                        "dX[1] = RATEIV[1] - (Ke + KCP)*X[1] + KPC*X[2]",
                        "dX[2] = KCP*X[1] - KPC*X[2]"
                      ),
                      list(
                        "dX[1] = RATEIV[1] - (CL + Q)/V1*X[1] + Q/V2*X[2]",
                        "dX[2] = Q/V1*X[1] - Q/V2*X[2]"
                      ),
                      list(
                        "dX[1] = BOLUS[1] - Ka*X[1]",
                        "dX[2] = RATEIV[1] + Ka*X[1] - (Ke + KCP)*X[2] + KPC*X[3]",
                        "dX[3] = KCP*X[2] - KPC*X[3]"
                      ),
                      list(
                        "dX[1] = BOLUS[1] - Ka*X[1]",
                        "dX[2] = RATEIV[1] + Ka*X[1] - (CL + Q)/V2*X[2] + Q/V3*X[3]",
                        "dX[3] = Q/V2*X[2] - Q/V3*X[3]"
                      )
                    )
                    
                    
                    if (is.null(name)) {
                      print(
                        mod_table %>%
                        dplyr::rowwise() %>%
                        dplyr::mutate(ODE = paste(unlist(ODE), collapse = "\n")) %>%
                        flextable::flextable() %>%
                        flextable::set_header_labels(ODE = "Corresponding ODE") %>%
                        flextable::autofit()
                      )
                      
                      return(invisible(NULL))
                    }
                    
                    if (!tolower(name) %in%
                    c(
                      glue::glue("one_comp_iv{c('','_cl')}"),
                      glue::glue("two_comp_bolus{c('','_cl')}"),
                      glue::glue("two_comp_iv{c('','_cl')}"),
                      glue::glue("three_comp_bolus{c('','_cl')}"),
                      glue::glue("advan{1:4}"),
                      glue::glue("advan{1:4}-trans1"),
                      glue::glue("advan{1:2}-trans2"),
                      "advan3-trans4",
                      "advan4-trans4"
                    )) {
                      cli::cli_abort(c("x" = "Invalid model name"))
                    }
                    
                    if (show) {
                      print(
                        mod_table %>% dplyr::filter(`Primary Name` == name) %>%
                        dplyr::mutate(ODE = paste(unlist(ODE), collapse = "\n")) %>%
                        flextable::flextable() %>%
                        flextable::set_header_labels(ODE = "Corresponding ODE") %>%
                        flextable::autofit()
                      )
                    }
                    
                    return(
                      invisible(
                        mod_table %>% dplyr::filter(`Primary Name` == name) %>% dplyr::select(ODE) %>% purrr::pluck(1, 1) %>% unlist()
                      )
                    )
                  }
                  