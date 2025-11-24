####### INTERNAL FUNCTIONS ########
# Returns a list of names of all Pmetrics model library objects in the global environment.
mod_lib_names <- function(){
  ls(envir = .GlobalEnv) %>% purrr::map(\(x) x[inherits(get(x), "PM_lib")]) %>% purrr::discard(\(x) length(x) == 0) 
}

# Returns a list of alt names of all Pmetrics model library objects in the global environment.
alt_mod_lib_names <- function(){
  mod_names <- mod_lib_names()
  if (length(mod_names) > 0){
    alt_map <- tibble(primary = unlist(mod_names)) %>% 
    rowwise() %>%
    mutate(alt = list(get(primary)$alt_names)) %>% 
    tidyr::unnest_longer(alt)
    return(alt_map)
  } else {
    return(0)
  }
}

# returns model from detected template, 0 if none, and -1 if more than one
get_found_model <- function(fun){
  eqns <- as.list(body(fun)[-1])
  found_pri <- map(eqns, \(x) deparse(x) %in% mod_lib_names()) %>% unlist()
  found_alt <- map(eqns, \(x) deparse(x) %in% alt_mod_lib_names()$alt) %>% unlist()
  all_found <- sum(found_pri, found_alt)
  
  if(all_found > 1){
    cli::cli_inform(c(
      "x" = "Multiple library model templates detected",
      "i" = "Maximum of one library model template allowed."
    ))
    return(-1)
  } 
  
  if(all_found == 0){
    return(0)
  }
  
  if (any(found_pri)) { # found a primary model name
    found_model_name <- eqns[[which(found_pri)]] %>% deparse()
  } else { # found an alternative model name
    found_model_name <- alt_mod_lib_names() %>%
    filter(alt == eqns[[which(found_alt)]] %>% deparse()) %>%
    pull(primary)
  }
  
  if(length(found_model_name)>0) {
    found_model <- get(found_model_name)
  } else {
    found_model <- 0
  }
  return(found_model)
}

# assemble library and load into global environment
build_model_lib <- function(){
  purrr::walk(mod_list, \(x) {
    PM_lib$new(x) -> mod
    assign(tolower(mod$name), mod, envir = .GlobalEnv)
  })
}

####################################

#' @title Model Library
#' 
#' @description
#' Returns a table of all available model templates in the Pmetrics library.
#' @param show Logical indicating if the table should be printed. Default is TRUE.
#' @return Invisibly, a tibble containing the model templates or a specific model template.
#' @export
#' 
model_lib <- function(show = TRUE) {
  
  mod_table <- tibble(
    Name = map_chr(mod_list, \(x) x$name),
    alt_names = map_chr(mod_list, \(x) paste(x$alt_names, collapse = "\n")),
    Description = map_chr(mod_list, \(x) paste(x$description, collapse = "\n")),
    Compartments = map_chr(mod_list, \(x) paste(x$compartments, collapse = "\n")),
    Parameters = map_chr(mod_list, \(x) paste(names(x$arg_list$pri), collapse = ", ")),
    ode = map_chr(mod_list, \(x) paste(func_to_char(x$arg_list$eqn), collapse = "\n"))
  )
  
  if(show) {
    print(mod_table %>%
      dplyr::rowwise() %>%
      flextable::flextable() %>%
      flextable::set_header_labels(alt_names = "Alt Names", ode = "Corresponding ODE") %>%
      flextable::autofit())
      
      cli::cli_h1("Model Library Notes")
      ul <- cli::cli_ul()
      cli::cli_li("Use the {.code PM_model$new()} function to create a model from a template in the library.")
      cli::cli_li("Include the unquoted model {.emph Name} or one of the {.emph Alt Names} in the {.code EQN} block of the model")
      cli::cli_li("Ensure the model {.emph Parameters} are defined as named (case-insensitive) in the appropriate model blocks, e.g. {.code PRI}, {.code SEC}, {.code EQN}, or {.code OUT}.")
      cli::cli_li("Additional parameters and blocks, e.g. lag, bioavailaiblity, initial conditions, and covariates may be added.")
      cli::cli_li("The model {.emph Compartments} are numbered and can be referenced in the {.code OUT} block to define outputs.")
      cli::cli_li("Model inputs are indicated in the {.emph Corresponding ODE}, i.e. bolus (B[1]) and infusion (R[1]). These cannot be changed.")
      cli::cli_li("{.emph ODE} are used only for plotting purposes.")
      cli::cli_end(ul)
    }
    
    
    return(invisible(mod_table))
  }
  
  
  
  #Primary Name", "Alt Names", "Description", "Compartments", "Parameters"
  
  
  
  PM_lib <- R6::R6Class(
    "PM_lib",
    public = list(
      #' @field name Name of the model template
      name = NULL,
      #' @field alt_names Alterantive names for the model template
      alt_names = NULL,
      #' @field description Brief description of the model template
      description = NULL,
      #' @field compartments Names for compartment numbers used in the model template
      compartments = NULL,
      #' @field analytical Boolean indicating if the model is fitted analytically
      analytical = NULL,
      #' @field arg_list List of model blocks with token values
      arg_list = NULL,
      
      initialize = function(x){
        self$name <- x[[1]]
        self$alt_names <- x[[2]]
        self$description <- x[[3]]
        self$compartments <- x[[4]]
        self$analytical <- x[[5]]
        self$arg_list <- x[[6]]
      },
      print = function(){
        cli::cli_div(theme = list(
          span.tip = list(color = "dodgerblue", "font-style" = "italic")))
          cli::cli_h1("Model summary")
          cli::cli_h3("Primary Name")
          cli::cli_text("{.tip Use this name in the {.code EQN} block}")
          cli::cli_text(self$name)
          cli::cli_h3("Alternative Names")
          cli::cli_text("{.tip These names may also be used in the {.code EQN} block}")
          purrr::walk(self$alt_names, cli::cli_text)
          cli::cli_h3("Description")
          purrr::walk(self$description, \(x) cli::cli_bullets(c(">" = "{x}")))
          cli::cli_h3("Compartments")      
          cli::cli_text("{.tip Any compartment can have outputs defined in the {.code OUT} block}")
          purrr::walk(self$compartments, cli::cli_text)
          cli::cli_h3("Parameters")
          cli::cli_text("{.tip Ensure these exact names appear in any of the {.code PRI}, {.code SEC}, {.code EQN}, or {.code OUT} blocks}")
          purrr::walk(self$parameters, cli::cli_text)
          cli::cli_h3("Equations")
          if(self$analytical){
            cli::cli_text("{.tip These are shown to describe the model structure, but the model can be fitted analytically, without a differential equation solver}")
          } else {
            cli::cli_text("{.tip These describe the model structure, and the model is fitted with a differential equation solver}")
          }
          purrr::walk(func_to_char(self$arg_list$eqn), cli::cli_text)
          cli::cli_end()
        },
        plot = function(...){
          plot.PM_model(self,...)
        }
      ), # end public
      active = list(
        #' @field ncomp Number of compartments in the model
        ncomp = function(){
          length(self$compartments)
        },
        #' @field bolus Logical indicating if the model has a bolus input
        bolus = function(){
          grepl("bolus", self$name, ignore.case = TRUE)
        },
        #' @field parameters Names of the primary parameters in the model
        parameters = function(){
          names(self$arg_list$pri)
        }
      ) # end active
    ) # end PM_lib class definition
    
    
##### default models in the library
    mod_list <- list(
      
      list(
        name = "one_comp_iv", 
        alt_names = c("advan1","advan1_trans1"), 
        description = c("One compartment", "Infusion into central compartment #1", "Ke elimination from central compartment #1"), 
        compartments = "1 = Central",
        analytical = TRUE,
        arg_list = list(
          pri = c(
            Ke = ab(0,5),
            V = ab(0,100)
          ),
          eqn = function(){
            dX[1] = R[1] - Ke*X[1]
          },
          out = function(){
            Y[1] = X[1]/V
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      list(
        name = "one_comp_iv_cl", 
        alt_names = "advan1_trans2", 
        description = c("One compartment", "Infusion into central compartment #1", "CLearance from central compartment #1"), 
        compartments = "1 = Central", 
        analytical = TRUE,
        arg_list = list(
          pri = c(
            CL = ab(0, 500),
            V = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = R[1] - CL/V*X[1]
          },
          out = function(){
            Y[1] = X[1]/V
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      list(
        name = "two_comp_bolus", 
        alt_names = c("advan2", "advan2_trans1"), 
        description = c("Two compartments", "Bolus input to compartment #1, infusion to central compartment #2", "Ke elimination from central compartment #2"), 
        compartments = c("1 = Bolus", "2 = Central"), 
        analytical = TRUE,
        arg_list = list(
          pri = c(
            Ka = ab(0, 5),
            Ke = ab(0, 5),
            V = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = B[1] - Ka*X[1]
            dX[2] = R[1] + Ka*X[1] - Ke*X[2]
          },
          out = function(){
            Y[1] = X[2]/V
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      
      list(
        name = "two_comp_bolus_cl", 
        alt_names = "advan2_trans2", 
        description = c("Two compartments", "Bolus input to compartment #1, infusion to central compartment #2", "CLearance from central compartment #2"), 
        compartments = c("1 = Bolus", "2 = Central"), 
        analytical = TRUE,
        arg_list = list(
          pri = c(
            Ka = ab(0, 5),
            CL = ab(0, 500),
            V = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = B[1] - Ka*X[1]
            dX[2] = R[1] + Ka*X[1] - CL/V*X[2]
          },
          out = function(){
            Y[1] = X[2]/V
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      
      list(
        name = "two_comp_iv",
        alt_names = c("advan3", "advan3_trans1"),
        description = c("Two compartments", "Infusion into central compartment #1", "Distribution to/from peripheral compartment #2", "Ke elimination from central compartment #1"),
        compartments = c("1 = Central", "2 = Peripheral"),
        analytical = TRUE,
        arg_list = list(
          pri = c(
            Ke = ab(0, 5),
            Kcp = ab(0, 5),
            Kpc = ab(0, 5),
            V = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = R[1] - (Ke + Kcp)*X[1] + Kpc*X[2]
            dX[2] = Kcp*X[1] - Kpc*X[2]
          },
          out = function(){
            Y[1] = X[1]/V
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      
      list(
        name = "two_comp_iv_cl",
        alt_names = "advan3_trans4",
        description = c("Two compartments", "Infusion into central compartment #1", "Distribution to/from peripheral compartment #2", "CLearance from central compartment #1"),
        compartments = c("1 = Central", "2 = Peripheral"),
        analytical = TRUE,
        arg_list = list(
          pri = c(
            CL = ab(0, 500),
            Q = ab(0, 100),
            Vc = ab(0, 100),
            Vp = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = R[1] - (CL + Q)/Vc*X[1] + Q/Vp*X[2]
            dX[2] = Q/Vc*X[1] - Q/Vp*X[2]
          },
          out = function(){
            Y[1] = X[1]/Vc
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      list(
        name = "three_comp_bolus",
        alt_names = c("advan4", "advan4_trans1"),
        description = c("Three compartments", "Bolus input to compartment #1, infusion into central compartment #2", "Distribution to/from peripheral compartment #3", "Ke elimination from central compartment #2"),
        compartments = c("1 = Bolus", "2 = Central", "3 = Peripheral"),
        analytical = TRUE,
        arg_list = list(
          pri = c(
            Ka = ab(0, 5),
            Ke = ab(0, 5),
            Kcp = ab(0, 5),
            Kpc = ab(0, 5),
            V = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = B[1] - Ka*X[1]
            dX[2] = R[1] + Ka*X[1] - (Ke + Kcp)*X[2] + Kpc*X[3]
            dX[3] = Kcp*X[2] - Kpc*X[3]
          },
          out = function(){
            Y[1] = X[2]/V
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      ),
      
      
      list(
        name = "three_comp_bolus_cl",
        alt_names = "advan4_trans4",
        description = c("Three compartments", "Bolus input to compartment #1, infusion into central compartment #2", "Distribution to/from peripheral compartment #3", "CLearance from central compartment #2"),
        compartments = c("1 = Bolus", "2 = Central", "3 = Peripheral"),
        analytical = TRUE,
        arg_list = list(
          pri = c(
            Ka = ab(0, 5),
            CL = ab(0, 500),
            Q = ab(0, 100),
            Vc = ab(0, 100),
            Vp = ab(0, 100)
          ),
          eqn = function(){
            dX[1] = B[1] - Ka*X[1]
            dX[2] = R[1] + Ka*X[1] - (CL + Q)/Vc*X[2] + Q/Vp*X[3]
            dX[3] = Q/Vc*X[2] - Q/Vp*X[3]
          },
          out = function(){
            Y[1] = X[2]/Vc
          },
          err = c(
            proportional(5, c(0.1, 0.1, 0, 0))
          )
        )
      )
    )
    
    
    