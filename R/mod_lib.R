model_lib <- function(name = NULL, show = TRUE) {
  
  mod_table <- tibble(
    Name = map_chr(mod_list, \(x) x$name),
    alt_names = map_chr(mod_list, \(x) paste(x$alt_names, collapse = "\n")),
    Description = map_chr(mod_list, \(x) paste(x$description, collapse = "\n")),
    Compartments = map_chr(mod_list, \(x) paste(x$compartments, collapse = "\n")),
    Parameters = map_chr(mod_list, \(x) paste(names(x$arg_list$pri), collapse = ", ")),
    ode = map_chr(mod_list, \(x) paste(func_to_char(x$arg_list$eqn), collapse = "\n"))
  )
  
  if (is.null(name)) {
    print(mod_table %>%
            dplyr::rowwise() %>%
            flextable::flextable() %>%
            flextable::set_header_labels(alt_names = "Alt Names", ode = "Corresponding ODE") %>%
            flextable::autofit())
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
      )
      
  ) {
    cli::cli_abort(c("x" = "Invalid model name"))
  }
  
  if (show) {
    print(mod_table %>% dplyr::filter(Name == name) %>%
            flextable::flextable() %>%
            flextable::set_header_labels(alt_names = "Alt Names", ode = "Corresponding ODE") %>%
            flextable::autofit())
  }
  
  return(invisible(mod_table %>% dplyr::filter(Name == name) %>% dplyr::select(ode) %>% stringr::str_split("\n") %>% unlist()))
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
      cli::cli_text("{.tip Use this name in the {.code TEM} block}")
      cli::cli_text(self$name)
      cli::cli_h3("Alternative Names")
      cli::cli_text("{.tip These names may also be used in the {.code TEM} block}")
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

mod_list <- list(
  
  list(
    name = "one_comp_iv", 
    alt_names = c("advan1", "advan1-trans1"), 
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
    alt_names = c("advan1", "advan1-trans2"), 
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
    alt_names = c("advan2", "advan2-trans1"), 
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
    alt_names = c("advan2", "advan2-trans2"), 
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
    alt_names = c("advan3", "advan3-trans1"),
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
    alt_names = c("advan3", "advan3-trans4"),
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
    alt_names = c("advan4", "advan4-trans1"),
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
    alt_names = c("advan4", "advan4-trans4"),
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

purrr::walk(mod_list, \(x) {
  PM_lib$new(x) -> mod
  assign(tolower(mod$name), mod, envir = .GlobalEnv)
})
