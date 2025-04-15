
model_lib <- function(name = NULL, show = TRUE){
  
  
  mod_table <- matrix(
    c(
      
      "one_comp_iv", "advan1\nadvan1-trans1", "One compartment IV input, Ke", "1 = Central", "Ke, V",
      "one_comp_iv_cl", "advan1\nadvan1-trans2", "One compartment IV input, CL", "1 = Central", "CL, V",
      
      "two_comp_bolus", "advan2\nadvan2-trans1", "Two compartment bolus input, Ke", "1 = Bolus\n2 = Central", "Ka, Ke, V",
      "two_comp_bolus_cl", "advan2\nadvan2-trans2", "Two compartment bolus input, CL", "1 = Bolus\n2 = Central", "Ka, CL, V",
      
      "two_comp_iv", "advan3\nadvan3-trans1", "Two compartment IV input, Ke", "1 = Central\n2 = Peripheral", "Ke, V, KCP, KPC",
      "two_comp_iv_cl", "advan3\nadvan3-trans4", "Two compartment IV input, CL", "1 = Central\n2 = Peripheral", "CL, VC, Q, VP",
      
      "three_comp_bolus", "advan4\nadvan4-trans1", "Three compartment bolus input, Ke", "1 = Bolus\n2 = Central\n3 = Peripheral", "Ka, Ke, V, KCP, KPC",
      "three_comp_bolus_cl", "advan4\nadvan4-trans4", "Three compartment bolus input, CL", "1 = Bolus\n2 = Central\n3 = Peripheral", "Ka, CL, VC, Q, VP"
    ),
    ncol = 5, byrow = TRUE
  ) %>%
    as.data.frame() %>%
    stats::setNames(c("Primary Name", "Alt Names", "Description", "Compartments", "Parameters")) %>%
    tibble::as_tibble()
  
  
  mod_table$ODE <- list(
    list(
      "dX[1] = RATEIV[1] - Ke*X[1]"
    ),
    list(
      "dX[1] = RATEIV[1] - CL/V*X[1]"
    ),
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
      "dX[1] = RATEIV[1] - (CL + Q)/VC*X[1] + Q/VP*X[2]",
      "dX[2] = Q/VC*X[1] - Q/VP*X[2]"
    ),
    list(
      "dX[1] = BOLUS[1] - Ka*X[1]",
      "dX[2] = RATEIV[1] + Ka*X[1] - (Ke + KCP)*X[2] + KPC*X[3]",
      "dX[3] = KCP*X[2] - KPC*X[3]"
    ),
    list(
      "dX[1] = BOLUS[1] - Ka*X[1]",
      "dX[2] = RATEIV[1] + Ka*X[1] - (CL + Q)/VC*X[2] + Q/VP*X[3]",
      "dX[3] = Q/VC*X[2] - Q/VP*X[3]"
    )
  )
  
  
  if(is.null(name)){
    print(mod_table %>%
            dplyr::rowwise() %>%
            dplyr::mutate(ODE = paste(unlist(ODE), collapse = "\n")) %>% 
            flextable::flextable() %>% 
            flextable::set_header_labels(ODE = "Corresponding ODE") %>%
            flextable::autofit())
    return(invisible(NULL))
  }
  
  if(! tolower(name) %in% 
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
     
  ){
    cli::cli_abort(c("x" = "Invalid model name"))
  }
  
  if(show){
    print(mod_table %>% dplyr::filter(`Primary Name` == name) %>% 
            dplyr::mutate(ODE = paste(unlist(ODE), collapse = "\n")) %>% 
            flextable::flextable() %>% 
            flextable::set_header_labels(ODE = "Corresponding ODE") %>%
            flextable::autofit())
  }
  
  return(invisible(mod_table %>% dplyr::filter(`Primary Name` == name) %>% dplyr::select(ODE) %>% purrr::pluck(1,1) %>% unlist()  ))
}



