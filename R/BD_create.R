# goal is to export a PM model from a file, and return a list of the model parameters

# This will be the example for the workshop
# 1. create a function to write and save the model to a .json file
# 2. check the model is passing all the checks in BestDose
# 3. check the model is running in BestDose (posterior and future predictions)

# ==================================================================____
# Helper function to perform ODE conversion from model_lib
# ==================================================================____

#' @title Get the ODE from the model library
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' @param model_name A character string for the name of the model in the model library
#' @return A list of the ODE equations for the model

getODEfromLib <- function(model_name) {
  # get model library
  model_lib <- model_lib(show = FALSE)
  # extract the ODE from the model library
  ode <- model_lib |>
    dplyr::filter(Name == model_name) |>
    dplyr::pull(ode) |>
    tolower()
  
  # get the ODE as a character vector, split by line breaks
  vect_ode <- unlist(stringr::str_split(ode, "\n"))

  # get Name
  ode_name <- stringr::str_extract(vect_ode, "(?i)dx\\[[0-9]+\\]")
  ode_name <- tolower(stringr::str_replace_all(ode_name, "\\[|\\]", ""))

  ode_list <- list()
  # convert the ODE to a character string
  for (i in seq_along(vect_ode)) {
    ode_list[[ode_name[i]]] <- vect_ode[i]
  }
  
  return(ode_list)
}



# ==================================================================____
# Function to extract the model into a BestDose file
# ==================================================================____
#' Extract primary from a PM model and format them for BestDose
#' @param PMmodel A PM model object
#' @return A list of priors formatted for BestDose

extractPMPrimary <- function(PMmodel) {
  # extract the priors from the PM model
  priors <- PMmodel$pri
  
  # create a list to store the priors in the format required by BestDose
  priors_list <- list()
  
  # loop through each parameter and extract the min, max, and mean values
  for (param in names(priors)) {
    priors_list[[param]] <- list(
      type = "ab",
      min = priors[[param]]$min,
      max = priors[[param]]$max
    )
  }
  
  return(priors_list)
}



#' Extract covariates from a PM model and format them for BestDose
#' @param PMmodel A PM model object
#' 
#' @return A list of covariates formatted for BestDose
#' 


extractPMCovariates <- function(PMmodel) {
  # early return if covariates are not present in the PM model
  if (is.null(PMmodel$cov)) {
    return(NULL)
  }

  cov <- PMmodel$cov

  # cov names
  cov_names <- names(cov)
  
  cov_list <- list()
  for (i in seq_along(cov)) {
    cov_list[[cov_names[i]]] <- list(
      interp = ifelse(cov[[i]] == 1, "linear", "none")
    )
  }

  return(cov_list)
}


#' @title Extract the secondary parameters from a PM model and format them for BestDose
#' @param PMmodel A PM model object
#' @return A list of secondary parameters formatted for BestDose


extractPMSecondary <- function(PMmodel) {
  if (is.null(PMmodel$sec)) {
    return(NULL)
  }

  sec <- deparse(PMmodel$sec)

  # detect all lines with an assignment operator (= or <-) and trim the white space at the beginning and end of the string
  bloc <- sec[stringr::str_detect(sec, "=|<-")]
  bloc <- stringr::str_trim(bloc)

  # get name for the list
  sec_name <- stringr::str_extract(bloc, "^[a-zA-Z0-9_]+")

  sec_list <- list()
  for (i in seq_along(bloc)) {
    sec_list[[sec_name[i]]] <- bloc[i]
  }

  return(sec_list)
}



#' Extract the equations from a PM model and format them for BestDose
#' @param PMmodel A PM model object
#' @return A list of equations formatted for BestDose


extractPMequation <- function(PMmodel) {

  # equation deparse
  eqn <- stringr::str_trim(deparse(PMmodel$eqn))

  # get library name
  library_name <- model_lib(show = FALSE)$Name

  # check if equation are from the library
  if (any(eqn %in% library_name)) {
    # extract the model name from the equation and remove NA values
    model_name <- stringr::str_extract(eqn, paste0("(?i)(", paste(library_name, collapse = "|"), ")"))
    model_name <- model_name[!is.na(model_name)]
    return(getODEfromLib(model_name))
  }

  
  # capture the derivates equation and trim the white space at the beginning and end of the string
  # note (?i) is for case insensitive search, and we are looking for "dx[1]", "dx[2]", etc. and capture the whole line
  bloc <- eqn[stringr::str_detect(eqn, "(?i)dx\\[[0-9]+\\]")]

  # get name for the list
  eqn_name <- stringr::str_extract(bloc, "dx\\[[0-9]+\\]")
  eqn_name <- stringr::str_replace_all(eqn_name, "\\[|\\]", "")

  eqn_bloc <- list()

  for (i in seq_along(bloc)) {
    eqn_bloc[[eqn_name[i]]] <- bloc[i]
  }

  return(eqn_bloc)
}
#' @title Extract the lag time parameters from a PM model and format them for BestDose
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' @param PMmodel A PM model object
#' @return A list of the lag time parameter if present in the PM model, otherwise NULL
 

extractPMLag <- function(PMmodel) {
  if (is.null(PMmodel$lag)) return(NULL)
  
  # extract lag time parameter and format it for BestDose
  lag <- deparse(PMmodel$lag)

  # extract lag time parameter and trim the white space at the beginning and end of the string
  # case incensitive search, hence the '(?i)' for "lag[1]", "lag[2]", etc. and capture the whole line
  bloc <- lag[stringr::str_detect(lag, "(?i)lag\\[[0-9]+\\]")]
  bloc <- stringr::str_trim(bloc)

  # get name for the list
  lag_name <- stringr::str_extract(bloc, "(?i)lag\\[[0-9]+\\]")
  lag_name <- stringr::str_replace_all(lag_name, "\\[|\\]", "")

  lag_list <- list()
  for (i in seq_along(bloc)) {
    lag_list[[lag_name[i]]] <- bloc[i]
  }

  return(lag_list)
}


#' @title Extract the fa parameters from a PM model and format them for BestDose
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' @param PMmodel A PM model object
#' @return A list of the fa parameter if present in the PM model, otherwise NULL

extractPMFa <- function(PMmodel) {
if (is.null(PMmodel$fa)) return(NULL)


# extract fa parameter and format it for BestDose
  fa <- deparse(PMmodel$fa)

  # extract fa parameter and trim the white space at the beginning and end of the string
  # case incensitive search, hence the '(?i)' for "fa[1]", "fa[2]", etc. and capture the whole line
  bloc <- fa[stringr::str_detect(fa, "(?i)fa\\[[0-9]+\\]")]
  bloc <- stringr::str_trim(bloc)

  # get name for the list
  fa_name <- stringr::str_extract(bloc, "(?i)fa\\[[0-9]+\\]")
  fa_name <- stringr::str_replace_all(fa_name, "\\[|\\]", "")

  fa_list <- list()
  for (i in seq_along(bloc)) {
    fa_list[[fa_name[i]]] <- bloc[i]
  }

  return(fa_list)

}

#' @title Extract the initial parameters from a PM model and format them for BestDose
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' @param PMmodel A PM model object
#' @return A list of the initial parameters if present in the PM model, otherwise NULL

extractPMInitialVal <- function(PMmodel) {
if (is.null(PMmodel$ini)) return(NULL)


ini <- deparse(PMmodel$ini)

  # extract initial value parameter and trim the white space at the beginning and end of the string
  # case insensitive search, hence the '(?i)' for "x[1]", "x[2]", etc. and capture the whole line
  bloc <- ini[stringr::str_detect(ini, "(?i)x\\[[0-9]+\\]")]
  bloc <- stringr::str_trim(bloc)

  # get name for the list
  ini_name <- stringr::str_extract(bloc, "(?i)x\\[[0-9]+\\]")
  ini_name <- stringr::str_replace_all(ini_name, "\\[|\\]", "")

  ini_list <- list()
  for (i in seq_along(bloc)) {
    ini_list[[ini_name[i]]] <- bloc[i]
  }

  return(ini_list)
}


#' @title Extract the output parameters from a PM model and format them for BestDose
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' @param PMmodel A PM model object
#' @return A list of the output parameter if present in the PM model, otherwise NULL


extractPMOuteq <- function(PMmodel) {
  if (is.null(PMmodel$out)) {
    cli::cli_abort(c("x" = "The output block is NULL. Please check your PM model."))
  }

  # deparse the function call
  out <- deparse(PMmodel$out)

  # extract out time parameter and trim the white space at the beginning and end of the string
  # case insensitive search, hence the '(?i)' for "y[1]", "y[2]", etc. and capture the whole line
  bloc <- out[stringr::str_detect(out, "(?i)y\\[[0-9]+\\]")]
  bloc <- stringr::str_trim(bloc)

  # get name for the list
  out_name <- stringr::str_extract(bloc, "(?i)y\\[[0-9]+\\]")
  out_name <- stringr::str_replace_all(out_name, "\\[|\\]", "")

  out_list <- list()
  for (i in seq_along(bloc)) {
    out_list[[out_name[i]]] <- bloc[i]
  }

  return(out_list)
}


#' @title Extract the error parameters from a PM model and format them for BestDose
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' @param PMmodel A PM model object
#' @return A list of the error parameter if present in the PM model, otherwise NULL

extractPMError <- function(PMmodel) {

  if (is.null(PMmodel$err)) {
    cli::cli_abort(c("x" = "The error block is NULL. Please check your PM model."))
  }

  # extract error parameter and format it for BestDose
  err <- PMmodel$err[[1]]
  error_list <- list(
    type = err$type,
    initial_value = err$initial,
    coefficient = data.frame(c0 = err$coeff[1], c1 = err$coeff[2], c2 = err$coeff[3], c3 = err$coeff[4])
  )

  return(error_list)
}

#' @title Extract the output equations from a PM model and format them for BestDose
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param PM_result A PM model object
#' @param covariates the covariates information extracted with `extractPMCovariates` function.
#' @param ode the ordinary differential equations information extracted with `extractPMequation` function.
#' @param drug_name A character string for the drug name to be used in the description
#' 
#' @return A description list for the bestdose model.

createBDdescription <- function(PM_result, covariates = NULL, ode = NULL, drug_name = "Drug") {
  # gather basic elements of the model to be used in BestDose
  PMmodel <- PM_result$model$arg_list
  covariates_list <- NULL
  # retrieve covariates informations based on the covariates block in the PM model, if it exists
  if (!is.null(covariates)) {
    covariates_list <- list(
      number = length(covariates),
      names = names(covariates),
      label = names(covariates),
      units = rep("to update", length(covariates)),
      types = rep("numeric", length(covariates)),
      value = lapply(covariates, function(x) data.frame(min = 0, max = 200, default = 70)),
      description = lapply(covariates, function(x) "Pls update the description as needed")
    )
  }

  # get the number of compartments from the number of ode
  if (!is.null(ode)) {
    compartments <- length(ode)
  } else {
    compartments <- 1
  }

  # create the description block
  description <- list(
    drug = drug_name,
    route = data.frame(route = "IV", compartment = 1),
    name = paste0(drug_name, ".json"),
    pmx_path = NULL,
    version = 1.0,
    compartments = compartments,
    target = "concentration",
    target_unit = "mg/L",
    dose_unit = "mg",
    description = "This is a model imported from a PM model object.",
    reference = "This model was imported from a PM model object.",
    covariates = covariates_list
  )

  return(description)
}

#' @title Create a BestDose model list from a PM model object
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' @param PM_result A PM model result object
#' @param drug_name A character string for the drug name to be used in the description
#' @return A list containing the description, model, and support points formatted for BestDose
#' 
#' @export

createBDmodel <- function(PM_result, drug_name = "Drug") {
  # gather basic elements of the model to be used in BestDose
  PMmodel <- PM_result$model$arg_list
  support_points <- PM_result$final$popPoints
  ode = extractPMequation(PMmodel)

  # create the model part of the file
  model_list <- list(
    primary = extractPMPrimary(PMmodel),
    covariates = extractPMCovariates(PMmodel),
    secondary = extractPMSecondary(PMmodel),
    initial_conditions = extractPMInitialVal(PMmodel),
    fa = extractPMFa(PMmodel),
    lag = extractPMLag(PMmodel),
    equation = ode,
    out = extractPMOuteq(PMmodel),
    error = extractPMError(PMmodel)
  )

  # create the description part of the file
  description <- createBDdescription(PM_result, covariates = model_list$covariates, ode = ode, drug_name = drug_name)

  # create the R object to write the model file
  model_file <- list(
    description = description,
    model = model_list,
    support_point = support_points
  )

  return(model_file)
}
