# Export a Pmetrics fit as a BestDose model file (data/drug_model/<name>.json)

# ==================================================================____
# Helpers
# ==================================================================____

#' @title BestDose block key
#' @description Mirrors `parse_equation_lines()` in model_schema.rs: strip everything that is not
#' alphanumeric and lowercase it, so `dx[1]` -> `dx1`, `Y[2]` -> `y2`, `Ke` -> `ke`.
#'
#' @param x A character vector of left-hand sides
#' @return A character vector of BestDose block keys

bdKey <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

#' @title Drop NULL entries from a list
#' @description BestDose deserializes an absent key to its default, but the `{}` jsonlite writes
#' for a NULL is a hard error on scalar fields such as `compartment$peripheral`.
#'
#' @param x A list
#' @return The list without its NULL elements

dropNulls <- function(x) Filter(Negate(is.null), x)

#' @title Deparse a PM model block, one line per statement
#' @description `deparse()` wraps at 60 characters, which splits a long equation across several
#' elements and silently drops its tail once the lines are filtered by pattern. Deparsing each
#' statement of the block on its own keeps one equation per line whatever its length.
#'
#' @param block A PM model block: a function, a braced expression or a character vector
#' @return A character vector with one trimmed line per statement

deparseLines <- function(block) {
  if (is.character(block)) {
    return(stringr::str_trim(block))
  }

  expr <- if (is.function(block)) body(block) else block
  statements <- if (is.expression(expr)) {
    as.list(expr)
  } else if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    as.list(expr)[-1]
  } else {
    list(expr)
  }

  # a single statement longer than 500 characters still wraps, which no PK equation
  # reaches. Raise the cutoff, or paste the pieces back, if one ever does.
  vapply(
    statements,
    function(statement) {
      paste(
        stringr::str_trim(deparse(statement, width.cutoff = 500L)),
        collapse = " "
      )
    },
    character(1)
  )
}

#' @title Canonical BestDose covariate name
#' @description model_check.rs rejects the aliases listed in its `COVARIATE_NAME_SYNONYMS`, so
#' rename them here rather than shipping a file that fails validation on import.
#'
#' @param x A character vector of covariate names
#' @return The canonical lowercase names

bdCovariateName <- function(x) {
  synonyms <- c(
    wt = "weight",
    tbw = "weight",
    gfr = "crcl",
    egfr = "crcl",
    ccr = "crcl",
    clcr = "crcl",
    dfg = "crcl",
    edfg = "crcl",
    creatinine = "creat",
    scr = "creat",
    ht = "height",
    size = "height",
    imc = "bmi",
    body_mass_index = "bmi",
    sc = "bsa",
    body_surface_area = "bsa",
    sexe = "sex",
    gender = "sex"
  )
  x <- tolower(x)
  unname(ifelse(x %in% names(synonyms), synonyms[x], x))
}


#' @title Extract an indexed block from a PM model
#' @description One parser for every `keyword[N] = ...` block (`ini`, `fa`, `lag`, `y`), replacing
#' the four copy-pasted extractors this file used to carry. Keys follow the BestDose convention
#' (`ini1`, `fa1`, `lag2`, `y1`, ...).
#'
#' @param block A PM model block (function or expression), may be NULL
#' @param keyword The block keyword: "ini", "fa", "lag" or "y"
#' @return A named list of equation lines, or NULL when the block is absent or empty

extractPMBlock <- function(block, keyword) {
  if (is.null(block)) {
    return(NULL)
  }

  # case insensitive, anchored so only the assignment line is captured, not a reference to it
  pattern <- paste0("(?i)^", keyword, "\\s*\\[[0-9]+\\]")
  bloc <- deparseLines(block)
  bloc <- bloc[stringr::str_detect(bloc, pattern)]
  if (length(bloc) == 0) {
    return(NULL)
  }

  block_list <- as.list(bloc)
  names(block_list) <- bdKey(stringr::str_extract(bloc, pattern))

  # replace "<-" by "="
  block_list <- lapply(block_list, function(x) {
    stringr::str_replace(x, "<-", "=")
  })

  # input route replacte "rateiv[n]" or "R[n]" by "r[n]" and "Bolus[n]" or "b[n]" by "B[n]"
  # this should only work on administration
  block_list <- lapply(block_list, function(x) {
    x <- stringr::str_replace_all(
      x,
      "(?i)rateiv\\s*\\[\\s*([0-9]+)\\s*\\]",
      "r[\\1]"
    )
    x <- stringr::str_replace_all(
      x,
      "(?i)R\\s*\\[\\s*([0-9]+)\\s*\\]",
      "r[\\1]"
    )
    x <- stringr::str_replace_all(
      x,
      "(?i)bolus\\s*\\[\\s*([0-9]+)\\s*\\]",
      "B[\\1]"
    )
    x <- stringr::str_replace_all(
      x,
      "(?i)b\\s*\\[\\s*([0-9]+)\\s*\\]",
      "B[\\1]"
    )
    x
  })

  return(block_list)
}

# ==================================================================____
# model block
# ==================================================================____

#' @title Extract primary parameters from a PM model and format them for BestDose
#' @param PMmodel A PM model object
#' @return A named list of `{type, min, max}` priors

extractPMPrimary <- function(PMmodel) {
  priors <- PMmodel$pri
  priors_list <- list()

  for (param in names(priors)) {
    prior <- priors[[param]]
    # BestDose stores both prior flavours in min/max: "ab" = bounds, "msd" = mean/SD
    priors_list[[param]] <- if (!is.null(prior$min)) {
      list(type = "ab", min = prior$min, max = prior$max)
    } else {
      list(type = "msd", min = prior$mean, max = prior$sd)
    }
  }

  return(priors_list)
}


#' @title Extract covariates from a PM model and format them for BestDose
#' @param PMmodel A PM model object
#' @return A named list of `{interp}` settings, or NULL when the model has no covariates

extractPMCovariates <- function(PMmodel) {
  # early return if covariates are not present in the PM model
  if (is.null(PMmodel$cov)) {
    return(NULL)
  }

  cov <- PMmodel$cov
  cov_names <- bdCovariateName(names(cov))
  if (!identical(cov_names, tolower(names(cov)))) {
    message(
      "Covariates renamed to their BestDose canonical name: ",
      paste(names(cov), "->", cov_names, collapse = ", ")
    )
    message(
      "The secondary/ODE equations still use the original names, update them before saving."
    )
  }

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
#' @return A named list of secondary equations, or NULL when the block is absent

extractPMSecondary <- function(PMmodel) {
  if (is.null(PMmodel$sec)) {
    return(NULL)
  }

  sec <- deparseLines(PMmodel$sec)

  # detect all lines with an assignment operator (= or <-) and trim the white space at the beginning and end of the string
  bloc <- sec[stringr::str_detect(sec, "=|<-")]
  # keep the "name = expression" lines only, dropping the function() header deparse adds
  bloc <- bloc[stringr::str_detect(bloc, "^[a-zA-Z][a-zA-Z0-9_]*\\s*(=|<-)")]
  if (length(bloc) == 0) {
    return(NULL)
  }

  sec_list <- as.list(bloc)
  names(sec_list) <- bdKey(stringr::str_extract(bloc, "^[a-zA-Z0-9_]+"))

  return(sec_list)
}

#' @title Extract the error model from a PM model and format it for BestDose
#' @param PMmodel A PM model object
#' @return The BestDose error block

extractPMError <- function(PMmodel) {
  if (is.null(PMmodel$err)) {
    stop("The error block is NULL. Please check your PM model.")
  }

  err <- PMmodel$err[[1]]
  # BestDose only knows "additive" and "proportional" (Pmetrics' L and G)
  err_type <- switch(
    tolower(as.character(err$type)),
    "l" = "additive",
    "additive" = "additive",
    "g" = "proportional",
    "proportional" = "proportional",
    stop("Unsupported error model type: ", err$type)
  )

  error_list <- list(
    type = err_type,
    initial_value = err$initial,
    coefficient = data.frame(
      c0 = err$coeff[1],
      c1 = err$coeff[2],
      c2 = err$coeff[3],
      c3 = err$coeff[4]
    )
  )

  return(error_list)
}

# ==================================================================____
# description block
# ==================================================================____

#' @title Derive the administration routes from the equations
#' @description BestDose needs at least one `{route, compartment}` entry, and that compartment
#' index is what the engine actually doses into. Infusion tokens (`R[N]`, `RATEIV[N]`) map to IV,
#' bolus tokens (`B[N]`) to an extravascular depot. The compartment is inferred, the human label is
#' a guess: confirm it.
#'
#' @param equations The equation list from `extractPMequation`
#' @return A data.frame of routes

extractBDroutes <- function(equations) {
  text <- tolower(paste(unlist(equations), collapse = " "))
  indices <- function(pattern) {
    unique(as.integer(stringr::str_match_all(text, pattern)[[1]][, 2]))
  }

  iv <- indices("(?:^|[^a-z0-9_])(?:r|rateiv)\\s*\\[\\s*([0-9]+)\\s*\\]")
  po <- indices("(?:^|[^a-z0-9_])b\\s*\\[\\s*([0-9]+)\\s*\\]")

  if (length(iv) + length(po) == 0) {
    message(
      "No dose input (R[N]/B[N]) found in the equations, defaulting to IV into compartment 1."
    )
    return(data.frame(route = "IV", compartment = 1L))
  }

  routes <- data.frame(
    route = c(rep("IV", length(iv)), rep("PO", length(po))),
    compartment = as.integer(c(iv, po))
  )
  message(
    "Routes inferred from the equations, check the labels: ",
    paste(routes$route, "->", routes$compartment, collapse = ", ")
  )

  return(routes)
}

#' @title Build the compartment block
#' @description `number` counts the *named* compartments (one per output equation), not the ODE
#' states: an absorption depot has an equation but no output. `outputs[i]$equation` is a Y index.
#'
#' @param out The output list from `extractPMBlock(PMmodel$out, "y")`
#' @return The BestDose compartment block

createBDcompartment <- function(out) {
  n <- length(out)
  if (n == 0) {
    stop("The output block is NULL. Please check your PM model.")
  }

  # the first output must be named "central", the others follow the BestDose naming convention
  names_out <- c(
    "central",
    "peripheral",
    "effect",
    paste0("compartment", seq_len(max(n - 3, 0)) + 3)
  )[seq_len(n)]

  dropNulls(list(
    number = n,
    central = 1L,
    peripheral = if (n >= 2) 2L else NULL,
    outputs = data.frame(name = names_out, equation = seq_len(n))
  ))
}

# Starting ranges for the covariates BestDose knows about. These are the clinical bounds shown in
# the UI, not statistical ones: a Pmetrics fit carries none of this, so anything missing here is
# filled with a placeholder that needs a review.
BD_COVARIATE_INFO <- list(
  weight = list(
    label = "Weight",
    unit = "kg",
    min = 2,
    max = 650,
    default = 70,
    description = "Patient weight in kilograms"
  ),
  height = list(
    label = "Height",
    unit = "cm",
    min = 30,
    max = 250,
    default = 170,
    description = "Patient height in centimeters"
  ),
  age = list(
    label = "Age",
    unit = "years",
    min = 0,
    max = 120,
    default = 50,
    description = "Patient age in years"
  ),
  crcl = list(
    label = "Creatinine clearance",
    unit = "mL/min",
    min = 0,
    max = 200,
    default = 90,
    description = "Creatinine clearance in mL/min calculated using the Cockcroft-Gault equation"
  ),
  creat = list(
    label = "Serum creatinine",
    unit = "mg/dL",
    min = 0,
    max = 20,
    default = 1,
    description = "Serum creatinine concentration"
  ),
  bmi = list(
    label = "Body mass index",
    unit = "kg/m2",
    min = 10,
    max = 60,
    default = 25,
    description = "Body mass index"
  ),
  bsa = list(
    label = "Body surface area",
    unit = "m2",
    min = 0.1,
    max = 3,
    default = 1.8,
    description = "Body surface area"
  )
)

#' @title Build the covariates description block
#' @description Only the covariate *names* come from the fit: labels, units, ranges and
#' descriptions are clinical metadata, taken from `BD_COVARIATE_INFO` when known and filled with a
#' placeholder otherwise. `I()` keeps the single-covariate vectors as JSON arrays.
#'
#' @param covariates The covariate list from `extractPMCovariates`
#' @return The BestDose covariates description block, or NULL when the model has no covariate

createBDcovariates <- function(covariates) {
  if (length(covariates) == 0) {
    return(NULL)
  }

  cov_names <- names(covariates)
  info <- lapply(cov_names, function(name) {
    known <- BD_COVARIATE_INFO[[name]]
    if (is.null(known)) {
      message(
        "Unknown covariate '",
        name,
        "': update its label, unit, range and description before using the model."
      )
      known <- list(
        label = name,
        unit = "to update",
        min = 0,
        max = 200,
        default = 70,
        description = "Please update the description as needed"
      )
    }
    known
  })
  names(info) <- cov_names

  list(
    number = length(cov_names),
    names = I(cov_names),
    label = I(vapply(
      info,
      function(x) x$label,
      character(1),
      USE.NAMES = FALSE
    )),
    units = I(vapply(
      info,
      function(x) x$unit,
      character(1),
      USE.NAMES = FALSE
    )),
    types = I(rep("numeric", length(cov_names))),
    value = lapply(info, function(x) {
      data.frame(min = x$min, max = x$max, default = x$default)
    }),
    description = lapply(info, function(x) x$description)
  )
}

#' @title Build the description block of a BestDose model file
#'
#' @param model_list The model block built by `createBDmodel`
#' @param drug_name The drug the model attaches to (stored lowercase)
#' @param model_name The model file name, without the .json extension
#' @param description Free text description of the model
#' @param reference Citation for the model
#' @param reference_url URL for the citation
#'
#' @return The BestDose description block

createBDdescription <- function(
  model_list,
  drug_name,
  model_name,
  description = "",
  reference = "",
  reference_url = ""
) {
  dropNulls(list(
    drug = tolower(drug_name),
    route = extractBDroutes(model_list$equation),
    name = paste0(model_name, ".json"),
    compartment = createBDcompartment(model_list$out),
    version = 1L,
    description = description,
    reference = reference,
    reference_url = reference_url,
    covariates = createBDcovariates(model_list$covariates)
  ))
}

# ==================================================================____
# support points
# ==================================================================____

#' @title Extract the final cycle population points as BestDose support points
#' @description One row per support point, one column per primary parameter plus `prob`. BestDose
#' requires those columns to match the primary parameter names exactly.
#'
#' @param PM_result A PM result object
#' @param primary_names The names of the primary parameters
#' @return A data.frame of support points

extractBDsupportPoints <- function(PM_result, primary_names) {
  points <- PM_result$final$popPoints
  if (is.null(points)) {
    warning(
      "No population points found in the PM result, the model file will carry no support point."
    )
    return(list())
  }

  points <- as.data.frame(points)
  names(points)[tolower(names(points)) %in% c("prob", "probability")] <- "prob"

  missing <- setdiff(primary_names, names(points))
  if (length(missing) > 0) {
    stop(
      "Support points are missing the primary parameter(s): ",
      paste(missing, collapse = ", ")
    )
  }
  if (!"prob" %in% names(points)) {
    stop("Support points are missing the 'prob' column.")
  }

  points[, c(primary_names, "prob"), drop = FALSE]
}

# ==================================================================____
# assemble, check and save
# ==================================================================____

#' @title Create a BestDose model list from a PM model object
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param PM_result A PM model result object
#' @param drug_name The drug the model attaches to
#' @param model_name The model file name, without the .json extension
#' @param description Free text description of the model
#' @param reference Citation for the model
#' @param reference_url URL for the citation
#' @return A list containing the description, model and support points formatted for BestDose
#'
#' @export

createBDmodel <- function(
  PM_result,
  drug_name = "Drug",
  model_name = drug_name,
  description = "Model imported from a Pmetrics fit.",
  reference = "",
  reference_url = ""
) {
  # gather basic elements of the model to be used in BestDose
  PMmodel <- PM_result$model$arg_list

  # create the model part of the file
  model_list <- dropNulls(list(
    primary = extractPMPrimary(PMmodel),
    covariates = extractPMCovariates(PMmodel),
    secondary = extractPMSecondary(PMmodel),
    initial_conditions = extractPMBlock(PMmodel$ini, "ini"),
    fa = extractPMBlock(PMmodel$fa, "fa"),
    lag = extractPMBlock(PMmodel$lag, "lag"),
    equation = extractPMBlock(PMmodel$eqn, "dx"),
    out = extractPMBlock(PMmodel$out, "y"),
    error = extractPMError(PMmodel)
  ))

  model_file <- list(
    description = createBDdescription(
      model_list,
      drug_name,
      model_name,
      description,
      reference,
      reference_url
    ),
    model = model_list,
    support_point = extractBDsupportPoints(PM_result, names(model_list$primary))
  )

  checkBDmodel(model_file)

  return(model_file)
}

#' @title Check a BestDose model list before writing it
#' @description The blocking subset of `model_check.rs`, i.e. everything that makes BestDose refuse
#' the file outright. The app runs the full check, warnings included, on import.
#'
#' @param model_file A model list built by `createBDmodel`
#' @return `invisible(TRUE)`, or stops with the list of problems

checkBDmodel <- function(model_file) {
  description <- model_file$description
  model <- model_file$model
  issues <- character(0)

  if (nchar(description$drug) == 0) {
    issues <- c(issues, "A drug must be assigned to the model.")
  }
  if (nchar(description$name) == 0) {
    issues <- c(issues, "The model name is required.")
  }
  if (nrow(description$route) == 0) {
    issues <- c(issues, "At least one administration route is required.")
  }
  if (length(model$primary) < 2) {
    issues <- c(issues, "At least two primary parameters are required.")
  }

  # compartment and output consistency
  n_compartment <- description$compartment$number
  if (n_compartment < 1 || n_compartment > 6) {
    issues <- c(issues, "Compartment count must be between 1 and 6.")
  }
  if (n_compartment > length(model$equation)) {
    issues <- c(
      issues,
      "Compartment count exceeds the number of differential equations."
    )
  }
  if (length(model$out) != n_compartment) {
    issues <- c(
      issues,
      "The number of output equations does not match the number of named compartments."
    )
  }
  if (
    !setequal(
      names(model$out),
      paste0("y", description$compartment$outputs$equation)
    )
  ) {
    issues <- c(
      issues,
      "The output equations do not match the compartment output indices."
    )
  }

  # a route may dose into an implicit depot, so it is bounded by the ODE count, not the compartment count
  state_count <- max(length(model$equation), n_compartment)
  if (
    any(
      description$route$compartment < 1 |
        description$route$compartment > state_count
    )
  ) {
    issues <- c(
      issues,
      "A route maps to a compartment outside the model's range."
    )
  }

  # every covariate declared in the description needs a model level interpolation, and the reverse
  cov_described <- if (is.null(description$covariates)) {
    character(0)
  } else {
    as.character(description$covariates$names)
  }
  if (!setequal(cov_described, names(model$covariates))) {
    issues <- c(
      issues,
      "The covariates in the description and in the model block do not match."
    )
  }

  # support point columns must be exactly the primary parameters plus prob
  if (
    length(model_file$support_point) > 0 &&
      !setequal(
        names(model_file$support_point),
        c(names(model$primary), "prob")
      )
  ) {
    issues <- c(
      issues,
      "The support point columns do not match the primary parameters."
    )
  }

  if (length(issues) > 0) {
    stop(
      "The model is not valid for BestDose:\n- ",
      paste(issues, collapse = "\n- ")
    )
  }

  invisible(TRUE)
}
