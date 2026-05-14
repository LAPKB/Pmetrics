####### INTERNAL FUNCTIONS ########
# Returns a list of names of all models in mod_list
mod_lib_names <- function() {
  purrr::map_chr(mod_list, \(x) x$name)
}

# Returns a tibble of primary model names and their alternative names from mod_list
alt_mod_lib_names <- function() {
  alt_map <- purrr::map_df(mod_list, \(x) {
    tibble(primary = x$name, alt = x$alt_names)
  }) |>
    tidyr::unnest_longer(alt)
  return(alt_map)
}

# returns model definition from mod_list based on detected template name, 0 if none, and -1 if more than one
get_found_model <- function(fun) {
  eqns <- as.list(body(fun)[-1])
  expr_to_char <- \(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n")

  primary_names <- mod_lib_names()
  alt_map <- alt_mod_lib_names()

  found_pri <- map_lgl(eqns, \(x) expr_to_char(x) %in% primary_names)
  found_alt <- map_lgl(eqns, \(x) expr_to_char(x) %in% alt_map$alt)
  all_found <- sum(found_pri, found_alt)

  if (all_found > 1) {
    cli::cli_inform(c(
      "x" = "Multiple library model templates detected",
      "i" = "Maximum of one library model template allowed."
    ))
    return(-1)
  }

  if (all_found == 0) {
    return(0)
  }

  if (any(found_pri)) { # found a primary model name
    found_model_name <- expr_to_char(eqns[[which(found_pri)]])
  } else { # found an alternative model name
    found_model_name <- alt_map |>
      filter(alt == expr_to_char(eqns[[which(found_alt)]])) |>
      pull(primary) |>
      unique() |>
      head(1)
  }

  if (length(found_model_name) > 0) {
    found_model <- purrr::detect(mod_list, \(x) x$name == found_model_name)
    if (is.null(found_model)) {
      found_model <- 0
    } else {
      found_model$name <- found_model_name
    }
  } else {
    found_model <- 0
  }
  return(found_model)
}

# assemble library and load into global environment
# build_model_lib <- function(){
#   purrr::walk(mod_list, \(x) {
#     PM_lib$new(x) -> mod
#     assign(tolower(mod$name), mod, envir = .GlobalEnv)
#   })
# }

####################################

# Generate PM_model$new() code text from a mod_list entry (no compilation needed)
generate_model_code_text <- function(lib_model) {
  arg_list <- lib_model$arg_list
  model_name <- lib_model$name

  pri <- paste0(
    "  pri = list(\n",
    paste(
      purrr::map_chr(names(arg_list$pri), \(i) {
        sprintf("    %s = ab(%.3f, %.3f)", i, arg_list$pri[[i]]$min, arg_list$pri[[i]]$max)
      }),
      collapse = ",\n"
    ),
    "\n  ),"
  )

  eqn <- sprintf("  eqn = function() {\n    %s\n  },", model_name)

  out_lines <- deparse(arg_list$out)
  out <- paste0("  out = ", paste(out_lines, collapse = "\n  "), ",")

  err <- paste0(
    "  err = list(\n",
    paste(
      purrr::map_chr(arg_list$err, \(i) {
        sprintf(
          "    %s(%i, c(%.1f, %.1f, %.1f, %.1f)%s)",
          i$type, i$initial,
          ifelse(length(i$coeff) >= 1, i$coeff[1], 0),
          ifelse(length(i$coeff) >= 2, i$coeff[2], 0),
          ifelse(length(i$coeff) >= 3, i$coeff[3], 0),
          ifelse(length(i$coeff) >= 4, i$coeff[4], 0),
          ifelse(isTRUE(i$fixed), ", fixed = TRUE", "")
        )
      }),
      collapse = ",\n"
    ),
    "\n  )"
  )

  paste0(
    "mod <- PM_model$new(\n\n",
    paste(c(pri, eqn, out, err), collapse = "\n\n"),
    "\n\n)"
  )
}


#' @title Model Library
#'
#' @description
#' Launches a Shiny app for browsing all available pharmacokinetic model
#' templates in the Pmetrics library. Select a model to view its parameters,
#' compartment structure, differential equations, and ready-to-use
#' `PM_model$new()` code. Use the Copy button to send the code to the
#' clipboard. Individual creator functions such as [one_comp_iv()] return a
#' compiled `PM_model` and copy the code automatically.
#' @param show Logical. If `TRUE` (default), launches the Shiny browser app.
#'   If `FALSE`, returns the model tibble invisibly without launching the app.
#' @param launch.browser Logical. Passed to the standalone app package's
#'   `run_app()` function when `show = TRUE`.
#' @return Invisibly, a tibble of all model templates.
#' @export
model_lib <- function(show = TRUE, launch.browser = TRUE) {
  mod_table <- tibble::tibble(
    Name         = purrr::map_chr(mod_list, \(x) x$name),
    alt_names    = purrr::map_chr(mod_list, \(x) paste(x$alt_names, collapse = ", ")),
    Description  = purrr::map_chr(mod_list, \(x) paste(x$description, collapse = "\n")),
    Compartments = purrr::map_chr(mod_list, \(x) paste(x$compartments, collapse = "\n")),
    Parameters   = purrr::map_chr(mod_list, \(x) paste(names(x$arg_list$pri), collapse = ", "))
  )

  if (show) {
    if (!requireNamespace("PmetricsModelLib", quietly = TRUE)) {
      cli::cli_abort(c(
        "x" = "The {.pkg PmetricsModelLib} package is required to launch model_lib.",
        "i" = "Install it using install.packages('PmetricsModelLib', repos = 'https://lapkb.r-universe.dev')."
      ))
    }
    getExportedValue("PmetricsModelLib", "run_app")(launch.browser = launch.browser)
  }

  return(invisible(mod_table))
}


# Primary Name", "Alt Names", "Description", "Compartments", "Parameters"


# Legacy PM_lib implementation retained temporarily for reference.
if (FALSE) {
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
      #' @field export Character vector with code that can be pasted to create PM_model
      export = NULL,
      initialize = function(x) {
        self$name <- x[[1]]
        self$alt_names <- x[[2]]
        self$description <- x[[3]]
        self$compartments <- x[[4]]
        self$analytical <- x[[5]]
        self$arg_list <- x[[6]]
        self$export <- self$export_model()
      },
      print = function() {
        if (requireNamespace("clipr", quietly = TRUE)) {
          clipr::write_clip(self$export)
          cli::cli_inform(c(
            "i" = "Model code copied to clipboard.",
            ">" = "Paste the code into your script to create a PM_model based on this template.",
            ">" = "Edit the model as needed, ensuring parameter names and order are preserved in the {.code PRI} block.",
            ">" = "You can add parameters to the {.code PRI} block to define any used in {.code LAG}, {.code FA}, or {.code INI} blocks.",
            ">" = "You can add a {.code SEC} block but preserve {.code PRI} names, e.g. v = v * wt."
          ))
        } else {
          cli::cli_inform(c(
            "i" = "Please install the {.pkg clipr} package to enable clipboard functionality.",
            ">" = "Model code generated, but clipboard copy is unavailable.",
            ">" = "Copy and paste the code below to create a PM_model based on this template.",
            ">" = "Edit the model as needed, ensuring parameter names and order are preserved in the {.code PRI} block.",
            ">" = "You can add parameters to the {.code PRI} block to define any used in {.code LAG}, {.code FA}, or {.code INI} blocks.",
            ">" = "You can add a {.code SEC} block but preserve {.code PRI} names, e.g. v = v * wt."
          ))
          cat("\n", self$export, "\n")
        }
        return(invisible(self))
      },
      notes = function() {
        cli::cli_div(theme = list(
          span.tip = list(color = "dodgerblue", "font-style" = "italic")
        ))
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
        if (self$analytical) {
          cli::cli_text("{.tip These are shown to describe the model structure, but the model can be fitted analytically, without a differential equation solver}")
        } else {
          cli::cli_text("{.tip These describe the model structure, and the model is fitted with a differential equation solver}")
        }
        purrr::walk(func_to_char(self$arg_list$eqn), cli::cli_text)
        cli::cli_end()
        return(invisible(self))
      },
      export_model = function() {
        arg_list <- self$arg_list

        fmt_num <- function(x) {
          format(x, scientific = FALSE, trim = TRUE, digits = 15)
        }

        pri <- c(
          "    pri = list(\n",
          purrr::map_chr(names(arg_list$pri), \(i) {
            sprintf(
              "        %s = ab(%s,%s)",
              i,
              fmt_num(arg_list$pri[[i]]$min),
              fmt_num(arg_list$pri[[i]]$max)
            )
          }) |>
            paste(collapse = ",\n"),
          "\n    ),"
        )

        if ("cov" %in% names(arg_list)) {
          cov <- c(
            "\n    cov = list(\n",
            purrr::map_chr(names(arg_list$cov), \(i) {
              sprintf("        %s = interp(%s)", i, ifelse(arg_list$cov[[i]] == 0, "\"none\"", ""))
            }) |>
              paste(collapse = ",\n"),
            "\n    ),"
          )
        } else {
          cov <- NULL
        }

        if (!is.null(arg_list$sec)) {
          sec <- c(
            "\n    sec = ",
            paste0(deparse(arg_list$sec), collapse = "\n    "),
            ","
          )
        } else {
          sec <- NULL
        }

        if (!is.null(arg_list$fa)) {
          fa <- c(
            "\n    fa = ",
            paste0(deparse(arg_list$fa), collapse = "\n    "),
            ","
          )
        } else {
          fa <- NULL
        }

        if (!is.null(arg_list$ini)) {
          ini <- c(
            "\n    ini = ",
            paste0(deparse(arg_list$ini), collapse = "\n    "),
            ","
          )
        } else {
          ini <- NULL
        }

        if (!is.null(arg_list$lag)) {
          lag <- c(
            "\n    lag = ",
            paste0(deparse(arg_list$lag), collapse = "\n    "),
            ","
          )
        } else {
          lag <- NULL
        }

        eqn <- c(
          "\n    eqn = function(){\n",
          sprintf("        %s", self$name),
          "\n    },"
        )

        out <- c(
          "\n    out = ",
          paste0(deparse(arg_list$out), collapse = "\n    "),
          ","
        )

        err <- c(
          "\n    err = list(\n",
          purrr::map_chr((arg_list$err), \(i) {
            sprintf(
              "        %s(%s, c(%s,%s,%s,%s)%s)",
              i$type,
              fmt_num(i$initial),
              fmt_num(ifelse(length(i$coeff) >= 1, i$coeff[1], 0)),
              fmt_num(ifelse(length(i$coeff) >= 2, i$coeff[2], 0)),
              fmt_num(ifelse(length(i$coeff) >= 3, i$coeff[3], 0)),
              fmt_num(ifelse(length(i$coeff) >= 4, i$coeff[4], 0)),
              ifelse(i$fixed, ", fixed = TRUE", "")
            )
          }) |>
            paste(collapse = ",\n"),
          "\n    )"
        )

        model_export <- c(
          "mod <- PM_model$new(\n",
          paste0(c(pri, cov, sec, fa, ini, lag, eqn, out, err), collapse = ""),
          "\n)"
        )

        return(invisible(model_export))
      },
      plot = function(...) {
        plot.PM_model(self, ...)
      }
    ), # end public
    active = list(
      #' @field ncomp Number of compartments in the model
      ncomp = function() {
        length(self$compartments)
      },
      #' @field bolus Logical indicating if the model has a bolus input
      bolus = function() {
        grepl("bolus", self$name, ignore.case = TRUE)
      },
      #' @field parameters Names of the primary parameters in the model
      parameters = function() {
        names(self$arg_list$pri)
      }
    ) # end active
  ) # end PM_lib class definition
}


##### default models in the library
mod_list <- list(
  list(
    name = "one_comp_iv",
    alt_names = c("advan1", "advan1_trans1"),
    description = c("One compartment", "Infusion into central compartment #1", "Ke elimination from central compartment #1"),
    compartments = "1 = Central",
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ke = ab(0, 5),
        V = ab(0, 100)
      ),
      eqn = function() {
        dX[1] <- R[1] - Ke * X[1]
      },
      out = function() {
        Y[1] <- X[1] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "one_comp_iv_cl",
    alt_names = "advan1_trans2",
    description = c("Infusion into central compartment #1", "CLearance from central compartment #1"),
    compartments = "1 = Central",
    analytical = TRUE,
    arg_list = list(
      pri = c(
        CL = ab(0, 500),
        V = ab(0, 100)
      ),
      eqn = function() {
        Ke <- CL / V
        dX[1] <- R[1] - Ke * X[1]
      },
      out = function() {
        Y[1] <- X[1] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "one_comp_bolus",
    alt_names = c("advan2", "advan2_trans1"),
    description = c("Bolus input to compartment #1, infusion to central compartment #2", "Ke elimination from central compartment #2"),
    compartments = c("1 = Bolus", "2 = Central"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ka = ab(0, 5),
        Ke = ab(0, 5),
        V = ab(0, 100)
      ),
      eqn = function() {
        dX[1] <- B[1] - Ka * X[1]
        dX[2] <- R[1] + Ka * X[1] - Ke * X[2]
      },
      out = function() {
        Y[1] <- X[2] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "one_comp_bolus_cl",
    alt_names = "advan2_trans2",
    description = c("Bolus input to compartment #1, infusion to central compartment #2", "CLearance from central compartment #2"),
    compartments = c("1 = Bolus", "2 = Central"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ka = ab(0, 5),
        CL = ab(0, 500),
        V = ab(0, 100)
      ),
      eqn = function() {
        Ke <- CL / V
        dX[1] <- B[1] - Ka * X[1]
        dX[2] <- R[1] + Ka * X[1] - Ke * X[2]
      },
      out = function() {
        Y[1] <- X[2] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "two_comp_iv",
    alt_names = c("advan3", "advan3_trans1"),
    description = c("Infusion into central compartment #1", "Distribution to/from peripheral compartment #2", "Ke elimination from central compartment #1"),
    compartments = c("1 = Central", "2 = Peripheral"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ke = ab(0, 5),
        K12 = ab(0, 5),
        K21 = ab(0, 5),
        V = ab(0, 100)
      ),
      eqn = function() {
        dX[1] <- R[1] - (Ke + K12) * X[1] + K21 * X[2]
        dX[2] <- K12 * X[1] - K21 * X[2]
      },
      out = function() {
        Y[1] <- X[1] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "two_comp_iv_cl",
    alt_names = "advan3_trans4",
    description = c("Infusion into central compartment #1", "Distribution to/from peripheral compartment #2", "CLearance from central compartment #1"),
    compartments = c("1 = Central", "2 = Peripheral"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        CL = ab(0, 500),
        Q = ab(0, 100),
        V1 = ab(0, 100),
        V2 = ab(0, 100)
      ),
      eqn = function() {
        Ke <- CL / V1
        K12 <- Q / V1
        K21 <- Q / V2
        dX[1] <- R[1] - (Ke + K12) * X[1] + K21 * X[2]
        dX[2] <- K12 * X[1] - K21 * X[2]
      },
      out = function() {
        Y[1] <- X[1] / V1
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "two_comp_bolus",
    alt_names = c("advan4", "advan4_trans1"),
    description = c("Bolus input to compartment #1, infusion into central compartment #2", "Distribution to/from peripheral compartment #3", "Ke elimination from central compartment #2"),
    compartments = c("1 = Bolus", "2 = Central", "3 = Peripheral"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ke = ab(0, 5),
        Ka = ab(0, 5),
        K23 = ab(0, 5),
        K32 = ab(0, 5),
        V = ab(0, 100)
      ),
      eqn = function() {
        dX[1] <- B[1] - Ka * X[1]
        dX[2] <- R[1] + Ka * X[1] - (Ke + K23) * X[2] + K32 * X[3]
        dX[3] <- K23 * X[2] - K32 * X[3]
      },
      out = function() {
        Y[1] <- X[2] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "two_comp_bolus_cl",
    alt_names = "advan4_trans4",
    description = c("Bolus input to compartment #1, infusion into central compartment #2", "Distribution to/from peripheral compartment #3", "CLearance from central compartment #2"),
    compartments = c("1 = Bolus", "2 = Central", "3 = Peripheral"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ka = ab(0, 5),
        CL = ab(0, 500),
        Q = ab(0, 100),
        V2 = ab(0, 100),
        V3 = ab(0, 100)
      ),
      eqn = function() {
        Ke <- CL / V2
        K23 <- Q / V2
        K32 <- Q / V3
        dX[1] <- B[1] - Ka * X[1]
        dX[2] <- R[1] + Ka * X[1] - (Ke + K23) * X[2] + K32 * X[3]
        dX[3] <- K23 * X[2] - K32 * X[3]
      },
      out = function() {
        Y[1] <- X[2] / V2
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "three_comp_iv",
    alt_names = c("advan11", "advan11_trans1"),
    description = c("Infusion into central compartment #1", "Distribution to/from peripheral compartments #2 and #3", "Elimination from central compartment #1"),
    compartments = c("1 = Central", "2 = Peripheral 1", "3 = Peripheral 2"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ke = ab(0, 5),
        K12 = ab(0, 5),
        K13 = ab(0, 5),
        K21 = ab(0, 5),
        K31 = ab(0, 5),
        V = ab(0, 100)
      ),
      eqn = function() {
        dX[1] <- R[1] - (Ke + K12 + K13) * X[1] + K21 * X[2] + K31 * X[3]
        dX[2] <- K12 * X[1] - K21 * X[2]
        dX[3] <- K13 * X[1] - K31 * X[3]
      },
      out = function() {
        Y[1] <- X[1] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "three_comp_iv_cl",
    alt_names = "advan11_trans4",
    description = c("Infusion into central compartment #1", "Distribution to/from peripheral compartments #2 and #3", "CLearance from central compartment #1"),
    compartments = c("1 = Central", "2 = Peripheral 1", "3 = Peripheral 2"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        CL = ab(0, 500),
        Q2 = ab(0, 100),
        Q3 = ab(0, 100),
        V1 = ab(0, 100),
        V2 = ab(0, 100),
        V3 = ab(0, 100)
      ),
      eqn = function() {
        Ke <- CL / V1
        K12 <- Q2 / V1
        K21 <- Q2 / V2
        K13 <- Q3 / V1
        K31 <- Q3 / V3
        dX[1] <- R[1] - (Ke + K12 + K13) * X[1] + K21 * X[2] + K31 * X[3]
        dX[2] <- K12 * X[1] - K21 * X[2]
        dX[3] <- K13 * X[1] - K31 * X[3]
      },
      out = function() {
        Y[1] <- X[1] / V1
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "three_comp_bolus",
    alt_names = c("advan12", "advan12_trans1"),
    description = c("Bolus into compartment #1", "Infusion into central compartment #2", "Distribution to/from peripheral compartments #3 and #4", "Elimination from central compartment #2"),
    compartments = c("1 = Bolus", "2 = Central", "3 = Peripheral 1", "4 = Peripheral 2"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ka = ab(0, 5),
        Ke = ab(0, 5),
        K23 = ab(0, 5),
        K24 = ab(0, 5),
        K32 = ab(0, 5),
        K42 = ab(0, 5),
        V = ab(0, 100)
      ),
      eqn = function() {
        dX[1] <- B[1] - Ka * X[1]
        dX[2] <- R[1] + Ka * X[1] - (Ke + K23 + K24) * X[2] + K32 * X[3] + K42 * X[4]
        dX[3] <- K23 * X[2] - K32 * X[3]
        dX[4] <- K24 * X[2] - K42 * X[4]
      },
      out = function() {
        Y[1] <- X[2] / V
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  ),
  list(
    name = "three_comp_bolus_cl",
    alt_names = "advan12_trans4",
    description = c("Bolus into compartment #1", "Infusion into central compartment #2", "Distribution to/from peripheral compartments #3 and #4", "CLearance from central compartment #2"),
    compartments = c("1 = Bolus", "2 = Central", "3 = Peripheral 1", "4 = Peripheral 2"),
    analytical = TRUE,
    arg_list = list(
      pri = c(
        Ka = ab(0, 5),
        CL = ab(0, 500),
        Q3 = ab(0, 100),
        Q4 = ab(0, 100),
        V2 = ab(0, 100),
        V3 = ab(0, 100),
        V4 = ab(0, 100)
      ),
      eqn = function() {
        Ke <- CL / V2
        K23 <- Q3 / V2
        K32 <- Q3 / V3
        K24 <- Q4 / V2
        K42 <- Q4 / V4
        dX[1] <- B[1] - Ka * X[1]
        dX[2] <- R[1] + Ka * X[1] - (Ke + K23 + K24) * X[2] + K32 * X[3] + K42 * X[4]
        dX[3] <- K23 * X[2] - K32 * X[3]
        dX[4] <- K24 * X[2] - K42 * X[4]
      },
      out = function() {
        Y[1] <- X[2] / V2
      },
      err = c(
        proportional(5, c(0.1, 0.1, 0, 0))
      )
    )
  )
) # end mod_list


# model creator helpers -----------------------------------------------------

get_model_library_entry <- function(model_name) {
  mod_idx <- purrr::detect_index(mod_list, \(x) x$name == model_name)

  if (mod_idx == 0) {
    cli::cli_abort(c(
      "x" = "Model template {.val {model_name}} not found in the model library."
    ))
  }

  mod_list[[mod_idx]]
}

new_pm_model_from_library <- function(model_name) {
  lib_model <- get_model_library_entry(model_name)

  args <- list(
    pri = as.list(lib_model$arg_list$pri),
    eqn = eval(parse(text = sprintf("function(){ %s }", model_name))),
    out = lib_model$arg_list$out,
    err = as.list(lib_model$arg_list$err)
  )

  if ("cov" %in% names(lib_model$arg_list)) {
    args$cov <- lib_model$arg_list$cov
  }

  if ("sec" %in% names(lib_model$arg_list)) {
    args$sec <- lib_model$arg_list$sec
  }

  if ("fa" %in% names(lib_model$arg_list)) {
    args$fa <- lib_model$arg_list$fa
  }

  if ("ini" %in% names(lib_model$arg_list)) {
    args$ini <- lib_model$arg_list$ini
  }

  if ("lag" %in% names(lib_model$arg_list)) {
    args$lag <- lib_model$arg_list$lag
  }

  mod <- do.call(PM_model$new, args)
  mod$copy()

  invisible(mod)
}


#' @title One-compartment IV model template
#' @description Create a `PM_model` object for the `one_comp_iv` model template.
#' @return A `PM_model` object.
#' @export
one_comp_iv <- function() {
  new_pm_model_from_library("one_comp_iv")
}

#' @title One-compartment IV clearance model template
#' @description Create a `PM_model` object for the `one_comp_iv_cl` model template.
#' @return A `PM_model` object.
#' @export
one_comp_iv_cl <- function() {
  new_pm_model_from_library("one_comp_iv_cl")
}

#' @title One-compartment bolus model template
#' @description Create a `PM_model` object for the `one_comp_bolus` model template.
#' @return A `PM_model` object.
#' @export
one_comp_bolus <- function() {
  new_pm_model_from_library("one_comp_bolus")
}

#' @title One-compartment bolus clearance model template
#' @description Create a `PM_model` object for the `one_comp_bolus_cl` model template.
#' @return A `PM_model` object.
#' @export
one_comp_bolus_cl <- function() {
  new_pm_model_from_library("one_comp_bolus_cl")
}

#' @title Two-compartment IV model template
#' @description Create a `PM_model` object for the `two_comp_iv` model template.
#' @return A `PM_model` object.
#' @export
two_comp_iv <- function() {
  new_pm_model_from_library("two_comp_iv")
}

#' @title Two-compartment IV clearance model template
#' @description Create a `PM_model` object for the `two_comp_iv_cl` model template.
#' @return A `PM_model` object.
#' @export
two_comp_iv_cl <- function() {
  new_pm_model_from_library("two_comp_iv_cl")
}

#' @title Two-compartment bolus model template
#' @description Create a `PM_model` object for the `two_comp_bolus` model template.
#' @return A `PM_model` object.
#' @export
two_comp_bolus <- function() {
  new_pm_model_from_library("two_comp_bolus")
}

#' @title Two-compartment bolus clearance model template
#' @description Create a `PM_model` object for the `two_comp_bolus_cl` model template.
#' @return A `PM_model` object.
#' @export
two_comp_bolus_cl <- function() {
  new_pm_model_from_library("two_comp_bolus_cl")
}

#' @title Three-compartment IV model template
#' @description Create a `PM_model` object for the `three_comp_iv` model template.
#' @return A `PM_model` object.
#' @export
three_comp_iv <- function() {
  new_pm_model_from_library("three_comp_iv")
}

#' @title Three-compartment IV clearance model template
#' @description Create a `PM_model` object for the `three_comp_iv_cl` model template.
#' @return A `PM_model` object.
#' @export
three_comp_iv_cl <- function() {
  new_pm_model_from_library("three_comp_iv_cl")
}

#' @title Three-compartment bolus model template
#' @description Create a `PM_model` object for the `three_comp_bolus` model template.
#' @return A `PM_model` object.
#' @export
three_comp_bolus <- function() {
  new_pm_model_from_library("three_comp_bolus")
}

#' @title Three-compartment bolus clearance model template
#' @description Create a `PM_model` object for the `three_comp_bolus_cl` model template.
#' @return A `PM_model` object.
#' @export
three_comp_bolus_cl <- function() {
  new_pm_model_from_library("three_comp_bolus_cl")
}
