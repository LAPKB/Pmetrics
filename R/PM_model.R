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
#' PM_model objects are passed to \code{\link{PM_fit}} objects to initiate a
#' population analysis. The object is created by defining a list of lists
#' directly in R, or by reading a model text file in the current working directory.
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
    #' mod2 <- PmetricsData::modEx$clone() #create an independent copy of modEx called mod2
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
    #' PmetricsData::modEx
    update = function(changes_list) {
      return(invisible())
    },
    #' @description Write a `PM_model` object to a text file
    #' @param model_path Full name of the file to be created, including the path
    #' relative to the current working directory
    #' @param engine Currently only "npag".
    #' @examples
    #' \dontrun{
    #' PmetricsData::modEx$write("model.txt")
    #' }
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
    model <- PM_model_list$new(model$model_list)
  } else {
    cli::cli_abort(c("x" = "Non supported model type: {typeof(model)}"))
  }
  if (getPMoptions()$backend == "rust") {
    model$compile()
  }
  return(model)
}

#' @title Additive error model
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Create an additive (lambda) error model
#' @param add Initial value for lambda
#' @param constant Estimate if `FALSE` (default).
#' @export
additive <- function(add, constant = FALSE) {
  PM_Vinput$new(add, add, "additive", constant)
}



#' @title Proportional error model
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Create an proportional (gamma) error model
#' @param prop Initial value for gamma
#' @param constant Estimate if `FALSE` (default).
#' @export
proportional <- function(prop, constant = FALSE) {
  PM_Vinput$new(prop, prop, "proportional", constant)
}

#' @title Combination error model
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a combination additive (lambda) and proportional error model
#' @details
#' This function is not yet implemented.
#' @param add Initial value for lambda
#' @param prop  Initial value for gamma
#' @param constant Estimate if `FALSE` (default).
#' @export
combination <- function(add, prop, constant = FALSE) {
  PM_Vinput$new(add, prop, "combination", constant)
}

#' @title Assay error coefficients
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Specify the coefficients for the assay error polynomial.
#' @param coeffs Vector of up to four values for C0, C1, C2, C3,
#' e.g. `c(0.15, 0.1, 0, 0)`
#' @param constant If `FALSE` (default), use values in data first, but if
#' missing, use values in model. If `TRUE`, use values in model regardless.
#' @export
errorPoly <- function(coeffs, constant = FALSE) {
  PM_Vinput$new(coeffs, NULL, "coefficients", constant)
}

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
#' @param gtz Greater than zero. If `FALSE` (default), ensure parameter values
#' remain positive by discarding negative values. Only relevant for parametric
#' analyses, since lower limit of parameter values for nonparametric are strictly
#' controlled by [ab].
#' @export
ab <- function(min, max, gtz = FALSE) {
  PM_Vinput$new(min, max, "ab", constant = FALSE, gtz)
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
#' @param gtz Greater than zero. If `FALSE` (default), ensure parameter values
#' remain positive by discarding negative values. Only relevant for parametric
#' analyses, since lower limit of parameter values for nonparametric are strictly
#' controlled by the range.
#' @export
msd <- function(mean, sd, gtz = FALSE) {
  PM_Vinput$new(mean, sd, "msd", constant = FALSE, gtz)
}

#' @title Fixed primary parameter values
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Fix parameter values to be the same in the population.
#' @param fixed The starting value for the fixed parameter.
#' @param constant If `FALSE` (default), the value for `fixed` will serve
#' as the initial estimate for a parameter with unknown mean and zero variance.
#' The parameter value will be updated to a final value at convergence
#' or when the maximum number of cycles is reached. If `TRUE`, the value for
#' `fixed` will remain unchanged, creating a parameter with known mean and zero
#' variance, i.e. a constant value in the population.
#' @param gtz Greater than zero. If `FALSE` (default), ensure parameter values
#' remain positive by discarding negative values. Only relevant for parametric
#' analyses, since lower limit of parameter values for nonparametric are strictly
#' controlled by the range.
#' @export
fixed <- function(fixed, constant = FALSE, gtz = FALSE) {
  PM_Vinput$new(fixed, fixed, "fixed", constant, gtz)
}

#' @title Model covariate declaration
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Declare covariates in the model that are in the data. Order in the model
#' should be the same as the order in the data.
#' @param name Name of the covariate in quotation marks.
#' @param constant If `FALSE` (default), allow the covariate value to be
#' linearly interpolated between values. **NOTE** that covariate values
#' are only applied at the times of doses. Values on observation rows are
#' ignored because a covariate value is an input, not an output. See the
#' [Data Objects](https://lapkb.github.io/Pmetrics/articles/data.html) article
#' for details on this.
#' @export
covariate <- function(name, constant = FALSE) {
  PM_Vinput$new(name, mode = "covariate", constant = constant)
}

# PM_Vmodel ---------------------------------------------------------------


# Virtual Class
# Here is where the model_list is printed to the console
PM_Vmodel <- R6::R6Class("PM_model",
  public = list(
    name = NULL, # used by PM_model_legacy
    # error = NULL,
    initialize = function() {
      cli::cli_abort(c("x" = "Unable to initialize abstract class"))
    },
    print = function(...) {
      cat("$model_list\n")
      mlist <- self$model_list
      blockNames <- names(mlist)

      # internal function to add space blocks
      sp <- function(n) {
        paste0(rep("   ", n), collapse = "")
      }

      sapply(blockNames, function(x) {
        if (x == "pri") {
          # cat("\t$pri\n")
          cat(sp(1), "$pri\n")
          for (i in 1:length(mlist$pri)) {
            thispri <- mlist$pri[[i]]
            thisname <- names(mlist$pri)[i]
            if (is.null(thispri$fixed)) {
              cat(paste0(
                sp(2), "$", thisname, "\n", sp(3), "$min: ", round(thispri$min, 3),
                "\n", sp(3), "$max: ", round(thispri$max, 3),
                "\n", sp(3), "$mean: ", round(thispri$mean, 3),
                "\n", sp(3), "$sd: ", round(thispri$sd, 3),
                "\n", sp(3), "$gtz: ", thispri$gtz, "\n"
              ))
            } else {
              cat(paste0(
                sp(2), "$", thisname, "\n", sp(3), "$fixed: ", round(thispri$fixed, 3),
                "\n", sp(3), "$contant: ", thispri$constant,
                "\n"
              ))
            }
          }
        } else if (x == "cov") {
          cat("\n", sp(1), "$cov\n")
          for (i in seq_along(mlist$cov)) {
            thisout <- mlist$cov[[i]]
            cat(paste0(
              sp(2), "$covariate: ", thisout$covariate, "\n",
              sp(3), "$constant: ", thisout$constant, "\n",
              "\n"
            ))
          }
        } else if (x == "ext") {
          cat("\n", sp(1), "$ext\n", paste0(sp(2), "[", 1:length(mlist$ext), "] \"", mlist$ext, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "sec") {
          cat("\n", sp(1), "$sec\n", paste0(sp(2), "[", 1:length(mlist$sec), "] \"", mlist$sec, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "dif" | x == "eqn") {
          if (is.null(mlist$eqn)) {
            cat("Please change the name of your #dif block to #eqn.")
            mlist$eqn <- mlist$dif
          }
          cat("\n", sp(1), "$eqn\n", paste0(sp(2), "[", 1:length(mlist$eqn), "] \"", mlist$eqn, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "lag") {
          cat("\n", sp(1), "$lag\n", paste0(sp(2), "[", 1:length(mlist$lag), "] \"", mlist$lag, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "bol") {
          cat("\n", sp(1), "$bol\n", paste0(sp(2), "[", 1:length(mlist$bol), "] \"", mlist$bol, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "fa") {
          cat("\n", sp(1), "$fa\n", paste0(sp(2), "[", 1:length(mlist$fa), "] \"", mlist$fa, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "ini") {
          cat("\n", sp(1), "$ini\n", paste0(sp(2), "[", 1:length(mlist$ini), "] \"", mlist$ini, "\"", collapse = "\n "))
          cat("\n")
        } else if (x == "out") {
          cat("\n", sp(1), "$out\n")
          for (i in 1:length(mlist$out)) {
            thisout <- mlist$out[[i]]
            cat(paste0(
              sp(2), "$Y", i, "\n",
              sp(3), "$val: \"", thisout[[1]], "\"\n",
              sp(3), "$err\n",
              sp(4), "$model\n",
              sp(5), "$additive: ", thisout$err$model$additive, "\n",
              sp(5), "$proportional: ", thisout$err$model$proportional, "\n",
              sp(5), "$constant: ", thisout$err$model$constant, "\n",
              sp(4), "$assay\n",
              sp(5), "$coefficients: ",
              paste0("[", 1:length(thisout$err$assay$coefficients), "] ", thisout$err$assay$coefficients, collapse = ", "), "\n",
              sp(5), "$constant: ", thisout$err$assay$constant, "\n",
              "\n"
            ))
          }
          cat("\n")
        }
      }) # end sapply

      invisible(self)
    },
    plot = function(...) {
      tryCatch(plot.PM_model(self, ...), error = function(e) {
        cat(crayon::red("Error:"), e$message, "\n")
      })
    },
    get_primary = function() {
      return(tolower(names(self$model_list$pri)))
    }
  ),
  private = list(
    validate = function() {
      # add checks here
    }
  )
)

# PM_input ----------------------------------------------------------------


# private classes
# TODO: Should I make these fields private?
# This generates text which will be written to genmodel.txt
PM_Vinput <- R6::R6Class(
  "PM_Vinput",
  public <- list(
    mode = NULL,
    min = NULL,
    max = NULL,
    mean = NULL,
    sd = NULL,
    fixed = NULL,
    constant = NULL,
    param = NULL, # is this used?
    additive = NULL,
    proportional = NULL,
    coefficients = NULL,
    covariate = NULL,
    gtz = NULL,
    initialize = function(a, b, mode, constant = FALSE, gtz = FALSE) {
      if (!mode %in% c(
        "ab", "msd", "fixed", "additive",
        "proportional", "combination",
        "coefficients", "covariate"
      )) {
        cli_abort(c(
          "x" = "{mode} is not a valid term.",
          "i" = "See help for {.fn PM_model}."
        ))
      }
      self$gtz <- gtz
      self$constant <- constant
      self$mode <- mode
      if (mode == "ab") {
        self$min <- a
        self$max <- b
        self$mean <- a + round((b - a) / 2, 3)
        self$sd <- round((b - a) / 6, 3)
      } else if (mode == "msd") {
        self$mean <- a
        self$sd <- b
        self$min <- a - 3 * b
        self$max <- a + 3 * b
      } else if (mode == "fixed") {
        self$fixed <- a
        # note that a fixed input with constant=T becomes a constant
      } else if (mode == "additive") {
        self$additive <- a
      } else if (mode == "proportional") {
        self$proportional <- a
      } else if (mode == "combination") {
        cli::cli_abort(c("x" = "Combination error models are not supported yet"))
        self$additive <- a
        self$proportional <- b
      } else if (mode == "coefficients") {
        self$coefficients <- a # a is a vector in this case
      } else if (mode == "covariate") {
        self$covariate <- a # a is a character vector in this case
      }
    },
    print_to = function(mode_not_used, engine) {
      # TODO:use mode and self$mode to translate to the right set of outputs
      if (engine == "npag" | engine == "it2b") {
        if (self$mode == "ab" | self$mode == "msd") {
          if (self$gtz) {
            return(sprintf("+%f, %f", self$mind, self$max))
          } else {
            return(sprintf("%f, %f", self$min, self$max))
          }
        } else if (self$mode == "fixed") {
          if (self$constant) {
            return(sprintf("%f!", self$fixed))
          } else {
            return(sprintf("%f", self$fixed))
          }
        } else if (self$mode == "additive") {
          if (engine == "it2b") {
            cli::cli_abort(c("x" = "Lambda is not defined in IT2B."))
          }
          if (self$constant) {
            return(sprintf("L=%f!", self$additive))
          } else {
            return(sprintf("L=%f", self$additive))
          }
        } else if (self$mode == "proportional") {
          if (self$constant) {
            return(sprintf("G=%f!", self$proportional))
          } else {
            return(sprintf("G=%f", self$proportional))
          }
        } else if (self$mode == "coefficients") {
          if (self$constant) {
            return(do.call(sprintf, c("%f, %f, %f, %f!", as.list(self$coefficients))))
          } else {
            return(do.call(sprintf, c("%f, %f, %f, %f", as.list(self$coefficients))))
          }
        } else if (self$mode == "covariate") {
          if (self$constant) {
            return(sprintf("%s!", self$covariate))
          } else {
            return(sprintf("%s", self$covariate))
          }
        } else if (engine == "rpem") {
          if (self$mode == "ab") {
            if (self$gtz) {
              return(sprintf("+%f, %f", self$min, self$max))
            } else {
              return(sprintf("%f, %f", self$min, self$max))
            }
          } else if (self$mode == "msd") {
            if (self$gtz) {
              return(sprintf("+%%%f, %f", self$mean, self$sd))
            } else {
              return(sprintf("%%%f, %f", self$mean, self$sd))
            }
          } else if (self$mode == "fixed") {
            if (self$constant) {
              return(sprintf("%f!", self$fixed))
            } else {
              return(sprintf("%f", self$fixed))
            }
          } else if (self$mode == "additive") {
            if (self$constant) {
              return(sprintf("L=%f!", self$additive))
            } else {
              return(sprintf("L=%f", self$additive))
            }
          } else if (self$mode == "proportional") {
            if (self$constant) {
              return(sprintf("G=%f!", self$proportional))
            } else {
              return(sprintf("G=%f", self$proportional))
            }
          }
        }
      }
    }
  )
)


# PM_model_list -----------------------------------------------------------


PM_model_list <- R6::R6Class("PM_model_list",
  inherit = PM_Vmodel,
  public = list(
    model_list = NULL,
    binary_path = NULL, # Path to the compiled model binary. Used by the rust backend.
    initialize = function(model_list) {
      # guarantees primary keys are lowercase and max first 3 characters
      orig_names <- names(model_list)
      names(model_list) <- private$lower3(names(model_list))
      model_blocks <- names(model_list)
      if (!identical(model_blocks, orig_names)) cat("Model block names standardized to 3 lowercase characters.\n")
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

      self$model_list <- model_list
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

      covs <- c()
      for (key in tolower(purrr::map_chr(self$model_list$cov, \(x) x$covariate))) {
        covs <- append(covs, sprintf("%s", key))
      }
      content <- gsub("</covs>", covs %>% paste(collapse = ","), content)

      seq <- self$model_list$sec %>% purrr::map(function(l) {
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
      }) # end line by line mapping of seq
      content <- gsub("</seq>", seq %>% paste(collapse = ""), content)

      eqs <- self$model_list$eqn %>% tolower()
      # look for xp() or dx[]
      if (length(eqs) == 0) {
        cli::cli_abort(c("x" = "PMcore does not support analytic equations, provide an {.code eqn} block."))
      } else {
        neqs <- sum(sapply(stringr::str_extract_all(eqs, "xp\\(\\d+\\)|dx\\[\\d+\\]"), function(x) length(x) > 0))
        content <- gsub("</neqs>", neqs, content)
      }

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

      lag <- ""
      for (line in self$model_list$lag %>% tolower()) {
        match <- stringr::str_match(line, "tlag\\((\\d+)\\)\\s*=\\s*(\\w+)")
        lag <- append(lag, sprintf("%i=>%s,", strtoi(match[2])-1, private$rust_up(match[3])))
      }
      # lag <- lag %>% purrr::map(\(l) private$rust_up(l))
      content <- gsub("</lag>", lag %>% paste0(collapse = ""), content)

      fa <- ""
      for (line in self$model_list$fa %>% tolower()) {
        match <- stringr::str_match(line, "fa\\((\\d+)\\)\\s*=\\s*(\\w+)")
        fa <- append(fa, sprintf("%i=>%s,", strtoi(match[2]), match[3]))
      }
      fa <- fa %>% purrr::map(\(l) private$rust_up(l))
      content <- gsub("</fa>", fa %>% paste0(collapse = ""), content)

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


      readr::write_file(content, file_name)
    },
    update = function(changes_list) {
      keys <- names(changes_list)
      if (!private$lower3(keys) %in% c("pri", "sec", "dif", "eqn", "ini", "cov", "lag", "bol", "out", "err", "fa", "ext")) {
        cli::cli_abort(c(
          "x" = "Invalid block name: {keys}",
          "i" = "See help for {.fn PM_model}."
        ))
      }
      self$model_list <- modifyList(self$model_list, changes_list)
    },
    #' @title Compile PM_model object
    #' @description
    #' Compiles a PM_model object using the Rust backend.
    #' 
    #' @details
    #' This function compiles a PM_model object into a binary format using the Rust backend.
    #' It writes the model to a temporary file, compiles it, and stores the path to the compiled binary.
    #' 
    #' @note
    #' This function can only be used with the Rust backend. If the backend is not set to "rust",
    #' an error will be thrown.
    #' 
    #' @examples
    #' \dontrun{
    #' model$compile()
    #' }
    #' 
    #' @export
    compile = function(){
      if (getPMoptions()$backend != "rust") {
        cli::cli_abort(c("x" = "This function can only be used with the rust backend."))
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
    #' @title Simulate One Scenario
    #' @description
    #' Simulates a single scenario using the provided data and parameter values.
    #'
    #' @param data A `PM_data` object containing the data for the simulation.
    #' @param spp A numeric vector representing the parameter values for the simulation.
    #'
    #' @details
    #' This function simulates a single scenario using the provided data and parameter values.
    #' It requires the data to be a `PM_data` object and the parameter values to be a numeric vector.
    #' The length of the parameter vector must match the number of parameters in the model.
    #' The function writes the data to a temporary CSV file and uses the Rust backend to perform the simulation.
    #' If the model is not already compiled, it will be compiled before the simulation.
    #' 
    #' If the data contains more than one scenario, only the first scenario will be used for the simulation.
    #'
    #' @return The result of the simulation.
    #'
    #' @examples
    #' \dontrun{
    #' data <- PM_data$new(...)
    #' spp <- c(1.0, 2.0, 3.0)
    #' result <- model$simulate_one(data, spp)
    #' }
    #'
    #' @export
    simulate_one = function(data, spp){
      if (!inherits(data, "PM_data")) {
        cli::cli_abort(c("x" = "Data must be a PM_data object."))
      }
      if (!is.numeric(spp) || !is.vector(spp)) {
        cli::cli_abort(c("x" = "spp must be a numeric vector."))
      }
      if (length(spp) != length(self$parameters())) {
        cli::cli_abort(c("x" = "spp must have the same length as the number of parameters."))
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
        sim <- simulate_one(temp_csv, self$binary_path, spp)
      } else {
        cli::cli_abort(c("x" = "This function can only be used with the rust backend."))
      }
      sim
    },
    #' @title Simulate All Scenarios
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
    #' @return The result of the simulation.
    #'
    #' @examples
    #' \dontrun{
    #' data <- PM_data$new(...)
    #' theta <- matrix(c(1.0, 20.0, 2.0, 70.0), nrow = 2, byrow = TRUE)
    #' result <- model$simulate_all(data, theta)
    #' }
    #'
    #' @export
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

      # deal with exponents - not bullet proof. Need to separate closing ) with space if not part of exponent
      #TODO: There is a bug here when the exponent is a variable not between parentheses
      pattern2 <- "\\*{2}\\(([^)]+)\\)|\\*{2}([\\d.]+)|\\^\\(([^)]+)\\)|\\^([\\d.]+)"
      replace2 <- "\\.powf\\(\\1\\2\\) " # will use first match if complex, second if simple
      .l <- gsub(pattern2, replace2, .l, perl = TRUE)

      # deal with integers, exclude [x] and alphax
      pattern3 <- "(?<![\\.\\w\\[])(\\d+)(?![\\.\\]\\d])"
      replace3 <- "\\1\\.0"
      .l <- gsub(pattern3, replace3, .l, perl = TRUE)

      # deal with if statements
      if_fix <- function(code, .l) {
        if (code == "if" | code == "else if") {
          pattern <- paste0("^&*", code, "(\\((?:[^)(]+|(?1))*+\\))")
          n_found <- regexpr(pattern = pattern, text = .l, perl = TRUE)
          if (n_found > -1) { # found something
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
      if (stringr::str_detect(.l2, stringr::regex(pattern4, ignore_case = TRUE))) {
        .l <- paste0("let ", .l)
      }

      return(.l)
    }, # end rust_up function
    lower3 = function(chr) {
      purrr::map_chr(chr, function(x) {
        substr(tolower(x), 1, 3)
      })
    },
    write_block = function(lines, key, block, engine) {
      if (private$lower3(key) == "fa") {
        key <- "f"
      }
      lines <- append(lines, sprintf("#%s", key))
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
    }
  )
)



# Read model.txt file -----------------------------------------------------

PM_model_file <- R6::R6Class("PM_model_file",
  inherit = PM_model_list,
  public = list(
    content = NULL,
    initialize = function(model_filename) {
      self$name <- basename(model_filename)[1]
      self$model_list <- private$makeR6model(model_filename)
      self$content <- readChar(model_filename, file.info(model_filename)$size)
    }
  ),
  private = list(
    makeR6model = function(file) {
      msg <- ""

      blocks <- parseBlocks(file) # this function is in PMutilities

      # check for reserved variable names
      reserved <- c(
        "ndim", "t", "x", "xp", "rpar", "ipar", "p", "r", "b", "npl", "numeqt", "ndrug", "nadd", "rateiv", "cv",
        "n", "nd", "ni", "nup", "nuic", "np", "nbcomp", "psym", "fa", "lag", "tin", "tout"
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

      # build model_list to be given to PM_model_list
      model_list <- list()
      # this function makes pri for PM_model
      model_list$pri <- sapply(strsplit(blocks$primVar, sep), function(x) {
        # find out if constrained to be positive
        const_pos <- any(grepl("\\+", x))
        if (const_pos) {
          x <- gsub("\\+", "", x)
          gtz <- TRUE
          msg <- c(msg, "Truncating variables to positive ranges is not recommended.\n
               Consider log transformation instead.\n")
        } else {
          gtz <- FALSE
        }

        # find out if constant
        const_var <- any(grepl("!", x))
        if (const_var) {
          x <- gsub("!", "", x)
        }

        values <- as.numeric(x[-1])

        if (length(x[-1]) == 1) { # fixed
          thisItem <- list(fixed(values[1], constant = const_var, gtz = gtz))
        } else { # range
          thisItem <- list(ab(values[1], values[2], gtz = gtz))
        }
        names(thisItem) <- x[1]
        thisItem
      }) # end sapply

      # covariates
      # process constant covariates
      covar <- blocks$covar
      const_covar <- grepl("!", covar) # returns boolean vector, length = nout
      covar <- gsub("!", "", covar) # remove "!"
      # cycle through covariates
      if (covar[1] != "") {
        covar_list <- list()
        for (i in 1:length(covar)) {
          covar_list[[i]] <- covariate(name = covar[i], constant = const_covar[i])
        }
      } else {
        covar_list <- NULL
      }
      # add to model_list
      model_list$cov <- covar_list

      # extra
      if (blocks$extra[1] != "") {
        model_list$ext <- blocks$extra
      }

      # secondary variables
      if (blocks$secVar[1] != "") {
        model_list$sec <- as.list(blocks$secVar)
      }

      # bioavailability
      if (blocks$f[1] != "") {
        model_list$fa <- as.list(blocks$f)
      }

      # bolus
      if (blocks$bol[1] != "") {
        model_list$bol <- as.list(blocks$bol)
      }

      # initial conditions
      if (blocks$ini[1] != "") {
        model_list$ini <- as.list(blocks$ini)
      }

      # lag time
      if (blocks$lag[1] != "") {
        model_list$lag <- as.list(blocks$lag)
      }

      # differential equations - legacy
      if (!is.null(blocks$diffeq) && blocks$diffeq[1] != "") {
        model_list$eqn <- as.list(blocks$diffeq)
      }

      # model equations - will eventually replace diffeq above
      if (blocks$eqn[1] != "") {
        model_list$eqn <- as.list(blocks$eqn)
      }

      # out/err
      n_outputLines <- length(blocks$output)
      outputLines <- grep("Y\\([[:digit:]]+\\)|Y\\[[[:digit:]]+\\]", blocks$output)
      if (length(outputLines) == 0) {
        return(list(status = -1, msg = "\nYou must have at least one output equation of the form 'Y[1] = ...'\n"))
      }
      otherLines <- (1:n_outputLines)[!(1:n_outputLines) %in% outputLines] # find other lines
      if (length(otherLines) > 0) {
        model_list$sec <- c(model_list$sec, blocks$output[otherLines]) # append to #sec block
      }
      output <- blocks$output[outputLines]
      remParen <- stringr::str_replace(output, regex("Y(?:\\[|\\()(\\d+)(?:\\]|\\))", ignore_case = TRUE), "Y\\1")
      diffeq <- stringr::str_split(remParen, "\\s*=\\s*")
      diffList <- sapply(diffeq, function(x) x[2])
      num_out <- length(diffList)

      err <- tolower(gsub("[[:space:]]", "", blocks$error))
      # process constant gamma/lambda
      gamma <- grepl("^g", err[1])
      const_gamlam <- grepl("!", err[1])
      gamlam_value <- as.numeric(stringr::str_match(err[1], "\\d+\\.?\\d*"))
      # process constant coefficients
      const_coeff <- grepl("!", err[-1]) # returns boolean vector, length = nout
      err <- gsub("!", "", err) # remove "!"


      out <- list()
      for (i in 1:num_out) {
        out[[i]] <- list(
          val = diffList[i],
          err = list(
            model = if ((1 + as.numeric(gamma)) == 1) {
              additive(gamlam_value, constant = const_gamlam)
            } else {
              proportional(gamlam_value, constant = const_gamlam)
            },
            assay = errorPoly(stringr::str_split(err[i + 1], ",")[[1]] %>% as.numeric(), const_coeff[i])
          )
        )
      }
      names(out) <- sapply(diffeq, function(x) x[1])
      model_list$out <- out

      cat(msg)
      flush.console()

      return(model_list)
    }
  ) # end private list
)




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
#' @param ... Not used.
#' @return Plots the object.
#' @author Markus Hovd, Julian Otalvaro, Michael Neely
#' @seealso [PM_model], [ggraph::ggraph()], [ggplot2::ggplot()]
#' @export
#' @examples
#' library(PmetricsData)
#' NPex$model$plot()
#' @family PMplots

plot.PM_model <- function(x, marker = TRUE, line = TRUE, explicit, implicit, ...) {
  if (!checkRequiredPackages("PmetricsApps", repos = "LAPKB/PmetricsApps", quietly = FALSE)) {
    return(invisible(NULL))
  }
  model <- x
  marker <- if (is.list(marker) || marker) {
    amendMarker(marker, default = list(color = "dodgerblue", size = 0.25, line = list(width = 0.5)))
  } else {
    FALSE
  }
  line <- if (is.list(line) || line) {
    amendLine(line, default = list(color = "black"))
  } else {
    FALSE
  }

  # Add equations for algebraic models
  if (is.null(model$model_list$eqn)) {
    key_vars <- c("ke", "v", "ka", "kcp", "kpc")
    pri <- names(model$model_list$pri)
    found_pri_keys <- key_vars %in% tolower(pri)

    if (!is.null(model$model_list$sec)) {
      found_sec_keys <- purrr::map_lgl(key_vars, \(x) stringr::str_detect(
        model$model_list$sec,
        stringr::regex(x, ignore_case = TRUE)
      ))
    } else {
      found_sec_keys <- rep(NA, 5)
    }
    found_keys <- key_vars[found_pri_keys | found_sec_keys] %>% na.exclude()
    model$model_list$eqn <- dplyr::case_when(
      all(found_keys %in% c("ke", "v")) ~ c(
        "dX[1] = RATEIV[1] - Ke*X[1]",
        NA,
        NA
      ),
      all(found_keys %in% c("ke", "v", "ka")) ~ c(
        "dX[1] = BOLUS[1] - Ka*X[1]",
        "dX[2] = RATEIV[1] + Ka*X[1] - Ke*X[2]",
        NA
      ),
      all(found_keys %in% c("ke", "v", "kcp", "kpc")) ~ c(
        "dX[1] = RATEIV[1] - (Ke+KCP)*X[1] + KPC*X[2]",
        "dX[2] = KCP*X[1] - KPC*X[2]",
        NA
      ),
      all(found_keys %in% c("ke", "v", "ka", "kcp", "kpc")) ~ c(
        "dX[1] = BOLUS[1] - Ka*X[1]",
        "dX[2] = RATEIV[1] + Ka*X[1] - (Ke+KCP)*X[2] + KPC*X[3]",
        "dX[3] = KCP*X[2] - KPC*X[3]"
      ),
      .size = 3
    ) %>% na.exclude()
  }

  # filter any equations that are not diffeq and make everything capital
  this_model <- model$model_list$eqn %>%
    map(purrr::keep, stringr::str_detect, stringr::regex("dX\\[\\d+\\]|XP\\(\\d+\\)", ignore_case = TRUE)) %>%
    unlist()

  tree <- parse(text = this_model)
  if (length(tree) == 0) {
    cli::cli_abort(c("x" = "No differential equations detected. Use {.code dX[i]} for changes and {.code X[i]} for amounts (case insensitive)."))
  }
  index <- 0

  parse_arrows <- function(tree, arrows = list()) {
    # browser()
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
      lhs <- parse(text = paste(sapply(2:nterms, function(x) as.character(lhs[[2]][[x]])),
        as.character(op),
        deparse(rhs),
        collapse = paste0(" ", as.character(lhs[[2]][[1]]), " ")
      ))[[1]]
      rhs <- ""
    }

    if (length(rhs) > 1 && rhs[[1]] == "(") {
      # expand distribution
      nterms <- length(rhs[[2]])
      rhs <- parse(text = paste(deparse(lhs),
        as.character(op),
        sapply(2:nterms, function(x) as.character(rhs[[2]][[x]])),
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
    # cat("index", index,"\n\nlhs= ",deparse(lhs),"\nrhs = ",deparse(rhs),"\nl = ",deparse(l),"\nr = ",deparse(r),"\ntree = ",deparse(tree),"\n________________\n")


    if (l == "x" || r == "x" || l == "X" || r == "X") {
      # cat("arrows before: ",paste0(as.character(arrows),collapse = ", "),"\n")
      arrows <- append(arrows, tree)
      # cat(deparse(tree), "appended\n")
      # cat("arrows after: ",paste0(as.character(arrows),collapse = ", "),"\n")

      return(arrows)
    }

    index <<- index + 1
    if (is.call(lhs)) { # cat("Calling from lhs...\n")
      arrows <- parse_arrows(lhs, arrows)
    }
    # cat("\nReturned lhs arrows: ", paste(as.character(arrows),collapse = ", "), "\n\n")

    if (is.call(rhs)) { # cat("Calling from rhs...\n")
      arrows <- parse_arrows(rhs, arrows)
    }
    # cat("\nReturned rhs arrows: ", paste(as.character(arrows),collapse = ", "), "\n\n")

    return(arrows)
  }

  parse_inputs <- function(input, itree) {
    itree <- paste(itree, collapse = "")
    if (grepl(input, itree, ignore.case = TRUE)) {
      type <- toupper(substr(input, 1, 1))
      number <- stringr::str_extract(
        itree,
        regex(paste0(input, "(\\(|\\[)\\d+(\\)|\\])"),
          ignore_case = TRUE
        )
      ) %>%
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
          nodes <- append(nodes, list(node = list(
            node = as.character(lhs),
            arrows = as.character(parse_arrows(rhs)),
            bolus = parse_inputs("bolus", deparse(itree)),
            rateiv = parse_inputs("rateiv", deparse(itree))
          )))
        } else { # only one equation
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
        arrows = unlist(purrr::map(x$arrows, ~ swap_if_needed(stringr::str_split_1(.x, " \\* ")))),
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
    dplyr::rename(
      to = from,
      from = to
    )

  # pause to define inputs
  input_cmt <- layout %>%
    dplyr::select(to, bolus, rateiv) %>%
    dplyr::filter(bolus != "" | rateiv != "") %>%
    distinct() %>%
    tidyr::pivot_longer(c(bolus, rateiv), names_to = "type", values_to = "input") %>%
    dplyr::select(-type) %>%
    dplyr::filter(input != "") %>%
    dplyr::rename(cmt = to)

  # resume layout
  layout <- layout %>%
    dplyr::select(-bolus, -rateiv) %>%
    dplyr::group_by(arrow) %>%
    dplyr::filter(n() == 1 | n() > 1 & from != "") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(from = ifelse(from == "", to, from)) %>%
    dplyr::mutate(to = ifelse(from == to, "", to)) %>%
    dplyr::mutate(to = ifelse(to == "", as.numeric(max(c(to, from), na.rm = T)) + 1, to)) %>%
    dplyr::mutate(to = ifelse(is.na(to), as.numeric(max(c(to, from), na.rm = T)) + 1, to)) %>%
    dplyr::filter(arrow != "") %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    dplyr::mutate(node = stringr::str_extract(node, "\\d+")) %>%
    dplyr::distinct(node, from, to) %>%
    dplyr::mutate(implicit = FALSE)

  # outputs
  if (!is.null(purrr::pluck(model, "model_list", "out", 1, "val"))) {
    cmts <- map_chr(model$model_list$out, ~ stringr::str_extract(.x$val, "\\d+"))
    output_cmt <- dplyr::tibble(out = paste0("Y", seq_along(cmts)), cmt = cmts)
  } else {
    output_cmt <- dplyr::tibble(out = "", cmt = "1")
  }

  # add explicit arrows from user
  if (!missing(explicit)) {
    max_to <- max(as.numeric(layout$to))

    if (!all(names(explicit) %in% c("from", "to"))) {
      cli::cli_abort(c("x" = "{.code explicit} should be a data frame with names {.code from} and {.code to}"))
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
      cli::cli_abort(c("x" = "{.code implicit} should be a data frame with names {.code from} and {.code to}"))
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
  if (!is.logical(marker)) { # will only be logical if FALSE
    g <- g +
      ggraph::geom_node_tile(aes(fill = position, linetype = position),
        color = marker$line$color,
        lwd = marker$line$width,
        width = marker$size, height = marker$size, alpha = marker$opacity
      ) +
      ggraph::geom_node_text(aes(label = input), nudge_x = .07, nudge_y = .05, color = "white") +
      ggraph::geom_node_text(aes(label = out), nudge_x = -.07, nudge_y = .05, color = "black") +
      ggplot2::scale_fill_manual(values = c(marker$color, "grey80"))
  }
  if (!is.logical(line)) { # will only be logical if FALSE
    g <- g +
      ggraph::geom_edge_fan(
        aes(linetype = as.numeric(implicit) + 1),
        arrow = grid::arrow(
          angle = 15, type = "closed",
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
  print(g)
  return(invisible(graph))
}
# convert_r_to_rust <- function(r_code) {
#   # Helper functions
#   add_float_notation <- function(text) {
#     # Add `.0_f64` to integers
#     gsub("\\b(\\d+)(?!\\.\\d*|\\w)", "\\1.0_f64", text, perl = TRUE)
#   }
  
#   convert_assignment <- function(text) {
#     # Replace `<-` or `=` with `=` for assignments
#     text <- gsub("<-|=", "=", text)
#     if (grepl("^\\s*[a-zA-Z][a-zA-Z0-9_]*\\s*=", text) && 
#         !grepl("^\\s*(if|else|for)", text)) {
#       text <- paste0("let ", text)
#     }
#     text
#   }
  
#   convert_operators <- function(text) {
#     # Convert `**` to `.powf()`
#     gsub("(\\w+|\\d+(?:\\.\\d+)?_f64?)\\s*\\*\\*\\s*(\\w+|\\d+(?:\\.\\d+)?_f64?)", 
#          "\\1.powf(\\2)", text)
#   }
  
#   convert_math_functions <- function(text) {
#     # Replace math functions (handles nested expressions)
#     text <- gsub("sqrt\\(([^)]+)\\)", "\\1.sqrt()", text)
#     text <- gsub("log\\(([^)]+)\\)", "\\1.ln()", text)
#     text
#   }
  
#   convert_control_structures <- function(text) {
#     # Convert for loop
#     text <- gsub("for\\s*\\((\\w+)\\s+in\\s+(\\d+):(\\d+)\\)", 
#                  "for \\1 in \\2.0_f64..\\3.0_f64", text)
#     # Convert if condition
#     text <- gsub("if\\s*\\((.+?)\\)", "if (\\1)", text)
#     text
#   }
  
#   add_semicolon <- function(text) {
#     # Add semicolon if it's a standalone expression or assignment
#     if (!grepl("[{}]$", text) && 
#         !grepl("^\\s*(if|else|for|while)", trimws(text))) {
#       paste0(text, ";")
#     } else {
#       text
#     }
#   }
  
#   process_line <- function(line) {
#     if (trimws(line) == "") return("")
    
#     line <- trimws(line)
#     line <- add_float_notation(line)
#     line <- convert_operators(line)
#     line <- convert_math_functions(line)
#     line <- convert_control_structures(line)
#     line <- convert_assignment(line)
#     line <- add_semicolon(line)
    
#     line
#   }
  
#   # Process each line
#   lines <- strsplit(r_code, "\n")[[1]]
#   processed <- vapply(lines, process_line, character(1))
#   processed <- processed[processed != ""]
  
#   # Add braces for control structures
#   processed <- gsub("if \\((.+?)\\)", "if (\\1) {", processed)
#   processed <- gsub("else$", "} else {", processed)
#   processed <- gsub("for (\\w+ in .+)", "\\1 {", processed)
#   processed <- gsub("}$", "}", processed) # Close braces
  
#   # Join processed lines
#   paste(processed, collapse = "\n")
# }

# r_code <- "
# a <- 3
# b = 4
# c <- a**b
# c1 <- 3.0**b
# c2 <- a**4.1
# if (c > 10) {
#   d <- sqrt(c)
# } else {
#   d <- log(c)
# }
# for (i in 1:10) {
#   print(i)
# }
# e = 3.14
# "

# cat(convert_r_to_rust(r_code))