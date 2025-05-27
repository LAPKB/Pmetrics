PM_model <- R6::R6Class("PM_model",
  public = list(
    model_list = NULL,
    binary_path = NULL,
    file_content = NULL,
    type = NULL,
    initialize = function(pri, cov = NULL, eqn = NULL, sec = NULL, tem = NULL, lag = NULL, fa = NULL, ini = NULL, out = NULL, error = NULL) {
      # Primary parameters must be provided
      if (is.null(pri)) {
        cli::cli_abort(c(
          "x" = "Primary parameters are missing.",
          "i" = "Please provide a list of primary parameters."
        ))
      }

      # Can not use both eqn and sec/tem together
      if (!is.null(eqn)) {
        if (!is.null(sec) || !is.null(tem)) {
          cli::cli_abort(c(
            "x" = "Eqn and sec/tem cannot be used together.",
            "i" = "Please use either eqn or sec/tem, not both."
          ))
        }
      }

      # Either an ODE-based model or an analytical model must be provided
      if (is.null(eqn) && is.null(sec) && is.null(tem)) {
        cli::cli_abort(c(
          "x" = "No equations provided.",
          "i" = "Please provide either eqn or sec/tem."
        ))
      }

      if (!is.null(tem)) {
        if (!is.null(eqn)) {
          cli::cli_abort(c(
            "x" = "Eqn and tem cannot be used together.",
            "i" = "Please use either eqn or tem, not both."
          ))
        }
      }

      # Auxiliary variable to identify the type of execution
      type <- NULL

      if (!is.null(eqn)) type <- "ode"
      if (!is.null(sec) || !is.null(tem)) type <- "analytical"

      ## Get the names of the parameters
      parameters <- tolower(names(pri))
      covariates <- tolower(cov)
      # eqn
      if (type == "ode") {
        eqn <- transpile_ode(eqn, parameters, covariates)
      } else if (type == "analytical") {
        eqn <- NULL
      }

      if (type == "analytical") {
        if (!is.null(sec)) {
          sec <- transpile_sec(sec, parameters, covariates)
        } else {
          sec <- empty_sec()
        }
      } else if (type == "ode") {
        sec <- NULL
        tem <- NULL
      }

      if (!is.null(fa)) {
        fa <- transpile_fa(fa, parameters, covariates)
      } else {
        fa <- empty_fa()
      }

      if (!is.null(lag)) {
        lag <- transpile_lag(lag, parameters, covariates)
      } else {
        lag <- empty_lag()
      }

      if (!is.null(ini)) {
        ini <- transpile_ini(ini, parameters, covariates)
      } else {
        ini <- empty_ini()
      }

      if (!is.null(out)) {
        out <- transpile_out(out, parameters, covariates)
      } else {
        out <- empty_out()
      }

      # Number of equations
      n_eqn <- get_assignments(eqn, "dx")
      n_out <- get_assignments(out, "y")


      model_list <- list(
        eqn = eqn,
        pri = pri,
        sec = sec,
        tem = tem,
        lag = lag,
        fa = fa,
        ini = ini,
        out = out,
        neqs = n_eqn,
        nouteqs = n_out,
        parameters = parameters,
        covariates = covariates
      )
      self$model_list <- model_list
      self$type <- "closure"
    },
    write_model_file = function(file_path = "model.rs") {
      # Check if model_list is not NULL
      if (is.null(self$model_list)) {
        cli::cli_abort(c(
          "x" = "Model list is empty.",
          "i" = "Please provide a valid model list."
        ))
      }

      type <- "ode"


      if (type == "analytical") {
        base <- "equation::Analytical::new(
            <tem>,
            <sec>,
            <lag>,
            <fa>,
            <ini>,
            <out>,
            (<neqs>, <nouteqs>),
          )"
      } else if (type == "ode") {
        base <- "equation::ODE::new(
            <eqn>,
            <lag>,
            <fa>,
            <ini>,
            <out>,
            (<neqs>, <nouteqs>),
        )"
      } else {
        cli::cli_abort(c(
          "x" = "Invalid model type.",
          "i" = "Please provide a valid model type."
        ))
      }

      base <- gsub(pattern = "<eqn>", replacement = self$model_list$eqn, x = base)
      base <- gsub(pattern = "<tem>", replacement = self$model_list$tem, x = base)
      base <- gsub(pattern = "<sec>", replacement = self$model_list$sec, x = base)
      base <- gsub(pattern = "<lag>", replacement = self$model_list$lag, x = base)
      base <- gsub(pattern = "<fa>", replacement = self$model_list$eqn, x = base)
      base <- gsub(pattern = "<ini>", replacement = self$model_list$eqn, x = base)
      base <- gsub(pattern = "<out>", replacement = self$model_list$out, x = base)
      base <- gsub(pattern = "<neqs>", replacement = self$model_list$neqs, x = base)
      base <- gsub(pattern = "<nouteqs>", replacement = self$model_list$nouteqs, x = base)

      # Write the model to a file
      writeLines(base, file_path)
    },
    from_file = function(file_path) {
      self$model_list <- private$makeR6model(model_filename)
      self$content <- readChar(model_filename, file.info(model_filename)$size)
      self$type <- "file"
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
        } else if (x == "tem") {
          cat("\n", sp(1), "$tem\n", paste0(sp(2), "[1] \"", mlist$tem, "\"", collapse = "\n "))
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
    },
    write_rust = function() {

    }
  ),
  private = list(
    makeR6model = function(file) {
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
      } else {
        model_list$extra <- NULL
      }

      # secondary variables
      if (blocks$secVar[1] != "") {
        model_list$sec <- as.list(blocks$secVar)
      }

      # bioavailability
      if (blocks$f[1] != "") {
        model_list$fa <- as.list(blocks$f)
      } else {
        model_list$f <- NULL
      }

      # bolus
      if (blocks$bol[1] != "") {
        model_list$bol <- as.list(blocks$bol)
      } else {
        model_list$bol <- NULL
      }

      # initial conditions
      if (blocks$ini[1] != "") {
        model_list$ini <- as.list(blocks$ini)
      } else {
        model_list$ini <- NULL
      }

      # lag time
      if (blocks$lag[1] != "") {
        model_list$lag <- as.list(blocks$lag)
      } else {
        model_list$lag <- NULL
      }

      # analytic model name
      if (blocks$tem[1] != "") {
        model_list$tem <- blocks$tem
      } else {
        model_list$tem <- NULL
      }

      # differential equations - legacy
      if (!is.null(blocks$diffeq) && blocks$diffeq[1] != "") {
        model_list$eqn <- as.list(blocks$diffeq)
      } else {
        model_list$diffeq <- NULL
      }

      # model equations - will eventually replace diffeq above
      if (blocks$eqn[1] != "") {
        model_list$eqn <- as.list(blocks$eqn)
      } else {
        model_list$eqn <- NULL
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
      model_list <- PM_model_list$new(model_list)$model_list
      return(model_list)
    }
  )
)



# PM_input ----------------------------------------------------------------


# private classes
# This generates text which will be written to genmodel.txt
PM_input <- R6::R6Class(
  "PM_input",
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

##### These functions create various model components

#' @title Additive error model
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Create an additive (lambda) error model
#' @param add Initial value for lambda
#' @param constant Estimate if `FALSE` (default).
#' @export
additive <- function(add, constant = FALSE) {
  PM_input$new(add, add, "additive", constant)
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
  PM_input$new(prop, prop, "proportional", constant)
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
  PM_input$new(add, prop, "combination", constant)
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
  PM_input$new(coeffs, NULL, "coefficients", constant)
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
  PM_input$new(min, max, "ab", constant = FALSE, gtz)
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
  PM_input$new(mean, sd, "msd", constant = FALSE, gtz)
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
  PM_input$new(fixed, fixed, "fixed", constant, gtz)
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
  PM_input$new(name, mode = "covariate", constant = constant)
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
#' @param ... Not used.
#' @return Plots the object.
#' @author Markus Hovd, Julian Otalvaro, Michael Neely
#' @seealso [PM_model], [ggraph::ggraph()], [ggplot2::ggplot()]
#' @export
#' @examples
#' \dontrun{
#' NPex$model$plot()
#' }
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
      "one_comp_iv", "advan1\nadvan1-trans1", "One compartment IV input, Ke", "1 = Central", "Ke, V",
      "one_comp_iv_cl", "advan1\nadvan1-trans2", "One compartment IV input, CL", "1 = Central", "CL, V",
      "two_comp_bolus", "advan2\nadvan2-trans1", "Two compartment bolus input, Ke", "1 = Bolus\n2 = Central", "Ka, Ke, V",
      "two_comp_bolus_cl", "advan2\nadvan2-trans2", "Two compartment bolus input, CL", "1 = Bolus\n2 = Central", "Ka, CL, V",
      "two_comp_iv", "advan3\nadvan3-trans1", "Two compartment IV input, Ke", "1 = Central\n2 = Peripheral", "Ke, V, KCP, KPC",
      "two_comp_iv_cl", "advan3\nadvan3-trans4", "Two compartment IV input, CL", "1 = Central\n2 = Peripheral", "CL, V1, Q, V2",
      "three_comp_bolus", "advan4\nadvan4-trans1", "Three compartment bolus input, Ke", "1 = Bolus\n2 = Central\n3 = Peripheral", "Ka, Ke, V, KCP, KPC",
      "three_comp_bolus_cl", "advan4\nadvan4-trans4", "Three compartment bolus input, CL", "1 = Bolus\n2 = Central\n3 = Peripheral", "Ka, CL, V2, Q, V3"
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
    print(mod_table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ODE = paste(unlist(ODE), collapse = "\n")) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(ODE = "Corresponding ODE") %>%
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
    print(mod_table %>% dplyr::filter(`Primary Name` == name) %>%
      dplyr::mutate(ODE = paste(unlist(ODE), collapse = "\n")) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(ODE = "Corresponding ODE") %>%
      flextable::autofit())
  }

  return(invisible(mod_table %>% dplyr::filter(`Primary Name` == name) %>% dplyr::select(ODE) %>% purrr::pluck(1, 1) %>% unlist()))
}
