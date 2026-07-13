# PM_parse ----------------------------------------------------------------


#' @title Write run configuration to settings.json
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes a `settings.json` file describing a fit's configuration. The Rust
#' backend writes the estimation artifacts (theta.csv, posterior.csv, pred.csv,
#' covs.csv, cycles.csv, result.json) but not the configuration file that the
#' Pmetrics output parsers ([PM_op], [PM_final], [PM_cycle], etc.) expect. This
#' helper reconstructs that file from the model and fit settings.
#' @param path Full path (including filename) to write the `settings.json` file.
#' @param param_ranges A named list mapping parameter names to `c(min, max)`
#' ranges, in parameter order.
#' @param error_models A list of `PM_err` objects, each carrying `type`,
#' `initial`, `coeff`, `fixed`, and `outeq`.
#' @param algorithm The fitting algorithm (e.g. "NPAG").
#' @param cycles Maximum number of cycles.
#' @param idelta Prediction interval used when writing outputs.
#' @param tad Additional time after the last event used when writing outputs.
#' @param prior The prior specification ("sobol" or "prior.csv").
#' @param points Number of Sobol grid points.
#' @param seed Random seed used to generate the Sobol grid.
#' @return Invisibly returns the path written.
#' @keywords internal
write_settings_json <- function(path, param_ranges, error_models, algorithm,
                                cycles, idelta, tad, prior, points, seed) {
  # Parameter declarations, preserving order.
  parameters <- lapply(names(param_ranges), function(nm) {
    r <- param_ranges[[nm]]
    list(name = nm, lower = r[1], upper = r[2])
  })

  # Error models, keyed by output slot. A leading "None" placeholder mirrors the
  # convention used by the parsers (`decode_error_model_rows`), so the error
  # model for output `outeq` occupies index `outeq` (1-based) in the array.
  n_out <- length(error_models)
  models <- vector("list", n_out + 1L)
  models[[1]] <- "None"
  for (e in error_models) {
    oq <- as.integer(e$outeq)
    coeff <- as.numeric(e$coeff)
    length(coeff) <- 4 # pad with NA if shorter; then replace NA with 0
    coeff[is.na(coeff)] <- 0
    poly <- list(c0 = coeff[1], c1 = coeff[2], c2 = coeff[3], c3 = coeff[4])
    value_kind <- if (isTRUE(e$fixed)) "Fixed" else "Variable"
    value <- stats::setNames(list(e$initial), value_kind)
    models[[oq + 1L]] <- if (identical(e$type, "additive")) {
      list(Additive = list(lambda = value, poly = poly))
    } else {
      list(Proportional = list(gamma = value, poly = poly))
    }
  }

  prior_json <- if (identical(prior, "sobol")) {
    list(Sobol = c(points, seed))
  } else {
    list(File = as.character(prior))
  }

  settings <- list(
    config = list(cycles = cycles, algorithm = algorithm, progress = TRUE),
    parameters = list(parameters = parameters),
    errormodels = list(models = models),
    predictions = list(idelta = idelta, tad = tad),
    prior = prior_json
  )

  jsonlite::write_json(settings, path = path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}


#' @title Parse Pmetrics output
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A flexible parser for Pmetrics output
#' @details
#' Currently written for the Rust implementation of NPAG
#' @param path The directory containing the output from the Rust-implementation of NPAG
#' @param fit The relative path to a "fit.rds" file, which is normal output after a fit containing the data and model used.
#' @param write If `TRUE` (default), saves the output as "PMout.Rdata" in the specified path.
#' @return The output of `PM_parse` is a list containing the following elements
#' * **op** Written to the standard of [PM_op()]
#' * **pop** Written to the standard of [PM_pop()]
#' * **post** Written to the standard of [PM_post()]
#' * **cycles** Written to the standard of [PM_cycle()]
#' * **final** Written to the standard of [PM_final()]
#' * **cov** Written to the standard of [PM_cov()]
#' @author Michael Neely and Markus Hovd
#' @export
#' @keywords internal

PM_parse <- function(path = ".", fit = "fit.rds", write = TRUE) {
  if (is.character(fit) && file.exists(file.path(path, "../inputs", fit))) {
    # fit is a character string pointing to a file, load it
    fit_object <- readRDS(file.path(path, "../inputs", fit))
  } else {
    # fit does not meet any of the above conditions, set to NULL
    fit_object <- NULL
  }


  if (!dir.exists(path)) {
    cli::cli_abort(c("x" = "The directory {.path {path}} does not exist."))
  }

  # assumes pred.csv and settings.json are in wd
  op <- rlang::try_fetch(PM_op$new(path = path),
    error = function(e) {
      cli::cli_warn("Unable to create {.cls PM_op} object", parent = e)
      return(NULL)
    }
  )

  # assumes theta.csv and posterior.csv are in wd
  final <- rlang::try_fetch(PM_final$new(path = path),
    error = function(e) {
      cli::cli_warn("Unable to create {.cls PM_final} object", parent = e)
      return(NULL)
    }
  )

  # assumes cycles.csv, and settings.json are in wd
  cycle <- rlang::try_fetch(PM_cycle$new(path = path),
    error = function(e) {
      cli::cli_warn("Unable to create {.cls PM_cycle} object", parent = e)
      return(NULL)
    }
  )

  # assumes pred.csv is in wd
  pop <- rlang::try_fetch(PM_pop$new(path = path),
    error = function(e) {
      cli::cli_warn("Unable to create {.cls PM_pop} object", parent = e)
      return(NULL)
    }
  )

  # assumes pred.csv is in wd
  post <- rlang::try_fetch(PM_post$new(path = path),
    error = function(e) {
      cli::cli_warn("Unable to create {.cls PM_post} object", parent = e)
      return(NULL)
    }
  )

  cov <- rlang::try_fetch(PM_cov$new(path = path),
    error = function(e) {
      cli::cli_warn("Unable to create {.cls PM_cov} object", parent = e)
      return(NULL)
    }
  )

  config <- rlang::try_fetch(jsonlite::fromJSON(suppressWarnings(readLines(file.path(path, "settings.json"), warn = FALSE))),
      error = function(e) {
        cli::cli_warn(c("!" = "Unable to read {.file {file.path(path, 'settings.json')}}"))
        return(NULL)
      }
    )

  core <- list(
    data = fit_object$data,
    model = fit_object$model,
    model_binary_path = if (!is.null(fit_object) && inherits(fit_object$model, "PM_model")) fit_object$model$binary_path else NULL,
    op = op,
    cov = cov,
    post = post,
    pop = pop,
    cycle = cycle,
    final = final,
    converge = cycle$data$converged,
    config = config,
    sys = {
      info <- as.list(Sys.info())
      info |> keep(names(info) %in% c("sysname", "machine")) |> paste(collapse = " ")
    }
  )

  class(core) <- "PM_result"


  if (write) {
    suppressWarnings(
      save(core, file = file.path(path, "PMout.Rdata"))
    )
  }

  return(invisible(core))
}
