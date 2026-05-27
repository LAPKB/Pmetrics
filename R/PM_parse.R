# PM_parse ----------------------------------------------------------------

# nolint start


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

load_pm_parse_fit_object <- function(path, fit = "fit.rds") {
  if (!is.character(fit) || length(fit) != 1) {
    return(NULL)
  }

  fit_path <- normalizePath(file.path(path, "..", "inputs", fit), mustWork = FALSE)
  if (!file.exists(fit_path)) {
    return(NULL)
  }

  tryCatch(readRDS(fit_path), error = function(e) NULL)
}

PM_parse <- function(path = ".", fit = "fit.rds", write = TRUE) {
  fit_object <- load_pm_parse_fit_object(path, fit = fit)


  if (!dir.exists(path)) {
    cli::cli_abort(c("x" = "The directory {.path {path}} does not exist."))
  }

  fit_payload <- read_fit_payload_from_outputs(path)

  op <- PM_op$new(fit_payload, path = path)
  final <- PM_final$new(fit_payload, path = path)
  cycle <- PM_cycle$new(fit_payload, path = path)
  pop <- PM_pop$new(fit_payload, path = path)
  post <- PM_post$new(fit_payload, path = path)
  cov <- PM_cov$new(fit_payload, path = path)

  core <- list(
    data = fit_object$data,
    model = fit_object$model,
    op = op,
    cov = cov,
    post = post,
    pop = pop,
    cycle = cycle,
    final = final,
    converge = cycle$data$converged,
    config = fit_payload$config,
    sys = {
      info <- as.list(Sys.info())
      info |>
        keep(names(info) %in% c("sysname", "machine")) |>
        paste(collapse = " ")
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

# nolint end
