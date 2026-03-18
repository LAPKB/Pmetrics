# PM_parse ----------------------------------------------------------------


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
  # Fix: accept a PM_fit object directly and tolerate a missing fit.rds file.
  if (inherits(fit, "PM_fit")) {
    fit_object <- fit
  } else if (is.character(fit) && file.exists(file.path(path, "../inputs", fit))) {
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

config <- rlang::try_fetch(
  jsonlite::fromJSON(suppressWarnings(readLines(file.path(path, "settings.json"), warn = FALSE))),
  error = function(e) {
    cli::cli_warn(c("!" = "Unable to read {.file {file.path(path, 'settings.json')}}"))
    return(NULL)
  }
)

# Fix: keep system info outside try_fetch so it is stored on the result, not passed as an argument.
sys_info <- as.list(Sys.info()) %>%
  purrr::keep(names(.) %in% c("sysname", "machine")) %>%
  paste(collapse = " ")

core <- list(
  data = if (is.null(fit_object)) NULL else fit_object$data,
  model = if (is.null(fit_object)) NULL else fit_object$model,
  op = op,
  cov = cov,
  post = post,
  pop = pop,
  cycle = cycle,
  final = final,
  backend = "rust",
  algorithm = "NPAG",
  numeqt = 1,
  # Fix: avoid dereferencing cycle$data when PM_cycle creation failed.
  converge = if (is.null(cycle) || is.null(cycle$data$converged)) NULL else cycle$data$converged,
  config = config,
  sys = sys_info
)

class(core) <- "PM_result"


if (write) {
  save(core, file = file.path(path, "PMout.Rdata"))
}

return(invisible(core))
}
