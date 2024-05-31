# PM_parse ----------------------------------------------------------------


#' @title Parse Pmetrics output
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A flexible parser for Pmetrics output
#' @details
#' Currently written for the Rust implementation of NPAG
#' @param wd The directory containing the output from the Rust-implementation of NPAG
#' @param write A logical value indicating if the results should be returned (`FALSE`, default) or written to disk (`TRUE`)
#' @param fit Either a \code{PM_fit} object or the absolute path to a "fit.Rdata"
#' @return The output of \code{PM_parse} is a list containing the following elements
#' \item{op }{Written to the standard of PM_op}
#' \item{pop }{Written to the standard of PM_pop}
#' \item{post }{Written to the standard of PM_post}
#' \item{cycles }{Written to the standard of PM_cycle}
#' \item{final }{Written to the standard of PM_final}
#' \item{cov }{Written to the standard of PM_cov}
#'
#' @seealso \code{\link{NPparse}}
#' @import dplyr
#' @import tidyr
#' @importFrom dplyr select rename mutate relocate left_join case_when first across
#' @importFrom jsonlite fromJSON
#' @export

PM_parse <- function(wd = getwd(), fit = "fit.Rdata", write = TRUE) {
  # Default paths
  # pred_file <- paste(wd, "pred.csv", sep = "/")
  # obs_file <- paste(wd, "obs.csv", sep = "/")
  # config_file <- paste(wd, "settings.json", sep = "/")
  # cycle_file <- paste(wd, "cycles.csv", sep = "/")
  # theta_file <- paste(wd, "theta.csv", sep = "/")
  # post_file <- paste(wd, "posterior.csv", sep = "/")
  
  if (inherits(fit, "PM_fit")) {
    # fit is a PM_fit object, use it directly
    fit_object <- fit
  } else if (is.character(fit) && file.exists(fit)) {
    # fit is a character string pointing to a file, load it
    fit_object <- get(load(fit))
  } else {
    # fit does not meet any of the above conditions, set to NULL
    fit_object <- NULL
  }
  
  cwd <- getwd()
  setwd(wd)
  
  # assumes pred.csv, obs.csv, and settings.json are in wd
  tryCatch({ op <- PM_op$new() },
           error = function(e) {
             e <- NULL
             cat(
               crayon::red("Error:"),
               "There was an error parsing pred.csv, obs.csv, and/or settings.json for the observed/predicted information.\n"
             )
             return(NULL)
           }
  )
  
  # assumes theta.csv and posterior.csv are in wd
  tryCatch({ final <- PM_final$new() },
           error = function(e) {
             e <- NULL
             cat(
               crayon::red("Error:"),
               "There was an error parsing theta.csv and/or posterior.csv for the final parameter information.\n"
             )
             return(NULL)
           }
  )
  
  # assumes cycles.csv, obs.csv, and settings.json are in wd
  tryCatch({ cycle <- PM_cycle$new() }, 
           error = function(e) {
             e <- NULL
             cat(
               crayon::red("Error:"),
               "There was an error parsing cycles.csv, obs.csv, and/or settings.json for cycle information.\n"
             )
             return(NULL)
           }
  )
  
  # assumes pred.csv is in wd
  tryCatch({ pop <- PM_pop$new() }, 
           error = function(e) {
             e <- NULL
             cat(
               crayon::red("Error:"),
               "There was an error parsing pred.csv for population predictions\n"
             )
             return(NULL)
           }
  )
  
  # assumes pred.csv is in wd
  tryCatch({ post <- PM_post$new() }, 
           error = function(e) {
             e <- NULL
             cat(
               crayon::red("Error:"),
               "There was an error parsing pred.csv for posterior predictions\n"
             )
             return(NULL)
           }
  )  
  
  
  cov <- NULL
  tryCatch({ 
    if (!is.null(fit)) {
      cov <- PM_cov$new(list(final = final, data = fit_object$data))
    } 
  }, 
  error = function(e) {
    e <- NULL
    cat(
      crayon::red("Error:"),
      "There was an error parsing parameters and data for covariate-parameter relationships.\n"
    )
    return(NULL)
  }
  ) 

  
  NPcore <- list(
    data = fit_object$data,
    model = fit_object$model,
    op = op,
    cov = cov,
    post = post,
    pop = pop,
    cycle = cycle,
    final = final,
    backend = "rust",
    algorithm = "NPAG",
    numeqt = 1,
    converge = cycle$converged,
    config = fromJSON(readLines("settings.json", warn = FALSE))
  )
  
  class(NPcore) <- "PM_result"
  setwd(cwd)
  
  if (write) {
    save(NPcore, file = "PMout.Rdata")
    return(invisible(NPcore))
  }
  

  return(NPcore)
}







# make_Config -------------------------------------------------------------

make_Config <- function(settings_file) {
  fromJSON(settings_file)
}
