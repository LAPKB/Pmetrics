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
  setwd(wd) #will be /outputs by default
  
  # assumes pred.csv, obs.csv, and settings.json are in wd
  op <- rlang::try_fetch(PM_op$new(),
                         error = function(e){
                           cli::cli_warn("Unable to create {.cls PM_op} object", parent = e)
                           return(NULL)
                         }
  )
  
  # assumes theta.csv and posterior.csv are in wd
  final <- rlang::try_fetch(PM_final$new(),
                            error = function(e){
                              cli::cli_warn("Unable to create {.cls PM_final} object", parent = e)
                              return(NULL)
                            }
  )
  
  # assumes cycles.csv, obs.csv, and settings.json are in wd
  cycle <- rlang::try_fetch(PM_cycle$new(),
                            error = function(e){
                              cli::cli_warn("Unable to create {.cls PM_cycle} object", parent = e)
                              return(NULL)
                            }
  )
  
  # assumes pred.csv is in wd
  pop <- rlang::try_fetch(PM_pop$new(),
                          error = function(e){
                            cli::cli_warn("Unable to create {.cls PM_pop} object", parent = e)
                            return(NULL)
                          }
  )
  
  # assumes pred.csv is in wd
  post <- rlang::try_fetch(PM_post$new(),
                           error = function(e){
                             cli::cli_warn("Unable to create {.cls PM_post} object", parent = e)
                             return(NULL)
                           }
  )
  
  cov <- rlang::try_fetch(PM_cov$new(),
                          error = function(e){
                            cli::cli_warn("Unable to create {.cls PM_cov} object", parent = e)
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
    config = rlang::try_fetch(jsonlite::fromJSON(suppressWarnings(readLines("settings.json", warn = FALSE))),
                              error = function(e){
                                cli::cli_warn(c("!" = "Unable to read {.file settings.json}"))
                                return(NULL)
                              }
    )
  )
  
  class(NPcore) <- "PM_result"
  
  
  if (write) {
    save(NPcore, file = "PMout.Rdata")
    return(invisible(NPcore))
  }
  
  setwd(cwd) #should be run folder
  return(NPcore)
}







# make_Config -------------------------------------------------------------

make_Config <- function(settings_file) {
  fromJSON(settings_file)
}
