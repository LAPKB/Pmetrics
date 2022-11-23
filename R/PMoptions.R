#' Get user options for Pmetrics
#'
#' This function will get user options for Pmetrics.  Current user options are
#' \itemize{
#' \item sep Field separator in data files
#' \item dec Decimal separator in numbers
#' }
#'
#' @title Get Pmetrics User Options
#' @param opt The option to retrieve.  If omitted, all option values will be returned.
#' @return The user options file will be updated.  This will persist from session to session.
#' @author Michael Neely
#' @export

getPMoptions <- function(opt) {
  # options file name
  PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")
  # if it doesn't exist, create it with defaults
  if (!file.exists(PMoptionsFile)) {
    loc <- substr(Sys.getlocale("LC_TIME"), 1, 2) # get system language
    locales <- NULL
    data(locales, envir = environment())
    language <- locales$language[which(locales$iso639_2 == loc)]
    cat(paste0("Based on system, setting default langauge to ", language))

    gfortran_command <- if (OS == 1 && isM1()) {
      "/opt/homebrew/bin/gfortran"
    } else {
      "gfortran"
    }

    PMopts <-
      list(
        sep = ",",
        dec = ".",
        lang = loc,
        compilation_statements = c(
          sprintf("%s -march=native -w -O3 -o <exec> <files>", gfortran_command),
          sprintf("%s -march=native -w -fopenmp -fmax-stack-var-size=32768 -O3 -o <exec> <files>", gfortran_command)
        ),
        server_address = "http://localhost:5000",
        op_stats = T
      )
    options(PMopts)
    jsonlite::write_json(PMopts, path = PMoptionsFile, auto_unbox = T)
  }
  # read the options file
  PMopts <- jsonlite::read_json(path = PMoptionsFile)
  if (missing(opt)) {
    return(PMopts)
  } else {
    index <- which(names(PMopts) == opt)
    if (length(index) == 0) {
      return(NULL)
    } else {
      return(PMopts[[index]])
    }
  }
}

#' Set user options for Pmetrics
#'
#' This function will set user options for Pmetrics. When the package is first installed,
#' it will obtain the user's locale from system information and set the appropriate
#' language.
#'
#' @title Set Pmetrics User Options
#' @param sep The field separator character; \dQuote{,} by default, but could be \dQuote{;}
#' @param dec The decimal separator character; \dQuote{.} by default, but could be \dQuote{,}
#' @param server_address Specify address of server for remote runs.  Server must be set up separately.
#' This functionality is coming soon.
#' @param compilation_statements a vector with tho string elements that defines the compilation arguments for
#' single thread and parallel executions. Custom compile commands should be entered using '<exec>' as a placeholder
#' for the executable filename, and '<files>' as a placeholder for the files to be linked and compiled.
#' Example: gfortran -O3 -o <exec> <files>.
#' If a simgle compilation statement is provided, it will be used for both kind of compilations.
#' @param op_stats Argument to include/format regression statistics in observed vs. predicted plots.
#' Default is `FALSE`, but could either be set to `TRUE` for default format or a list of format options.
#' The default format when set to `TRUE` is
#' `list(x= 0.4, y = 0.1, bold = F, font = list(color = "black", family = "Arial", size = 8))`
#' @return The user preferences file will be updated.  This will persist from session to session.
#' @author Michael Neely
#' @export

setPMoptions <- function(sep, dec, server_address, compilation_statements, op_stats) {
  # read old values first
  PMopts <- getPMoptions()
  # update/add options
  if (!missing(sep)) PMopts$sep <- sep
  if (!missing(dec)) PMopts$dec <- dec
  if (!missing(server_address)) PMopts$server_address <- server_address
  if (!missing(compilation_statements)) PMopts$compilation_statements <- compilation_statements
  if (!missing(op_stats)) PMopts$op_stats <- op_stats
  PMopts$lang <- substr(Sys.getlocale("LC_TIME"), 1, 2)
  # set the options
  options(PMopts)

  # store the options
  PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")
  jsonlite::write_json(PMopts, path = PMoptionsFile, auto_unbox = T)
}
