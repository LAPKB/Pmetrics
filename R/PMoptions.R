#' @title Get Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Get user options for Pmetrics
#' @details
#' This function will get user options for Pmetrics.
#'
#' @param opt The option to retrieve.  If omitted, all option values will be returned.
#' @param warn Warn if options file doesn't exist. Default `TRUE`.
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return A list with the current options.
#' @author Michael Neely
#' @export

getPMoptions <- function(opt, warn = T, quiet = F) {
  # check for existing options
  opt_dir <- dplyr::case_when(
    getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
  )

  if (dir.exists(opt_dir)) { # external options file exists
    PMoptionsFile <- file.path(opt_dir, "PMoptions.json")
  } else { # external options file does not exist
    PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")
  }


  # if it doesn't exist, warn and exit
  if (!file.exists(PMoptionsFile)) {
    if (warn & !quiet) cat("Run setPMoptions() to create a Pmetrics options file.\n")
    return(invisible(-1))
  }

  # read the options file
  PMopts <- jsonlite::read_json(path = PMoptionsFile, simplifyVector = T)
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

#' @title Set Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Set user options for Pmetrics
#' @details
#' When the Pmetrics package is first loaded with `library(Pmetrics)`,
#' this function will be called. It will obtain
#' the user's locale from system information and set the appropriate
#' language. It will look for a *PMoptions.json* file in a hidden folder outside
#' of the Pmetrics package. If that does not exist, it will offer to create this for you.
#' With this choice, your options will persist when Pmetrics is updated. If you choose
#' to store the options within the package architecture, your options will be erased
#' every time you update Pmetrics. After you make your choice, you will not be presented
#' with that choice again. However, if you wish to later move your options from the internal
#' location to the external one, use [movePMoptions].
#'
#' The function will obtain Pmetrics user options from the *PMoptions.json*
#' file by calling [getPMoptions], set them for the session and update any
#' missing options with default values.
#'
#' @param sep The field separator character; "," by default, but could be ";" or another separator.
#' @param dec The decimal separator character; "." by default, but could be "," for example.
#' @param compilation_statements a vector with tho string elements that defines the compilation arguments for
#' single thread and parallel executions. Custom compile commands should be entered using `<exec>` as a placeholder
#' for the executable filename, and `<files>` as a placeholder for the files to be linked and compiled.
#' Example: `gfortran -O3 -o <exec> <files>`.
#' If a single compilation statement is provided, it will be used for both kind of compilations.
#' @param backend Name of compiler to use. Default is "fortran" (for now) but can be "rust" to use rust compiler.
#' Currently, only NPAG is available in rust.
#' @param rust_template Only used if `backend` is set to "rust".
#' @param report_template Format of the plots included in the summary report presented at the end of a run.
#' Default is to use "plotly", but can be set to "ggplot".
#' @param gfortran_path Path to gfortran compiler.
# #' @param func_defaults Change the default argument value for any Pmetrics function or method.
#' `func_defaults` should be a list, with each item itself a list of function name = list(arg1 = value, arg2 = value,...).
#' For example: `func_defaults = list(plot.PM_op = list(stats = T, marker = list(color = "red")), qgrowth = list(percentile = 50))`.
#' This is an incredibly powerful tool to customize your Pmetrics experience. If you store your
#' options in the external location (recommended), these custom argument values will persist
#' when Pmetrics is updated, as long as the argument is unchanged in the function. You can
#' only change Pmetrics function arguments, as this is a very powerful tool that can mess up your
#' R installation if used carelessly. You can add new argument defaults to the same function at a
#' later time, as old entries will not be erased. To restore original defaults, either change
#' it back to the original value or use [editPMoptions] to directly edit the JSON file and delete
#' the entries no longer desired. Use caution with this, as a malformed file will cause errors.
#' To remove all defaults the JSON file should contain `"func_defaults": {}`. After saving the file,
#' you will need to [base::detach()] the Pmetrics package and reload it with [base::library()]
#' for the changes to take effect.
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return The user preferences file will be updated.  This will persist from session to session
#' and if stored in the external location, through Pmetrics versions.
#' @author Michael Neely
#' @export

setPMoptions <- function(sep, dec, server_address, compilation_statements,
                         backend, rust_template, report_template,
                         gfortran_path,
                         #func_defaults, 
                         quiet = F) {
  # read old values first
  PMopts <- getPMoptions(warn = F)

  # set defaults
  loc <- substr(Sys.getlocale("LC_TIME"), 1, 2) # get system language

  defaultOpts <- list(
    sep = ",",
    dec = ".",
    lang = loc,
    compilation_statements = c(
      sprintf("%s -march=native -w -O3 -o <exec> <files>", PMopts$gfortran_path),
      sprintf("%s -march=native -w -fopenmp -fmax-stack-var-size=32768 -O3 -o <exec> <files>", PMopts$gfortran_path)
    ),
    server_address = "http://localhost:5000",
    backend = "fortran",
    rust_template = NULL,
    report_template = "plotly",
    gfortran_path = if (getOS() == 1 && isM1()) {
      "/opt/homebrew/bin/gfortran"
    } else {
      "gfortran"
    }
#    func_defaults = NULL
  )


  # missing so create
  if (PMopts[[1]] == -1) {
    PMopts <- defaultOpts
  }

  if (!identical(loc, PMopts$lang)) {
    language <- locales$language[which(locales$iso639_2 == loc)]
    if (!quiet) cat(paste0("Language has changed. Based on system, setting default language to ", language, "."))
  }

  # add missing defaults
  PMopts <- utils::modifyList(defaultOpts, PMopts)

  # update user values
  if (!missing(sep)) PMopts$sep <- sep
  if (!missing(dec)) PMopts$dec <- dec
  PMopts$lang <- loc
  if (!missing(compilation_statements)) {
    if (length(compilation_statements) == 1) {
      PMopts$compilation_statements <- rep(compilation_statements, 2)
    } else {
      PMopts$compilation_statements <- compilation_statements
    }
  }
  if (!missing(backend)) PMopts$backend <- backend
  if (!missing(rust_template)) PMopts$rust_template <- rust_template
  if (!missing(report_template)) PMopts$report_template <- report_template
  # if (!missing(func_defaults)) {
  #   if (is.null(PMopts$func_defaults)) { # not previously defined
  #     PMopts$func_defaults <- func_defaults
  #   } else {
  #     PMopts$func_defaults <- utils::modifyList(PMopts$func_defaults, func_defaults)
  #   }
  # }
  if (!missing(gfortran_path)) {
    PMopts$gfortran_path <- gfortran_path
    PMopts$compilation_statements <- c(
      sprintf("%s -march=native -w -O3 -o <exec> <files>", PMopts$gfortran_path),
      sprintf("%s -march=native -w -fopenmp -fmax-stack-var-size=32768 -O3 -o <exec> <files>", PMopts$gfortran_path)
    )
  }

  # set the options for everything except func_defaults...
  options(purrr::keep(PMopts, names(PMopts) != "func_defaults"))

  # ...which are handled through updateArgs
  # updateArgs(PMopts$func_defaults)

  # store the options
  opt_dir <- dplyr::case_when(
    getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
  )

  if (dir.exists(opt_dir)) { # user has elected to save options outside Pmetrics in the past
    jsonlite::write_json(PMopts, path = file.path(opt_dir, "PMoptions.json"), pretty = T)
  } else { # external options folder does not exist, so...
    PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")

    if (file.exists(PMoptionsFile)) { # ...if package options file exists, write to it
      jsonlite::write_json(PMopts, path = PMoptionsFile, pretty = T)
    } else { # package options file does not exist
      if (!quiet) {
        cat(
          "Pmetrics can store all your options and function defaults in a folder outside the package.\n",
          "This allows them to persist across installations of differing Pmetrics versions.\n"
        )
        cat(paste0(
          "On your system, Pmetrics will place a PMoptions.json file in ", opt_dir,
          ", which is a hidden folder.\n\n"
        ))
        cat(
          paste0(
            "Enter ", crayon::red("<1>"), " to write to the external folder (persistent) or ",
            crayon::red("<2>"), " to write to the Pmetrics package (destroyed with new package version).\n"
          ),
          "You will not be asked this again with this Pmetrics version, but see help for setPMoptions().\n"
        )
        ans <- ""
        while (ans != "1" & ans != "2") {
          ans <- readline("Response: ")
        }
      } else { # default is to proceed with external if quiet = T
        ans <- 1
      }
      if (ans == "1") { # write to external
        dir.create(opt_dir)
        jsonlite::write_json(PMopts, path = file.path(opt_dir, "PMoptions.json"), pretty = T)
      } else { # write to internal
        jsonlite::write_json(PMopts, path = PMoptionsFile, auto_unbox = T)
      }
    }
  }
}

#' Move user options file for Pmetrics
#'
#' This function will move user options file for Pmetrics from internal to external
#' location so that options can persist when the Pmetrics package is updated.
#'
#' @title Move Pmetrics User Options
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return NULL, invisibly.
#' @author Michael Neely
#' @export
movePMoptions <- function(quiet = F) {
  original_options <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")

  if (!file.exists(original_options)) {
    cat(paste0(original_options, " does not exist. Move aborted.\n"))
    return(invisible(NULL))
  }

  # store the options
  opt_dir <- dplyr::case_when(
    getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
  )

  if (!quiet) {
    cat(paste0(crayon::red("WARNING: "), "This will move internal saved options to ", opt_dir, ".\n"))
    cat("Pmetrics will now save and use options from that folder.\n")
  }

  destination_options <- file.path(opt_dir, "PMoptions.json")
  if (file.exists(destination_options) & !quiet) {
    cat(crayon::blue("You already have a PMoptions.json file in that folder. It will be overwritten.\n"))
  }

  if (!quiet) {
    cat(paste0(
      "Enter ", crayon::red("<1>"), " to proceed or ",
      crayon::red("<2>"), " to abort.\n"
    ))
    ans <- ""
    while (ans != "1" & ans != "2") {
      ans <- readline("Response: ")
    }
  } else { # default when quiet = T
    ans <- 1
  }
  if (ans == "2") {
    cat("Aborting PMoptions copy.\n")
    return(invisible(NULL))
  }

  if (!dir.exists(opt_dir)) {
    dir.create(opt_dir)
    if (!quiet) cat(paste0(opt_dir, " has been created.\n"))
  }

  file.copy(original_options, destination_options, overwrite = T)
  file.remove(original_options)
  if (!quiet) cat(paste0(destination_options, " written.\n"))
  return(invisible(NULL))
}

#' Edit user options for Pmetrics
#'
#' This function will open the user options file for Pmetrics in the system
#' default editor for JSON. Use **caution** directly editing the file to avoid errors.
#' This is the easiest way to remove specific Pmetrics function argument overrides.
#' See further details in the `func_defaults` argument to [setPMoptions].
#'
#' @title Edit Pmetrics User Options
#' @return A list with the current options.
#' @author Michael Neely
#' @export
editPMoptions <- function() {
  opt_dir <- dplyr::case_when(
    getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
  )

  if (dir.exists(opt_dir)) { # external options file exists
    PMoptionsFile <- file.path(opt_dir, "PMoptions.json")
  } else { # external options file does not exist
    PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")
  }


  # if it doesn't exist, warn and exit
  if (!file.exists(PMoptionsFile)) {
    cat("Run setPMoptions() to create a Pmetrics options file.\n")
    return(invisible(-1))
  }
  system2("open", PMoptionsFile)
  return(invisible(NULL))
}


# updateArgs <- function(args) {
#   funcs <- names(args)
# 
#   for (i in seq(funcs)) {
#     # check if function is in Pmetrics
#     e <- tryCatch(environment(getFromNamespace(funcs[i], ns = "Pmetrics")),
#       error = function(e) NA
#     )
#     if (!is.environment(e)) next # move to next one
# 
#     # check if S3 method
#     S3method <- isS3method(funcs[i], envir = e)
# 
#     # obtain the function
#     f <- getFromNamespace(funcs[i], ns = "Pmetrics")
# 
#     # obtain the arguments
#     fargs <- formals(f)
# 
#     # update the arguments
#     fargs <- utils::modifyList(fargs, args[[i]])
# 
#     # create new function
#     formals(f) <- fargs
# 
#     if (S3method) {
#       assignInNamespace(funcs[i], f, ns = "Pmetrics")
#     } else {
#       locked <- bindingIsLocked(funcs[i], e)
#       if (locked) {
#         unlockBinding(funcs[i], e)
#       }
#       assign(funcs[i], f, e) # update in package
#       assign(funcs[i], f, .GlobalEnv) # also in global
#       if (locked) {
#         lockBinding(funcs[i], e)
#       }
#     }
#   } # end for loop
# } # end updateArgs
