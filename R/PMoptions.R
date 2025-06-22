#' @title Get Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Get user options for Pmetrics
#' @details
#' This function will get user options for Pmetrics. It will look for a *PMoptions.json* file
#' in a hidden folder outside of the Pmetrics package. If that does not exist,
#' it will look for a default options file in the package options folder. See [setPMoptions] for 
#' details on where the options file is stored and how to set options.
#'
#' @param opt The option to retrieve.  If omitted, all option values will be returned.
#' @param warn Warn if options file doesn't exist. Default `TRUE`.
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return A list with the current options.
#' @author Michael Neely
#' @export

getPMoptions <- function(opt, warn = TRUE, quiet = FALSE) {
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
    if (warn & !quiet) cli::cli_inform("Run {.help setPMoptions} to create a Pmetrics options file.")
    return(invisible(-1))
  }
  
  # read the options file
  PMopts <- jsonlite::read_json(path = PMoptionsFile, simplifyVector = TRUE)
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
#' @param rust_template Only used if `backend` is set to "rust".
#' @param report_template Format of the plots included in the summary report presented at the end of a run.
#' Default is to use "plotly", but can be set to "ggplot".
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return The user preferences file will be updated.  This will persist from session to session
#' and if stored in the external location, through Pmetrics versions.
#' @author Michael Neely
#' @export

setPMoptions <- function(sep, dec, rust_template, report_template, quiet = FALSE) {
  # read old values first
  PMopts <- getPMoptions(warn = FALSE)
  
  # set defaults
  loc <- substr(Sys.getlocale("LC_TIME"), 1, 2) # get system language
  
  defaultOpts <- list(
    sep = ",",
    dec = ".",
    lang = loc,
    rust_template = NULL,
    report_template = "plotly"
  )
  
  
  # missing so create
  if (PMopts[[1]] == -1) {
    PMopts <- defaultOpts
  }
  
  # add missing defaults
  PMopts <- utils::modifyList(defaultOpts, PMopts)
  
  # update user values
  if (!missing(sep)) PMopts$sep <- sep
  if (!missing(dec)) PMopts$dec <- dec
  PMopts$lang <- loc
  PMopts$backend <- "rust"
  if (!missing(rust_template)) PMopts$rust_template <- rust_template
  if (!missing(report_template)) PMopts$report_template <- report_template
  
  # set the options for everything except func_defaults...
  options(names(PMopts))
  
  
  # store the options
  opt_dir <- dplyr::case_when(
    getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
    getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
  )
  
  if (dir.exists(opt_dir)) { # user has elected to save options outside Pmetrics in the past
    jsonlite::write_json(PMopts, path = file.path(opt_dir, "PMoptions.json"), pretty = TRUE)
  } else { # external options folder does not exist, so...
    PMoptionsFile <- paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/")
    
    if (file.exists(PMoptionsFile)) { # ...if package options file exists, write to it
      jsonlite::write_json(PMopts, path = PMoptionsFile, pretty = TRUE)
    } else { # package options file does not exist
      if (!quiet) {
        cli::cli_div(theme = list(
          span.red = list(color = "red", "font-weight" = "bold"),          
          span.blue = list(color = "blue", "font-style" = "italic")
        ))
        cli::cli_text(c(
          "{.strong Pmetrics} can store all your options in {.blue {paste(opt_dir, 'PMoptions.json', sep = '/')}}. ",
          "This allows them to persist across installations of differing package versions. ",
          "\n\nChoose one of the following options:\n\n"))
          cli::cli_text("{.red 1} to write to the external folder (persistent)")
          cli::cli_text("{.red 2} to write to the Pmetrics package (destroyed with new package version).")
          cli::cli_text( "You will not be asked this again, but see help for {.help setPMoptions()}.")
          cli::cli_end()
          
          ans <- ""
          while (ans != "1" & ans != "2") {
            ans <- readline("Response: ")
          }
          
          
        } else { # default is to proceed with external if quiet = T
          ans <- 1
        }
        if (ans == "1") { # write to external
          dir.create(opt_dir)
          jsonlite::write_json(PMopts, path = file.path(opt_dir, "PMoptions.json"), pretty = TRUE)
        } else { # write to internal
          jsonlite::write_json(PMopts, path = PMoptionsFile, auto_unbox = TRUE)
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
    cli::cli_div(theme = list(
      span.red = list(color = "red", "font-weight" = "bold"),          
      span.blue = list(color = "blue", "font-style" = "italic")
    ))
    
    if (!file.exists(original_options)) {
      cli::cli_abort("{.blue {original_options}} does not exist.")
    }
    
    # store the options
    opt_dir <- dplyr::case_when(
      getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
      getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
    )
    
    if (!quiet) {
      cli::cli_warn(c("This will move internal saved options to {.blue {opt_dir}}.",
      "Pmetrics will now save and use options from that folder."))
    }
    
    destination_options <- file.path(opt_dir, "PMoptions.json")
    if (file.exists(destination_options) & !quiet) {
      cli::cli_warn(c("!" = "You already have a {.blue PMoptions.json} file in that folder. It will be overwritten."))
    }
    
    if (!quiet) {
      cli::cli_text(c("Choose one of the following options:"))
      cli::cli_text("{.red 1} to proceed")
      cli::cli_text("{.red 2} to abort")
      cli::cli_end()
      
      ans <- ""
      while (ans != "1" & ans != "2") {
        ans <- readline("Response: ")
      }
      

    } else { # default when quiet = T
      ans <- 1
    }
    if (ans == "2") {
      cli::cli_text("Aborting PMoptions copy.")
      return(invisible(NULL))
    }
    
    if (!dir.exists(opt_dir)) {
      dir.create(opt_dir)
      if (!quiet) cli::cli_inform("{.blue {opt_dir}} has been created.")
    }
    
    file.copy(original_options, destination_options, overwrite = T)
    file.remove(original_options)
    if (!quiet) cli::cli_inform("{.blue {destination_options}} written.")
    return(invisible(NULL))
  }
  
  #' Edit user options for Pmetrics
  #'
  #' This function will open the user options file for Pmetrics in the system
  #' default editor for JSON. Use **caution** directly editing the file to avoid errors.
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
      cli::cli_text("Run {.fn setPMoptions} to create a Pmetrics options file.")
      return(invisible(-1))
    }
    system2("open", PMoptionsFile)
    return(invisible(NULL))
  }
  
  