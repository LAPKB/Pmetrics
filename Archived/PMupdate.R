#' Download and install Pmetrics updates from LAPK website
#'
#' @title Download and install Pmetrics updates
#' @param force Boolean operator to force downloading and installing.  Default is false.
#' @return The latest system-specific Pmetrics update will be downloaded to a temporary
#' folder and then installed.  You need to restart R (Rstudio) and then reload Pmetrics with
#' the \code{library(Pmetrics)} command to complete the installation.
#' @author Michael Neely


PMupdate <- function(force = F) {
  gitREST <- curl::curl(url = "https://api.github.com/repos/LAPKB/Pmetrics/releases") # github api

  currentVersion <- package_version(suppressWarnings(
    tryCatch(
      jsonlite::fromJSON(gitREST)$name[1] %>%
        stringr::str_extract("\\d+\\.\\d+\\.\\d+$"),
      error = function(e) e <- "0.1"
    )
  ))
  if (currentVersion == "0.1") {
    cat("LAPKB github server not available. Check your internet connection.\n")
    return(invisible(FALSE))
  }
  installedVersion <- packageVersion("Pmetrics")
  if (!force & installedVersion >= currentVersion) {
    cat("You have the most current version of Pmetrics.\n")
    return(invisible(FALSE))
  } else {
    # check for remotes
    remotes_installed <- requireNamespace("remotes", quietly = TRUE)
    if (!remotes_installed) {
      cat("The remotes package is required to install from github.\n")
      cat(paste0(
        "Enter ", crayon::blue("<1>"), " to install remotes or ",
        crayon::blue("<2>"), " to abort.\n"
      ))
      ans <- ""
      while (ans != "1" & ans != "2") {
        ans <- readline("Response: ")
      }
      if (ans == "1") {
        install.packages("remotes")
      } else {
        cat("Pmetrics update aborted.\n")
        return(invisible(FALSE))
      }
    }

    # check for options
    opt_dir <- dplyr::case_when(
      getOS() == 1 | getOS() == 3 ~ "~/.PMopts", # Mac, Linux
      getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts")
    )

    if (!dir.exists(opt_dir)) {
      cat(paste0(crayon::red("WARNING: "), "Your Pmetrics options appear to be stored within the package architecture.\n"))
      cat("This means they will be reset to default values after the update.\n")
      cat("If you wish to move them to a hidden external directory, use movePMoptions().\n")
      cat(paste0(
        "Enter ", crayon::blue("<1>"), " to proceed with update or ",
        crayon::blue("<2>"), " to abort.\n"
      ))
      ans <- ""
      while (ans != "1" & ans != "2") {
        ans <- readline("Response: ")
      }
      if (ans == "2") {
        cat("Pmetrics update aborted.\n")
        return(invisible(FALSE))
      }
    }

    # update
    OS <- getOS()
    if (OS != 2) { # Mac/Linux
      Pmetrics_installed <- tryCatch(remotes::install_github(repo = "LAPKB/Pmetrics", force = force),
        error = function(e) -1
      )
      if (Pmetrics_installed != -1) {
        cat("\nInstallation was successful. You may need to restart R/Rstudio to complete the process.\n")
        return(invisible(TRUE))
      }
    } else { # Windows
      cat(paste0(crayon::blue("NOTE: "), "Windows is unable to update loaded packages.\nPlease do the following.\n"))
      cat("\n1. Paste the following into the R console: detach(\"package:Pmetrics\", unload = TRUE)\n")
      if (force) {
        cat("2. Paste the following into the R console: remotes::install_packages(repo = \"LAPKB/Pmetrics\", force = T)\n")
      } else {
        cat("2. Paste the following into the R console: remotes::install_packages(repo = \"LAPKB/Pmetrics\")\n")
      }
      cat("3. Allow Rstudio to restart session.\n")
      cat("4. Rstudio will complete installation.\n")
      cat("5. You may need to restart Rstudio when installation is complete.\n")
    }



    return(invisible(TRUE))
  }
}
