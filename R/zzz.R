.onAttach <- function(...) {
  # version and OS-specific startup messages
  OS <- getOS()

  if (interactive()) {
    installedVersion <- packageVersion("Pmetrics")
    file <- "http://www.lapk.org/PMmsg.txt"
    msg <- c(
      paste("\nWelcome to Pmetrics, version ", packageVersion("Pmetrics"), ".", sep = ""),
      "\nUse PMmanual() or visit the LAPK website at http://www.lapk.org/pmetrics.php for help.",
      "\n", crayon::green("Use PM_tutorial() for an introduction.")
    )

    # response <- tryCatch(
    #   httr::GET(
    #     "https://api.github.com/repos/LAPKB/Pmetrics/releases/latest",
    #     httr::add_headers(Accept = "application/vnd.github.v3+json")
    #   ),
    #   error = function(e) {
    #     packageStartupMessage(e)
    #     return(NULL)
    #   }
    # )
    # currentVersion <- "0.1"
    # if (!is.null(response)) {
    #   if (response$status == 200) {
    #     currentVersion <- package_version(gsub("v", "", httr::content(response)$tag_name))
    #   }
    # }

    # if (currentVersion > installedVersion) {
    #   packageStartupMessage(paste("\nPmetrics version ", currentVersion, " is available from www.lapk.org/Pmetrics_install.php. You have version ", installedVersion, ".\n", sep = ""))
    # }

    # messages <- suppressWarnings(
    #   tryCatch(scan(file, skip = 2, sep = "\n", what = "character", quiet = T),
    #     error = function(e) e <- "NoConnect"
    #   )
    # )
    # if (length(messages) > 1) {
    #   for (i in messages) {
    #     temp <- strsplit(i, ">")
    #     if (temp[[1]][1] == "all") {
    #       msg <- c(msg, paste("\n", temp[[1]][2], "\n", sep = ""))
    #     } else {
    #       temp2 <- strsplit(temp[[1]][1], ",")
    #       if (temp2[[1]][1] == installedVersion & temp2[[1]][2] == OS) msg <- c(msg, (paste("\n", temp[[1]][2], "\n", sep = "")))
    #     }
    #   }
    # }
    packageStartupMessage(msg)
  }

  # check for binary fortran files
  if (!binaries.installed()) {
    packageStartupMessage(paste0("\n", crayon::red("CRITICAL: "), "Execute PMbuild() in R to complete Pmetrics installation.\n"))
  }

  # check for PmetricsData
  if (!requireNamespace("PmetricsData", quietly = TRUE)) {
    packageStartupMessage(paste0(crayon::green("Important: "), 
                                 "PmetricsData package required for examples and PMtest().\n",
                                 "Run remotes::install_github('LAPKB/PmetricsData') to install.\n"))
  }

  # set user options for the session
  setPMoptions()

}
