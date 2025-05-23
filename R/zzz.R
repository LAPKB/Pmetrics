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
    packageStartupMessage(msg)
  }

  # check for binary fortran files
  if (!binaries.installed()) {
    packageStartupMessage(paste0("\n", crayon::red("CRITICAL: "), "Execute PM_build() in R to complete Pmetrics installation.\n"))
  }

  # set user options for the session
  setPMoptions()
}
