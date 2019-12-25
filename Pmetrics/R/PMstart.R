
.onLoad <- function(...) {
  if (interactive()) {
    currentVersion <- package_version(suppressWarnings(
      tryCatch(scan("http://www.lapk.org/software/Pmetrics/PmetricsVersion.txt", what = "character", quiet = T),
               error = function(e) e <- "0.1")))
    installedVersion <- packageVersion("Pmetrics")

    if (currentVersion > installedVersion) {
      packageStartupMessage(paste("\nPmetrics version ", currentVersion, " is available from www.lapk.org/software.  You have version ", installedVersion, ".\n", sep = ""))
    }
  }

  #restore user defaults - deprecated
  #if(length(system.file(package="Defaults"))==1){PMreadDefaults()}
  sch_str <- c("which -s gfortran", "where gfortran", "which -s gfortran")
  OS <- getOS()
  env = Sys.getenv("env")
  if (env != "Development") {
    if (!.is_fortran_installed()) {
      cat("Compiled binaries not found\n")
      if (system(sch_str[OS]) != 0) {
        cat("Gfortran not found \n Starting automatic installation\n")
        input <- readline(prompt = "Do you want Pmetrics to perform an automatic installation of Gfortran \n This might install Gfortran and its dependencys onto your computer \n Do you Agree? (Y/N)")
        if (input == "Y" || input == "YES" || input == "Yes" || input == "y" || input == "yes") {
          if (.getGfortran()) {
            cat("Gfortran installed \n Building Pmetrics\n")
            PMbuild()
          } else {
            cat("ERROR: Could not install gfortran automatically, please install Gfortran manually and then run PMbuild() \n")
          }
        } else {
          cat("Please install Gfortran manually and then run PMbuild() \n")
        }
      } else {
        cat("Gfortran found \n Building Pmetrics\n")
        PMbuild()
      }

    } else {
      cat("Previously compiled binaries found!\n")
    }
  }

}

.onAttach <- function(...) {

  #version and OS-specific startup messages
  OS <- getOS()
  if (interactive()) {
    installedVersion <- packageVersion("Pmetrics")
    file <- "http://www.lapk.org/PMmsg.txt"
    msg <- c(paste("\nWelcome to Pmetrics, version ", packageVersion("Pmetrics"), ".", sep = ""),
             "\nUse PMmanual() or visit the LAPK website at http://www.lapk.org/pmetrics.php for help.",
             "\nSee PMnews() for version log.\n")
    messages <- suppressWarnings(
      tryCatch(scan(file, skip = 2, sep = "\n", what = "character", quiet = T),
               error = function(e) e <- "NoConnect"))
    if (length(messages) > 1) {
      for (i in messages) {
        temp <- strsplit(i, ">")
        if (temp[[1]][1] == "all") {
          msg <- c(msg, paste("\n", temp[[1]][2], "\n", sep = ""))

        } else {
          temp2 <- strsplit(temp[[1]][1], ",")
          if (temp2[[1]][1] == installedVersion & temp2[[1]][2] == OS) msg <- c(msg, (paste("\n", temp[[1]][2], "\n", sep = "")))
        }
      }
    }
    packageStartupMessage(msg)
  }

  #check for need to compile fortran objects
  #new fortran 
  newfort <- paste(system.file("config", package = "Pmetrics"), "newFort.txt", sep = "/")
  needToBuild <- F
  if (!file.exists(newfort)) {
    needToBuild <- T
  } else {
    if (readLines(newfort) == "1") needToBuild <- T
  }
  #never compiled before
  destdir <- paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/")
  #TODO: change this
  if (!file.exists(destdir)) {
    needToBuild <- T
  }

  if (needToBuild) {
    packageStartupMessage("\nCRITICAL: Pmetrics needs to compile fortran modules.  You must run PMbuild().\n")
  }

  #set user options for the session
  setPMoptions()
}

.is_fortran_installed <- function() {
  library(purrr)
  exists <- function(name) {
    paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/") %>%
    paste(name, sep = "/") %>%
    file.exists()
  }
  c("DOprep.exe", "mb2csv.exe", "pNPeng.o",
    "sDOeng.o", "sITeng.o", "sITerr.o", "sITprep.o",
    "sNPeng.o", "sNPprep.o", "sSIMeng.o") %>%
  map(exists) %>% unlist() %>% all() %>% return()
}
