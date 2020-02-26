
.onLoad <- function(...) {
  checkRequiredPackages("purrr")
  if (interactive()) {
    currentVersion <- package_version(suppressWarnings(
      tryCatch(scan("http://www.lapk.org/software/Pmetrics/PmetricsVersion.txt", what = "character", quiet = T),
               error = function(e) e <- "0.1")))
    installedVersion <- packageVersion("Pmetrics")

    if (currentVersion > installedVersion) {
      packageStartupMessage(paste("\nPmetrics version ", currentVersion, " is available from www.lapk.org/software.  You have version ", installedVersion, ".\n", sep = ""))
    }
  }
  .check_and_install_gfortran()
}

.onAttach <- function(...) {
  checkRequiredPackages("purrr")
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
  .check_and_install_gfortran()
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
  checkRequiredPackages("purrr")
  #library(purrr)
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

.check_and_install_gfortran <- function() {
  #restore user defaults - deprecated
  #if(length(system.file(package="Defaults"))==1){PMreadDefaults()}
  sch_str <- c("which -s gfortran", "where gfortran", "which -s gfortran")
  OS <- getOS()
  env = Sys.getenv("env")
  if (env != "Development") {
    if (!.is_fortran_installed()) {
      cat("Pmetrics cannot find required compiled binary files.\n")
      if (system(sch_str[OS]) != 0) {
        cat("Pmetrics cannot detect gfortran and will attempt to download and install all components.\n")
        input <- tolower(readline(prompt = "Do you agree? (Y/N)"))
        if (substr(input,1,1) == "y") {
          if (.getGfortran()) {
            cat("Pmetrics has installed gfortran and will now compile required binary files.\n")
            PMbuild()
          } else {
            cat("ERROR: Pmetrics did not install gfortran automatically.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
          }
        } else {
          cat("You must have gfortran to run Pmetrics.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
        }
      } else {
        cat("Pmetrics has detected gfortran and will compile required binary files.\n")
        PMbuild()
      }

    } else {
      cat("Pmetrics has found required compiled binary files.\n")
    }
  }
  else{
    print("You are inside the development folder, skipping gfortran installation")
  }
}
