#' \code{PMBuild} will ensure all dependent packages are installed and compile
#' Fortran source code for permanent Pmetrics modules
#'
#' @title Build Pmetrics
#' @author Michael Neely
#' @export



PMbuild <- function(skipRegistration = F, autoyes = F, rebuild = F) {
  if (getPMoptions()$backend == "fortran") {
    if (.check_and_install_gfortran(skipRegistration, autoyes)) {
      currwd <- getwd()
      OS <- getOS()

      # load necessary packages
      # packages <- packageDescription("Pmetrics")$Suggests
      # packages <- gsub("\n", "", packages)
      # packages <- unlist(strsplit(packages, ","))
      # cat("\nChecking for required packages...\n")
      # for (i in packages) {
      #   if (system.file(package = i) == "") {
      #     if (getOption("repos")[1] == "") { setRepositories() }
      #     install.packages(i, repos = getOption("repos"), dependencies = T)
      #   }

      # }

      compiler <- getPMoptions()$compilation_statements
      # check if parallel is possible
      if (length(compiler) == 2 & getBits() == "64") {
        parallel <- T
      } else {
        parallel <- F
      }
      sourcedir <- system.file("code", package = "Pmetrics")
      destdir <- paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/")
      # remove old files if present
      oldfiles <- c(Sys.glob(paste(destdir, "*.o", sep = "/")), Sys.glob(paste(destdir, "*.exe", sep = "/")))
      if (rebuild) {
        system(paste0("rm -rf ", destdir))
      }

      if (length(oldfiles) > 0) {
        file.remove(oldfiles)
      }
      # compile new files
      setwd(sourcedir)
      if (!file.exists(destdir)) dir.create(destdir, showWarnings = F)
      PMfiles <- data.frame(filename = as.character(c("NPprep", "NPeng", "ITprep", "ITeng", "ITerr", "SIMeng", "DOprep", "DOeng", "mb2csv")))
      PMfiles$path <- sapply(
        PMfiles$filename,
        function(x) {
          shQuote(
            list.files(
              getwd(),
              pattern = as.character(paste(x, "_[[:digit:]]+\\.f", sep = ""))
            )
          )
        }
      )


      for (i in 1:nrow(PMfiles)) {
        cat(paste("\nCompiling ", i, " of ", nrow(PMfiles), ": ", PMfiles$filename[i], "...", sep = ""))
        flush.console()
        if (PMfiles$filename[i] %in% c("DOprep", "mb2csv")) {
          # list of compiled and linked files
          serialCommand <- sub("<exec>", paste(PMfiles$filename[i], ".exe", sep = ""), compiler[1])
          serialCommand <- sub("<files>", PMfiles$path[i], serialCommand)
        } else {
          if (i == 2) { # NPAG
            system("gfortran -c -march=native -w -fmax-stack-var-size=32768 -O3 -o sNPpatch_120.o NPpatch_120.f90")
            serialCommand <- sub("<exec>", paste("s", PMfiles$filename[i], ".o -c", sep = ""), compiler[1])
          } else {
            serialCommand <- sub("<exec>", paste("s", PMfiles$filename[i], ".o -c", sep = ""), compiler[1])
          }
          serialCommand <- sub("<files>", PMfiles$path[i], serialCommand)
        }
        serialFortstatus <- suppressWarnings(system(serialCommand, intern = T, ignore.stderr = F))
        if (!is.null(attr(serialFortstatus, "status"))) {
          stop(paste("\nThere was an error compiling ", PMfiles$filename[i], ".\nDid you select the right fortran compiler?  If yes, try reinstalling fortran.\nFor gfortran, log into www.lapk.org and access system-specific tips on the Pmetrics installation page (step 5).\n", sep = ""))
        }
        if (i == 2 & parallel) {
          # parallel compilation for NPAG only; f90 module first
          #  system("gfortran -c -march=native -fopenmp -w -fmax-stack-var-size=32768 -O3 -o pNPpatch_120.o NPpatch_120.f90")
          parallelCommand <- sub("<exec>", paste("p", "NPpatch_120", ".o -c", sep = ""), compiler[2])
          parallelCommand <- sub("<files>", "NPpatch_120.f90", parallelCommand)
          parallelFortstatus <- suppressWarnings(system(parallelCommand, intern = T, ignore.stderr = F))
          # now link module to engine
          parallelCommand <- sub("<exec>", paste("p", PMfiles$filename[i], ".o -c", sep = ""), compiler[2])
          parallelCommand <- sub("<files>", PMfiles$path[i], parallelCommand)
          parallelFortstatus <- suppressWarnings(system(parallelCommand, intern = T, ignore.stderr = F))
          #
          if (!is.null(attr(parallelFortstatus, "status"))) {
            stop(paste("\nThere was an error compiling ", PMfiles$filename[i], ".\nDid you select the right fortran compiler?  If yes, try reinstalling fortran.\nFor gfortran, log into www.lapk.org and access system-specific tips on the Pmetrics installation page (step 5).\n", sep = ""))
          }
        }
      }


      cat("\nAll packages installed and permanent Fortran modules compiled.\n")
      flush.console()
      invisible(file.copy(from = Sys.glob(c("*.o", "*.exe")), to = destdir))
      invisible(file.remove(Sys.glob(c("*.o", "*.exe"))))
      fort <- paste(system.file("config", package = "Pmetrics"), "newFort.txt", sep = "/")
      writeLines("0", fort) # reset to zero
      setwd(currwd)
    }
  } else if (getPMoptions()$backend == "rust") {
    if (is_rustup_installed()) {
      cat("Rustup was detected in your system, Fetching dependencies and building base project.\n")
      system("rustup install stable")
      system("rustup default stable")
      cwd <- getwd()
      # This might not work if the folder is deleted afther the R Session is closed
      # If that is the case, we should create a folder inside the Pmetrics Package folder
      setwd(system.file("Rust", package = "Pmetrics"))
      system("cargo new template")
      setwd("template")
      # system("cd template")
      system("cargo add --git https://github.com/Siel/ode-solvers --branch mut_system")
      system("cargo add --git https://github.com/LAPKB/NPcore")
      system("cargo add eyre")
      system("cargo build --release")
      setPMoptions(rust_template = getwd())
      setwd(cwd)
    } else {
      cat("\n Rustup was not detected in your system, this can be caused by multiple reasons:\n")
      cat("* You have not installed rustup in your system, Follow the installation instructions at https://www.rust-lang.org/tools/install\n")
      cat("* You might have rustup installed in your system but your $PATH has not been updated (Windows), try closing and re-opening your R session, and/or Rstudio.\n")
      cat("\n If this error persists, please refer to our discussions website: https://github.com/LAPKB/Pmetrics/discussions\n")
    }
  } else {
    cat("\nError: Non-supported backend\n")
  }
}

.check_and_install_gfortran <- function(skipRegistration, autoyes) {
  # restore user defaults - deprecated
  # if(length(system.file(package="Defaults"))==1){PMreadDefaults()}
  sch_str <- c("which -s gfortran", "where gfortran", "which gfortran")
  OS <- getOS()
  if (isM1()) {
    sch_str <- c("which -s /opt/homebrew/bin/gfortran")
    # sch_str <- c("which -s /opt/R/arm64/gfortran/bin")
  }
  # env = Sys.getenv("env")
  env <- "standard"
  if (env != "Development") {
    if (!binaries.installed()) {
      cat("Pmetrics cannot find required compiled binary files.\n")
      if (system(sch_str[OS]) != 0) {
        cat("Pmetrics cannot detect gfortran and will attempt to download and install all components.\n")
        input <- if (autoyes) {
          "y"
        } else {
          tolower(readline(prompt = "Do you agree? (Y/N)"))
        }
        if (substr(input, 1, 1) == "y") {
          if (.installOrUpdateGfortran()) {
            cat("Pmetrics has installed gfortran and will now compile required binary files.\n")
            cat("Pmetrics has anonymously registered your installation of this version.\nLAPKB does not collect or store any personal or identifying information.\n")
            cat("If the registration time outs, please run PMbuild(skipRegistration=T) ")
            if (skipRegistration == F) {
              .PMremote_registerNewInstallation()
            }
            return(T)
          } else {
            cat("ERROR: Pmetrics did not install gfortran automatically.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
            return(F)
          }
        } else {
          cat("You must have gfortran to run Pmetrics.\nPlease install gfortran manually and then run PMbuild().\nGo to http://www.lapk.org/Pmetrics_install.php for help.\n")
          return(F)
        }
      } else {
        cat("Pmetrics has detected gfortran and will compile required binary files.\n")
        cat("Pmetrics has anonymously registered your installation of this version.\nLAPKB does not collect or store any personal or identifying information.\n")
        cat("If the registration time outs, please run PMbuild(skipRegistration=T) ")
        if (skipRegistration == F) {
          .PMremote_registerNewInstallation()
        }
        return(T)
      }
    } else {
      cat("Pmetrics has found required compiled binary files.\n")
      return(F)
    }
  } else {
    print("You are inside the development folder, skipping gfortran installation")
    return(F)
  }
}

is_rustup_installed <- function() {
  flag <- system("which rustup")
  # Sometimes R does not find rustup even if it is installed,
  # Fix: create a symlink to any of the folders watched by system("echo $PATH")
  # for rustup and cargo
  # We cannot do it automatically because it requires elevated permissions
  if (flag == 0) {
    return(T)
  } else {
    return(F)
  }
}
