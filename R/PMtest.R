#' @title Test Pmetrics
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Check Pmetrics fortran installation by trying to compile sample files
#'
#' @author Michael Neely
#' @export
#'
PMtest <- function() {
  currwd <- getwd()
  tempwd <- tempdir()
  setwd(tempwd)
  NPex <- NULL # avoid R CMD check flag
  data(NPex, package = "PmetricsData", envir = environment())
  NPex$data$write("data.csv")
  msg <- "Congratulations; you have successfully installed all components of Pmetrics.\n"


  # writeLines(modeltxt, "model.txt")
  NPex$model$write("model.txt")

  engine <- list(
    alg = "NP", nsubtot = 1, nsub = 1, activesub = 1, ncov = 5, covnames = c("wt", "africa", "age", "gender", "height"),
    ndrug = 1, tol = 0.01,
    salt = 1, numeqt = 1, cycles = 1, icen = "median", indpts = 6, aucint = 24, idelta = 12, xmic = 1,
    ode = -4, limits = NA, priorString = 1, wrkFlag = F
  )
  trans <- makeModel(model = "model.txt", data = "data.csv", engine = engine, write = T, quiet = T)

  OS <- getOS()
  fortSource <- paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/")
  # TODO: change this
  if (!file.exists(fortSource)) {
    msg <- c(msg, "You must run PMbuild().\n")
  } else {
    msg <- c(msg, "You have the Fortran source files.\n")
  }

  compiler <- getPMoptions()$compilation_statements

  # substitution string for directory separator according to OS
  rep <- c("/", "\\\\", "/")[OS]

  # generate the names of the permanent modules
  prepfiles <- shQuote(normalizePath(list.files(fortSource, pattern = "sNPprep", full.names = T)))

  # generate names of files that will be created
  prepFileName <- "np_prep"
  runFileName <- "np_run"
  drivFileName <- "npagdriv.f"

  # run command
  runPrep <- c(
    paste("./", prepFileName, " MacOSX", sep = ""),
    paste(prepFileName, " DOS", sep = ""),
    paste("./", prepFileName, " MacOSX", sep = "")
  )[OS]

  # generate the compile statements
  prepcompile <- sub("<exec>", prepFileName, compiler[1])
  prepcompile <- sub("<files>", prepfiles, prepcompile, fixed = T)

  # compile prep program as test
  if (OS == 1 | OS == 3) {
    # Mac and Linux
    error <- system(paste(prepcompile, "2>error.txt"))
    if (error != 0) {
      errormsg <- readLines("error.txt")
      if (length(grep("command not found", errormsg[1])) > 0) {
        msg <- c(msg, "You have not installed a fortran compiler or have chosen the wrong compiler.\nRe-install, or tell Pmetrics the correct compiler in setPMoptions().\n")
      } else {
        msg <- c(msg, "Your fortran compiler appears to be functional.\n")
      }
    } else {
      # ok np_prep compiled
      # error=system(paste(runPrep,"2>error.txt"))
    }
  }

  if (OS == 2) {
    # Windows
    error <- suppressWarnings(shell(paste(prepcompile, "2>error.txt")))
    if (error != 0) {
      errormsg <- readLines("error.txt")
      if (length(grep("is not recognized", errormsg[1])) > 0) {
        msg <- c(msg, "You have not installed a fortran compiler or have chosen the wrong compiler.\nRe-install, or tell Pmetrics the correct compiler in setPMoptions().\n")
      } else {
        msg <- c(msg, "Your fortran compiler appears to be functional.\n")
      }
    } else {
      # ok np_prep compiled
      # error=shell(paste(runPrep,"2>error.txt"))
    }
  }




  if (length(msg) > 1) {
    cat(msg[-1])
  } else {
    cat(msg)
  }

  setwd(currwd)
  unlink(tempwd)
}
# end function
