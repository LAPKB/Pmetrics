#' Computes D-optimal sampling times.
#'
#' This algorithm calculates the D-optimal sample times for each grid point 
#' in a nonparametric population model.  It returns all the sample times and the mean weighted
#' results.  The optimization is run twice for stability checking, and the results of both runs
#' are reported.
#' 
#' @title Compute D-optimal Sample Times
#' 
#' @param run Run number of an NPAG model run that you wish to use to calculate the D-optimal sample times. An error will result if you try to use a run that was not NPAG.
#' @param data An optional character vector with the filename of a Pmetrics data file which serves as the template for the dosage regimen and the initial times for the D-optimal sampling.  The format of this file is the same as for any other Pmetrics run, except that EVID=0 observations are the initial sampling times.  OUT values for these lines will be ignored.  Only the first subject will be used as a template.  If a Pmetrics data file is not specified, the first subject will be used from the original run as a template.  
#' @param clean Boolean operator to clean (delete) temporary files made during the optimization.
#' @return A list of class \emph{PMdopt} with 2 items.
#' \item{allDopt }{A data frame with 5 columns: timenum, time1, time2, gridpt, prob.  The \code{timenum} column contains a number from 1 to the number of D-optimal sample times.  The \code{time1} and \code{time2} columns contain the optimal times for each gridpoint and run.  Two separate optimizations are made for stability checking so that the results can be compared.  The \code{gridpt} column contains the gridpoint number for each set of optimal sample times.  The \code{prob} column reports the probability of each gridpoint, and thus of each set of optimal sample times.}
#' \item{means }{A data frame with the weighted mean optimal sample times for each of the two runs.}
#' @author Michael Neely
#' @seealso \code{\link{SIMrun}}, \code{\link{plot.MMopt}}, \code{\link{print.MMopt}}
#' @export

Dopt <- function(run, data, clean = T) {

  #copy output file if available
  if (!missing(run)) {
    if (!file.exists(as.character(run))) { stop(paste(run, " not found in the current working directory.\n", sep = "")) }
    if (!file.exists(paste(run, "/dopt", sep = ""))) dir.create(paste(run, "/dopt", sep = ""))

    #check to make sure run is NPAG
    PMload(run)
    final <- get(paste("final", run, sep = "."))
    if (!inherits(final, "NPAG")) stop(paste("Run ", run, " does not contain an NPAG run.\n", sep = ""))

    #get output file
    outfile <- basename(Sys.glob(paste(run, "/outputs/OUT[0-9]*", sep = "")))
    if (length(outfile) == 0) {
      #no output file found
      outfile <- readline(paste("Default output file not found.\nEnter another filename or 'end' to quit: \n"))
      if (tolower(outfile) == "end") { stop(paste("No output file specified.\n")) }
    }
    if (length(outfile) > 1) {
      #multiple output files found
      cat("The following output files were found.\n")
      for (i in 1:length(outfile)) {
        cat(paste(i, ". ", outfile[i], "\n", sep = ""))
      }
      outfileIndex <- readline(paste("Enter the number of the file you want to use  or 'end' to quit: \n"))
      if (tolower(outfileIndex) == "end") { stop(paste("No output file specified.\n")) } else { outfile <- outfile[as.numeric(outfileIndex)] }

    }

    #copy this output file to new /dopt folder
    invisible(file.copy(from = paste(run, "/outputs/", outfile, sep = ""), to = paste(run, "/dopt", sep = "")))

    #now move the data file in dopt folder if supplied  
    if (!missing(data)) {
      if (file.exists(data)) {
        mdata <- PMreadMatrix(data, quiet = T)
        nsub <- length(unique(mdata$id))
        if (nsub > 1) {
          cat("Your template data file has multiple subjects.\n")
          flush.console()
          useID <- readline("Enter the ID you wish to use (press return for the first subject). ")
          if (useID == "") {
            mdata <- mdata[mdata$id == unique(mdata$id)[1],]
          } else {
            if (!useID %in% unique(mdata$id)) {
              stop(paste(useID, "is not in your data file."))
            } else { mdata <- mdata[mdata$id == useID,] }
          }
          file.copy(from = data, to = paste("orig_", data, sep = ""), overwrite = T)
          PMwriteMatrix(mdata, data, override = T)
        }
        nouts <- length(mdata$out[mdata$evid == 0])
        if (nouts > 10) {
          cat("You cannot have more than 10 initial sample times.\n")
          cat("Randomly sampling observation times...\n")
          outrows <- which(mdata$evid == 0)
          deleterows <- sample(outrows, nouts - 10, replace = F)
          mdata <- mdata[-deleterows,]
          file.copy(from = data, to = paste("orig_", data, sep = ""), overwrite = T)
          PMwriteMatrix(mdata, data, override = T)
        }

        file.copy(from = data, to = paste(run, "/dopt", sep = ""), overwrite = T)
        file.remove(data)
      } else { stop(paste(data, "is not in current directory.\n")) }
    } else {
      data <- "PATQZPX.001" #default file name
      RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste(run, "/outputs/??_RF0001.TXT", sep = ""))), error = function(e) NULL))
      if (length(RFfile) > 0) {
        datafileName <- tail(RFfile, 1)
      } else { stop(paste("\nUnable to find data file name from run ", run, ".", sep = "")) }
      mdata <- PMreadMatrix(paste(run, "/inputs/", datafileName, sep = ""), quiet = T)
      nouts <- tapply(mdata$out[mdata$evid == 0], mdata$id[mdata$evid == 0], length)
      if (nouts[1] > 10) {
        cat(paste("Your original datafile, which is embedded in run ", run, ", has more than 10 observations for subject 1.\n", sep = ""))
        cat("Please use a different template file as the data argument.\n")
        return(invisible())
      }

    }
  } else { stop("Please supply a run number.\n") }


  OS <- getOS()
   #Ensure that gfortran is properly set up
  if (!binaries.installed()) {
    PMbuild()
  }
  compiler <- compilation_statement()
  #choose serial compiliation
  if (length(compiler) == 2) {
    compiler <- compiler[1]
  }
  if (is.null(compiler)) { cat("\nExecute DOPT after fortran is installed.\n"); return(invisible(NULL)) }



  enginefiles <- shQuote(normalizePath(list.files(fortSource, pattern = "DOeng", full.names = T)))
  enginecompile <- sub("<exec>", "do_run", compiler)
  enginecompile <- sub("<files>", paste(enginefiles, "modelqzpx.for"), enginecompile, fixed = T)


  #make the control file
  if (data == "PATQZPX.001") {
    ControlFile <- c(outfile, #output file name file
                     "1", #don't change density file
                     "1", #don't change  data file
                     "1") #don't change model file

  } else {
    #get the number of covariates
    datafile <- PMreadMatrix(paste(run, "/dopt/", data, sep = ""), quiet = T)
    ncov <- ncol(datafile) - getFixedColNum()
    if (ncov > 0) {
      covstring <- rep("1", ncov)
    } else { covstring <- NULL }

    ControlFile <- c(outfile, #output file name file
                     "1", #don't change density file
                     "0", #change  data file
                     "1", #use CSV format
                     data, #enter data filename
                     "go", #continue
                     covstring, #covariates
                     "1") #don't change model file
  }


  f <- file(paste(run, "/dopt/DOcontrol", sep = ""), "w")
  writeLines(ControlFile, f)
  close(f)


  wd <- getwd()
  setwd(paste(run, "/dopt", sep = ""))


  #run prep program
  cat("Reading files....\n")
  flush.console()
  invisible(file.copy(from = paste(fortSource, "/DOprep.exe", sep = ""), to = getwd()))
  if (OS == 1 | OS == 3) { system("./DOprep.exe MacOSX < DOcontrol", ignore.stdout = T) }
  if (OS == 2) { shell("DOprep.exe DOS < DOcontrol") }



  # RUN  engine
  cat("Calculating requested D-optimal times.  ....\n")
  flush.console()

  if (OS == 1 | OS == 3) {
    if (!file.exists("extnum")) {
      system("echo 1 > extnum")
      system("echo 0 > doptrun")

    }
    system(enginecompile, ignore.stdout = T)
    system("./do_run < doptrun", ignore.stdout = T)
  } else {
    if (!file.exists("extnum")) {
      shell("echo 1 > extnum")
      shell("echo 0 > doptrun")

    }
    shell(enginecompile)
    shell("do_run < doptrun")
  }

  #get dopt calc number
  doptNum <- scan("extnum", quiet = T)
  doptNum <- doptNum - 1

  #read results
  doptRes <- readLines(paste("OUTDOPT", sprintf("%04d", doptNum), sep = ""))
  lineNum <- grep("DESIGN OF ", doptRes)
  count <- 0
  while (doptRes[lineNum[1] + count] != "") {
    count <- count + 1
  }
  doptNum <- count - 1
  gridPts <- grep("FOR GRID PT.", doptRes)
  ngrid <- 0.5 * length(gridPts)
  probPos <- regexpr("[[:digit:]]+\\.*[[:digit:]]*E-*[[:digit:]]*", doptRes[gridPts])
  probs <- sapply(1:length(gridPts), function(x) substr(doptRes[gridPts[x]], start = probPos[x], stop = probPos[x] + attr(probPos, "match.length")[x]))
  eachDopt <- doptRes[unlist(lapply(gridPts, function(x) x + 2 + (1:doptNum)))]
  DoptBreaks <- c(1, (0.5 * length(eachDopt)), (0.5 * length(eachDopt)) + 1, length(eachDopt))
  allDopt <- data.frame(timenum = rep(1:doptNum, ngrid),
                        time1 = as.numeric(gsub(" ", "", eachDopt[DoptBreaks[1]:DoptBreaks[2]])),
                        time2 = as.numeric(gsub(" ", "", eachDopt[DoptBreaks[3]:DoptBreaks[4]])))
  allDopt$gridpoint <- rep(1:ngrid, each = doptNum)
  allDopt$prob <- as.numeric(rep(gsub(" ", "", probs[1:ngrid]), each = doptNum))
  meanDoptLine <- grep("WEIGHTED D-OPTIMAL DESIGN", doptRes)
  meanDopt <- data.frame(sapply(meanDoptLine, function(x) as.numeric(gsub(" ", "", doptRes[(x + 1):(x + doptNum)]))))
  names(meanDopt) <- c("Run1", "Run2")
  results <- list(all = allDopt, means = meanDopt)
  class(results) <- c("PMdopt", "list")

  #clean up run
  if (clean) {
    toDelete <- Sys.glob(c("*QZPX*", "DOprep*", "DOcontrol", "fort*", "do_run*", "extnum", "doptrun"))
    file.remove(toDelete)
  }
  setwd(wd)
  return(results)
}
#end function

