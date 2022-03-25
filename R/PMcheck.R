#' This function will check a .csv file or a data frame containing a
#' previously loaded .csv file (the output of \code{\link{PMreadMatrix}} for errors
#' which would cause the analysis to fail.  If a model file is provided, and the data
#' file has no errors, it will also check the model file for errors.
#'
#' Either a filename or a data object in memory are accepted as \code{data}.
#' The format of the .csv matrix file is fairly rigid.
#' It must have the following features.  Text is case-sensitive.
#' \itemize{
#'  \item A header in row 1 with the appropriate version, currently \dQuote{POPDATA DEC_11}
#'	\item Column headers in row 2.  These headers are: #ID, EVID, TIME, DUR, DOSE, ADDL, II, INPUT, OUT, OUTEQ,
#' C0, C1, C2, C3.
#'  \item No cell should be empty.  It should either contain a value or \dQuote{.} as a placeholder.
#'	\item Columns after C3 are interpreted as covariates.
#'	\item All subject records must begin with TIME=0.
#'  \item All dose events (EVID=1) must have entries in ID, EVID, TIME, DUR, DOSE and INPUT.  ADDL and II are optional, but if ADDL is not 0 or
#' missing, then II is mandatory.
#'  \item All observation events (EVID=0) must have entries in ID, EVID, TIME, OUT, OUTEQ.
#'  If an observation is missing, use \emph{-99}; otherwise use a \dQuote{.} as a placeholder
#'  in cells that are not required (e.g. INPUT for an observation event).
#'  \item If covariates are present in the data, there must be an entry for every covariate at time 0 for each subject.
#'  \item All covariates must be numeric.
#'  \item All times within a subject ID must be monotonically increasing.
#'  \item All subject IDs must be contiguous.
#'  \item All rows must have EVID and TIME values.
#'  \item All columns must be numeric except ID which may be alpha-numeric.
#'  \item All subjects must have at least one observation, which could be missing, i.e. -99.
#'  }
#'  
#' To use this function, see the example below.
#'  
#' After running PMcheck and looking at the errors in the errors.xlsx file, you can fix the
#' errors manually directly in the errors.xlsx file and resave it as a .csv file.
#' Alternatively, you could then try to fix the problem(s) with \code{mdata2 <- PMcheck(mdata,fix=T)}.  Note that we are now returning
#' a PMmatrix data object called mdata2 (hopefully cleaned of errors) rather than the PMerr object returned when \code{fix=FALSE}.
#' Pmetrics handles each of the errors in the following ways.
#' \itemize{
#'  \item If the columns are simply out of order, they will be reordered.  If some are missing, the fix must
#'  be done by the user, i.e. manually.
#'  \item All id and covariate values are truncated to 11 characters.
#'  \item Missing observations are set to -99 (not \dQuote{.}).
#'  \item Incomplete dose records are flagged for the user to fix manually.
#'  \item Incomplete observation records are flagged for the user to fix manually.
#'  \item Subjects without an EVID=1 as first event are flagged for the user to fix manually.
#'  \item Subjects with TIME != 0 as first event have dummy dose=0 events inserted at time 0.
#'  \item Subjects with a missing covariate at time 0 are flagged for the user to fix manually.
#'  \item Non-numeric covariates are converted to numeric (via \link{factor}).
#'  \item Non-ordered times are sorted within a subject if there are no EVID=4 events; otherwise the
#'  user must fix manually.
#'  \item Non-contiguous subject ID rows are combined and sorted if there are no EVID=4 events; otherwise the
#'  user must fix manually.
#'  \item Rows missing an EVID are assigned a value of 0 if DOSE is  missing, 1 otherwise.
#'  \item Rows missing a TIME value are flagged for the user to fix manually.
#'  \item Columns that are non-numeric which must be numeric are flagged for the user to fix manually.
#'  These are EVID, TIME, DUR, DOSE, ADDL, II, INPUT, OUT, OUTEQ, C0, C1, C2, and C3.  
#'  Covariate columns are fixed separately (see above).
#' }
#' 
#' @title Check Pmetrics Inputs for Errors
#' @param data The name of a Pmetrics .csv matrix file in the current working directory,
#' the full path to one not in the current working directory, or a data.frame containing 
#' the output of a previous \code{\link{PMreadMatrix}} command.
#' @param model The filename of a Pmetrics model file in the current working directory.  This parameter is optional.
#' If specified, and the data object has no errors, the model file will be evaluated.
#' @param fix Boolean operator; if \code{TRUE}, Pmetrics will attempt to fix errors in the data file.
#' Default is \code{FALSE}.
#' @param quiet Boolean operator to suppress printed output.  Default is false.
#' @return If \code{fix=TRUE}, then \code{PMcheck} returns a PMmatrix data object which has been
#' cleaned of errors as much as possible, displaying a report on the console.  
#' If \code{fix=FALSE}, then \code{PMcheck} creates a file in the working directory called \dQuote{errors.xlsx}.
#' This file can be opened by Microsoft Excel or any other program that is capable of reading .xlsx files.  This file
#' contains highlighted areas that are erroneous, with clarifying comments.  You can correct the errors in the file
#' and then re-save as a .csv file.
#' 
#' When \code{fix=FALSE}, the function also returns a list of objects of class \emph{PMerr}.  Each object is itself a list whose 
#' first object (\code{$msg}) is a character vector with \dQuote{OK} plus a brief description if there is no error, or the error.  
#' The second object (\code{$results}) is a vector of the row numbers that contain that error.
#'  \item{colorder}{The first 14 columns must be named id, evid, time, dur, dose, addl, ii, input, out, outeq, c0, c1, c2, and c3 in that order.} 
#'  \item{maxcharCol}{All column names should be less than or equal to 11 characters.}
#'  \item{maxcharID}{All id values should be less than or equal to 11 characters.}
#'  \item{missEVID}{Ensure that all rows have an EVID value.}
#'  \item{missTIME}{Ensure that all rows have a TIME value.}
#'  \item{doseDur}{Make sure all dose records are complete, i.e. contain a duration.}
#'  \item{doseDose}{Make sure all dose records are complete, i.e. contain a dose.}
#'  \item{doseInput}{Make sure all dose records are complete, i.e. contain an input number.}
#'  \item{obsOut}{Make sure all observation records are complete, i.e. contain an output.}
#'  \item{obsOuteq}{Make sure all observation records are complete, i.e. contain and outeq number.}
#'  \item{T0}{Make sure each subject's first time=0.}
#'  \item{covT0}{Make sure that there is an non-missing entry for each covariate at time=0 for each subject.}
#'  \item{timeOrder}{Ensure that all times within a subject ID are monotonically increasing.}
#'  \item{contigID}{Ensure that all subject IDs are contiguous.}
#'  \item{nonNum}{Ensure that all columns except ID are numeric.}
#'  \item{noObs}{Ensure that all subjects have at least one observation, which could be missing, i.e. -99.}

#' @author Michael Neely and Patrick Nolain
#' @seealso \code{\link{PMwriteMatrix}}, \code{\link{PMreadMatrix}}
#' @examples
#' \dontrun{
#' data(badCSV)
#' err <- PMcheck(badCSV)
#' #look at the errors.xlsx file in the working directory
#' #try to automatically fix what can be fixed
#' goodData <- PMcheck(badCSV,fix=T)
#' PMcheck(goodData)
#' #you have to fix manually problems which require data entry
#' }
#' @export



PMcheck <- function(data, model, fix = F, quiet = F) {
  
  #checkRequiredPackages("openxlsx")
  #here's the subfunction to check for errors 
  errcheck <- function(data2, model, quiet = quiet) {
    err <- list(colorder = list(msg = "OK - The first 14 columns are appropriately named and ordered.", results = NA, col = NA, code = NA),
                maxCharCol = list(msg = "OK - All columns contain entries of 11 or fewer characters.", results = NA, col = NA, code = NA),
                maxCharID = list(msg = "OK - All subject IDs are 11 or fewer characters.", results = NA, col = 1, code = 1),
                missEVID = list(msg = "OK - All rows have an EVID value.", results = NA, col = 2, code = 2),
                missTIME = list(msg = "OK - All rows have a TIME value.", results = NA, col = 3, code = 3),
                doseDur = list(msg = "OK - All dose records have a duration.", results = NA, col = 4, code = 4),
                doseDose = list(msg = "OK - All dose records have a dose.", results = NA, col = 5, code = 5),
                doseInput = list(msg = "OK - All dose records have an input.", results = NA, col = 8, code = 6),
                obsOut = list(msg = "OK - All observation records have an output.", results = NA, col = 9, code = 7),
                obsOuteq = list(msg = "OK - All observation records have an output equation.", results = NA, col = 10, code = 8),
                T0 = list(msg = "OK - All subjects have time=0 as first record.", results = NA, col = 3, code = 9),
                covT0 = list(msg = "OK - There are no covariates in the dataset.", results = NA, col = 15, code = 10),
                timeOrder = list(msg = "OK - All times are increasing within a subject, given any EVID=4.", results = NA, col = 3, code = 11),
                contigID = list(msg = "OK - All subject IDs are contiguous.", results = NA, col = 1, code = 12),
                nonNum = list(msg = "OK - All columns that must be numeric are numeric.", results = NA, col = NA, code = 13),
                noObs = list(msg = "OK - All subjects have at least one observation.", results = NA, col = 1, code = 14)
                
    )
    #set initial attribute to 0 for no error
    attr(err, "error") <- 0
    
    #define fixed column names
    fixedColNames <- getFixedColNames()
    
    #define number of columns and number of covariates
    numcol <- ncol(data2)
    numfix <- getFixedColNum()
    numcov <- getCov(data2)$ncov
    
    # check to make sure first 14 columns are correct
    t <- tolower(names(data2))
    if (any(!c("id", "time", "evid") %in% t)) {
      #must at least have id, evid, and time columns to proceed with the check
      return(-1)
    }
    if (length(t) < numfix | any(!fixedColNames %in% t)) {
      err$colorder$msg <- paste("FAIL - The first ", numfix, " columns must be named id, evid, time, dur, dose, addl, ii, input, out, outeq, c0, c1, c2, and c3 in that order", sep = "")
      attr(err, "error") <- -1
    } else {
      if (!identical(t[1:numfix], fixedColNames)) {
        err$colorder$msg <- paste("FAIL - The first ", numfix, " columns must be named id, evid, time, dur, dose, addl, ii, input, out, outeq, c0, c1, c2, and c3 in that order.", sep = "")
        attr(err, "error") <- -1
      }
    }
    
    # check to make sure cols names are 11 char or less
    t <- which(nchar(names(data2)) > 11)
    if (length(t) > 0) {
      err$maxCharCol$msg <- "FAIL - The following row numbers have columns that contain entries >11 characters:"
      err$maxCharCol$results <- t
      attr(err, "error") <- -1
    }
    
    # check to make sure ids are 11 char or less
    t <- which(nchar(as.character(data2$id)) > 11)
    if (length(t) > 0) {
      err$maxCharID$msg <- "FAIL - The following row numbers have ID values that contain entries >11 characters:"
      err$maxCharID$results <- t
      attr(err, "error") <- -1
    }
    
    #check that all records have an EVID value
    t <- which(is.na(data2$evid))
    if (length(t) > 0) {
      err$missEVID$msg <- "FAIL - The following row numbers have missing EVID values:"
      err$missEVID$results <- t
      attr(err, "error") <- -1
    }
    
    #check that all records have an TIME value
    t <- which(is.na(data2$time))
    if (length(t) > 0) {
      err$missTIME$msg <- "FAIL - The following row numbers have missing TIME values:"
      err$missTIME$results <- t
      attr(err, "error") <- -1
    }
    
    #check for dur on dose records
    t <- which(data2$evid != 0 & is.na(data2$dur))
    if (length(t) > 0) {
      err$doseDur$msg <- "FAIL - The following row numbers are dose events without DUR (unused addl or ii should have '.' placeholders):"
      err$doseDur$results <- t
      attr(err, "error") <- -1
    }
    
    #check for dose on dose records
    t <- which(data2$evid != 0 & is.na(data2$dose))
    if (length(t) > 0) {
      err$doseDose$msg <- "FAIL - The following row numbers are dose events without DOSE (unused addl or ii should have '.' placeholders):"
      err$doseDose$results <- t
      attr(err, "error") <- -1
    }
    
    #check for input on dose records
    t <- which(data2$evid != 0 & is.na(data2$input))
    if (length(t) > 0) {
      err$doseInput$msg <- "FAIL - The following row numbers are dose events without INPUT (unused addl or ii should have '.' placeholders):"
      err$doseInput$results <- t
      attr(err, "error") <- -1
    }
    
    #check for out on observation records
    t <- which(data2$evid == 0 & is.na(data2$out))
    if (length(t) > 0) {
      err$obsOut$msg <- "FAIL - The following row numbers are observation events without OUT:"
      err$obsOut$results <- t
      attr(err, "error") <- -1
    }
    
    #check for outeq on observation records
    t <- which(data2$evid == 0 & is.na(data2$outeq))
    if (length(t) > 0) {
      err$obsOuteq$msg <- "FAIL - The following row numbers are observation events without OUTEQ:"
      err$obsOuteq$results <- t
      attr(err, "error") <- -1
    }
    
    
    #check for time=0 for each subject as first record
    t <- which(tapply(data2$time, data2$id, function(x) x[1]) != 0)
    t2 <- match(names(t), data2$id)
    if (length(t) > 0) {
      err$T0$msg <- "FAIL - The following row numbers do not have time=0 as first record:"
      err$T0$results <- t2
      attr(err, "error") <- -1
    }
    
    #covariate checks
    if (numcov > 0) {
      covinfo <- getCov(data2)
      #check for missing covariates at time 0
      time0 <- which(data2$time == 0 & data2$evid == 1)
      if (length(time0) > 1) { t <- apply(as.matrix(data2[time0, covinfo$covstart:covinfo$covend], ncol = numcov), 1, function(x) any(is.na(x))) } else { t <- is.na(time0) }
      if (length(time0[t]) > 0) {
        err$covT0$msg <- "FAIL - The following row numbers are subjects with missing covariate data at time 0."
        err$covT0$results <- time0[t]
        attr(err, "error") <- -1
      } else { err$covT0$msg <- "OK - All subjects have covariate data at time 0." }
    }
    
    #check that all times within a given ID block are monotonically increasing
    misorder <- NA
    for (i in 2:nrow(data2)) {
      if ((data2$time[i] - data2$time[i - 1] < 0) & data2$id[i] == data2$id[i - 1] & data2$evid[i] != 4) misorder <- c(misorder, i)
    }
    if (length(misorder) > 1) {
      err$timeOrder$msg <- "FAIL - The following rows are from subject IDs with unsorted time entries."
      err$timeOrder$results <- misorder[-1]
      attr(err, "error") <- -1
    }
    #check that all records for a given subject ID are grouped
    temp <- data.frame(row = 1:nrow(data2), id = data2$id)
    t <- tapply(temp$row, temp$id, function(x) any(diff(x) > 1))
    if (any(t)) { t2 <- which(data2$id %in% sort(unique(data2$id))[t]) } else { t2 <- NULL }
    if (length(t2) > 0) {
      err$contigID$msg <- "FAIL - The following rows are from subject IDs that are not contiguous."
      err$contigID$results <- t2
      attr(err, "error") <- -1
    }
    
    
    #check that all non-missing columns other than ID are numeric
    allMiss <- which(apply(data2[, 2:numcol], 2, function(x) all(is.na(x))))
    nonNumeric <- which(sapply(data2[, 2:numcol], function(x)!is.numeric(x)))
    if (length(allMiss) > 0) {
      nonNumeric <- nonNumeric[!nonNumeric %in% allMiss]
    }
    if (length(nonNumeric) > 0) {
      err$nonNum$msg <- "FAIL - The following columns must be all numeric."
      err$nonNum$results <- nonNumeric + 1
      attr(err, "error") <- -1
    }
    
    #check that all subjects have at least one observation
    subjObs <- tapply(data2$evid, data2$id, function(x) sum(x == 0, na.rm = T))
    if (any(subjObs == 0)) {
      subjMissObs <- unique(data2$id)[which(subjObs == 0)]
      err$noObs$msg <- "FAIL - The following rows are subjects with no observations."
      err$noObs$results <- which(data2$id %in% subjMissObs)
    }
    
    # Definition of a table of n types of errors, each one with 'code' and 'color' properties
    errorsTable <- data.frame(comment = c("ID > 11 characters",
                                          "Missing EVID",
                                          "Missing TIME",
                                          "Missing DUR for dose event",
                                          "Missing DOSE for dose event",
                                          "Missing INPUT for dose event",
                                          "Missing OUT for output (use -99)",
                                          "Missing OUTEQ for observation",
                                          "TIME not 0 at first event for subject",
                                          "Missing one or more covariate values at TIME=0",
                                          "TIME entry out of order",
                                          "Non-contiguous subject ID",
                                          "Column with non-numeric rows (green)",
                                          "Subject with no observations"),
                              stringsAsFactors = F)
    numError <- nrow(errorsTable)
    errorsTable$code <- 1:numError
    
    #assign errors with row, column, and code
    errList <- lapply(err[3:length(err)], function(x)(lapply(x$results, function(y) c(y, x$col, x$code))))
    errDF <- data.frame(t(data.frame(errList)))
    row.names(errDF) <- 1:nrow(errDF)
    names(errDF) <- c("row", "column", "code")
    errors <- errDF[!is.na(errDF$row),]
    
    
    
    class(err) <- c("PMerr", "list")
    if (!quiet) cat("\nDATA VALIDATION REPORT:\n")
    if (!quiet) {print(err); flush.console() }
    
    if (nrow(errors) > 0) {
      # Initializing a new Excel Workbook
      wb <- openxlsx::createWorkbook()
      pmVersion <- "POPDATA DEC_11"
      formattedCols <- toupper(names(data2))
      formattedCols[1] <- "#ID"
      errColor <- "#FFFF00" #yellow
      errColor2 <- "#00FF00" #green
      
      errStyle1 <- openxlsx::createStyle(fgFill = errColor)
      errStyle2 <- openxlsx::createStyle(fgFill = errColor2)
      
      
      # Adding a new Worksheet
      sheet <- openxlsx::addWorksheet(wb, sheetName = "errors")
      
      # Writing out the header of the Pmetrics data file : version line and data frame column names
      openxlsx::writeData(wb, sheet, pmVersion, xy = c(1, 1)) # POPDATA...
      openxlsx::writeData(wb, sheet, t(formattedCols), xy = c(1, 2), colNames = F) # #ID,EVID,...
      
      
      # Highlight the cells with errors
      for (i in 1:nrow(errors)) {
        thisErr <- errors[i,]
        comment <- openxlsx::createComment(errorsTable[errorsTable$code == thisErr$code,]$comment, author = "Pmetrics", visible = F)
        colIndex <- thisErr$column
        rowIndex <- thisErr$row
        
        
        #special highlighting - overwrite some values
        if (errorsTable$comment[match(thisErr$code, errorsTable$code)] == "Missing one or more covariate values at TIME=0") {
          #if covariate error
          colIndex <- (numfix + 1):numcol
        }
        if (errorsTable$comment[match(thisErr$code, errorsTable$code)] == "Column with non-numeric rows (green)") {
          #special for non-numeric columns
          rowIndex <- 2
          colIndex <- thisErr$row #because of the way the error is detected    
          
          is.char.num <- function(x) {
            if (!is.na(x) && suppressWarnings(is.na(as.numeric(x)))) {
              return(T)
            } else { return(F) }
          }
          
          #find the non-numeric cells in a column
          rowIndex2 <- which(sapply(data2[, colIndex], is.char.num)) + 2
          #highlight them
          openxlsx::addStyle(wb, sheet, errStyle2, rowIndex2, colIndex)
          
        } else {
          #not non-numeric column error
          rowIndex <- thisErr$row + 2
          colIndex <- thisErr$column
        }
        
        #add the highlighting and comments
        openxlsx::addStyle(wb, sheet, errStyle1, rowIndex, colIndex)
        openxlsx::writeComment(wb, sheet, col = colIndex, row = rowIndex, comment = comment)
      }
      
      #Add the data
      openxlsx::writeData(wb, sheet, data2, rowNames = F, colNames = F, xy = c(1, 3), 
                          keepNA = T, na.string = ".")
      
      # Save the workbook ...
      openxlsx::saveWorkbook(wb, file = "errors.xlsx", overwrite = T)
    } #end writing of datafile with highlighted errors
    

    #if no errors in data, and model is specified, check it for errors too
    if (all(unlist(sapply(err, function(x) is.na(x$results)))) & !is.na(model)) {
      #get information from data      
      
      if (numcov > 0) { covnames <- getCov(data2)$covnames } else { covnames <- NA }
      numeqt <- max(data2$outeq, na.rm = T)
      
      modeltxt <- model
      #attempt to translate model file into separate fortran model file and instruction files
      engine <- list(alg = "NP", ncov = numcov, covnames = covnames, numeqt = numeqt, indpts = -99, limits = NA)
      
      if (!quiet) cat("\nMODEL REPORT:\n")
      trans <- makeModel(model = model, data = data, engine = engine, write = F, silent = quiet)
      
      if (trans$status == -1) {
        if ("mQRZZZ.txt" %in% Sys.glob("*", T)) {
          file.remove(model)
          file.rename("mQRZZZ.txt", model)
        }
        if (!quiet) cat("There are errors in your model file.\n")
        cat(trans$msg)
      }
      if (trans$status == 0) {
        if (!inherits(data, "PMmatrix") & !quiet) cat(paste("Associated data file: ", data, sep = ""))
        if (!quiet) cat("\nYou are using a Fortran model file rather than the new text format.\nThis is permitted, but see www.lapk.org/ModelTemp.php for details.\n")
      }
      if (trans$status == 1) {
        if (!inherits(data, "PMmatrix") & !quiet) cat(paste("Associated data file: ", data, sep = ""))
        if (!quiet) cat("\nExcellent - there were no errors found in your model file.\n")
        if (trans$model %in% Sys.glob("*", T)) {
          file.remove(trans$model)
        }
      }
    }
    if (!quiet) flush.console()
    return(err)
  }
  
  #here's the function to try and fix errors in the data file
  errfix <- function(data2, model, quiet) {
    report <- NA
    err <- errcheck(data = data2, model = model, quiet = T)
    numcol <- ncol(data2)
    #Fix first fixed columns
    if (length(grep("FAIL", err$colorder$msg)) > 0) {
      fixedColNames <- getFixedColNames()
      t <- tolower(names(data2))
      PMcols <- match(fixedColNames, t)
      if (any(is.na(PMcols))) {
        misscols <- fixedColNames[is.na(PMcols)]
        report <- c(report, paste("Cannot fix columns; the following are missing: ", paste(misscols, collapse = "'', '"), ".", sep = ""))
      } else {
        covcols <- (1:numcol)[!(1:numcol) %in% PMcols]
        data2 <- data2[, c(PMcols, covcols)]
        report <- c(report, paste("Columns are now ordered appropriately."))
      }
    }
    #Make sure ids and cols are 11 char or less
    if (length(grep("FAIL", err$maxchar$msg)) > 0) {
      names(data2) <- substr(names(data2), 1, 11)
      report <- c(report, paste("Column names are all 11 characters or fewer."))
    }
    #Check for NA observations (should be -99)
    if (length(grep("FAIL", err$obsMiss$msg)) > 0) {
      data2 <- data2[err$obsMiss$results, "out"] < -99
      report <- c(report, paste("Missing observations for evid=0 have been replaced with -99."))
      err <- errcheck(data = data2, model = model, quiet = T)
    }
    #Check for DUR dose records
    if (length(grep("FAIL", err$doseDur$msg)) > 0) {
      report <- c(report, paste("Dose records (evid=1 or evid=4) must have DUR.  Fix manually."))
    }
    #Check for DOSE dose records
    if (length(grep("FAIL", err$doseDose$msg)) > 0) {
      report <- c(report, paste("Dose records (evid=1 or evid=4) must have DOSE.  Fix manually."))
    }
    #Check for INPUT dose records
    if (length(grep("FAIL", err$doseInput$msg)) > 0) {
      report <- c(report, paste("Dose records (evid=1 or evid=4) must have INPUT.  Fix manually."))
    }
    #Check for OUT observation records
    if (length(grep("FAIL", err$obsOut$msg)) > 0) {
      report <- c(report, paste("Observation records (evid=0) must have OUT. Fix manually."))
    }
    #Check for OUTEQ observation records
    if (length(grep("FAIL", err$obsOuteq$msg)) > 0) {
      report <- c(report, paste("Observation records (evid=0) must have OUTEQ. Fix manually."))
    }
    
    #Insert dummy doses of 0 for those missing time=0 first events
    if (length(grep("FAIL", err$T0$msg)) > 0) {
      T0 <- data2[err$T0$results,]
      T0$time <- 0;
      T0$evid <- 1;
      T0$dose <- 0
      data2 <- rbind(data2, T0)
      data2 <- data2[order(data2$id, data2$time),]
      report <- c(report, paste("Subjects with first time > 0 have had a dummy dose of 0 inserted at time 0."))
      err <- errcheck(data = data2, model = model, quiet = T)
    }
    
    #Alert for missing covariate data
    if (length(grep("FAIL", err$covT0$msg)) > 0) {
      report <- c(report, paste("All covariates must have values for each subject's first event.  Fix manually."))
    }
    
    #Reorder times
    if (length(grep("FAIL", err$timeOrder$msg)) > 0) {
      if (any(data2$evid == 4)) {
        report <- c(report, paste("Your dataset has EVID=4 events. Unable to sort times automatically."))
      } else {
        data2 <- data2[order(data2$id, data2$time),]
        report <- c(report, paste("Times for each subject have been ordered."))
      }
    }
    #Reorder IDs
    if (length(grep("FAIL", err$contigID$msg)) > 0) {
      if (any(data2$evid == 4)) {
        report <- c(report, paste("Your dataset has EVID=4 events. Unable to sort subjects and times automatically."))
      } else {
        data2 <- data2[order(data2$id, data2$time),]
        report <- c(report, paste("Subjects have been grouped and ordered."))
      }
    }
    #Fix missing EVID
    if (length(grep("FAIL", err$missEVID$msg)) > 0) {
      data2$evid[err$missEVID$results] <- ifelse(is.na(data2$dose[err$missEVID$results]), 0, 1)
      report <- c(report, paste("EVID for events with doses changed to 1, otherwise 0."))
    }
    
    #Report missing TIME
    if (length(grep("FAIL", err$missTIME$msg)) > 0) {
      report <- c(report, paste("Your dataset has missing times.  Fix manually."))
    }
    
    #Report non-numeric columns
    if (length(grep("FAIL", err$nonNum$msg)) > 0) {
      report <- c(report, paste("Your dataset has non-numeric columns.  Fix manually."))
    }
    
    #Report subjects with no observations
    if (length(grep("FAIL", err$noObs$msg)) > 0) {
      report <- c(report, paste("Your dataset has subjects with no observations.  Fix manually."))
    }
    
    if (!quiet) cat("\nFIX DATA REPORT:\n\n")
    report <- report[-1]
    cat(paste0("(", 1:length(report), ") ",report))
    flush.console()
    #row.names(data2) <- 1:nrow(data2)
    return(data2)
  }
  
  #get the data
  if (is.character(data)) {
    data2 <- tryCatch(suppressWarnings(PMreadMatrix(data, quiet = T)), error = function(e) return(invisible(e)))
  } else { data2 <- data }
  if (missing(model)) model <- NA
  
  #check for errors
  err <- errcheck(data2, model = model, quiet = quiet)
  if (length(err) == 1) {
    cat("You must at least have id, evid, and time columns to proceed with the check.\n")
    flush.console()
    return(invisible(NULL))
  }
  maxTime <- max(data2$time, na.rm = T)
  if (maxTime > 24 * 48 & !quiet) cat(paste("Warning: The maximum number of AUC intervals in NPAG is 48.\nYour longest event horizon is ", maxTime, " hours.\nPmetrics will automatically choose an AUC interval of at least ", ceiling(maxTime / 48), " hours during an NPAG run.\nYou can calculate AUCs for other intervals after the run using makeAUC().\n\n", sep = ""))
  flush.console()
  
  
  #try to fix errors if asked
  if (fix) {
    if (attr(err, "error") == 0) {
      if(!quiet) {cat("\nFIX DATA REPORT:\n\nThere were no errors to fix in you data file.\n")}
      return(invisible(data2))
    } else {
      newdata <- errfix(data = data2, model = model, quiet = quiet)
      flush.console()
      return(invisible(newdata))
    }
  } else {
    #didn't ask to fix errors so return error object
    return(invisible(err))
  }
  
}










