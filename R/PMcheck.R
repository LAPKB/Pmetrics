#' This function will check a .csv file or a data frame containing a
#' previously loaded .csv file (the output of [PMreadMatrix] for errors
#' which would cause the analysis to fail.  If a model file is provided, and the data
#' file has no errors, it will also check the model file for errors. Note that as of 
#' Pmetrics Version 2, this function is called automatically when a new [PM_data]
#' object is created, and users generally no longer need to call the function directly.
#' In `PM_data$new()`, the data object is first standardized to contain all required columns,
#' since only "ID", "TIME", "DOSE" and "OUT" are required at minimum, and then checked with PMcheck. 
#'
#' If calling PMcheck directly, either a filename or a Pmetrics data object in memory are accepted as `data`.
#' Because there is no standardization with direct calls, in this case the format of the .csv matrix file is fairly rigid.
#' It must have the following features.  Text is case-sensitive.
#'  * A header in row 1 with the appropriate version, currently "POPDATA DEC_11"
#'	* Column headers in row 2.  These headers are: #ID, EVID, TIME, DUR, DOSE, ADDL, II, INPUT, OUT, OUTEQ,
#' C0, C1, C2, C3.
#'  * No cell should be empty.  It should either contain a value or "." as a placeholder.
#'	* Columns after C3 are interpreted as covariates.
#'	* All subject records must begin with TIME=0.
#'  * All dose events (EVID=1) must have entries in ID, EVID, TIME, DUR, DOSE and INPUT.  ADDL and II are optional, but if ADDL is not 0 or
#' missing, then II is mandatory.
#'  * All observation events (EVID=0) must have entries in ID, EVID, TIME, OUT, OUTEQ.
#'  If an observation is missing, use -99; otherwise use a "." as a placeholder
#'  in cells that are not required (e.g. INPUT for an observation event).
#'  * If covariates are present in the data, there must be an entry for every covariate at time 0 for each subject.
#'  * All covariates must be numeric.
#'  * All times within a subject ID must be monotonically increasing.
#'  * All subject IDs must be contiguous.
#'  * All rows must have EVID and TIME values.
#'  * All columns must be numeric except ID which may be alpha-numeric.
#'  * All subjects must have at least one observation, which could be missing, i.e. -99.
#'  * Cells which are not needed (e.g. dose on an observation event, EVID=0), should contain ".".
#'  
#' To use this function, see the example below.
#'  
#' After running PMcheck and looking at the errors in the errors.xlsx file, you can fix the
#' errors manually directly in the errors.xlsx file and resave it as a .csv file.
#' Alternatively, you could then try to fix the problem(s) with `mdata2 <- PMcheck(mdata,fix=T)`.  Note that we are now returning
#' a PMmatrix data object called mdata2 (hopefully cleaned of errors) rather than the PMerr object returned when `fix=FALSE`.
#' Pmetrics handles each of the errors in the following ways.
#'  * If the columns are simply out of order, they will be reordered.  If some are missing, the fix must
#'  be done by the user, i.e. manually.
#'  * All id and covariate values are truncated to 11 characters.
#'  * Missing observations are set to -99 (not ".").
#'  * Incomplete dose records are flagged for the user to fix manually.
#'  * Incomplete observation records are flagged for the user to fix manually.
#'  * Subjects without an EVID=1 as first event are flagged for the user to fix manually.
#'  * Subjects with TIME != 0 as first event have dummy dose=0 events inserted at time 0.
#'  * Subjects with a missing covariate at time 0 are flagged for the user to fix manually.
#'  * Non-numeric covariates are converted to numeric (via [factor()]).
#'  * Non-ordered times are sorted within a subject if there are no EVID=4 events; otherwise the
#'  user must fix manually.
#'  * Non-contiguous subject ID rows are combined and sorted if there are no EVID=4 events; otherwise the
#'  user must fix manually.
#'  * Rows missing an EVID are assigned a value of 0 if DOSE is  missing, 1 otherwise.
#'  * Rows missing a TIME value are flagged for the user to fix manually.
#'  * Cells with malformed NA values are attempted to be fixed.
#'  * Columns that are non-numeric which must be numeric are flagged for the user to fix manually.
#'  These are EVID, TIME, DUR, DOSE, ADDL, II, INPUT, OUT, OUTEQ, C0, C1, C2, and C3.  
#'  Covariate columns are fixed separately (see above).
#' 
#' @title Check Pmetrics Inputs for Errors
#' @param data The name of a Pmetrics .csv matrix file in the current working directory,
#' the full path to one not in the current working directory, or a data.frame containing 
#' the output of a previous [PMreadMatrix] command.
#' @param model The filename of a Pmetrics model file in the current working directory.  This parameter is optional.
#' If specified, and the data object has no errors, the model file will be evaluated.
#' @param fix Boolean operator; if `TRUE`, Pmetrics will attempt to fix errors in the data file.
#' Default is `FALSE`.
#' @param quiet Boolean operator to suppress printed output.  Default is false.
#' @return If `fix=TRUE`, then [PMcheck] returns
#' * The original data if no errors are found, or
#' * A PMmatrix data object which has been
#' cleaned of errors as much as possible, displaying a report on the console.  
#' 
#' If `fix=FALSE`, then [PMcheck] creates a file in the working directory called "errors.xlsx".
#' This file can be opened by Microsoft Excel or any other program that is capable of reading .xlsx files.  This file
#' contains highlighted areas that are erroneous, with clarifying comments.  You can correct the errors in the file
#' and then re-save as a .csv file.
#' 
#' When `fix=FALSE`, the function also returns a list of objects of class *PMerr*.  Each object is itself a list whose 
#' first object (`$msg`) is a character vector with "OK" plus a brief description if there is no error, or the error.  
#' The second object (`$results`) is a vector of the row numbers that contain that error.
#'  * colorder The first 14 columns must be named id, evid, time, dur, dose, addl, ii, input, out, outeq, c0, c1, c2, and c3 in that order. 
#'  * maxcharCol All column names should be less than or equal to 11 characters.
#'  * maxcharID All id values should be less than or equal to 11 characters.
#'  * missEVID Ensure that all rows have an EVID value.
#'  * missTIME Ensure that all rows have a TIME value.
#'  * doseDur Make sure all dose records are complete, i.e. contain a duration.
#'  * doseDose Make sure all dose records are complete, i.e. contain a dose.
#'  * doseInput Make sure all dose records are complete, i.e. contain an input number.
#'  * obsOut Make sure all observation records are complete, i.e. contain an output.
#'  * obsOuteq Make sure all observation records are complete, i.e. contain and outeq number.
#'  * T0 Make sure each subject's first time=0.
#'  * covT0 Make sure that there is an non-missing entry for each covariate at time=0 for each subject.
#'  * timeOrder Ensure that all times within a subject ID are monotonically increasing.
#'  * contigID Ensure that all subject IDs are contiguous.
#'  * nonNum Ensure that all columns except ID are numeric.
#'  * noObs Ensure that all subjects have at least one observation, which could be missing, i.e. -99.
#'  * mal_NA Ensure that all NA values are ".", not ". ", " .", "..", or other malformations.
#'  

#' @author Michael Neely and Patrick Nolain
#' @seealso [PMwriteMatrix}}, \code{\link{PMreadMatrix]
#' @examples
#' \dontrun{
#' err <- PMcheck(badData)
#' #look at the errors.xlsx file in the working directory
#' #try to automatically fix what can be fixed
#' goodData <- PMcheck(badCSV,fix=T)
#' PMcheck(goodData)
#' #you have to fix manually problems which require data entry
#' }
#' @export

PMcheck <- function(data, model, fix = F, quiet = F) {
  
  #get the data
  if (is.character(data)) { #data is a filename
    data2 <- tryCatch(suppressWarnings(PMreadMatrix(data, quiet = T)), error = function(e) return(invisible(e)))
    data_orig <- NULL
    legacy <- attr(data2, "legacy") 
  } else if (inherits(data, "PM_data")){
    cat("Running PMcheck on PM_data object, so using $standard_data.\n")
    data2 <- data$standard_data
    data_orig <- data$data
    legacy <- F
  } else if (is.list(data) & !is.data.frame(data)){ #data is a list coming from PM_data$private$validate
    data2 <- data$standard
    data_orig <- data$original
    legacy <- F 
  } else { #data is a PMmatrix object
    data2 <- data
    data_orig <- NULL
    legacy <- attr(data2, "legacy") 
  }
  if (is.null(legacy)){ legacy <- F}
  
  
  if (missing(model)) model <- NA
  
  #check for errors
  err <- errcheck(data2, model = model, quiet = quiet)
  if (length(err) == 1) {
    cat("You must at least have id, evid, and time columns to proceed with the check.\n")
    flush.console()
    return(invisible(NULL))
  }
  
  #report errors in errors.xlsx
  if (attr(err, "error") != 0){
    # Initialize an  Excel Workbook
    wb <- openxlsx::createWorkbook()
    # Add a  Worksheet
    sheet <- openxlsx::addWorksheet(wb, sheetName = "Errors")
    wb <- writeErrorFile(data2, err, legacy = legacy, wb, sheet)
    if (!fix){
      # Save the workbook if not going to fix
      wb <- createInstructions(wb)
      openxlsx::saveWorkbook(wb, file = "errors.xlsx", overwrite = T)
    }
  }
  
  #Provide warning on console about maximum time
  maxTime <- tryCatch(max(data2$time, na.rm = T), error = function(e) NA)
  if (!is.na(maxTime) && !is.character(maxTime) && maxTime > 24 * 48 & !quiet){
    cat(paste0(crayon::red("Warning: "), "The maximum number of AUC intervals in NPAG is 48.\nYour longest event horizon is ", maxTime, " hours.\nPmetrics will automatically choose an AUC interval of at least ", ceiling(maxTime / 48), " hours during an NPAG run.\nYou can calculate AUCs for other intervals after the run using makeAUC().\n\n"))
  } 
  flush.console()
  
  
  #try to fix errors if asked
  if (fix) {
    if (attr(err, "error") == 0) {
      if(!quiet) {cat("\nFIX DATA REPORT:\n\nThere were no errors to fix in you data file.\n")}
      return(invisible(data2))
    } else {
      newdata <- errfix(data = data2, model = model, err = err, quiet = quiet)
      err2 <- errcheck(newdata, model = NA, quiet = T)
      # Add a  Worksheet
      sheet <- openxlsx::addWorksheet(wb, sheetName = "After_Fix")
      wb <- writeErrorFile(newdata, err2, legacy = legacy, wb, sheet)
      # Save the workbook ...
      wb <- createInstructions(wb)
      openxlsx::saveWorkbook(wb, file = "errors.xlsx", overwrite = T)
      return(invisible(newdata))
    }
  } else {
    #didn't ask to fix errors so return error object
    return(invisible(err))
  }
  
}


########### ERROR CHECKING, REPORTING AND FIXING FUNCTIONS

# errcheck ----------------------------------------------------------------

#Check for errors 
errcheck <- function(data2, model, quiet = quiet) {
  
  #each list element has msg when OK, results for rows with errors, column, code for excel
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
              noObs = list(msg = "OK - All subjects have at least one observation.", results = NA, col = 1, code = 14),
              mal_NA = list(msg = "OK - all unrequired cells have proper NA values.", results = NA, col = NA, code = 15)
              
  )
  #set initial attribute to 0 for no error
  attr(err, "error") <- 0
  
  #define fixed column names
  fixedColNames <- getFixedColNames()
  
  #define number of columns and number of covariates
  numcol <- ncol(data2)
  numfix <- getFixedColNum()
  numcov <- getCov(data2)$ncov
  
  #ensure lowercase
  t <- tolower(names(data2))
  
  # check to make sure first 14 columns are correct
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
  
  #check that all records have a TIME value
  t <- which(is.na(data2$time))
  if (length(t) > 0) {
    err$missTIME$msg <- "FAIL - The following row numbers have missing TIME values. Check date/time entries."
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
    suppressWarnings(tryCatch(time_diff <- data2$time[i] - data2$time[i - 1], error = function(e) NA))
    #if not missing (reported elsewhere) and diff<0 in same ID and not evid=4, misordered
    if (!is.na(time_diff) && (time_diff < 0 & data2$id[i] == data2$id[i - 1] & data2$evid[i] != 4)) misorder <- c(misorder, i)
  }
  if (length(misorder) > 1) {
    err$timeOrder$msg <- "FAIL - The following rows are from subject IDs with unsorted times. Check date/time entries."
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
  
  #check for columns with malformed NA values
  mal_NA <- purrr::map(as.list(data2), ~stringr::str_count(.x,"(?<!\\d)\\s*\\.+\\s*")) %>%
    map(~which(.x==1)) %>% purrr::map_vec(~length(.x)>0) %>% which()
  if (length(mal_NA)>0){
    err$mal_NA$msg <- "FAIL - The following columns contain malformed NA values."
    err$mal_NA$results <- mal_NA
  }
  
  class(err) <- c("PMerr", "list")
  if (!quiet) cat("\nDATA VALIDATION REPORT:\n")
  if (!quiet) {print(err); flush.console() }
  
  #if no errors in data, and model is specified, check it for errors too
  if (all(unlist(sapply(err, function(x) is.na(x$results)))) & !is.na(model)) {
    #get information from data      
    
    if (numcov > 0) { covnames <- getCov(data2)$covnames } else { covnames <- NA }
    numeqt <- max(data2$outeq, na.rm = T)
    
    modeltxt <- model
    #attempt to translate model file into separate fortran model file and instruction files
    engine <- list(alg = "NP", ncov = numcov, covnames = covnames, numeqt = numeqt, indpts = -99, limits = NA)
    
    if (!quiet) cat("\nMODEL REPORT:\n")
    trans <- makeModel(model = model, data = data, engine = engine, write = F, quiet = quiet)
    
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


# errfix ------------------------------------------------------------------


# try and fix errors in the data file
errfix <- function(data2, model, err, quiet) {
  report <- NA
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
    report <- c(report, paste("Column names have been truncated to maximum 11 characters."))
  }
  #Check for NA observations (should be -99)
  if (length(grep("FAIL", err$obsMiss$msg)) > 0) {
    data2 <- data2[err$obsMiss$results, "out"] < -99
    report <- c(report, paste("Missing observations for evid=0 have been replaced with -99."))
    err <- errcheck(data = data2, model = model, quiet = T)
  }
  #Check for DUR dose records
  if (length(grep("FAIL", err$doseDur$msg)) > 0) {
    report <- c(report, paste("Dose records (evid=1 or evid=4) must have DUR.  See errors.xlsx and fix manually."))
  }
  #Check for DOSE dose records
  if (length(grep("FAIL", err$doseDose$msg)) > 0) {
    report <- c(report, paste("Dose records (evid=1 or evid=4) must have DOSE.  See errors.xlsx and fix manually."))
  }
  #Check for INPUT dose records
  if (length(grep("FAIL", err$doseInput$msg)) > 0) {
    report <- c(report, paste("Dose records (evid=1 or evid=4) must have INPUT.  See errors.xlsx and fix manually."))
  }
  #Check for OUT observation records
  if (length(grep("FAIL", err$obsOut$msg)) > 0) {
    report <- c(report, paste("Observation records (evid=0) must have OUT. See errors.xlsx and fix manually."))
  }
  #Check for OUTEQ observation records
  if (length(grep("FAIL", err$obsOuteq$msg)) > 0) {
    report <- c(report, paste("Observation records (evid=0) must have OUTEQ. See errors.xlsx and fix manually."))
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
    report <- c(report, paste("All covariates must have values for each subject's first event.  See errors.xlsx and fix manually."))
  }
  
  #Reorder times - assume times are in correct block
  if (length(grep("FAIL", err$timeOrder$msg)) > 0) {
    
    data2 <- makePMmatrixBlock(data2) %>% dplyr::group_by(id, block) %>%
      dplyr::arrange(time, .by_group = T) %>% ungroup() %>% select(-block)
    
    if (any(data2$evid == 4)) {
      report <- c(report, paste("Your dataset has EVID=4 events. Times ordered within each event block."))
    } else {
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
  
  #Fix malformed NA
  if(length(grep("FAIL", err$mal_NA$msg)) > 0){
    #convert to "." then NA
    data2 <- data2 %>% mutate(across(everything(), ~str_replace_all(.x,"(?<!\\d)\\s*\\.+\\s*","."))) %>%
      mutate(across(everything(), ~na_if(.x, ".")))
    report <- c(report, paste("Malformed NAs corrected."))
    
  }
  
  #Report missing TIME
  if (length(grep("FAIL", err$missTIME$msg)) > 0) {
    report <- c(report, paste("Your dataset has missing times.  See errors.xlsx and fix manually."))
  }
  
  #Report non-numeric columns
  if (length(grep("FAIL", err$nonNum$msg)) > 0) {
    report <- c(report, paste("Your dataset has non-numeric columns.  See errors.xlsx and fix manually."))
  }
  
  #Report subjects with no observations
  if (length(grep("FAIL", err$noObs$msg)) > 0) {
    report <- c(report, paste("Your dataset has subjects with no observations.  See errors.xlsx and fix manually."))
  }
  
  if (!quiet){
    cat("\nFIX DATA REPORT:\n\n")
    report <- report[-1]
    cat(paste0("(", 1:length(report), ") ",report, collapse = "\n"))
    flush.console()
  } 
  return(data2)
}


# writeErrorFile ----------------------------------------------------------

writeErrorFile <- function(dat, err, legacy, wb, sheet){
  
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
                                        "Non-numeric entry",
                                        "Subject with no observations",
                                        "Malformed NA value"),
                            stringsAsFactors = F)
  numError <- nrow(errorsTable)
  errorsTable$code <- 1:numError
  
  #assign errors with row, column, and code
  errList <- lapply(err[3:length(err)], function(x)(lapply(x$results, function(y) c(y, x$col, x$code))))
  errDF <- data.frame(t(data.frame(errList)))
  row.names(errDF) <- 1:nrow(errDF)
  names(errDF) <- c("row", "column", "code")
  errors <- errDF[!is.na(errDF$row),]
  formattedCols <- names(dat)
  
  if(legacy){
    pmVersion <- "POPDATA DEC_11"
    formattedCols <- toupper(formattedCols)
    formattedCols[1] <- "#ID"
    legacy_offset <- 1
  } else {legacy_offset <- 0}
  
  #set colors for errors
  errColor <- "#FFFF00" #yellow, column specific
  errColor2 <- "#00FF00" #green, across columns
  errColor3 <- "#00AAFF" #blue, NA
  errColor4 <- "#FFAA00" #orange, summary
  
  #create styles for error formatting
  errStyle1 <- openxlsx::createStyle(fgFill = errColor)
  errStyle2 <- openxlsx::createStyle(fgFill = errColor2)
  errStyle3 <- openxlsx::createStyle(fgFill = errColor3)
  errStyle4 <- openxlsx::createStyle(fgFill = errColor4)
  
  
  #function to detect things that can't be coerced to numbers
  is.char.num <- function(x) {
    if (!is.na(x) && suppressWarnings(is.na(as.numeric(x)))) {
      return(T)
    } else { return(F) }
  }
  
  #make second table to summarize errors
  error_summary <- errors %>% filter(!code %in% c(10, 13, 15)) #we will add these back
  
  # Highlight the cells with errors
  for (i in 1:nrow(errors)) {
    thisErr <- errors[i,]
    colIndex <- thisErr$column
    rowIndex <- thisErr$row 
    
    #special highlighting - overwrite some values
    if (thisErr$code == 10) {
      #if covariate error
      covData <- getCov(dat)
      colIndex <- covData$covstart + 
        which(is.na(dat[rowIndex, covData$covstart:covData$covend])) - 1
      rowIndex <- rowIndex + 1 + legacy_offset
      error_summary <- dplyr::bind_rows(error_summary, 
                                        data.frame(row = rep(rowIndex, length(colIndex)),
                                                   column = colIndex,
                                                   code = 10))
      openxlsx::addStyle(wb, sheet, errStyle2, rows = rowIndex, cols = colIndex)
      purrr::walk2(colIndex, rowIndex, ~openxlsx::removeComment(wb, sheet, col = .x, row = .y)) #Excel throws a fit if two comments written
      purrr::walk2(colIndex, rowIndex, ~openxlsx::writeComment(wb, sheet, col = .x, row = .y, 
                                                                comment = openxlsx::createComment(errorsTable$comment[10], author = "Pmetrics", visible = F)))
      
    } else if (thisErr$code == 13) {
      #special for non-numeric columns
      colIndex <- thisErr$row #because of the way the error is detected    
      #find the non-numeric cells in a column
      rowIndex2 <- which(sapply(dplyr::pull(dat, colIndex), is.char.num)) + 1 + legacy_offset
      #find the malformed NAs as a special case and remove them (separate error below)
      #because openxlsx can't overwrite comments
      mal_NA <- stringr::str_count(dplyr::pull(dat, colIndex),"(?<!\\d)\\s*\\.+\\s*") %>%
        map(~which(.x==1)) %>% purrr::map_vec(~length(.x)>0) %>% which() + 1 + legacy_offset
      #remove any mal_NA from non-numeric
      rowIndex2 <- rowIndex2[!rowIndex2 %in% mal_NA]
      #highlight them if any left
      if(length(rowIndex2)>0){
        openxlsx::addStyle(wb, sheet, errStyle2, rows = rowIndex2, cols = colIndex)
        purrr::walk2(colIndex, rowIndex2, ~openxlsx::removeComment(wb, sheet, col = .x, row = .y)) #Excel throws a fit if two comments written
        purrr::walk2(colIndex, rowIndex2, ~openxlsx::writeComment(wb, sheet, col = .x, row = .y, 
                                                                  comment = openxlsx::createComment(errorsTable$comment[13], author = "Pmetrics", visible = F)))
        error_summary <- dplyr::bind_rows(error_summary, 
                                          data.frame(row = rowIndex2,
                                                     column = rep(colIndex,length(rowIndex2)),
                                                     code = 13))
      }
      
      
    } else if (thisErr$code == 15){
      #malformed NA
      colIndex <- thisErr$row #because of the way the error is detected 
      rowIndex3 <- stringr::str_count(dplyr::pull(dat, colIndex),"(?<!\\d)\\s*\\.+\\s*") %>%
        map(~which(.x==1)) %>% purrr::map_vec(~length(.x)>0) %>% which() + 1 + legacy_offset
      #highlight them
      openxlsx::addStyle(wb, sheet, errStyle3, rows = rowIndex3, cols = colIndex)
      purrr::walk2(colIndex, rowIndex3, ~openxlsx::writeComment(wb, sheet, col = .x, row = .y, 
                                                                comment = openxlsx::createComment(errorsTable$comment[15], author = "Pmetrics", visible = F)))
      error_summary <- dplyr::bind_rows(error_summary, 
                                        data.frame(row = rowIndex3,
                                                   column = rep(colIndex,length(rowIndex3)),
                                                   code = 15))
    } else {
      #add the highlighting and comments for other errors
      rowIndex <- rowIndex + 1 + legacy_offset
      comment <- openxlsx::createComment(errorsTable$comment[thisErr$code], author = "Pmetrics", visible = F)
      openxlsx::addStyle(wb, sheet, errStyle1, rowIndex, colIndex)
      openxlsx::writeComment(wb, sheet, xy = c(colIndex, rowIndex), comment = comment)
      
    } 
  } #end errors for loop
  
  #Add summaries to each column with errors
  sum_errors <- as_tibble(table(error_summary$column, error_summary$code, dnn = c("column", "code"))) %>% 
    group_by(column) %>%
    summarize(n_err = sum(n))
  
  openxlsx::addStyle(wb, sheet, errStyle4, rows = 1 + legacy_offset, cols = as.numeric(sum_errors$column))
  comments <- purrr::map(1:nrow(sum_errors), ~openxlsx::createComment(paste(sum_errors$n_err[.x],
                                                                            ifelse(sum_errors$n_err[.x]>1,"errors", "error")), author = "Pmetrics", visible = F))
  purrr::walk(1:nrow(sum_errors), ~openxlsx::writeComment(wb, sheet, col = as.numeric(sum_errors$column[.x]), row = 1 + legacy_offset, comment = comments[[.x]]))
  
  # Writing out the header of the Pmetrics data file : version line.... 
  if(legacy) {openxlsx::writeData(wb, sheet, pmVersion, xy = c(1, 1))}  # POPDATA...
  
  #...and data frame column names
  openxlsx::writeData(wb, sheet, t(formattedCols), xy = c(1, 1 + legacy_offset), colNames = F)
  
  #Add the data
  openxlsx::writeData(wb, sheet, dat, rowNames = F, colNames = F, xy = c(1, 2 + legacy_offset), 
                      keepNA = T, na.string = ".")
  
  return(wb)
  
}

createInstructions <- function(wb){
  #set colors for errors
  errColor <- "#FFFF00" #yellow, column header
  errColor2 <- "#00FF00" #green, cell
  errColor3 <- "#00AAFF" #blue, NA
  errColor4 <- "#FFAA00" #orange, summary
  
  #create styles for error formatting
  errStyle1 <- openxlsx::createStyle(fgFill = errColor)
  errStyle2 <- openxlsx::createStyle(fgFill = errColor2)
  errStyle3 <- openxlsx::createStyle(fgFill = errColor3)
  errStyle4 <- openxlsx::createStyle(fgFill = errColor4)
  textStyle <- openxlsx::createStyle(fontSize = 16)
  
  openxlsx::addWorksheet(wb, "Instructions", tabColour = "grey80")
  openxlsx::addStyle(wb, "Instructions", textStyle, rows = 1:8, cols = 1)
  openxlsx::addStyle(wb, "Instructions", textStyle, rows = 10:13, cols = 2)
  openxlsx::writeData(wb, "Instructions",
                      c("'Errors' tab contains your data which has been standardized if read using PM_data$new().",
                        "Cells with errors are color coded according to table below.",
                        "Hover your mouse over each cell to read pop-up comment with details.",
                        "Comments on column headers in orange contain the total number of errors in that column.",
                        "If fix=T, which is default for PM_data$new(), there will be an additional 'After_Fix' tab.",
                        "This tab contains your standardized data after Pmetrics attempted to repair your data.",
                        "Residual errors will be indicated as for the 'Errors' tab.",
                        "You can fix the remaining errors and save the 'After_Fix' tab as a new .csv data file."),
                      startCol = 1, startRow = 1)
  
  openxlsx::addStyle(wb, "Instructions", errStyle1, rows = 10, cols = 1)
  openxlsx::addStyle(wb, "Instructions", errStyle2, rows = 11, cols = 1)
  openxlsx::addStyle(wb, "Instructions", errStyle3, rows = 12, cols = 1)
  openxlsx::addStyle(wb, "Instructions", errStyle4, rows = 13, cols = 1)
  
  openxlsx::writeData(wb, "Instructions",
                      c("Errors specific to a particular column",
                        "Errors not specific to a defined column, i.e. non-numeric entries or missing covariates at time 0.",
                        "Malformed NA values, which should only be '.'",
                        "Used for column headers to report the total number of errors in that column."),
                      startCol = 2, startRow = 10)
  return(wb)
  
}




