#' Defines the PM_data object
#' 
#' PM_data objects are passed to \code{\link{PM_fit}} objects to initiate a
#' population analysis. The object is created by reading a delimited file in 
#' the current working directory. The data will be transformed into the standard
#' format which is the same for all engines, with a report of any assumptions
#' that were necessary to standardize the data. \code{\link{PMcheck}} is called
#' on the standard data to evaluate for errors. There are a number of methods
#' defined for a PM_data object, including to write the standard data back 
#' to a file for future use, to summarize and to plot the object, and to
#' conduct a non-compartmental analysis on the raw data using 
#' \code{\link{makeNCA}}.
#' 
#' @export
PM_data <- R6::R6Class("PM_data",
  public <- list(
    #' @field data Data frame containing the data to be modeled
    data = NULL,
    #' @field standard_data Data frame containing standardized version of the data
    standard_data = NULL,
    #' @description
    #' Create new data object
    #' @details
    #' Creation of a new \code{PM_data} objects from a file or
    #' a data frame. Data will be standardized and checked
    #' automatically to a fully specified, valid data object.
    #' @param data A quoted name of a file with full path if not
    #' in the working directory, or an unquoted name of a data frame
    #' in the current R environment.
    #' @param quiet Quietly validate. Default is \code{FALSE}.
    initialize = function(data, quiet = F) {
      self$data <- if (is.character(data)) {
        PMreadMatrix(data, quiet = T)
      } else {
        data
      }
      self$standard_data <- private$validate(self$data, quiet = quiet)
    },
    #' @description
    #' Write data to file
    #' @details
    #' Writes a delimited file (e.g. comma-separated)
    #' from the \code{standard_data} field 
    #' @param file_name A quoted name of the file to create 
    #' with full path if not
    #' in the working directory.
    write = function(file_name) {
      PMwriteMatrix(self$standard_data, file_name)
    },
    #' @description
    #' Perform non-compartmental analysis
    #' @details
    #' See \code{\link{makeNCA}}.
    #' @param ... Arguments passed to \code{\link{makeNCA}}.
    nca = function(...){
      makeNCA(self, ...)
    },
    #' @description
    #' Plot method
    #' @details
    #' See \code{\link{plot.PMmatrix}.
    #' @param ... Arguments passed to \code{\link{plot.PMmatrix}}.
    plot = function(...){
      plot.PMmatrix(self$standard_data, ...)
    },
    #' @description
    #' Print method
    #' @details
    #' Displays the PM_data object in a variety of ways.
    #' @param standard Display the standardized data if \code{TRUE}.
    #' Default is \code{FALSE}.
    #' @param viewer Display the Viewer if \code{TRUE}.
    #' Default is \code{TRUE}.
    #' @param ... Other arguments to \code{\link{print.data.frame}}. Only
    #' passed if \code{viewer = FALSE}.
    print = function(standard = F, viewer = T,...) {
      if (standard) {
        what <- self$standard_data
        title <- "Standardized Data"
      } else {
        what <- self$data
        title <- "Data"
      }
      if (viewer) {
        View(what, title = title)
      } else {
        print(what,...)
      }
      return(invisible(self))
    },
    #' @description
    #' Summary method
    #' @details
    #' See \code{\link{summary.PMmatrix}.
    #' @param ... Arguments passed to \code{\link{summary.PMmatrix}}.
    summary = function(...) {
      summary.PMmatrix(self$standard_data,...)
    }
  ), # end public
  private = list(
    #dataObj = NULL,
    validate = function(dataObj, quiet) {
      dataNames <- names(dataObj)
      standardNames <- getFixedColNames()

      covNames <- dataNames[!dataNames %in% standardNames]
      if ("date" %in% covNames) {
        covNames <- covNames[-which(covNames == "date")]
      }

      mandatory <- c("id", "time", "dose", "out")
      missingMandatory <- sapply(mandatory, function(x) !x %in% dataNames)
      if (any(missingMandatory)) {
        stop(paste0("Your data are missing these mandatory columns: ", mandatory[missingMandatory]))
      }

      msg <- c("DATA STANDARDIZATION REPORT:\n\n", "Data are in full format already.\n")

      if (!"evid" %in% dataNames) {
        dataObj$evid <- ifelse(is.na(dataObj$dose), 0, 1)
        msg <- c(msg, "EVID inferred as 0 for observations, 1 for doses.\n")
      }

      if ("date" %in% dataNames) {
        relTime <- PMmatrixRelTime(dataObj)
        dataObj$time <- relTime$relTime
        dataObj <- dataObj %>% select(-date)
        msg <- c(msg, "Dates and clock times converted to relative decimal times.\n")
      }

      if (!"dur" %in% dataNames) {
        dataObj$dur <- ifelse(is.na(dataObj$dose), NA, 0)
        msg <- c(msg, "All doses assumed to be oral (DUR = 0).\n")
      }

      if (!"addl" %in% dataNames) {
        dataObj$addl <- NA
        msg <- c(msg, "ADDL set to missing for all records.\n")
      }

      if (!"ii" %in% dataNames) {
        dataObj$ii <- NA
        msg <- c(msg, "II set to missing for all records.\n")
      }

      if (!"input" %in% dataNames) {
        dataObj$input <- ifelse(is.na(dataObj$dose), NA, 1)
        msg <- c(msg, "All doses assumed to be INPUT = 1.\n")
      }

      if (!"outeq" %in% dataNames) {
        dataObj$outeq <- ifelse(is.na(dataObj$out), NA, 1)
        msg <- c(msg, "All observations assumed to be OUTEQ = 1.\n")
      }

      errorCoef <- c("c0", "c1", "c2", "c3")
      missingError <- sapply(errorCoef, function(x) !x %in% dataNames)
      if (any(missingError)) {
        dataObj$c0 <- dataObj$c1 <- dataObj$c2 <- dataObj$c3 <- NA
        msg <- c(msg, "One or more error coefficients not specified. Error in model object will be used.\n")
      }

      dataObj <- dataObj %>% select(standardNames, all_of(covNames))
      if (length(msg) > 2) {
        msg <- msg[-2]
      } # data were not in standard format, so remove that message
      if(!quiet) {cat(msg)}

      validData <- PMcheck(data = dataObj, fix = T, quiet = quiet)
      return(validData)
    } # end validate function
  ) # end private
) # end PM_data

#' Summarize a Pmetrics PM_data object
#'
#' Summarize a PM_data object using S3 method. 
#' Calls \code{\link{summary.PMmatrix}}
#'
#' @export
summary.PM_data <- function(x, ...) {
  x$summary(...)
}