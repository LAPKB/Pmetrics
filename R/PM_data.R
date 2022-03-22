# PM_data -----------------------------------------------------------------


#' @export
PM_data <- R6::R6Class("PM_data",
  public <- list(
    #' @field data frame containing the data to be modeled
    data = NULL,
    #' @field data frame containing standardized version of the data
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
    initialize = function(data, quiet = F) {
      self$data <- if (is.character(data)) {
        PMreadMatrix(data, quiet = T)
      } else {
        data
      }
      self$standard_data <- private$validate(self$data, quiet = quiet)
    },
    write = function(file_name) {
      PMwriteMatrix(self$data, file_name)
    },
    plot = function(...){
      plot.PMmatrix(self$standard_data, ...)
    },
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
        print(what)
      }
      return(invisible(self))
    },
    summary = function(formula, FUN, include, exclude) {
      object <- self$standard_data
      # filter data if needed
      if (!missing(include)) {
        object <- subset(object, sub("[[:space:]]+", "", as.character(object$id)) %in% as.character(include))
      }
      if (!missing(exclude)) {
        object <- subset(object, !sub("[[:space:]]+", "", as.character(object$id)) %in% as.character(exclude))
      }

      # make results list
      results <- list()
      idOrder <- rank(unique(object$id))

      results$nsub <- length(unique(object$id))
      results$ndrug <- max(object$input, na.rm = T)
      results$numeqt <- max(object$outeq, na.rm = T)
      results$nobsXouteq <- tapply(object$evid, object$outeq, function(x) length(x == 0))
      results$missObsXouteq <- by(object, object$outeq, function(x) length(x$out[x$evid == 0 & x$out == -99]))
      covinfo <- getCov(object)
      ncov <- covinfo$ncov
      results$ncov <- ncov
      results$covnames <- covinfo$covnames
      results$ndoseXid <- tapply(object$evid, list(object$id, object$input), function(x) length(x != 0))[idOrder, ]
      results$nobsXid <- tapply(object$evid, list(object$id, object$outeq), function(x) length(x == 0))[idOrder, ]
      results$doseXid <- tapply(object$dose, list(object$id, object$input), function(x) x[!is.na(x)])[idOrder, ]
      results$obsXid <- tapply(object$out, list(object$id, object$outeq), function(x) x[!is.na(x)])[idOrder, ]
      if (ncov > 0) {
        # get each subject's covariate values
        results$cov <- lapply(1:ncov, function(y) {
          tapply(
            object[[covinfo$covstart + y - 1]], object$id,
            function(z) z[!is.na(z)]
          )[idOrder]
        })
        names(results$cov) <- covinfo$covnames
      }
      if (!missing(formula)) {
        results$formula <- aggregate(formula, object, FUN, ...)
      }

      class(results) <- c("summary.PMmatrix", "list")
      return(results)
    } # end summary function
  ), # end public
  private = list(
    dataObj = NULL,
    validate = function(dataObj = NULL, quiet) {
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
#' @export
summary.PM_data <- function(obj, ...) {
  obj$summary(...)
}