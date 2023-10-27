#' @title Defines the PM_data object
#'
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' PM_data R6 objects containing raw, standardized and valid data, and methods
#' to process the data
#'
#' @details
#' *PM_data* objects are passed to [PM_fit] objects to initiate a
#' population analysis. The object is created by reading a delimited file in
#' the current working directory. The data will be transformed into the standard
#' format which is the same for all engines, with a report of any assumptions
#' that were necessary to standardize the data. [PMcheck] is called
#' on the standard data to evaluate for errors. If dates and times are converted
#' to relative decimal times in the standard data, automatic detection of the correct
#' format will be attempted using [lubridate::parse_date_time()]. In the case of failure
#' due to an unusual format, use the 'dt' argument to specify the correct format in your
#' data. In the case of successful automatic detection, the format used will be
#' included in the standardization report generated upon creation of a new *PM_data*
#' object. Check carefully to make sure the correct format was chosen. Note that if
#' your clock times did not include seconds, they were appended as ":00" to the end
#' of each time and will appear that way in the copy of the original data.
#'
#' There are a number of methods
#' defined for a PM_data object, including to write the standard data back
#' to a file for future use, to summarize and to plot the object, to
#' conduct a non-compartmental analysis on the raw data using
#' [makeNCA], to calculate an AUC using [makeAUC], and to add event rows, which
#' is particularly useful for making simulation templates on the fly.
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
    #' Creation of a new `PM_data` objects from a file or
    #' a data frame. Data will be standardized and checked
    #' automatically to a fully specified, valid data object.
    #' @param ... Arguments to be passed further
    #' @param data A quoted name of a file with full path if not
    #' in the working directory, or an unquoted name of a data frame
    #' in the current R environment.
    #' @param dt Pmetrics will try a variety of date/time formats. If all 16 of
    #' them fail, use this parameter to specify the correct format as a
    #' character vector whose
    #' first element is date format and second is time. Use the following abbreviations:
    #' * Y = 4 digit year
    #' * y = 2 digit year
    #' * m = decimal month (1, 2, ..., 12)
    #' * d = decimal day (1, 2, ..., 31)
    #' * H = hours (0-23)
    #' * M = minutes (0-59)
    #' Example: `format = c("myd", "mh")`. Not one of the tried combinations!
    #' Always check to make sure that dates/times were parsed correctly and the
    #' relative times in the `PM_data$standard_data` field look correct.
    #' Other date/time formats are possible. See [lubridate::parse_date_time()] for these.
    #' @param quiet Quietly validate. Default is `FALSE`.
    #' @param validate Check for errors. Default is `TRUE`. Strongly recommended.
    initialize = function(data = NULL, dt = NULL, quiet = FALSE, validate = TRUE) {
      self$data <- if (is.character(data)) {
        PMreadMatrix(data, quiet = T)
      } else {
        data
      }
      if (!is.null(self$data) && validate) {
        self$standard_data <- private$validate(self$data, quiet = quiet, dt = dt)
      }
    },
    #' @description
    #' Write data to file
    #' @details
    #' Writes a delimited file (e.g. comma-separated)
    #' from the `standard_data` field
    #' @param file_name A quoted name of the file to create
    #' with full path if not
    #' in the working directory.
    #' @param ... Arguments passed to [PMwriteMatrix]
    write = function(file_name, ...) {
      if (!is.null(self$standard_data)) {
        PMwriteMatrix(self$standard_data, file_name, ...)
      } else {
        cat("Create a validated PM_data object before writing.")
      }
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC].
    #' @param ... Arguments passed to [makeAUC].
    auc = function(...) {
      if (!is.null(self$data)) {
        makeAUC(self, ...)
      } else {
        cat("Data have not been defined.")
      }
    },
    #' @description
    #' Perform non-compartmental analysis
    #' @details
    #' See [makeNCA].
    #' @param ... Arguments passed to [makeNCA].
    nca = function(...) {
      if (!is.null(self$data)) {
        makeNCA(self, ...)
      } else {
        cat("Data have not been defined.")
      }
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PMmatrix].
    #' @param ... Arguments passed to [plot.PM_data]
    plot = function(...) {
      if (!is.null(self$data)) {
        plot.PM_data(self, ...)
      } else {
        cat("Data have not been defined.")
      }
    },
    #' @description
    #' Print method
    #' @details
    #' Displays the PM_data object in a variety of ways.
    #' @param standard Display the standardized data if `TRUE`.
    #' Default is `FALSE`.
    #' @param viewer Display the Viewer if `TRUE`.
    #' Default is \code{TRUE}.
    #' @param ... Other arguments to [print.data.frame]. Only
    #' passed if `viewer = FALSE`.
    print = function(standard = F, viewer = T, ...) {
      if (is.null(self$data)) {
        cat("NULL data")
        return(invisible(self))
      }
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
        print(what, ...)
      }
      return(invisible(self))
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PMmatrix].
    #' @param ... Arguments passed to [summary.PMmatrix].
    summary = function(...) {
      if (!is.null(self$standard_data)) {
        summary.PMmatrix(self$standard_data, ...)
      } else {
        cat("Create a validated PM_data object before summarizing.")
      }
    },
    #' @description
    #' Add events to PM_data object
    #' @details
    #' Add lines to a PM_data object by supplying named columns and values.
    #' `ID` is always required. `Time` is handled differently depending on
    #' the sequence of `addEvent` calls (see **Chaining** below).
    #' * It is required for the first call to `addEvent` and should be 0.
    #' For example: For example: `dat <- PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 5, ii = 24)`
    #' * For subsequent calls to `addEvent` with specific times it should be included.
    #' For example: `dat <- PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 5, ii = 24)$addEvent(id = 1, time = 144, out = -1)`
    #' Here, because `out` wasn't in the original call *and* the next call contains a value for
    #' `time`, an `out` value of -1 will be added at time 144 and `out` will be set to `NA` for all the
    #' previous rows.
    #' * In contrast, the behavior is different if you omit `time` when your
    #' data object already has rows. In this case
    #' the arguments in the call to `addEvent` (without a value for `time`)
    #' will add those arguments as columns in the prior data with the specified value
    #'  or *replace* values in those columns if they
    #' already exist.  Be sure this is what you want.
    #' For example, building on the prior example: `dat$addEvent(id = 1, dur = 0.5)`.
    #' Note that we can chain to the previously created `dat` object. Here, a duration of 0.5 hours
    #' will be added to every previous row in `dat` to create the new `dat` object, but no new
    #' row is added since there is no `time` associated with it.
    #'
    #' Adding covariates is supported, but since valid subject records in Pmetrics
    #' with covariates must contain non-missing values at time 0, covariates should
    #' be included with the first call to `$addEvent()`.
    #'
    #' As we have seen in the examples above, `ADDL` and `II` are supported.
    #'
    #' **Chaining** Multiple `$addEvent()` calls can be chained with `PM_data$new()`
    #' to create a blank data object and then add rows.
    #' This can be particularly useful for creating simulation templates.
    #' See the example.
    #' @param ... Column names and values.
    #' @param dt Pmetrics will try a variety of date/time formats. If all 16 of
    #' them fail, use this parameter to specify the correct format as a
    #' character vector whose
    #' first element is date format and second is time. Use the following abbreviations:
    #' * Y = 4 digit year
    #' * y = 2 digit year
    #' * m = decimal month (1, 2, ..., 12)
    #' * d = decimal day (1, 2, ..., 31)
    #' * H = hours (0-23)
    #' * M = minutes (0-59)
    #' Example: `format = c("myd", "mh")`. Not one of the tried combinations!
    #' Always check to make sure that dates/times were parsed correctly and the
    #' relative times in the `PM_data$standard_data` field look correct.
    #' Other date/time formats are possible. See [lubridate::parse_date_time()] for these.
    #' @param quiet Quietly validate. Default is `FALSE`.
    #' @param validate Validate the new row or not. Default is `FALSE` as a new row
    #' added to a blank will result in a one-row data object, which is invalid. Also,
    #' only one event type (dose or observation) should be added at a time, so if the
    #' new object contains only doses while building, this would cause an error. You
    #' should set `validate = TRUE` for the final addition.
    #' @examples
    #' PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 4, ii = 12, 
    #' out = NA, wt = 75)$addEvent(id = 1, time = 60, out = -1)
    addEvent = function(..., dt = NULL, quiet = FALSE, validate = FALSE) {
      # browser()
      args <- list(...)
      arg_names <- tolower(names(args))

      if (!"id" %in% arg_names) stop("ID is required to add an event.")
      to_add <- data.frame(args)

      if (!is.null(self$data)) { # existing data
        old_names <- names(self$data)
        missing_args <- arg_names[!arg_names %in% old_names]
        if (length(missing_args) > 0) {
          self$data[missing_args] <- NA
        }
        if (!"time" %in% arg_names) {
          to_add <- to_add %>% dplyr::slice(rep(1, each = nrow(self$data)))
          self$data[arg_names] <- to_add
          if (validate) {
            self$data <- self$data %>% dplyr::select(where(~ !all(is.na(.x)))) # clean up
            self$standard_data <- private$validate(self$data, dt = dt, quiet = quiet)
          } else {
            self$standard_data <- NULL
          }
          return(invisible(self))
        }
      } else {
        if (!"time" %in% arg_names) stop("Time is required to add the first event.")
      }
      # check for addl and if present, expand
      if ("addl" %in% arg_names) {
        addl_lines <- to_add %>% dplyr::filter(!is.na(addl) & addl > 0)
        if (nrow(addl_lines) > 0) {
          new_lines <- addl_lines %>%
            tidyr::uncount(addl, .remove = F) %>%
            dplyr::group_by(id) %>%
            dplyr::mutate(time = ii * dplyr::row_number() + time)

          to_add <- dplyr::bind_rows(to_add, new_lines) %>%
            dplyr::arrange(id, time) %>%
            dplyr::mutate(
              addl = ifelse(addl == -1, -1, NA),
              ii = ifelse(addl == -1, ii, NA)
            )
        }
      }
      new_data <- dplyr::bind_rows(self$data, to_add) %>% dplyr::arrange(id, time)


      self$data <- new_data
      if (validate) {
        self$data <- self$data %>% dplyr::select(where(~ !all(is.na(.x))))
        self$standard_data <- private$validate(self$data, dt = dt, quiet = quiet)
      } else {
        self$standard_data <- NULL
      }
      return(invisible(self))
    }
  ), # end public
  private = list(
    validate = function(dataObj, quiet, dt) {
      dataObj_orig <- dataObj # keep the original to pass to PMcheck
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
        relTime <- PMmatrixRelTime(dataObj, format = dt)
        dataObj$time <- relTime$relTime
        dataObj <- dataObj %>% select(-date)
        msg <- c(msg, paste0("Dates and clock times converted to relative decimal times using ", attr(relTime, "dt_format"), ".\n"))
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

      # expand any ADDL > 0
      # preserve original order (necessary for EVID=4)
      dataObj$row <- 1:nrow(dataObj)
      addl_lines <- dataObj %>% filter(!is.na(addl) & addl > 0)
      if (nrow(addl_lines) > 0) {
        new_lines <- addl_lines %>%
          tidyr::uncount(addl, .remove = FALSE) %>%
          group_by(id, time) %>%
          mutate(time = ii * row_number() + time) %>% ungroup()

        dataObj <- bind_rows(dataObj, new_lines) %>%
          dplyr::arrange(id, time) %>%
          dplyr::mutate(
            addl = ifelse(addl == -1, -1, NA),
            ii = ifelse(addl == -1, ii, NA)
          ) %>%
          select(!row)

        msg <- c(msg, "ADDL > 0 rows expanded.\n")
      }
      dataObj <- dataObj %>% select(standardNames, dplyr::all_of(covNames))
      #dataObj <- dataObj %>% dplyr::arrange(id, time)

      if (length(msg) > 2) {
        msg <- msg[-2]
      } # data were not in standard format, so remove that message
      if (!quiet) {
        cat(msg)
      }

      validData <- PMcheck(data = list(standard = dataObj, original = dataObj_orig), fix = TRUE, quiet = quiet)
      return(validData)
    } # end validate function
  ) # end private
) # end PM_data

#' Summarize a Pmetrics PM_data object
#'
#' Summarize a PM_data object using S3 method.
#' Calls \code{\link{summary.PMmatrix}}
#' 
#' @param object Data to be summarized
#' @param ... Arguments to pass to [summary.PMmatrix]
#'
#' @export
#'
summary.PM_data <- function(object, ...) {
  object$summary(...)
}
