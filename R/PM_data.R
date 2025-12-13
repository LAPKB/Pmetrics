# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ----------------------------------------------------------------------

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
  #' @field pop The `$data` field from a [PM_pop] object. This makes it easy to add population predictions to a raw data plot. This field will be `NULL` until the [PM_data] object is added to the [PM_result] after a run. As examples:
  #' * `dat <- PM_data$new("data.csv")`. Here, `dat$pop` will be `NULL`.
  #' * `run1 <- PM_load(1)`. Here, `run1$data$pop` will be the same as `run1$pop$data`.
  pop = NULL,
  #' @field post The `$data` field from a [PM_post] object. See details in the `pop` argument above.
  post = NULL,
  #' @description
  #' Create new data object
  #' @details
  #' Creation of a new [PM_data] objects from a file or
  #' a data frame. Data will be standardized and checked
  #' automatically to a fully specified, valid data object.
  #' @param data A quoted name of a file with full path if not
  #' in the working directory, an unquoted name of a data frame
  #' in the current R environment, or a [PM_data] object, which will rebuild it.
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
  #' @param ... Other arguments (not currently used).
  initialize = function(
    data = NULL,
    dt = NULL,
    quiet = FALSE,
    validate = TRUE,
    ...) {
      if (is.character(data)) { # filename
        self$data <- rlang::try_fetch(PMreadMatrix(data, quiet = TRUE),
        error = function(e) {
          cli::cli_abort("Unable to create {.cls PM_data} object", parent = e)
          return(NULL)
        }
      )
      path <- dirname(data)
    } else if (inherits(data, "PM_data")) { # R6
      self$data <- data$data
      path <- getwd()
    } else { # something else
      self$data <- data
      path <- getwd()
    }
    
    if (!is.null(self$data) && validate) {
      self$standard_data <- private$validate(self$data, path = path, quiet = quiet, dt = dt)
    }
  },
  #' @description
  #' Save data to file
  #' @details
  #' Saves a delimited file (e.g. comma-separated)
  #' from the `standard_data` field
  #' @param file_name A quoted name of the file to create
  #' with full path if not
  #' in the working directory.
  #' @param ... Arguments passed to [PMwriteMatrix]
  save = function(file_name, ...) {
    if (!is.null(self$standard_data)) {
      PMwriteMatrix(self$standard_data, file_name, ...)
    } else {
      cli::cli_warn("Create a validated {.cls PM_data} object before writing.")
    }
  },
  #' @description
  #' Calculate AUC
  #' @details
  #' See [makeAUC].
  #' @param ... Arguments passed to [makeAUC].
  auc = function(...) {
    if (!is.null(self$data)) {
      rlang::try_fetch(makeAUC(self, ...),
      error = function(e) {
        cli::cli_warn("Unable to generate AUC.", parent = e)
        return(NULL)
      }
    )
  } else {
    cli::cli_warn("Data have not been defined.")
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
    cli::cli_warn("Data have not been defined.")
  }
},
#' @description
#' Plot method
#' @details
#' See [plot.PM_data].
#' @param ... Arguments passed to [plot.PM_data]
plot = function(...) {
  if (!is.null(self$data)) {
    plot.PM_data(self, ...)
  } else {
    cli::cli_warn("Data have not been defined.")
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
#' See [summary.PM_data].
#' @param ... Arguments passed to [summary.PM_data].
summary = function(...) {
  if (!is.null(self$standard_data)) {
    summary.PM_data(self$standard_data, ...)
  } else {
    cli::cli_warn("Create a validated PM_data object before summarizing.")
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
#' \dontrun{
#' PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 4, ii = 12,
#' out = NA, wt = 75)$addEvent(id = 1, time = 60, out = -1)
#' }

addEvent = function(..., dt = NULL, quiet = FALSE, validate = FALSE) {
  args <- list(...)
  arg_names <- tolower(names(args))
  
  if (!"id" %in% arg_names) {
    cli::cli_abort(c("x" = "ID is required to add an event."))
  }
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
        self$standard_data <- private$validate(self$data, path = getwd(), dt = dt, quiet = quiet)
      } else {
        self$standard_data <- NULL
      }
      return(invisible(self))
    }
  } else {
    if (!"time" %in% arg_names) {
      cli::cli_abort(c("x" = "Time is required to add the first event."))
    }
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
    self$standard_data <- private$validate(self$data, path = getwd(), dt = dt, quiet = quiet)
  } else {
    self$standard_data <- NULL
  }
  return(invisible(self))
} # end addEvent
), # end public
private = list(
  validate = function(dataObj, path, quiet, dt) {
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
      cli::cli_abort(c("x" = "Your data are missing these mandatory columns: {mandatory[missingMandatory]}"))
    }
    
    msg <- "Data are in full format already.\n"
    
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
    
    if (!"cens" %in% dataNames) {
      dataObj$cens <- ifelse(is.na(dataObj$out), NA, "none")
      msg <- c(msg, "All observations assumed to be uncensored.\n")
    } 
    
    if (is.numeric(dataObj$cens)) {
      dataObj$cens <- ifelse(is.na(dataObj$out), NA,
      dplyr::case_when(
        dataObj$cens == 0 ~ "none",
        dataObj$cens == 1 ~ "bloq",
        dataObj$cens == -1 ~ "aloq",
        TRUE ~ "none"
      )
    )
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
    mutate(time = ii * row_number() + time) %>%
    ungroup()
    
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
  # dataObj <- dataObj %>% dplyr::arrange(id, time)
  
  if (length(msg) > 1) {
    msg <- msg[-1]
  } # data were not in standard format, so remove that message
  
  if (!quiet) {
    cli::cli_h1("DATA STANDARDIZATION")
    cat(msg)
  }
  
  validData <- PMcheck(data = list(standard = dataObj, original = dataObj_orig), path = path, fix = TRUE, quiet = quiet)
  return(validData)
} # end validate function
) # end private
) # end PM_data

# MAKE (PMreadMatrix, PMmatrixRelTime, PMcheck) ---------------------------
#' @title Read a Pmetrics data file
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Reads a Pmetrics .csv matrix input file into R.
#' This function is largely superseded as the function is called automatically
#' when data are intialized as a [PM_data] object with `PM_data$new()`. There
#' is rarely need to call `PMreadMatrix()` directly any longer.
#'
#' @details
#' As of Pmetrics version 2, the structure of a valid .csv file relaxed.
#' Minimal required columns are id, time, dose, and out. This function is now included
#' as part of the [PM_data] R6 object to create new `PM_data` objects. Users should
#' rarely have a need to call `PMreadMatrix` as a standalone function unless they
#' continue to use Pmetrics in its legacy mode (versions < 2.0). Note that support
#' for legacy Pmetrics will eventually wither as the package evolves.
#'
#' There are a number of other options for columns in the data input.  Details can
#' be found in the [documentation](https://lapkb.github.io/Pmetrics/articles/data.html).
#'
#' @param file The name of the file to be loaded, including the full path if not
#'  in the current working directory (check with [getwd]).
#' @param sep Delimiter between columns, which is a comma by default, but can be changed with
#' [setPMoptions].
#' @param dec Decimal separator, which is a period by default, but can be changed with
#' [setPMoptions].
#' @param quiet Default is `FALSE`.  If `TRUE`, there will be no report to
#'  the console on the contents of file.
#' @param \dots Other parameters to be passed to [readr::read_delim()]
#' @return `PMreadMatrix` returns a data frame of class "PMmatrix".
#'  If the file is successfully read and `quiet=F`,
#'  the column headers will be reported to the console as a validation check.
#'  Note that this function converts the column headers in the
#'  `file` from upper to lowercase for convenient referencing in R.
#
#' @author Michael Neely
#' @seealso [PMwriteMatrix], [PMcheck], and [plot.PM_data]


PMreadMatrix <- function(
  file,
  sep = getPMoptions("sep"),
  dec = getPMoptions("dec"),
  quiet = FALSE, ...) {
    # get data
    if (missing(file)) {
      cli::cli_abort(c("x" = "Please provide filename of Pmetrics data file."))
    }
    
    file <- normalizePath(file, mustWork = FALSE)
    
    if (!file.exists(file)) {
      cli::cli_abort(c("x" = "The file {.code {basename(file)}} was not found in {.path {dirname(file)}}."))
    }
    
    # read the first line to understand the format
    headers <- scan(file,
      what = "character", quiet = TRUE, nlines = 1,
      sep = sep, dec = dec, strip.white = T
    )
    if (grepl(",", headers)[1]) {
      cli::cli_abort(c("x" = "Your .csv delimiter is not a comma. Use {.code setPMoptions(sep = \";\")}, for example."))
    }
    headers <- headers[headers != ""]
    skip <- ifelse(grepl("POPDATA .*", headers[1]), 1, 0) # 0 if current, 1 if legacy
    
    args1 <- list(
      file = file, delim = sep, col_names = TRUE, na = c(".", "NA", ""),
      locale = readr::locale(decimal_mark = dec),
      skip = skip, show_col_types = FALSE, progress = FALSE, num_threads = 1
    )
    args2 <- list(...)
    
    args <- modifyList(args1, args2)
    
    if (quiet) {
      data <- suppressWarnings(purrr::exec(readr::read_delim, !!!args))
    } else {
      data <- purrr::exec(readr::read_delim, !!!args)
    }
    
    # remove commented headers and lines
    if (grepl("#", names(data)[1])) {
    names(data)[1] <- sub("#", "", names(data)[1])
  }
  comments <- grep("#", t(data[, 1]))
  if (length(comments) > 0) {
    data <- data[-comments, ]
  }
  
  names(data) <- tolower(names(data))
  
  if (!quiet) {
    cat(paste("The file", sQuote(file), "contains these columns:\n", sep = " "))
    cat(paste(names(data), collapse = ", "))
    cat("\n")
  }
  
  attr(data, "legacy") <- ifelse(skip == 1, TRUE, FALSE) # if skip = 1, set attribute to TRUE
  class(data) <- c("PM_data_data", "data.frame")
  return(data)
}

#' @title Convert Absolute Dates and Times to Relative Hours
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Convert dates/times to relative times.
#' This function is largely superseded as it is called automatically when data
#' are initialized as a [PM_data] object. There is rarely a need to call it directly
#' any longer.
#'
#' @details
#' \code{PMmatrixRelTime} will convert absolute dates and times in a dataset
#' into relative hours, suitable for Pmetrics analysis.  Additionally, the user has
#' the option to split subjects into pseudosubjects every time a dose reset (evid=4)
#' is encountered.
#'
#' @param data The name of an R data object.
#' @param idCol A character vector with the name
#'  of the id column in \code{data} or the number of the id column, default is \dQuote{id}
#' @param dateCol A character vector with the name of the date
#'  column in \code{data} or the number of the date column, default is \dQuote{date}
#' @param timeCol A character vector with the name of the time
#'  column in \code{data} or the number of the time column, default is \dQuote{time}
#' @param evidCol A character vector with the name of the event id
#'  column in \code{data} or the number of the evid column, default is \dQuote{evid}
#' @param format Format of the date and time columns; default is
#'  m/d/y and h:m:s, as specified in the chron::chron function.
#'  Note the separators in each case (/ for dates and : for times).
#'  For dates, \emph{m} is months in digits and can be one or two digits;
#'  \emph{d} is the day of the month, again as one or two digits;
#'  \emph{y} is the year in 2 or 4 digits.  For times, all values can be one
#'  or two digits, but time is in 24-hour format, and \emph{s} is required
#'  to avoid ambiguity.
#' @param split If \emph{true}, \code{PMmatrixRelTime} will split every \code{id}
#'  into id.block, where block is defined by a dose reset, or evid=4,
#'  e.g. \code{id} 1.1, 1.2, 1.3, 2.1, 3.1, 3.2.
#' @return Returns a dataframe with columns *id, evid, relTime*.
#'  If \code{split}=T all evid values that were previously 4 will be converted to 1.
#' @author Michael Neely
#' @seealso \code{\link{PMreadMatrix}}
#' @export

PMmatrixRelTime <- function(
  data, idCol = "id", dateCol = "date", timeCol = "time", evidCol = "evid",
  format, split = F) {
    dataCols <- names(data)
    # convert numeric if necessary
    if (is.numeric(idCol)) idCol <- dataCols[idCol]
    if (is.numeric(dateCol)) dateCol <- dataCols[dateCol]
    if (is.numeric(timeCol)) timeCol <- dataCols[timeCol]
    if (is.numeric(evidCol)) evidCol <- dataCols[evidCol]
    
    # all reasonable combinations
    dt_df <- tidyr::crossing(date = c("dmy", "mdy", "ymd", "ydm"), time = c("HM", "HMS", "IMOp", "IMSOp"))
    dt_formats <- paste(dt_df$date, dt_df$time)
    
    if (!all(c(idCol, dateCol, timeCol, evidCol) %in% dataCols)) {
      cli::cli_abort(c("x" = "Please provide column names for id, date, time and evid."))
    }
    temp <- data.frame(id = data[, idCol], date = data[, dateCol], time = data[, timeCol], evid = data[, evidCol])
    temp$date <- as.character(temp$date)
    temp$time <- as.character(temp$time)
    temp$time <- unlist(lapply(temp$time, function(x) ifelse(length(gregexpr(":", x)[[1]]) == 1, paste(x, ":00", sep = ""), x)))
    
    get_dt_format <- function(test) {
      found_formats <- table(suppressWarnings(lubridate::guess_formats(paste(temp$date, temp$time), test)))
      format_str <- names(found_formats)[which(found_formats == max(found_formats))]
      O_str <- grep("O", format_str)
      if (length(O_str) > 0) {
        format_str <- format_str[-O_str]
      }
      the_format <- gsub("%", "", format_str)
      return(the_format)
    }
    
    
    dt <- NA
    if (!missing(format) && !is.null(format)) {
      if (format[2] == "HM") format[2] <- "HMS"
      format <- paste(format, collapse = " ")
      dt <- tryCatch(suppressWarnings(lubridate::parse_date_time(paste(temp$date, temp$time), quiet = TRUE, format)),
      error = function(e) e
    ) # try with specific format
    found_format <- get_dt_format(format)
  }
  if (all(is.na(dt))) { # didn't parse yet, try automatic parsing
    dt <- tryCatch(suppressWarnings(lubridate::parse_date_time(paste(temp$date, temp$time), quiet = TRUE, dt_formats)),
    error = function(e) e
  )
  found_format <- get_dt_format(dt_formats)
}

if (all(is.na(dt))) {
  cli::cli_abort(c("x" = "All dates/times failed to parse. Please specify correct format. "))
}


temp$dt <- dt # didn't have to stop, so at least some parsed

if (split) {
  # calculate PK event numbers for each patient
  for (i in unique(temp$id)) {
    pk.no <- 1
    temp2 <- subset(temp, temp$id == i)
    for (j in 1:nrow(temp2)) {
      if (temp2$evid[j] == 4) {
        pk.no <- pk.no + 1
      }
      temp2$pk.no[j] <- pk.no
    }
    temp$pk.no[temp$id == i] <- temp2$pk.no
  }
  # make new ID of form xxxxx.x for each PK event per patient
  temp$id <- temp$id + temp$pk.no / 10
  temp$evid[temp$evid == 4] <- 1
}

# calculate relative times
temp <- makePMmatrixBlock(temp) %>%
dplyr::group_by(id, block) %>%
dplyr::mutate(relTime = (dt - dt[1]) / lubridate::dhours(1))

temp$relTime <- round(temp$relTime, 2)
temp <- temp[, c("id", "evid", "relTime")]
attr(temp, "dt_format") <- found_format

return(temp)
}
#' @title Check Pmetrics Inputs for Errors
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is largely superseded as it is called automatically when
#' data are initialized as a [PM_data] object. It can still be called
#' independently of this route and will check for data errors.
#' @details
#' It  will check the data for errors
#' which would cause the analysis to fail.  Note that as of
#' Pmetrics Version 2, this function is called automatically when a new [PM_data]
#' object is created, and users generally no longer need to call the function directly.
#' In `PM_data$new()`, the data object is first standardized to contain all required columns,
#' since only "ID", "TIME", "DOSE" and "OUT" are required at minimum, and then checked with PMcheck.
#'
#' If calling PMcheck directly, either a filename or a Pmetrics data object in memory are accepted as `data`.
#' Because there is no standardization with direct calls, in this case the format of the .csv matrix file is fairly rigid.
#' It must have the following features.  Text is case-sensitive.
#'  * A header in row 1 with the appropriate version, currently "POPDATA DEC_11"
#' 	* Column headers in row 2.  These headers are: #ID, EVID, TIME, DUR, DOSE, ADDL, II, INPUT, OUT, CENS, OUTEQ,
#' C0, C1, C2, C3.
#'  * No cell should be empty.  It should either contain a value or "." as a placeholder.
#' 	* Columns after C3 are interpreted as covariates.
#' 	* All subject records must begin with TIME=0.
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
#' After running PMcheck and looking at the errors in the *errors.xlsx* file, you can fix the
#' errors manually directly in the *errors.xlsx* file and resave it as a .csv file.
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
#'  These are all columns except ID.
#'  Covariate columns are fixed separately (see above).
#'  * Dose events with censoring will be set to uncensored, with a warning to the user.
#'
#' @param data The name of a Pmetrics .csv matrix file in the current working directory,
#' the full path to one not in the current working directory, or a data.frame containing
#' the output of a previous [PMreadMatrix] command.
#' @param path The path of the data if originally a file
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
#' @seealso [PMwriteMatrix], [PMreadMatrix]
#' @examples
#' \dontrun{
#' err <- PMcheck(badData)
#' # look at the errors.xlsx file in the working directory
#' # try to automatically fix what can be fixed
#' goodData <- PMcheck(badCSV, fix = T)
#' PMcheck(goodData)
#' # you have to fix manually problems which require data entry
#' }
#' @export

PMcheck <- function(data, path, fix = FALSE, quiet = FALSE) {
  # get the data
  if (is.character(data)) { # data is a filename
    data2 <- tryCatch(PMreadMatrix(data, quiet = TRUE), error = function(e) {
      # return(invisible(e))
      cli::cli_abort(c("x" = "Unable to find {data} in current working directory, {getwd()}."))
    })
    data_orig <- NULL
    legacy <- attr(data2, "legacy")
    source <- "file"
  } else if (inherits(data, "PM_data")) {
    cat("Running PMcheck on PM_data object, so using $standard_data.\n")
    data2 <- data$standard_data
    data_orig <- data$data
    legacy <- FALSE
    source <- "PM_data"
  } else if (is.list(data) & !is.data.frame(data)) { # data is a list coming from PM_data$private$validate
    data2 <- data$standard
    data_orig <- data$original
    legacy <- FALSE
    source <- "list"
  } else { # data is a PMmatrix object
    data2 <- data
    data_orig <- NULL
    legacy <- attr(data2, "legacy")
    source <- "PMmatrix"
  }
  if (is.null(legacy)) {
    legacy <- F
  }
  
  
  # check for errors
  err <- errcheck(data2, quiet = quiet, source = source)
  if (length(err) == 1) {
    cli::cli_abort(c("x" = "You must at least have id, evid, and time columns to proceed with the check."))
  }
  
  # report errors in errors.xlsx
  if (attr(err, "error") != 0) {
    # Initialize an  Excel Workbook
    wb <- openxlsx::createWorkbook()
    # Add a  Worksheet
    sheet <- openxlsx::addWorksheet(wb, sheetName = "Errors")
    wb <- writeErrorFile(data2, err, legacy = legacy, wb, sheet)
    if (!fix) {
      # Save the workbook if not going to fix
      wb <- createInstructions(wb)
      openxlsx::saveWorkbook(wb, file = file.path(path, "errors.xlsx"), overwrite = TRUE)
    }
  }
  
  # Provide warning on console about maximum time
  maxTime <- tryCatch(max(data2$time, na.rm = T), error = function(e) NA)
  if (!is.na(maxTime) && !is.character(maxTime) && maxTime > 24 * 48 & !quiet) {
    cli::cli_warn(
      c(
        "!" = "Your longest event horizon is {maxTime} hours.",
        " " = "When fitting to a model, consider fewer predictions by making `idelta` longer than the default of 0.1 hours.",
        " " = "See {.help PM_model} for details."
      )
    )
  }
  
  
  # try to fix errors if asked
  if (fix) {
    if (attr(err, "error") == 0) {
      # if (!quiet) {
      #   cli::cli_inform(c(
      #     "i" = "FIX DATA REPORT:",
      #     " " = "There were no errors to fix in your data file."))
      # }
      return(invisible(data2))
    } else {
      newdata <- errfix(data2 = data2, err = err, quiet = quiet)
      err2 <- errcheck(newdata, quiet = TRUE)
      # Add a  Worksheet if any errors remain
      if (attr(err2, "error") != 0) {
        sheet <- openxlsx::addWorksheet(wb, sheetName = "After_Fix")
        wb <- writeErrorFile(newdata, err2, legacy = legacy, wb, sheet)
        # Save the workbook ...
        wb <- createInstructions(wb)
        openxlsx::saveWorkbook(wb, file = file.path(path, "errors.xlsx"), overwrite = TRUE)
      }
      
      return(invisible(newdata))
    }
  } else {
    # didn't ask to fix errors so return error object
    return(invisible(err))
  }
}



########### ERROR CHECKING, REPORTING AND FIXING FUNCTIONS

# errcheck ----------------------------------------------------------------

# Check for errors
errcheck <- function(data2, quiet, source) {
  # each list element has msg when OK, results for rows with errors, column, code for excel
  err <- list(
    colorder = list(msg = "OK - The first 14 columns are appropriately named and ordered.", results = NA, col = NA, code = NA),
    # maxCharCol = list(msg = "OK - All columns contain entries of 11 or fewer characters.", results = NA, col = NA, code = NA),
    # maxCharID = list(msg = "OK - All subject IDs are 11 or fewer characters.", results = NA, col = 1, code = 1),
    missEVID = list(msg = "OK - All rows have an EVID value.", results = NA, col = 2, code = 2),
    missTIME = list(msg = "OK - All rows have a TIME value.", results = NA, col = 3, code = 3),
    doseDur = list(msg = "OK - All dose records have a duration.", results = NA, col = 4, code = 4),
    doseDose = list(msg = "OK - All dose records have a dose.", results = NA, col = 5, code = 5),
    doseInput = list(msg = "OK - All dose records have an input.", results = NA, col = 8, code = 6),
    obsOut = list(msg = "OK - All observation records have an output.", results = NA, col = 9, code = 7),
    obsOuteq = list(msg = "OK - All observation records have an output equation.", results = NA, col = 10, code = 8),
    T0 = list(msg = "OK - All subjects have time=0 as first record.", results = NA, col = 3, code = 9),
    covT0 = list(msg = "OK - There are no covariates in the dataset.", results = NA, col = getFixedColNum() + 1, code = 10),
    timeOrder = list(msg = "OK - All times are increasing within a subject, given any EVID=4.", results = NA, col = 3, code = 11),
    contigID = list(msg = "OK - All subject IDs are contiguous.", results = NA, col = 1, code = 12),
    nonNum = list(msg = "OK - All columns that must be numeric are numeric.", results = NA, col = NA, code = 13),
    noObs = list(msg = "OK - All subjects have at least one observation.", results = NA, col = 1, code = 14),
    mal_NA = list(msg = "OK - all unrequired cells have proper NA values.", results = NA, col = NA, code = 15),
    doseOut = list(msg = "OK - All doses and observations separated.", results = NA, col = 5, code = 16)
  )
  # set initial attribute to 0 for no error
  attr(err, "error") <- 0
  
  # define fixed column names
  fixedColNames <- getFixedColNames()
  
  # define number of columns and number of covariates
  numcol <- ncol(data2)
  numfix <- getFixedColNum()
  numcov <- getCov(data2)$ncov
  
  # ensure lowercase
  t <- tolower(names(data2))
  
  # check to make sure first 14 columns are correct
  if (any(!c("id", "time", "evid") %in% t)) {
    # must at least have id, evid, and time columns to proceed with the check
    return(-1)
  }
  if (length(t) < numfix | any(!fixedColNames %in% t)) {
    err$colorder$msg <- paste("FAIL - The first ", numfix, " columns must be named id, evid, time, dur, dose, addl, ii, input, out, outeq, cens, c0, c1, c2, and c3 in that order", sep = "")
    attr(err, "error") <- -1
  } else {
    if (!identical(t[1:numfix], fixedColNames)) {
      err$colorder$msg <- paste("FAIL - The first ", numfix, " columns must be named id, evid, time, dur, dose, addl, ii, input, out, outeq, cens, c0, c1, c2, and c3 in that order.", sep = "")
      attr(err, "error") <- -1
    }
  }
  
  
  # check that all records have an EVID value
  t <- which(is.na(data2$evid))
  if (length(t) > 0) {
    err$missEVID$msg <- "FAIL - The following row numbers have missing EVID values:"
    err$missEVID$results <- t
    attr(err, "error") <- -1
  }
  
  # check that all records have a TIME value
  t <- which(is.na(data2$time))
  if (length(t) > 0) {
    err$missTIME$msg <- "FAIL - The following row numbers have missing TIME values. Check date/time entries."
    err$missTIME$results <- t
    attr(err, "error") <- -1
  }
  
  # check for dur on dose records
  t <- which(data2$evid != 0 & is.na(data2$dur))
  if (length(t) > 0) {
    err$doseDur$msg <- "FAIL - The following row numbers are dose events without DUR (unused addl or ii should have '.' placeholders):"
    err$doseDur$results <- t
    attr(err, "error") <- -1
  }
  
  # check for dose on dose records
  t <- which(data2$evid != 0 & is.na(data2$dose))
  if (length(t) > 0) {
    err$doseDose$msg <- "FAIL - The following row numbers are dose events without DOSE (unused addl or ii should have '.' placeholders):"
    err$doseDose$results <- t
    attr(err, "error") <- -1
  }
  
  # check for input on dose records
  t <- which(data2$evid != 0 & is.na(data2$input))
  if (length(t) > 0) {
    err$doseInput$msg <- "FAIL - The following row numbers are dose events without INPUT (unused addl or ii should have '.' placeholders):"
    err$doseInput$results <- t
    attr(err, "error") <- -1
  }
  
  # check for out on observation records
  t <- which(data2$evid == 0 & is.na(data2$out))
  if (length(t) > 0) {
    err$obsOut$msg <- "FAIL - The following row numbers are observation events without OUT:"
    err$obsOut$results <- t
    attr(err, "error") <- -1
  }
  
  # check for outeq on observation records
  t <- which(data2$evid == 0 & is.na(data2$outeq))
  if (length(t) > 0) {
    err$obsOuteq$msg <- "FAIL - The following row numbers are observation events without OUTEQ:"
    err$obsOuteq$results <- t
    attr(err, "error") <- -1
  }
  
  # check for time=0 for each subject as first record
  t <- which(tapply(data2$time, data2$id, function(x) x[1]) != 0)
  t2 <- match(names(t), data2$id)
  if (length(t) > 0) {
    err$T0$msg <- "FAIL - The following row numbers do not have time=0 as first record:"
    err$T0$results <- t2
    attr(err, "error") <- -1
  }
  
  # covariate checks
  if (numcov > 0) {
    covinfo <- getCov(data2)
    # check for missing covariates at time 0 
    time0 <- which(data2$time == 0 & data2$evid == 1)
    if (length(time0) > 1) {
      t <- apply(as.matrix(data2[time0, covinfo$covstart:covinfo$covend], ncol = numcov), 1, function(x) any(is.na(x)))
    } else {
      t <- is.na(time0)
    }
    if (length(time0[t]) > 0) {
      err$covT0$msg <- "FAIL - The following row numbers are subjects with missing covariate data at time 0."
      err$covT0$results <- time0[t]
      attr(err, "error") <- -1
    } else {
      err$covT0$msg <- "OK - All subjects have covariate data at time 0."
    }
  }
  
  # check that all times within a given ID block are monotonically increasing
  misorder <- NA
  for (i in 2:nrow(data2)) {
    time_diff <- suppressWarnings(tryCatch(data2$time[i] - data2$time[i - 1], error = function(e) NA))
    # if not missing (reported elsewhere) and diff<0 in same ID and not evid=4, misordered
    if (!is.na(time_diff) && (time_diff < 0 & data2$id[i] == data2$id[i - 1] & data2$evid[i] != 4)) misorder <- c(misorder, i)
  }
  if (length(misorder) > 1) {
    err$timeOrder$msg <- "FAIL - The following rows are from subject IDs with unsorted times. Check date/time entries."
    err$timeOrder$results <- misorder[-1]
    attr(err, "error") <- -1
  }
  
  # check that all records for a given subject ID are grouped
  temp <- data.frame(row = 1:nrow(data2), id = data2$id)
  t <- tapply(temp$row, temp$id, function(x) any(diff(x) > 1))
  if (any(t)) {
    t2 <- which(data2$id %in% sort(unique(data2$id))[t])
  } else {
    t2 <- NULL
  }
  if (length(t2) > 0) {
    err$contigID$msg <- "FAIL - The following rows are from subject IDs that are not contiguous."
    err$contigID$results <- t2
    attr(err, "error") <- -1
  }
  
  # check that all non-missing columns other than ID and cens are numeric
  
  allMiss <- names(data2)[which(apply(data2, 2, function(x) all(is.na(x))))]
  nonNumeric <- names(data2)[which(sapply(data2, function(x) !is.numeric(x)))]
  if (length(nonNumeric) > 0) {
    nonNumeric <- nonNumeric[!nonNumeric %in% allMiss] %>% purrr::discard(~.x %in% (c("id", "cens")))
  }
  if (length(nonNumeric) > 0 ) { # exclude id, cens columns
    err$nonNum$msg <- "FAIL - The following columns must be all numeric."
    err$nonNum$results <- nonNumeric
    attr(err, "error") <- -1
  }
  
  # check that all subjects have at least one observation
  subjObs <- tapply(data2$evid, data2$id, function(x) sum(x == 0, na.rm = T))
  if (any(subjObs == 0)) {
    subjMissObs <- unique(data2$id)[which(subjObs == 0)]
    err$noObs$msg <- "FAIL - The following rows are subjects with no observations."
    err$noObs$results <- which(data2$id %in% subjMissObs)
    attr(err, "error") <- -1
  }
  
  # check for columns with malformed NA values
  mal_NA <- purrr::map(as.list(data2), ~ stringr::str_count(.x, "(?<!\\d)\\s*\\.+\\s*")) %>%
  map(~ which(.x == 1)) %>%
  purrr::map_vec(~ length(.x) > 0) %>%
  which()
  if (length(mal_NA) > 0) {
    err$mal_NA$msg <- "FAIL - The following columns contain malformed NA values."
    err$mal_NA$results <- mal_NA
    attr(err, "error") <- -1
  }
  
  # check that doses and observations are separated
  doseOut <- which(!is.na(data2$dose) & !is.na(data2$out))
  if (length(doseOut) > 0) {
    err$doseOut$msg <- "FAIL - The following rows have both dose and observation values."
    err$doseOut$results <- doseOut
    attr(err, "error") <- -1
  }
  
  
  
  class(err) <- c("PMerr", "list")
  if (!quiet) {
    cli::cli_h1("DATA VALIDATION")
    print(err)
    flush.console()
  }
  
  
  if (!quiet) flush.console()
  return(err)
}


# errfix ------------------------------------------------------------------


# try and fix errors in the data file
errfix <- function(data2, err, quiet) {
  report <- NA
  numcol <- ncol(data2)
  # Fix first fixed columns
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
  
  # Check for NA observations (should be -99)
  if (length(grep("FAIL", err$obsMiss$msg)) > 0) {
    data2 <- data2[err$obsMiss$results, "out"] < -99
    report <- c(report, paste("Missing observations for evid=0 have been replaced with -99."))
    err <- errcheck(data2 = data2, quiet = T)
  }
  # Check for DUR dose records
  if (length(grep("FAIL", err$doseDur$msg)) > 0) {
    report <- c(report, paste("Dose records (evid=1 or evid=4) must have DUR.  See errors.xlsx and fix manually."))
  }
  # Check for DOSE dose records
  if (length(grep("FAIL", err$doseDose$msg)) > 0) {
    report <- c(report, paste("Dose records (evid=1 or evid=4) must have DOSE.  See errors.xlsx and fix manually."))
  }
  # Check for INPUT dose records
  if (length(grep("FAIL", err$doseInput$msg)) > 0) {
    report <- c(report, paste("Dose records (evid=1 or evid=4) must have INPUT.  See errors.xlsx and fix manually."))
  }
  # Check for OUT observation records
  if (length(grep("FAIL", err$obsOut$msg)) > 0) {
    report <- c(report, paste("Observation records (evid=0) must have OUT. See errors.xlsx and fix manually."))
  }
  # Check for OUTEQ observation records
  if (length(grep("FAIL", err$obsOuteq$msg)) > 0) {
    report <- c(report, paste("Observation records (evid=0) must have OUTEQ. See errors.xlsx and fix manually."))
  }
  
  # Insert dummy doses of 0 for those missing time=0 first events
  if (length(grep("FAIL", err$T0$msg)) > 0) {
    T0 <- data2[err$T0$results, ]
    T0$time <- 0
    T0$evid <- 1
    T0$dose <- 0
    T0$dur <- 0
    T0$input <- 1
    T0$addl <- NA
    T0$ii <- NA
    data2 <- rbind(data2, T0)
    data2 <- data2[order(data2$id, data2$time), ]
    report <- c(report, paste("Subjects with first time > 0 have had a dummy dose of 0 inserted at time 0."))
    err <- errcheck(data2 = data2, quiet = T)
  }
  
  # Alert for missing covariate data
  if (length(grep("FAIL", err$covT0$msg)) > 0) {
    report <- c(report, paste("All covariates must have values for each subject's first event.  See errors.xlsx and fix manually."))
  }
  
  # Reorder times - assume times are in correct block
  if (length(grep("FAIL", err$timeOrder$msg)) > 0) {
    data2 <- makePMmatrixBlock(data2) %>%
    dplyr::group_by(id, block) %>%
    dplyr::arrange(time, .by_group = T) %>%
    ungroup() %>%
    select(-block)
    
    if (any(data2$evid == 4)) {
      report <- c(report, paste("Your dataset has EVID=4 events. Times ordered within each event block."))
    } else {
      report <- c(report, paste("Times for each subject have been ordered."))
    }
  }
  # Reorder IDs
  if (length(grep("FAIL", err$contigID$msg)) > 0) {
    if (any(data2$evid == 4)) {
      report <- c(report, paste("Your dataset has EVID=4 events. Unable to sort subjects and times automatically."))
    } else {
      data2 <- data2[order(data2$id, data2$time), ]
      report <- c(report, paste("Subjects have been grouped and ordered."))
    }
  }
  # Fix missing EVID
  if (length(grep("FAIL", err$missEVID$msg)) > 0) {
    data2$evid[err$missEVID$results] <- ifelse(is.na(data2$dose[err$missEVID$results]), 0, 1)
    report <- c(report, paste("EVID for events with doses changed to 1, otherwise 0."))
  }
  
  # Fix doses and observations separated
  if (length(grep("FAIL", err$doseOut$msg)) > 0) {
    report <- c(report, paste("Rows with both dose and observation values must be fixed manually. See errors.xlsx."))
  }
  
  # Fix malformed NA
  if (length(grep("FAIL", err$mal_NA$msg)) > 0) {
    # convert to "." then NA
    data2 <- data2 %>%
    mutate(across(everything(), ~ str_replace_all(.x, "(?<!\\d)\\s*\\.+\\s*", "."))) %>%
    mutate(across(everything(), ~ dplyr::na_if(.x, ".")))
    report <- c(report, paste("Malformed NAs corrected."))
  }
  
  
  # Report missing TIME
  if (length(grep("FAIL", err$missTIME$msg)) > 0) {
    report <- c(report, paste("Your dataset has missing times.  See errors.xlsx and fix manually."))
  }
  
  # Report non-numeric columns
  if (length(grep("FAIL", err$nonNum$msg)) > 0) {
    report <- c(report, paste("Your dataset has non-numeric columns.  See errors.xlsx and fix manually."))
  }
  
  # Report subjects with no observations
  if (length(grep("FAIL", err$noObs$msg)) > 0) {
    report <- c(report, paste("Your dataset has subjects with no observations.  See errors.xlsx and fix manually."))
  }
  
  if (!quiet) {
    cli::cli_h1("FIX DATA REPORT:")
    report <- report[-1]
    cat(paste0("(", 1:length(report), ") ", report, collapse = "\n"))
    flush.console()
  }
  return(data2)
}


# writeErrorFile ----------------------------------------------------------

writeErrorFile <- function(dat, err, legacy, wb, sheet) {
  # Definition of a table of n types of errors, each one with 'code' and 'color' properties
  errorsTable <- data.frame(
    comment = c(
      "", # old error now not used
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
      "Malformed NA value",
      "Rows with both dose and observation values"),
      stringsAsFactors = F
    )
    numError <- nrow(errorsTable)
    errorsTable$code <- 1:numError
    
    # assign errors with row, column, and code
    errList <- lapply(err[3:length(err)], function(x) (lapply(x$results, function(y) c(y, x$col, x$code))))
    errDF <- data.frame(t(data.frame(errList)))
    row.names(errDF) <- 1:nrow(errDF)
    names(errDF) <- c("row", "column", "code")
    errors <- errDF[!is.na(errDF$row), ]
    formattedCols <- names(dat)
    
    if (legacy) {
      pmVersion <- "POPDATA DEC_11"
      formattedCols <- toupper(formattedCols)
      formattedCols[1] <- "#ID"
      legacy_offset <- 1
    } else {
      legacy_offset <- 0
    }
    
    # set colors for errors
    errColor <- "#FFFF00" # yellow, column specific
    errColor2 <- "#00FF00" # green, across columns
    errColor3 <- "#00AAFF" # blue, NA
    errColor4 <- "#FFAA00" # orange, summary
    
    # create styles for error formatting
    errStyle1 <- openxlsx::createStyle(fgFill = errColor)
    errStyle2 <- openxlsx::createStyle(fgFill = errColor2)
    errStyle3 <- openxlsx::createStyle(fgFill = errColor3)
    errStyle4 <- openxlsx::createStyle(fgFill = errColor4)
    
    
    # function to detect things that can't be coerced to numbers
    is.char.num <- function(x) {
      if (!is.na(x) && suppressWarnings(is.na(as.numeric(x)))) {
        return(T)
      } else {
        return(F)
      }
    }
    
    # make second table to summarize errors
    error_summary <- errors %>% filter(!code %in% c(10, 13, 15)) # we will add these back
    
    # Highlight the cells with errors
    for (i in 1:nrow(errors)) {
      thisErr <- errors[i, ]
      colIndex <- thisErr$column
      rowIndex <- thisErr$row
      # special highlighting - overwrite some values
      if (thisErr$code == 10) {
        # if covariate error
        covData <- getCov(dat)
        colIndex <- covData$covstart +
        which(is.na(dat[rowIndex, covData$covstart:covData$covend])) - 1
        rowIndex <- rowIndex + 1 + legacy_offset
        error_summary <- dplyr::bind_rows(
          error_summary,
          data.frame(
            row = rep(rowIndex, length(colIndex)),
            column = colIndex,
            code = 10
          )
        )
        openxlsx::addStyle(wb, sheet, errStyle2, rows = rowIndex, cols = colIndex)
        purrr::walk2(colIndex, rowIndex, ~ openxlsx::removeComment(wb, sheet, col = .x, row = .y)) # Excel throws a fit if two comments written
        purrr::walk2(colIndex, rowIndex, ~ openxlsx::writeComment(wb, sheet,
          col = .x, row = .y,
          comment = openxlsx::createComment(errorsTable$comment[10], author = "Pmetrics", visible = F)
        ))
      } else if (thisErr$code == 12) {
        # special for non-numeric columns
        colIndex <- thisErr$row # because of the way the error is detected
        # find the non-numeric cells in a column
        rowIndex2 <- which(sapply(dplyr::pull(dat, colIndex), is.char.num)) + 1 + legacy_offset
        # find the malformed NAs as a special case and remove them (separate error below)
        # because openxlsx can't overwrite comments
        mal_NA <- stringr::str_count(dplyr::pull(dat, colIndex), "(?<!\\d)\\s*\\.+\\s*") %>%
        map(~ which(.x == 1)) %>%
        purrr::map_vec(~ length(.x) > 0) %>%
        which() + 1 + legacy_offset
        # remove any mal_NA from non-numeric
        rowIndex2 <- rowIndex2[!rowIndex2 %in% mal_NA]
        # highlight them if any left
        if (length(rowIndex2) > 0) {
          openxlsx::addStyle(wb, sheet, errStyle2, rows = rowIndex2, cols = colIndex)
          purrr::walk2(colIndex, rowIndex2, ~ openxlsx::removeComment(wb, sheet, col = .x, row = .y)) # Excel throws a fit if two comments written
          purrr::walk2(colIndex, rowIndex2, ~ openxlsx::writeComment(wb, sheet,
            col = .x, row = .y,
            comment = openxlsx::createComment(errorsTable$comment[13], author = "Pmetrics", visible = F)
          ))
          error_summary <- dplyr::bind_rows(
            error_summary,
            data.frame(
              row = rowIndex2,
              column = rep(colIndex, length(rowIndex2)),
              code = 13
            )
          )
        }
      } else if (thisErr$code == 14) {
        # malformed NA
        colIndex <- thisErr$row # because of the way the error is detected
        rowIndex3 <- stringr::str_count(dplyr::pull(dat, colIndex), "(?<!\\d)\\s*\\.+\\s*") %>%
        map(~ which(.x == 1)) %>%
        purrr::map_vec(~ length(.x) > 0) %>%
        which() + 1 + legacy_offset
        # highlight them
        openxlsx::addStyle(wb, sheet, errStyle3, rows = rowIndex3, cols = colIndex)
        purrr::walk2(colIndex, rowIndex3, ~ openxlsx::writeComment(wb, sheet,
          col = .x, row = .y,
          comment = openxlsx::createComment(errorsTable$comment[15], author = "Pmetrics", visible = F)
        ))
        error_summary <- dplyr::bind_rows(
          error_summary,
          data.frame(
            row = rowIndex3,
            column = rep(colIndex, length(rowIndex3)),
            code = 15
          )
        )
      } else {
        # add the highlighting and comments for other errors
        rowIndex <- rowIndex + 1 + legacy_offset
        comment <- openxlsx::createComment(errorsTable$comment[thisErr$code], author = "Pmetrics", visible = F)
        openxlsx::addStyle(wb, sheet, errStyle1, rowIndex, colIndex)
        openxlsx::writeComment(wb, sheet, xy = c(colIndex, rowIndex), comment = comment)
      }
    } # end errors for loop
    
    # Add summaries to each column with errors
    sum_errors <- dplyr::as_tibble(table(error_summary$column, error_summary$code, dnn = c("column", "code"))) %>%
    group_by(column) %>%
    summarize(n_err = sum(n))
    
    openxlsx::addStyle(wb, sheet, errStyle4, rows = 1 + legacy_offset, cols = as.numeric(sum_errors$column))
    comments <- purrr::map(1:nrow(sum_errors), ~ openxlsx::createComment(paste(
      sum_errors$n_err[.x],
      ifelse(sum_errors$n_err[.x] > 1, "errors", "error")
    ), author = "Pmetrics", visible = F))
    purrr::walk(1:nrow(sum_errors), ~ openxlsx::writeComment(wb, sheet, col = as.numeric(sum_errors$column[.x]), row = 1 + legacy_offset, comment = comments[[.x]]))
    
    # Writing out the header of the Pmetrics data file : version line....
    if (legacy) {
      openxlsx::writeData(wb, sheet, pmVersion, xy = c(1, 1))
    } # POPDATA...
    
    # ...and data frame column names
    openxlsx::writeData(wb, sheet, t(formattedCols), xy = c(1, 1 + legacy_offset), colNames = F)
    
    # Add the data
    openxlsx::writeData(wb, sheet, dat,
      rowNames = F, colNames = F, xy = c(1, 2 + legacy_offset),
      keepNA = T, na.string = "."
    )
    
    return(wb)
  }
  
  createInstructions <- function(wb) {
    # set colors for errors
    errColor <- "#FFFF00" # yellow, column header
    errColor2 <- "#00FF00" # green, cell
    errColor3 <- "#00AAFF" # blue, NA
    errColor4 <- "#FFAA00" # orange, summary
    
    # create styles for error formatting
    errStyle1 <- openxlsx::createStyle(fgFill = errColor)
    errStyle2 <- openxlsx::createStyle(fgFill = errColor2)
    errStyle3 <- openxlsx::createStyle(fgFill = errColor3)
    errStyle4 <- openxlsx::createStyle(fgFill = errColor4)
    textStyle <- openxlsx::createStyle(fontSize = 16)
    
    openxlsx::addWorksheet(wb, "Instructions", tabColour = "grey80")
    openxlsx::addStyle(wb, "Instructions", textStyle, rows = 1:8, cols = 1)
    openxlsx::addStyle(wb, "Instructions", textStyle, rows = 10:13, cols = 2)
    openxlsx::writeData(wb, "Instructions",
    c(
      "'Errors' tab contains your data which has been standardized if read using PM_data$new().",
      "Cells with errors are color coded according to table below.",
      "Hover your mouse over each cell to read pop-up comment with details.",
      "Comments on column headers in orange contain the total number of errors in that column.",
      "If fix = TRUE, which is default for PM_data$new(), there will be an additional 'After_Fix' tab.",
      "This tab contains your standardized data after Pmetrics attempted to repair your data.",
      "Residual errors will be indicated as for the 'Errors' tab.",
      "You can fix the remaining errors and save the 'After_Fix' tab as a new .csv data file."
    ),
    startCol = 1, startRow = 1
  )
  
  openxlsx::addStyle(wb, "Instructions", errStyle1, rows = 10, cols = 1)
  openxlsx::addStyle(wb, "Instructions", errStyle2, rows = 11, cols = 1)
  openxlsx::addStyle(wb, "Instructions", errStyle3, rows = 12, cols = 1)
  openxlsx::addStyle(wb, "Instructions", errStyle4, rows = 13, cols = 1)
  
  openxlsx::writeData(wb, "Instructions",
  c(
    "Errors specific to a particular column",
    "Errors not specific to a defined column, i.e. non-numeric entries or missing covariates at time 0.",
    "Malformed NA values, which should only be '.'",
    "Used for column headers to report the total number of errors in that column."
  ),
  startCol = 2, startRow = 10
)
return(wb)
}



# PLOT --------------------------------------------------------------------

#' @title Plot PM_data Time-Output Data
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots *PM_data* objects
#' @details
#' This function will plot raw and fitted time and concentration data with a variety of options.
#' By default markers are included and  have the following plotly properties:
#' `list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))`.
#' Markers can be joined by lines, default is `FALSE`. If chosen to be `TRUE`,
#' the joining lines will have the following properties:
#' `list(color = "dodgerblue", width = 1, dash = "solid"`.
#' The grid and legend are omitted by default.
#'
#' @method plot PM_data
#' @param x The name of an `PM_data` data object or loaded as a field
#' in a [PM_result] object
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param line Controls characteristics of lines as for all plotly plots.
#' Here  `line` is a list of two elements:
#' * `join`  Can either be a boolean or a list. If set to `TRUE` or
#' a list of plotly line attributes, it
#' will generate line segments joining observations. If set to
#' `FALSE`, no segments will be generated. The color of the joining line
#' is the same as the marker color for that line. To avoid confusion, the line
#' color cannot be changed. The default
#' values for the other elements of the `join` list, both of which can be
#' overriden are:
#'     - `width `Width of the segments, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#' Example: `line = list(join = list(dash = "longdash", width = 2))`
#' * `pred` Default is `FALSE`, which means that predictions will not be included
#' in the plot. To include predictions, supply one of the following:
#' * If plotting data contained in a [PM_result], use "pop" or "post" to include population or posterior predictions.
#' ** Example 1: `run1 <- PM_load(1); run1$data$plot(line = list(pred = "post"))`
#' * If plotting data not contained in a [PM_result], you may add the
#' name of a population [PM_pop] or posterior [PM_post] prediction object in a [PM_result] object. 
#' This might be useful if you want to see how the predictions from one population match 
#' the raw data from another.
#' ** Example 2: `dat <- PM_data$new("new.csv"); dat$plot(line = list(pred = run1$post))`.
#'
#' To format the predictions, supply `pred` as
#' a list, with the prediction object first, followed by named options to control the
#' prediction plot:
#' * icen Chooses the median or mean of each
#' subject's Bayesian posterior parameter distribution.  Default is "median",
#' but could be "mean".
#' * As for `join`, the color of the `pred` line is fixed to the same color as the marker.
#' Other parameters to pass to plotly to control line characteristics that join
#' the predictions are `width`, and `dash`.
#' Continuing Example 1 above: `pred = list("post", icen = "mean", width = 2)`.
#' Default formats are the same as for the `join` argument, since normally one would not plot
#' both lines joining observations and prediction lines, i.e., typical use would be
#' `line = list(join = F, pred = "post")`.
#' @param marker Formats the symbols plotting observations. `r template("marker")`.
#' When a `group` and/or multiple `outeq` are specified, the `$color` element should be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors as groups within `group`, as colors
#' will be interpolated to generate the correct number. The default when `group`
#' is specified is the "Set1" palette. When there are groups, the `$color` element for join or pred lines
#' will be set to the same as `marker$color`.
#' @param group Character vector naming one column in `x` to **group** by, e.g. "id" or
#' a covariate like "gender"
#' @param group_names A character vector of names to label the **groups** if `legend = TRUE`.
#' This vector must be the same length as the number of groups within `group`. If missing,
#' the vector will be generated from the unique values in `group`.
#' Example: `c("Male", "Female")` if `color = "gender"` and "gender" is a covariate
#' in the data.
#' @param mult `r template("mult")`
#' @param outeq `r template("outeq")` Default is 1, but can be multiple if present in the data, e.g. `1:2` or `c(1, 3)`.
#' In the case of multiple outputs, `group_colors` will be used to color the lines and markers.
#' @param out_names Character vector of names to label the outputs if `legend = TRUE`. These can be combined with `group_names`.
#' The number must match the number of outputs in `outeq`. If missing, the default is "Output 1", "Output 2", etc.
#' @param block `r template("block")` Default is 1, but can be multiple if present in the data, as for `outeq`.
#' @param tad `r template("tad")`
#' @param overlay Operator to overlay all time concentration profiles in a single plot.
#' The default is `TRUE`. If `FALSE`, will trellisplot subjects one at a time. Can also be
#' specified as a vector with number of rows and columns, e.g. `c(3, 2)` for 3 rows and
#' 2 columns of subject splots to include in each trellis.
#' @param legend `r template("legend")` Default is `FALSE` unless groups are specified with `color`above.
#' @param log `r template("log")`
#' @param grid `r template("grid")`
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param xlab `r template("xlab")` Default is "Time".
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
#' @param \dots `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_data], [PM_result]
#' @export
#' @examples
#' \dontrun{
#' # basic spaghetti plot
#' dataEx$plot()
#' # format line and marker
#' dataEx$plot(
#'   marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
#'   line = list(join = list(color = "orange"))
#' )
#' # include predictions with default format and suppress joining lines
#' dataEx$plot(
#'   line = list(join = FALSE, pred = NPex$post),
#'   xlim = c(119, 146)
#' )
#' # customize prediction lines
#' dataEx$plot(
#'   line = list(
#'     pred = list(NPex$post, color = "slategrey", dash = "dash"),
#'     join = FALSE
#'   )
#' )
#' }

#' @family PMplots

plot.PM_data <- function(
  x,
  include = NULL,
  exclude = NULL,
  line = list(join = TRUE, pred = FALSE),
  marker = TRUE,
  group = NULL,
  group_names = NULL,
  mult = 1,
  outeq = 1,
  out_names = NULL,
  block = 1,
  tad = FALSE,
  overlay = TRUE,
  legend,
  log = FALSE,
  grid = FALSE,
  xlab = "Time",
  ylab = "Output",
  title = "",
  xlim, ylim,
  print = TRUE, ...) {
    # Plot parameters ---------------------------------------------------------
    
    # process marker
    marker <- amendMarker(marker)
    marker$color <- map_chr(marker$color, \(x) substr(x, 1, 7)) # remove alpha if present, controlled by opacity
    
    highlight_color <- opposite_color(marker$color[1]) # in plotly_Utils.R
    
    
    # process line
    if (any(!base::names(line) %in% c("join", "pred"))) {
      cli::cli_warn(c("!" = "{.code line} should be a list with at most two named elements: {.code join}, {.code loess}, and/or {.code pred}.", "i" = "See {.fn Pmetrics::plot.PM_data}."))
    }
    if (is.null(line$join)) {
      line$join <- FALSE
    }
    if (is.null(line$pred)) {
      line$pred <- FALSE
    }
    
    join <- amendLine(line$join)
    if (is.logical(line$pred) && !line$pred) { # if line$pred is FALSE
      line$pred <- NULL
    }
    pred <- line$pred # process further later
    
    
    # get the rest of the dots
    layout <- amendDots(list(...))
    
    # legend
    if (missing(legend)) {
      if (is.null(group)) {
        legend <- FALSE
      } else {
        legend <- TRUE
      }
    }
    
    legendList <- amendLegend(legend)
    layout <- modifyList(layout, list(showlegend = legendList$showlegend))
    if (length(legendList) > 1) {
      layout <- modifyList(layout, list(legend = within(legendList, rm(showlegend))))
    }
    
    
    # grid
    layout$xaxis <- setGrid(layout$xaxis, grid)
    layout$yaxis <- setGrid(layout$yaxis, grid)
    
    # axis labels if needed
    layout$xaxis$title <- amendTitle(xlab)
    if (is.character(ylab)) {
      layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
    } else {
      layout$yaxis$title <- amendTitle(ylab)
    }
    
    
    # axis ranges
    if (!missing(xlim)) {
      layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
    }
    if (!missing(ylim)) {
      layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
    }
    
    # log y axis
    if (log) {
      layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
    }
    
    # title
    layout$title <- amendTitle(title, default = list(size = 20))
    
    # overlay
    if (is.logical(overlay)) { # T/F
      if (!overlay) { # F,default
        nrows <- 1
        ncols <- 1
      } # if T, no need to set nrows or ncols
    } else { # specified as c(rows, cols)
      nrows <- overlay[1]
      ncols <- overlay[2]
      overlay <- FALSE
    }
    
    # Data processing ---------------------------------------------------------
    dat <- x$clone() #make copy of x to work with
    
    # make blocks
    dat$standard_data <- makePMmatrixBlock(dat$standard_data)
    
    # time after dose
    if (tad) {
      dat$standard_data$time <- calcTAD(dat$standard_data)
      dat$standard_data <- dat$standard_data %>% arrange(id, time)
    }
    
    # filter
    presub <- dat$standard_data %>%
    filter(outeq %in% !!outeq, block %in% !!block, evid == 0) %>%
    includeExclude(include, exclude)
    
    
    
    # make group column for groups
    if (!is.null(group)) {
      if (!group %in% base::names(dat$standard_data)) {
        cli::cli_abort(c("x" = "{group} is not a column in the data."))
      }
      if (is.null(group_names)) {
        presub$group <- presub[[group]]
      } else if (length(group_names) < length(unique(presub[[group]]))) {
        cli::cli_abort(c("x" = "The number of names in {.var group_names} must be at least as long as the number of unique values in {.var group}."))
      } else {
        presub$group <- factor(presub[[group]], labels = group_names)
      }
    } else { # group was NULL
      presub <- presub %>% mutate(group = "")
    }
    
    
    # make outeq labels if more than one output being plotted
    if (length(outeq) > 1) {
      if (is.null(out_names)) {
        out_names <- paste0("Output ", 1:max(outeq))
      } else if (length(out_names) < max(outeq)) {
        cli::cli_abort(c("x" = "The number of names in {.var out_names} must be at least as long as the maximum number of outputs in {.var outeq}."))
      }
      # add outeq to group
      presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", ", out_names[outeq]))
    }
    
    # add blocks if more than one being plotted
    if (length(block) > 1) {
      presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", Block ", block))
    }
    
    # there will always be an Obs group
    presub <- presub %>%
    rowwise() %>%
    mutate(group = paste0(group, ", Obs "))
    
    presub$group <- stringr::str_replace(presub$group, "^\\s*,*\\s*", "")
    
    # add cens column if missing
    if (!"cens" %in% names(presub)) {
      presub$cens <- "none"
    }
    
    # select relevant columns
    sub <- presub %>%
    select(id, time, out, cens, outeq, group) %>%
    mutate(id = as.character(id)) %>%
    ungroup()
    sub$group <- factor(sub$group)
    
    # add identifier
    sub$src <- "obs"
    
    # remove missing
    sub <- sub %>% filter(out != -99)
    
    
    # now process pred data if there
    if (!is.null(pred)) {
      if (inherits(pred, c("PM_post", "PM_pop"))) { # only PM_post/pop was supplied, make into a list of 1
        pred <- list(pred$data)
      } else if (inherits(pred, c("PM_post_data", "PM_pop_data"))) { # only PM_post_data/PM_pop_data was supplied, make into a list of 1
        pred <- list(pred)
      } else if (pred[[1]] %in% c("pop", "post")) { # pred[[1]] was "pop" or "post"
        thisPred <- pred[[1]]
        if (is.null(x[[thisPred]])) { # post/pop missing because x was data not from a PM_result
          cli::cli_warn(c(
            "!" = "{.code pred = {thisPred}} can only be used as a shortcut when plotting {.cls PM_data} from a {.cls PM_result}.",
            "i" = "Supply a {.cls PM_result} object, e.g. {.code line = list(pred = run2$post)}, if you wish to add predictions otherwise."
          ))
          pred <- NULL
        } else { # post/pop present
          pred[[1]] <- x[[thisPred]]
        }
      } else { # pred[[1]] was not "pop", "post", PM_result$pop, or PM_result$post
        cli::cli_warn(c(
          "!" = "The {.var pred} argument is mis-specified.",
          "i" = "See the help for {.code plot.PM_data}."
        ))
        pred <- NULL
      }
      
      # process pred list to determine formatting
      if (length(pred) == 1) { # default
        predArgs <- TRUE
        icen <- "median"
      } else { # not default, but need to extract icen if present
        icen <- purrr::pluck(pred, "icen") # check if icen is in list
        if (is.null(icen)) { # not in list so set default
          icen <- "median"
        } else {
          purrr::pluck(pred, "icen") <- NULL
        } # was in list, so remove after extraction
        predArgs <- pred[-1]
      }
      
      predArgs <- amendLine(predArgs) # color will be set by obs later
      
      # filter and group by id
      if (!is.null(pred[[1]])) { # if pred not reset to null b/c of invalid pred[[1]]
        predsub <- pred[[1]] %>%
        filter(outeq %in% !!outeq, block %in% !!block, icen == !!icen) %>%
        mutate(cens = "none") %>% # always none for predictions
        includeExclude(include, exclude) %>%
        group_by(id)
        
        # time after dose
        if (tad) {
          predsub$time <- calcTAD(predsub)
        }
        
        # select relevant columns and filter missing
        predsub <- predsub %>%
        select(id, time, out = pred, cens, outeq) %>%
        filter(out != -99 & (cens == "none" | cens == 0))
        
        
        # add group
        lookup <- dplyr::distinct(sub, id, outeq, group)
        predsub <- predsub %>% dplyr::left_join(lookup, by = c("id", "outeq")) %>%
        mutate(group = factor(stringr::str_replace_all(group, "Obs", "Pred")))
        
        # add identifier
        predsub$src <- "pred"
      } else { # pred was reset to NULL b/c of invalid pred[[1]]
        predsub <- NULL
      }
    } else { # pred was NULL from beginning
      predsub <- NULL
    } # end pred processing
    
    
    
    # Plot function ----------------------------------------------------------
    
    dataPlot <- function(allsub, overlay, includePred) {
      
      group_colors <- marker$color
      group_symbols <- marker$symbol
      if (!is.null(group) | length(outeq)>1 | length(block)>1) { # there was grouping beyond obs/pred
        
        n_colors <- length(unique(allsub$group))
        
        if (length(group_colors) < n_colors) { # fewer colors than groups, need to interpolate
          if (checkRequiredPackages("RColorBrewer")) {
            palettes <- RColorBrewer::brewer.pal.info %>% mutate(name = rownames(.))
            if (length(group_colors) == 1) { # only one color specified
              if (group_colors %in% palettes$name){# colors specified as a palette name
                max_colors <- palettes$maxcolors[match(group_colors, palettes$name)]
                group_colors <- colorRampPalette(RColorBrewer::brewer.pal(max_colors, group_colors))(n_colors)
              } else {
                group_colors <- c(group_colors, getDefaultColors(n_colors)[-1]) # in plotly_Utils, add default colors to specified color
              }
            } else { # length of group_colors > 1 but fewer than groups, so interpolate
              group_colors <- tryCatch(colorRampPalette(group_colors)(n_colors),
              error = function(e) {
                cli::cli_warn(c("!" = "Unable to interpolate colors, using default colors."))
                getDefaultColors(n_colors) # in plotly_Utils
              }
            )
          }
        } else {
          cli::cli_inform(c("i" = "Group colors are better with the {.pkg RColorBrewer} package installed."))
          colors <- getDefaultColors(n_colors) # in plotly_Utils
        }
      }
      
      if (length(group_symbols) < n_colors) { # fewer symbols than groups, need to interpolate
        if (length(group_symbols) == 1) { # only one symbol specified
          group_symbols <- rep(group_symbols, n_colors)
        } else { # multiple symbols specified, but fewer than groups
          group_symbols <- rep(group_symbols, length.out = n_colors)
        }
      }
      
    } else { # no grouping other than possibly pred
      if (includePred | join$width > 0) { # need colors for both obs and join or pred
        group_colors <- rep(group_colors, 2) # observed and predicted should be the same
      } 
    }
    
    
    # assign colors and symbols to each group, editing for censoring
    IDstring <- ifelse(overlay, "ID: {id}\n", "")
    allsub <- allsub %>%
    #rowwise() %>%
    mutate(
      color = group_colors[as.integer(group)],
      symbol = group_symbols[as.integer(group)]
    ) %>%
    mutate(
      color = dplyr::case_when(
        cens == "bloq" | cens == "1" | color == "aloq" | color == "-1" ~ opposite_color(color, degrees = 90),
        .default = color
      ),
      #color = ifelse(cens != "none" & cens != "0", opposite_color(color, degrees = 90), color),
      symbol = dplyr::case_when(
        cens == "bloq" | cens == "1" ~ "triangle-down", 
        cens == "none" | cens == "0" ~ as.character(symbol),
        cens == "aloq" | cens == "-1" ~ "triangle-up",
        .default = symbol),
        text_label = dplyr::case_when(
          cens == "bloq" | cens == "1"  ~ glue::glue(IDstring,"Time: {round2(time)}\nBLLQ: {round2(out)}\n{group}"),
          cens == "none" | cens == "0"  ~ glue::glue(IDstring,"Time: {round2(time)}\nOut: {round2(out)}\n{group}"),
          cens == "aloq" | cens == "-1" ~ glue::glue(IDstring,"Time: {round2(time)}\nAULQ: {round2(out)}\n{group}"),
          .default = glue::glue(IDstring,"Time: {round2(time)}\nPred: {round2(out)}\n{group}")
        )
      ) %>%
      ungroup()
      
      # if ID is numeric, arrange by numeric ID
      if(overlay && !any(is.na(suppressWarnings(as.numeric(allsub$id))))) {
        allsub <- allsub %>%
        mutate(id = as.numeric(id)) %>% arrange(id, time)
      }
      
      
      seen_groups <- NULL
      traces <- if(overlay) {allsub %>% dplyr::group_split(id)} else {list(allsub)}
      
      # Build plot
      p <- plot_ly()
      for (i in seq_along(traces)) {
        trace_data <- traces[[i]]
        if (any(!unique(trace_data$group) %in% seen_groups)) {
          seen_groups <- c(seen_groups, as.character(unique(trace_data$group)))
          legendShow <- TRUE
        } else {
          legendShow <- FALSE
        }
        this_id <- ifelse(overlay, trace_data$id[1], 1)
        
        p <- add_trace(
          p,
          data = trace_data %>% plotly::filter(src == "obs") %>% arrange(group, time),
          x = ~time, y = ~ out * mult,
          type = "scatter",
          mode = "markers",
          split = ~group,
          name = ~group,
          uid = as.character(this_id),
          meta = list(id = this_id),
          marker = list(color = ~I(color), symbol = ~I(symbol), size = marker$size, opacity = marker$opacity,
          line = list(color = marker$line$color, width = marker$line$width)),
          text = ~text_label,
          hoverinfo = "text",
          legendgroup = ~group,
          showlegend = legendShow
        )
        
        # add joining lines if needed
        if (join$width > 0){   
          trace_split <- trace_data %>% filter(src == "obs") %>% dplyr::group_split(color)
          for(j in seq_along(trace_split)){
            this_color <- trace_split[[j]]$color[1]
            p <- add_trace(
              p,
              data = trace_split[[j]],
              x = ~time, y = ~(out * mult),
              type = "scatter", mode = "lines",
              name = ~group,
              uid = as.character(this_id),
              meta = list(id = this_id),
              line = list(color = this_color, width = join$width, dash = join$dash),
              text = ~text_label,
              hoverinfo = "text",
              legendgroup = ~group,
              showlegend = FALSE
            )
          }
        }
        
        if (includePred) {   
          trace_split <- trace_data %>% filter(src == "pred") %>% dplyr::group_split(color)
          for(j in seq_along(trace_split)){
            this_color <- trace_split[[j]]$color[1]
            p <- add_trace(
              p,
              data = trace_split[[j]],
              x = ~time, y = ~(out * mult),
              type = "scatter", mode = "lines",
              name = ~group,
              uid = as.character(this_id),
              meta = list(id = this_id),
              line = list(color = this_color, width = predArgs$width, dash = predArgs$dash),
              text = ~text_label,
              hoverinfo = "text",
              legendgroup = ~group,
              showlegend = legendShow
            )
          }
        }
      }
      
      p <- p %>% plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        title = layout$title,
        showlegend = layout$showlegend,
        legend = layout$legend
      )
      return(invisible(p))
    } # end dataPlot
    
    
    # Call plot ---------------------------------------------------------------
    
    
    # if pred present, need to combine data and pred for proper display
    
    if (!is.null(predsub)) {
      allsub <- dplyr::bind_rows(sub, predsub) %>% dplyr::arrange(id, time)
      includePred <- TRUE
    } else {
      allsub <- sub
      includePred <- FALSE
    }
    
    
    # call the plot function and display appropriately
    if (overlay) {
      allsub <- allsub %>% dplyr::group_by(id)
      p <- dataPlot(allsub, overlay = TRUE, includePred)
      
      if (print) print(click_plot(p, highlight_color = highlight_color))
      return(invisible(p))
    } else { # overlay = FALSE, ie. split them
      
      if (!checkRequiredPackages("trelliscopejs")) {
        cli::cli_abort(c("x" = "Package {.pkg trelliscopejs} required to plot when {.code overlay = FALSE}."))
      }
      
      sub_split <- allsub %>%
      nest(data = -id) %>%
      mutate(panel = trelliscopejs::map_plot(data, \(x) dataPlot(x, overlay = FALSE, includePred = includePred)))
      p <- sub_split %>%
      ungroup() %>%
      trelliscopejs::trelliscope(name = "Data", nrow = nrows, ncol = ncols)
      if (print) print(p)
    }
    
    return(invisible(p))
  }
  # SUMMARY -----------------------------------------------------------------
  
  #' @title Summarize PM_data objects
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' Summarize the raw data used for a Pmetrics run.
  #'
  #' @method summary PM_data
  #' @param object A [PM_data] object.
  #' @param formula Optional formula for specifying custom summaries.  See [aggregate]
  #' and [formula] for details on how to specify formulae in R. If, for example, the data contain
  #' a covariate for weight named 'wt', then to summarize the mean dose in mg/kg per subject specify
  #' `formula = dose/wt ~ id` and  `FUN = mean`.
  #' @param FUN The summary function to apply to [formula], if specified. This is not
  #' quoted, and usual choices will be [mean], [median], [max], or [min].
  #' @param include A vector of subject IDs to include in the summary, e.g. `c(1:3,5,15)`
  #' @param exclude A vector of subject IDs to exclude in the summary, e.g. `c(4,6:14,16:20)`
  #' @param ... Additional arguments to `FUN`, e.g. `na.rm = TRUE`
  #' @return A list of class *summary.PM_data* with the following items:
  #' * **nsub** Number of subjects
  #' * **ndrug** Number of drug inputs
  #' * **numeqt** Number of outputs
  #' * **nobsXouteq** Number of observations by outeq
  #' * **missObsXouteq** Number of missing observations by outeq
  #' * **loqObsXouteq** Number of observations coded as below the limit of quantification by outeq
  #' * **ncov** Number of covariates
  #' * **covnames** Covariate names
  #' * **ndoseXid** Number of doses per input per subject
  #' * **nobsXid** Number of observations per outeq per subject
  #' * **doseXid** Doses per input per subject
  #' * **obsXid** Observations per outeq per subject
  #' * **formula** Results of including [formula]
  #' @author Michael Neely
  #' @seealso [aggregate]
  #' @export
  
  summary.PM_data <- function(object, formula, FUN, include, exclude, ...) {
    
    if(inherits(object, "PM_data")) {
      object <- object$standard_data
    } 
    
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
    
    # censored
    
    results$bloqObsXouteq <- purrr::map_int(1:max(object$outeq, na.rm = TRUE), \(x) sum(object$cens[object$outeq == x] == "1", object$cens[object$outeq == x] == "bloq", na.rm = TRUE)) 
    results$aloqObsXouteq <- purrr::map_int(1:max(object$outeq, na.rm = TRUE), \(x) sum(object$cens[object$outeq == x] == "-1", object$cens[object$outeq == x] == "aloq", na.rm = TRUE)) 
    
    covinfo <- getCov(object)
    ncov <- covinfo$ncov
    results$ncov <- ncov
    results$covnames <- covinfo$covnames
    results$ndoseXid <- as.matrix(tapply(object$evid, list(object$id, object$input), function(x) length(x != 0))[idOrder, ])
    results$nobsXid <- as.matrix(tapply(object$evid, list(object$id, object$outeq), function(x) length(x == 0))[idOrder, ])
    results$doseXid <- as.matrix(tapply(object$dose, list(object$id, object$input), function(x) x[!is.na(x)])[idOrder, ])
    results$obsXid <- as.matrix(tapply(object$out, list(object$id, object$outeq), function(x) x[!is.na(x)])[idOrder, ])
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
    
    class(results) <- c("summary.PM_data", "list")
    return(results)
  } # end function
  # PRINT SUMMARY -----------------------------------------------------------------
  
  #' @title Print Summary of Pmetrics Data
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' @details
  #' Print the summary of [PM_data] object.
  #'
  #' Summarize the raw data used for a Pmetrics run.
  #'
  #' @method print summary.PM_data
  #' @param x An object made by [summary.PM_data].
  #' @return A printed object
  #' @author Michael Neely
  #' @param ... Not used.
  #' @seealso [summary.PM_data]
  #' @examples
  #' \dontrun{
  #' dataEx$summary()
  #' }
  
  #' @export
  
  print.summary.PM_data <- function(x, ...) {
    #   order of objects
    #   nsub
    #   ndrug
    #   numeqt
    #   nobsXouteq
    #   missObsXouteq
    #   bloqObsXouteq
    #   aloqObsXouteq
    #   ncov
    #   ndoseXid
    #   nobsXid
    #   doseXid
    #   obsXid
    #   cov
    #   formula
    
    cli::cli_div(theme = list(
      span.blue = list(color = navy())
    ))
    cli::cli_h1("Data Summary")
    
    cli::cli_text("Number of subjects: {.blue {x$nsub}}")
    cli::cli_text("Number of inputs: {.blue {x$ndrug}}")
    cli::cli_text("Number of outputs: {.blue {x$numeqt}}")
    if (x$ncov > 0) {
      cli::cli_text(" Covariates: {.blue {x$covnames}}")
    }
    cli::cli_h2("Inputs: Mean (SD), Min to Max")
    for (i in 1:x$ndrug) {
      if (x$ndrug > 1) {
        cli::cli_h3("Input {i}")
      }
      cli::cli_text("Number of doses per subject: {.blue {sprintf('%.3f', mean(x$ndoseXid[, i], na.rm = T))}} ({.blue {sprintf('%.3f', sd(x$ndoseXid[, i], na.rm = T))}}), {.blue {sprintf('%.3f', min(x$ndoseXid[, i], na.rm = T))}} to {.blue {sprintf('%.3f', max(x$ndoseXid[, i], na.rm = T))}} ")
      cli::cli_text("Dose amount per subject: {.blue {sprintf('%.3f', mean(unlist(x$doseXid[, i]), na.rm = T))}} ({.blue {sprintf('%.3f', sd(unlist(x$doseXid[, i]), na.rm = T))}}), {.blue {sprintf('%.3f', min(unlist(x$doseXid[, i]), na.rm = T))}} to {.blue {sprintf('%.3f', max(unlist(x$doseXid[, i]), na.rm = T))}} ")
      
    }
    cli::cli_h2("Outputs: Mean (SD), Min to Max")
    for (i in 1:x$numeqt) {
      if (x$numeqt > 1) {
        cli::cli_h3("Output {i}")
      } 
      nobs <- unlist(x$nobsXid[, i])
      mean_nobs <- mean(nobs, na.rm = T)
      sd_nobs <- sd(nobs, na.rm = T)
      min_nobs <- min(nobs, na.rm = T)  
      max_nobs <- max(nobs, na.rm = T)
      
      obs <- unlist(x$obsXid[, i])
      obs <- obs[obs != -99]
      mean_obs <- mean(obs, na.rm = T)
      sd_obs <- sd(obs, na.rm = T)
      min_obs <- min(obs, na.rm = T)
      max_obs <- max(obs, na.rm = T)
      
      if (x$bloqObsXouteq[i] > 0) {
        extra_text <- ", and {.blue {x$bloqObsXouteq[i]}} ({.blue {sprintf('%.3f', 100 * x$bloqObsXouteq[i] / x$nobsXouteq[i])}%}) censored as below a lower LOQ"
      } else {
        extra_text <- ""
      }
      
      if (x$aloqObsXouteq[i] > 0) {
        extra_text <- paste0(extra_text, ", and {.blue {x$aloqObsXouteq[i]}} ({.blue {sprintf('%.3f', 100 * x$aloqObsXouteq[i] / x$nobsXouteq[i])}%}) censored as above an upper LOQ")
      } 
      cli::cli_text("Total across all subjects: {.blue {x$nobsXouteq[i]}}, with {.blue {x$missObsXouteq[i]}} ({.blue {sprintf('%.3f', 100 * x$missObsXouteq[i] / x$nobsXouteq[i])}%}) missing", extra_text, ".")
      cli::cli_text("Number per subject: {.blue {sprintf('%.3f', mean_nobs)}} ({.blue {sprintf('%.3f', sd_nobs)}}), {.blue {sprintf('%i', min_nobs)}} to {.blue {sprintf('%i', max_nobs)}} ")
      cli::cli_text("Value per subject: {.blue {sprintf('%.3f', mean_obs)}} ({.blue {sprintf('%.3f', sd_obs)}}), {.blue {sprintf('%.3f', min_obs)}} to {.blue {sprintf('%.3f', max_obs)}} ")
    }
    if (x$ncov > 0) {
      cli::cli_h2("Population level covariates: Mean (SD), Min to Max")
      for (i in 1:x$ncov) {
        cli::cli_text("{x$covnames[i]}: {.blue {sprintf('%.3f', mean(unlist(x$cov[[i]]), na.rm = T))}} ({.blue {sprintf('%.3f', sd(unlist(x$cov[[i]]), na.rm = T))}}),  {.blue {sprintf('%.3f', min(unlist(x$cov[[i]]), na.rm = T))}} to {.blue {sprintf('%.3f', max(unlist(x$cov[[i]]), na.rm = T))}}")
      }
    }
    
    if (!is.null(x$formula)) {
      cli::cli_h2("Formula Results")
      print(x$formula)
    }
    cli::cli_text("")
    cli::cli_text("{.strong Note:} See {.help summary.PM_data} for more summary options using {.arg formula}.")
  } # end function
  # WRITE -------------------------------------------------------------------
  
  #' @title Write a Pmetrics .csv Matrix File
  #' @description
  #' `r lifecycle::badge("superseded")`
  #'
  #' This function is largely superseded as the function is accessed with
  #' the `$save()` method for [PM_data] objects. There is rarely a need to call
  #' it directly. It is the companion function to [PMreadMatrix].
  #' It will write an appropriate R data object to a formatted .csv file.
  #' @details
  #' *PMwriteMatrix* will first run [PMcheck] to determine
  #' if there are any errors in the structure of `data`.  If the error check
  #' fails, the file will not be written and a message will be printed on the console.
  #'
  #' @param data Must be a data.frame with appropriate structure (see [PMcheck]).
  #' @param filename Name of file to create.
  #' @param override Boolean operator to write even if errors are detected.  Default is `FALSE`.
  #' @param version Which matrix data format version to write.  Default is the current version.
  #' @param header Is there a header row? Default is `FALSE` as this was the legacy format.
  #' @return Returns the error report (see [PMcheck] for details).
  #' @author Michael Neely
  #' @seealso [PM_data], [PMcheck], [PMreadMatrix]
  #' @export
  #' @examples
  #' \dontrun{
  #' # write to the current directory
  #' NPex$data$save("data.csv")
  #' }
  PMwriteMatrix <- function(
    data, filename, override = FALSE,
    version = "DEC_11", header = FALSE) {
      if (!override) {
        err <- PMcheck(data, quiet = TRUE)
        if (length(grep("FAIL", err)) > 0) {
          cli::cli_warn(c("!" = "Write failed; returning errors."))
          return(invisible(err))
        }
      } else {
        err <- NULL
      }
      # remove the block column if added during run
      if ("block" %in% names(data)) {
        data <- data %>% dplyr::select(-block)
      }
      
      versionNum <- as.numeric(substr(version, 5, 7)) + switch(substr(version, 1, 3),
      JAN = 1,
      FEB = 2,
      MAR = 3,
      APR = 4,
      MAY = 5,
      JUN = 6,
      JUL = 7,
      AUG = 8,
      SEP = 9,
      OCT = 10,
      NOV = 11,
      DEC = 12
    ) / 100
    if (versionNum < 11.12) {
      if (tolower(names(data)[6]) == "addl") data <- data[, c(-6, -7)]
    }
    OS <- getOS()
    eol <- c("\r\n", "\n", "\r\n")[OS]
    f <- file(filename, "w")
    if (header) {
      writeLines(paste("POPDATA ", version, "\n#", sep = ""), f, sep = "")
    }
    writeLines(toupper(names(data)[-ncol(data)]), sep = getPMoptions("sep"), f)
    writeLines(toupper(names(data)[ncol(data)]), f)
    write.table(data, f,
      row.names = FALSE, na = ".", quote = F, sep = getPMoptions("sep"),
      dec = getPMoptions("dec"), col.names = F, eol = eol
    )
    close(f)
    return(invisible(err))
  }
  