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

PMmatrixRelTime <- function(data, idCol = "id", dateCol = "date", timeCol = "time", evidCol = "evid",
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
    stop("Please provide column names for id, date, time and evid.\n")
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
    stop("All dates/times failed to parse. Please specify correct format. ")
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
