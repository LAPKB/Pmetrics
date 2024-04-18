#' @title Write a Pmetrics .csv Matrix File
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is largely superseded as the function is accessed with
#' the `$write()` method for [PM_data] objects. There is rarely a need to call
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
#' @param header Is there a header row? Default is `TRUE`.
#' @return Returns the error report (see [PMcheck] for details).
#' @author Michael Neely
#' @seealso [PM_data], [PMcheck], [PMreadMatrix]
#' @examples
#' \dontrun{
#' # write to the current directory
#' library(PmetricsData)
#' NPex$data$write("data.csv")
#' }
PMwriteMatrix <- function(data, filename, override = F, version = "DEC_11", header = T) {
  if (!override) {
    err <- PMcheck(data, quiet = T)
    if (length(grep("FAIL", err)) > 0) {
      cat("Write failed; returning errors.")
      return(invisible(err))
    }
  } else {
    err <- NULL
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
    row.names = F, na = ".", quote = F, sep = getPMoptions("sep"),
    dec = getPMoptions("dec"), col.names = F, eol = eol
  )
  close(f)
  return(invisible(err))
}
