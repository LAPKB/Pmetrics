#' \code{PMreadMatrix} reads an NPAG .csv matrix input file into R.
#'
#' The structure of a valid .csv file is fairly rigid.  See \code{\link{PMcheckMatrix}}
#'  for details.  Note that \code{PMreadMatrix} converts the column headers in the
#'  \code{matrixfile} from upper to lowercase for convenient referencing in R.
#'
#' @title Read a Pmetrics .csv Matrix Input File
#' @param file The name of the file to be loaded, including the full path if not
#'  in the current working directory (check with \code{\link{getwd}}).
#' @param skip Skip \emph{n} lines, with default set to 1.
#' @param sep Delimiter between columns, which is a comma by default, but can be changed with
#' \code{\link{setPMoptions}}.
#' @param dec Decimal separator, which is a period by default, but can be changed with
#' \code{\link{setPMoptions}}.
#' @param quiet Default is \emph{false}.  If \emph{true}, there will be no report to
#'  the console on the contents of file.
#' @param \dots Other parameters to be passed to \code{\link{read.table}}
#' @return \code{PMreadMatrix} returns a data.frame of class \dQuote{PMmatrix} with one row per
#'  event and the following columns.  
#'  \item{id }{The id value for each event.}
#'  \item{evid }{The evid value for each event, with 0=observation, 1=dose, 4=dose reset, which resets the time to 0 and all compartment amounts to 0.  Note that evid=2 and 3 are not currently implemented.}
#'  \item{time }{Relative time of the event in hours.}
#'  \item{dur }{Duration of the dose.  If dose is instantaneous, e.g. an oral dose into an absorptive compartment, \code{dur} should be 0.  Any values greater than 0 are interpreted to mean a constant infusion of that duration, equalling the \code{dose}.}
#'  \item{dose }{The dose.  Be sure that the units are consistent with \code{out}.}
#'  \item{addl }{Optional number of additional doses to add at an interval specified in \emph{ii}.  The default if missing is 0.  A value of -1
#'  will cause steady state conditions to be approximated.  Any value for \emph{addl} other than 0 or missing requires input in \emph{ii}.}
#'  \item{ii }{The interdose interval for \emph{addl} doses or dosing at steady state.}
#'  \item{input }{The input number corresponding to \code{dose}.}
#'  \item{out }{The measured output, equivalent to \dQuote{DV} in some other PK modeling software tools.}
#'  \item{outeq }{The number of the output equation specified in the model file which corresponds to the \code{out} value.}
#'  \item{C0 }{Assay error polynomial coefficient, e.g. SD = C0 + C1*obs + C2*obs^2 + C3*obs^3}
#'  \item{C1 }{See \code{C0}}
#'  \item{C2 }{See \code{C0}}
#'  \item{C3 }{See \code{C0}}
#'  \item{\dots }{Additional columns are interpreted to be covariates.}
#'  If the file is successfully read and \code{quiet}=F,
#'  the column headers of the scanned file will be reported to the console as a validation check.
#' @author Michael Neely 
#' @seealso \code{\link{PMwriteMatrix}}, \code{\link{PMcheckMatrix}}, and \code{\link{plot.PMmatrix}}
PMreadMatrix <- function(file,skip=1,sep=getPMoptions("sep"),dec=getPMoptions("dec"),quiet=F,...){
#get data
  if (missing(file)){
    warning("Please provide filename of Pmetrics block input file.\n")
    return(invisible())
  }
  if (!file.exists(file)){
    warning(paste("The file ",sQuote(file)," was not found in the current working directory:\n",getwd(),"\n",sep=""))
    return(invisible())
  }
  
  parnames <- scan(file,skip=skip,what="character",quiet=T,nlines=1,sep=sep,dec=dec,strip.white=T)
  parnames[1] <- "id"
  data <- read.table(file,skip=skip+1,header=F,na.strings=".",comment.char="#",sep=sep,dec=dec,stringsAsFactors=F,...)
  names(data) <- tolower(parnames)

  
  if(!quiet){
    cat(paste("The file",sQuote(file),"contains these columns:\n",sep=" "))
    cat(names(data))
    cat("\n")
  }
  
  class(data) <- c("PMmatrix","data.frame")
  return(data)
 }

