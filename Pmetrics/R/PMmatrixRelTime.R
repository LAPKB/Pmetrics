#' \code{PMmatrixRelTime} will convert absolute dates and times in a dataset
#' into relative hours, suitable for Pmetrics analysis.  Additionally, the user has
#' the option to split subjects into pseudosubjects every time a dose reset (evid=4)
#' is encountered.
#'
#' @title Convert Absolute Dates and Times to Relative Hours
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
#' @return Returns a dataframe with columns [id, evid, relTime].
#'  If \code{split}=T all evid values that were previously 4 will be converted to 1.
#' @author Michael Neely
#' @seealso \code{\link{PMreadMatrix}}

PMmatrixRelTime <- function(data,idCol="id",dateCol="date",timeCol="time",evidCol="evid",format=c("m/d/y","h:m"),split=F){
  
  if(length(grep("chron",installed.packages()[,1]))==0){
    install.packages("chron",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  chron.installed <- require(chron)
  if(!chron.installed) stop("Error: connect to internet and re-run PMmatrixRelTime to download and install chron package.\n")
  
  dataCols <- names(data)
  #convert numeric if necessary
  if(is.numeric(idCol)) idCol <- dataCols[idCol]
  if(is.numeric(dateCol)) dateCol <- dataCols[dateCol]
  if(is.numeric(timeCol)) timeCol <- dataCols[timeCol]
  if(is.numeric(evidCol)) evidCol <- dataCols[evidCol]
  
  if(!all(c(idCol,dateCol,timeCol,evidCol) %in% dataCols)){stop("Please provide column names for id, date, time and evid.\n")}
  temp <- data.frame(id=data[,idCol],date=data[,dateCol],time=data[,timeCol],evid=data[,evidCol])
  temp$date <- as.character(temp$date)
  temp$time <- as.character(temp$time)
  temp$time <- unlist(lapply(temp$time,function(x) ifelse(length(gregexpr(":",x)[[1]])==1,paste(x,":00",sep=""),x)))
  if(format[2]=="h:m") format[2] <- "h:m:s"
  temp$dt <- chron(dates.=temp$date,times.=temp$time,format=format)
  
  if(split){
    #calculate PK event numbers for each patient
    for (i in unique(temp$id)){
      pk.no <- 1
      temp2 <- subset(temp,temp$id==i)
      for (j in 1:nrow(temp2)){
        if (temp2$evid[j]==4) {pk.no <- pk.no + 1}
        temp2$pk.no[j] <- pk.no
      }
      temp$pk.no[temp$id==i] <- temp2$pk.no
    }  
    #make new ID of form xxxxx.x for each PK event per patient
    temp$id <- temp$id + temp$pk.no/10
    temp$evid[temp$evid==4] <- 1
    
  }
  
  temp$relTime <- 0
  reset <- which(temp$evid==4)
  new <- which(!duplicated(temp$id))
  reset <- c(reset,nrow(temp),new)
  reset <- sort(reset)
  for (i in 1:(length(reset)-1)){
    temp$relTime[reset[i]:reset[i+1]] <- 24*(temp$dt[reset[i]:reset[i+1]] - temp$dt[reset[i]])
  }
  
  temp$relTime <- round(temp$relTime,2)
  
  return(temp[,c("id","evid","relTime")])
  
}

