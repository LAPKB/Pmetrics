#' \code{PMwriteMatrix} is the companion function to \code{\link{PMreadMatrix}}.
#'  It will write an appropriate R data object to a formatted .csv file.
#'
#' \code{PMwriteMatrix} will first run \code{\link{PMcheck}} to determine
#'  if there are any errors in the structure of  \code{data}.  If the error check
#'  fails, the file will not be written and a message will be printed on the console.
#'
#' @title Write a Pmetrics .csv Matrix File
#' @param data Must be a data.frame with appropriate structure (see \code{\link{PMcheck}}.
#' @param filename Name of file to create.
#' @param override Boolean operator to write even if errors are detected.  Default is \code{False}.
#' @param version Which matrix data format version to write.  Default is the current version.
#' @return Returns the error report (see \code{\link{PMcheck}} for details).
#' @author Michael Neely
#' @seealso \code{\link{PMcheck}}, \code{\link{PMreadMatrix}}
#' @examples
#' \dontrun{
#' data <- PMreadMatrix(paste(.libPaths(),"/Pmetrics/example/NPAG/PMex1.csv",sep=""))
#' data
#' #write to the current directory
#' PMwriteMatrix(data,"PMex1.csv")
#' }

PMwriteMatrix <- function(data,filename,override=F,version="DEC_11"){
  
  if(!override){
    err <- PMcheck(data)
    if(length(grep("FAIL",err))>0){
      cat("Write failed; returning errors.")
      return(invisible(err))
    }
  } else {err <- NULL}
  versionNum <- as.numeric(substr(version,5,7)) + switch(substr(version,1,3),JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)/100 
  if(versionNum < 11.12){
    if(tolower(names(data)[6])=="addl") data <- data[,c(-6,-7)]
  }
  OS <- getOS()
  eol <- c("\r\n","\n","\r\n")[OS]  
  f <- file(filename,"w")  
  writeLines(paste("POPDATA ",version,"\n#",sep=""),f,sep="")
  writeLines(toupper(names(data)[-ncol(data)]),sep=getPMoptions("sep"),f)
  writeLines(toupper(names(data)[ncol(data)]),f)
  write.table(data,f,row.names=F,na=".",quote=F,sep=getPMoptions("sep"),
              dec=getPMoptions("dec"),col.names=F,eol=eol)
  close(f)
  return(invisible(err))
}

