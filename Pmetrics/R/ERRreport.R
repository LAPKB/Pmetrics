#' Generates a summary of an ERR run
#'
#' Creates an HTML page summarizing an ERR run.  This report is generated
#' automatically at the end of a successful run.
#'
#' @title Summarize ERR Run
#'
#' @param wd The working directory containing the ASS0001 file
#' @param icen Not used, but included for compatibility with other report functions
#' @param type Not used, but included for compatibility with other report functions
#' @return Two files are placed in the \code{wd}
#' \item{ASS0001 }{A text file of the results}
#' \item{errlog }{A text file with a log of the session},
#' @author Michael Neely

ERRreport <- function(wd,icen,type){
  setwd(wd)
  if (file.exists("ASS0001")){
    ERRdata <- readLines("ASS0001")
    cat("\n\n")
    flush.console()
    .CSSfile <- paste(.libPaths(),"/Pmetrics/report/Pmetrics.css",sep="")
    HTMLfileName <- paste(getwd(),"/ERRreport.html",sep="")
    if(file.exists(HTMLfileName)) file.remove(HTMLfileName)
    #open HTML file as global object
    .HTMLfile <<- file(HTMLfileName,open="a+")

    #HTML header
    writeHTML(c("<html>",
                "<head>",
                "<title>Pmetrics Report</title>",
                paste("<link rel=stylesheet href=\"",.CSSfile,"\" type=text/css>",sep=""),
                "</head>",
                "<body>"))
    
    #header
    writeHTML("<div class='wrap'>") #wrapper around header
    writeHTML("<div class='header'>")
    dtstamp <- format(Sys.time(),"%d %b %Y at %H:%M")
    writeHTML("<div class='wrap'>") #wrapper around columns
    writeHTML("<div class='leftcol'>")
    writeHTML(paste("<img src=\"",path.package("Pmetrics"),"/report/pmetrics-logo.png\" class=\"pmetrics\">",sep=""))
    writeHTML(paste("<h4>Report generated ",dtstamp,"</h4>",sep=""))
    writeHTML("</div>") #end left column
    writeHTML("<div class='fullrightcol'>")
    writeHTML("<h4>Laboratory of Applied Pharmacokinetics<br>4650 Sunset Blvd., Los Angeles, CA 90027<br>Tel: +01 323 361 2509
              <br><a href='http://www.lapk.org' target='_blank'>www.lapk.org</a><br>")
    writeHTML("</div>")  #end right column
    writeHTML("</div>")  #end column wrap
    writeHTML("</div>")  #end header
    writeHTML("</div>")  #end header wrap
    
    #HTML body    
    writeHTML("<h2>Assay Error Results (C0, C1, C2, C3)</h2>")

    errorLine <- grep(" ESTIMATES FOR \\[C0,C1,C2,C3\\] FOR EACH",ERRdata)
    if(length(errorLine)>0){
      outLine <- ERRdata[errorLine]
      nout <- as.numeric(strparse(" [[:digit:]]+ ",outLine))
      coeff <- ERRdata[errorLine+1:nout]
      coeff <- gsub("^ +","",coeff)
      coeff <- sapply(coeff,function(x) strsplit(x," +"))
      for(i in 1:nout){
        writeHTML(paste("Output ",i,": ",paste(coeff[[i]],collapse=", "),sep=""))
      }
      
    } else {
      writeHTML("No estimates found for C0, C1, C2, C3 in ASS0001")
    }

    close(.HTMLfile)

  } else {cat("ASS0001 is not in the current directory.\n")}
} #end function
