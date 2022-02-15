#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run
#'
#'
#'
#' @title Load Pmetrics NPAG or IT2B output
#' @param run The numerical value of the folder number containing the run results.
#' This parameter is \code{1} by default.
#' @param \dots Additional runs to load if desired.
#' @param remote Default is \code{FALSE}.  Set to \code{TRUE} if loading results of an NPAG run on remote server.
#' See \code{\link{NPrun}}. Currently remote runs are not configured for IT2B or the Simulator.
#' @param server_address If missing, will use the default server address returned by getPMoptions(). 
#' Pmetrics will prompt the user to set this address the first time the \code{remote} argument is set to \code{TRUE}
#' in \code{\link{NPrun}}. 
#' @return An R6 \code{\link{PM_result}}.
#' @author Michael Neely and Julian Otalvaro
#' @seealso \code{\link{PMreport}}, \code{\link{NPparse}}, \code{\link{ITparse}}, 
#' \code{\link{makeFinal}}, \code{\link{makeCycle}}, \code{\link{makeOP}}, \code{\link{makeCov}}, 
#' \code{\link{makePop}}, \code{\link{makePost}}
#' @export

PM_load <- function(run = 1, ..., remote = F, server_address) {
  
  #declare variables to avoid R CMD Check flag
  NPAGout <- NULL
  IT2Bout <- NULL
  
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  addlruns <- list(...)
  if (length(addlruns) > 0) {
    allruns <- c(run, unlist(addlruns))
  } else { allruns <- run }
  
  for (thisrun in allruns) {
    #check for NPAG output file
    filename <- "NPAGout.Rdata"
    outfile <- paste(thisrun, "outputs", filename, sep = "/")
    
    if (remote) { #only look on server
      status = .remoteLoad(thisrun, server_address)
      if (status == "finished") {
        result <- .splitOut(NPAGout)
      } else {
        sprintf("Warning: Remote run #%d has not finished yet.\nCurrent status: \"%s\"\n", thisrun, status) %>%
        cat()
      }
    } else if (file.exists(outfile)) { #remote F, so look locally
      # load(outfile, .GlobalEnv)
      load(outfile)
      result <-.splitOut(get("NPAGout"))
    } else {
      #check for IT2B output file
      filename <- "IT2Bout.Rdata"
      outfile <- paste(thisrun, "outputs", filename, sep = "/")
      if (file.exists(outfile)) {
        load(outfile)
        result<-.splitOut(get("IT2Bout"))
      } else {
        cat(paste(outfile, " not found in ", getwd(), "/", thisrun, "/outputs or ", getwd(), ".\n", sep = ""))
        return(invisible(F)) #error, abort
      }
    }

  }
  #end thisrun loop


  return(PM_result$new(result)) #no errors

}

.splitOut <- function(Out) {
  result<-list()
  for (i in 1:length(names(Out))) {
    aux_list <- list(Out[[i]])
    names(aux_list)<-names(Out)[i]
    result <- append(result,aux_list)
  }
  result
}

.remoteLoad <- function(run, server_address) {
  status = ""
  rid <- .getRemoteId(run)
  status = .PMremote_check(rid = rid, server_address = server_address)
  if (status == "finished") {
    sprintf("Remote run #%d finished successfuly.\n", run) %>%
        cat()
    .PMremote_outdata(run, server_address)

  }
  return(status)
}

.getRemoteId <- function(run) {
  run <- toString(run)
  fileName <- paste(run, "inputs", "id.txt", sep = "/")
  if (file.exists(fileName)) {
    return(readChar(fileName, file.info(fileName)$size) %>% gsub("\n", "", .data))
  } else {
    stop(sprintf("File id.txt not found in /%s/outputs.\n", run))
    return(NULL)
  }
}

