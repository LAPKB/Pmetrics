#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run.
#'
#'
#' @title Load Pmetrics NPAG or IT2B output
#' @param run The numerical value of the folder number containing the run results.
#' Loading results of a prior standard run in folder "1" are as
#' simple as `run1 <- PM_load(1)`. There is no default value for this, and if
#' missing, Pmetrics will only search the current working directory for output files.
#' @param file Optional name of an .Rdata file created by running the
#' `$save` method for a [PM_result] object. For example,
#' `run2 <- PM_load(2, "other.Rdata")` will look in the run 2 folder outputs
#' for a file named "other.Rdata". `PM_load(file = "other.Rdata")` will look in the
#' current working directory, since `run` is missing. If `file` is missing,
#' Pmetrics will attempt to find a "PMout.Rdata" or the older "NPAGout.Rdata" or
#' "IT2Bout.Rdata" files in either the current working directory (if `run` is not
#' specified) or the `run/outputs` folder, if `run` is provided.
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


PM_load <- function(run = 1, remote = F, server_address) {
  # If Rust
  npcore_out <- paste(run, "NPcore.Rdata", sep = "/")
  if (file.exists(npcore_out)) {
    load(npcore_out)
    result <- get("NPcore")
    return(PM_result$new(result, backend = "rust"))
  }

  # declare variables to avoid R CMD Check flag
  NPAGout <- NULL
  IT2Bout <- NULL


  if (missing(server_address)) server_address <- getPMoptions("server_address")


  # check for NPAG output file
  filename <- "NPAGout.Rdata"
  if (is.numeric(run)) {
    outfile <- paste(run, "outputs", filename, sep = "/")
  } else {
    outfile <- paste(run, filename, sep = "/")
  }

  if (remote) { # only look on server
    if (missing(server_address)) server_address <- getPMoptions("server_address")
    status <- .remoteLoad(thisrun, server_address)
    if (status == "finished") {
      result <- output2List(Out = NPAGout)
      return(PM_result$new(result, quiet = T)) # no errors
    } else {
      sprintf("Warning: Remote run #%d has not finished yet.\nCurrent status: \"%s\"\n", thisrun, status) %>%
        cat()
      return(invisible(NULL))
    }
  } else if (file.exists(outfile)) { # remote F, so look locally
    # load(outfile, .GlobalEnv)
    load(outfile)
    result <- output2List(Out = get("NPAGout"))
  } else {
    # check for IT2B output file
    filename <- "IT2Bout.Rdata"
    if (is.numeric(run)) {
      outfile <- paste(run, "outputs", filename, sep = "/")
    } else {
      outfile <- paste(run, filename, sep = "/")
    }
    if (file.exists(outfile)) {
      load(outfile)
      result <- output2List(Out = get("IT2Bout"))
    }
    # if file supplied
    if (!missing(file)) {
      # try in current wd
      if (file.exists(file)) {
        found <- T
      } else {
        # nope, try in an outputs folder
        if (!missing(run)) {
          file <- paste0(run, "/outputs/", file)
          if (file.exists(file)) {
            found <- T
          }
        }
      }
    } else {
      # didn't have file, so check for other outputs
      if (!missing(run)) {
        file_list <- c("PMout.Rdata", "NPAGout.Rdata", "IT2Bout.Rdata")
        for (i in file_list) {
          file <- paste0(run, "/outputs/", i)
          if (file.exists(file)) {
            found <- T
            break
          }
        }
      }
    }

    if (found) {
      result <- output2List(Out = get(load(file)))
      return(PM_result$new(result, quiet = T)) # no errors
    } else {
      stop(paste0("No Pmetrics output file found in ", getwd(), ".\n"))
    }
  }
}

output2List <- function(Out) {
  result <- list()
  for (i in 1:length(Out)) {
    aux_list <- list(Out[[i]])
    names(aux_list) <- names(Out)[i]
    result <- append(result, aux_list)
  }

  return(result)
}

#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run
#'
#'
#'
#' @title Load Pmetrics NPAG or IT2B output (Legacy)
#' @param run The numerical value of the folder number containing the run results.  This
#' number will also be used to name objects uniquely by appending \dQuote{.\code{run}},
#' e.g. NPdata.1 or ITdata.1 if run=1. This parameter is \code{1} by default.
#' @param \dots Additional runs to load if desired.
#' @param remote Default is \code{FALSE}.  Set to \code{TRUE} if loading results of an NPAG run on remote server.
#' See \code{\link{NPrun}}. Currently remote runs are not configured for IT2B or the Simulator.
#' @param server_address If missing, will use the default server address returned by getPMoptions().
#' Pmetrics will prompt the user to set this address the first time the \code{remote} argument is set to \code{TRUE}
#' in \code{\link{NPrun}}.
#' @return The following objects are loaded into R.
#' \item{NPdata/ITdata }{List with all output from NPAG/IT2B}
#' \item{pop }{ NPAG only: Population predictions for each output equation}
#' \item{post }{ NPAG only: Individual posterior predictions for each output equation}
#' \item{final }{Final cycle population support points and parameter summary statistics}
#' \item{cycle }{Cycle log-likelihood, AIC, BIC, Gamma/lambda, and normalized parameter means, medians and SDs}
#' \item{op }{List of observed vs. population and posterior predicted plots for each output equation}
#' \item{cov }{Data frame of subject ID, covariate values, and Bayesian posterior parameter estimates}
#' \item{mdata }{The original .csv data file used in the run}
#' \item{valid }{If \code{\link{makeValid}} has been executed after a run, this object will be added to
#' the save data.  It contains the information required to plot visual predictive checks and normalized prediction
#' error discrepancies via the npde code developed by Comets et al}
#' @author Michael Neely
#' @seealso \code{\link{PMreport}}, \code{\link{NPparse}}, \code{\link{ITparse}},
#' \code{\link{makeFinal}}, \code{\link{makeCycle}}, \code{\link{makeOP}}, \code{\link{makeCov}},
#' \code{\link{makePop}}, \code{\link{makePost}}
#' @export

PMload <- function(run = 1, ..., remote = F, server_address) {
  cat("This function is for legacy Pmetrics.\nPlease see documentation for R6 Pmetrics
      and PM_load\n")
  # declare variables to avoid R CMD Check flag
  NPAGout <- NULL
  IT2Bout <- NULL

  if (missing(server_address)) server_address <- getPMoptions("server_address")
  addlruns <- list(...)
  if (length(addlruns) > 0) {
    allruns <- c(run, unlist(addlruns))
  } else {
    allruns <- run
  }

  for (thisrun in allruns) {
    # check for NPAG output file
    filename <- "NPAGout.Rdata"
    outfile <- paste(thisrun, "outputs", filename, sep = "/")

    if (remote) { # only look on server
      status <- .remoteLoad(thisrun, server_address)
      if (status == "finished") {
        .splitOut(thisrun, NPAGout)
      } else {
        sprintf("Warning: Remote run #%d has not finished yet.\nCurrent status: \"%s\"\n", thisrun, status) %>%
          cat()
      }
    } else if (file.exists(outfile)) { # remote F, so look locally
      # load(outfile, .GlobalEnv)
      load(outfile)
      .splitOut(thisrun, get("NPAGout"))
    } else {
      # check for IT2B output file
      filename <- "IT2Bout.Rdata"
      outfile <- paste(thisrun, "outputs", filename, sep = "/")
      if (file.exists(outfile)) {
        load(outfile)
        .splitOut(thisrun, get("IT2Bout"))
      } else {
        cat(paste(outfile, " not found in ", getwd(), "/", thisrun, "/outputs or ", getwd(), ".\n", sep = ""))
        return(invisible(F)) # error, abort
      }
    }
  }
  # end thisrun loop


  return(invisible(T)) # no errors
}

.splitOut <- function(run, Out) {
  newNames <- paste(names(Out), ".", as.character(run), sep = "")
  for (i in 1:length(newNames)) {
    assign(newNames[i], Out[[i]], pos = .GlobalEnv)
  }
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

.remoteLoad <- function(run, server_address) {
  status <- ""
  rid <- .getRemoteId(run)
  status <- .PMremote_check(rid = rid, server_address = server_address)
  if (status == "finished") {
    sprintf("Remote run #%d finished successfuly.\n", run) %>%
      cat()
    .PMremote_outdata(run, server_address)
  }
  return(status)
}
