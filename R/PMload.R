#' @title Load Pmetrics NPAG or IT2B output
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run.
#'
#'
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
#' @param remote Default is `FALSE`.  Set to `TRUE` if loading results of an NPAG run on remote server.
#' See [NPrun]. Currently remote runs are not configured for IT2B or the Simulator.
#' @param server_address If missing, will use the default server address returned by getPMoptions().
#' Pmetrics will prompt the user to set this address the first time the `remote` argument is set to `TRUE`
#' in [NPrun].
#' @return An R6 [PM_result].
#' @author Michael Neely and Julian Otalvaro
#' @seealso [PMreport], [NPparse], [ITparse],
#' [makeFinal], [makeCycle], [makeOP], [makeCov],
#' [makePop], [makePost]
#' @export


PM_load <- function(run, file, remote = F, server_address) {
  # Code below replaced by general handling; MN 6/15/23
  # #If Rust
  # if (getPMoptions()$backend == "rust") {
  #   if (!missing(file) && file.exists(file)) {
  #     npcore_out <- file
  #   } else {
  #     npcore_out <- paste(run, "NPcore.Rdata", sep = "/")
  #   }
  #   if (file.exists(npcore_out)) {
  #     load(npcore_out)
  #     result <- get("NPcore")
  #     return(PM_result$new(result, backend = "rust"))
  #   } else {
  #     stop(paste0("No NPcore.Rdata file found in ", run, ".\n"))
  #   }
  # }
  
  
  # declare variables to avoid R CMD Check flag
  NPAGout <- NULL
  IT2Bout <- NULL
  found <- ""
  
  # internal function
  output2List <- function(Out) {
    result <- list()
    for (i in 1:length(Out)) {
      aux_list <- list(Out[[i]])
      names(aux_list) <- names(Out)[i]
      result <- append(result, aux_list)
    }
    
    return(result)
  }
  
  if (remote) { # only look on server - this needs to be updated
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
  } else if (!missing(file)) { # not remote, file supplied, so look for it
    # try from current wd
    if (file.exists(file)) {
      found <- file
    } else {
      # nope, try in an outputs folder
      if (!missing(run)) {
        file <- paste0(run, "/outputs/", file)
        if (file.exists(file)) {
          found <- file
        }
      }
    }
  } else { # didn't have file, so check for other outputs
    if (missing(run)) {
      wd <- "./" # we can only look in current directory
    } else {
      wd <- paste0(run, "/outputs/")
    }
    file_list <- c("NPcore.Rdata", "PMout.Rdata", "NPAGout.Rdata", "IT2Bout.Rdata")
    for (i in file_list) {
      file <- paste0(wd, i)
      if (file.exists(file)) {
        found <- file
        break
      }
    }
  }
  
  if (found != "") {
    result <- output2List(Out = get(load(found)))
    #update
    result2 <- update(result, found)
    return(PM_result$new(result2, quiet = TRUE))
    
    
  } else {
    stop(paste0("No Pmetrics output file found in ", getwd(), ".\n"))
  }
}


#internal update function
update <- function(res, found){
  msg <- NULL
  #CYCLE
  if(!is.null(res$cycle)){
    dat <- res$cycle
    if(
      !tibble::is_tibble(dat$gamlam) #version prior to 2.2, add next update via or join
    ){ 
      #start conversion
      n_cyc <- nrow(dat$mean)
      n_out <- max(res$op$outeq)
      dat$gamlam <- tibble::as_tibble(dat$gamlam, .name_repair = "minimal") 
      if(ncol(dat$gamlam) == 1 & n_out > 1){dat$gamlam <- cbind(dat$gamlam, replicate((n_out-1),dat$gamlam[,1]))} 
      names(dat$gamlam) <- as.character(1:ncol(dat$gamlam))
      dat$gamlam <- dat$gamlam %>% pivot_longer(cols = everything(), 
                                                values_to = "value", names_to = "outeq") %>%
        mutate(cycle = rep(1:n_cyc, each = n_out)) %>%
        select(cycle, value, outeq)
      if(is.matrix(dat$mean)){ #old fortran format, but not rust format
        dat$mean <- tibble::tibble(cycle = 1:n_cyc) %>% 
          dplyr::bind_cols(tidyr::as_tibble(dat$mean))
        dat$median <- tibble::tibble(cycle = 1:n_cyc) %>% 
          dplyr::bind_cols(tidyr::as_tibble(dat$median))
        dat$sd <- tibble::tibble(cycle = 1:n_cyc) %>% 
          dplyr::bind_cols(tidyr::as_tibble(dat$sd))
      }
      msg <- c(msg, "cycle")
      res$cycle <- dat
    }
  }
  
  ####### DONE PROCESSING, INFORM #########
  if(!is.null(msg)){
    cat(crayon::blue("NOTE: "), 
        "The", 
        crayon::green(dplyr::case_when(
          length(msg)==1 ~ msg,
          length(msg)==2 ~ paste(msg, collapse = " and "),
          length(msg)>2 ~ paste(msg, collapse = ", ")
        )[1]),
        ifelse(length(msg)>1, "fields", "field"), 
        "in your PM_result object",
        ifelse(length(msg)>1, "have", "has"),
        "been updated",
        "to the most current format.",
        "\n\n",
        crayon::blue("1"), "Save the updates\n",
        crayon::blue("2"), "Do not save updates\n ")
    flush.console()
    ans <- readline(" ")
    if(ans == 1){
      temp <- PM_result$new(res)
      temp$save(file = found)
      cat("Results saved\n")
    }
  }
  
  return(res)
}


#' @title Load Pmetrics NPAG or IT2B output
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run.
#' This function has been superseded by [PM_load], which returns objects.
#' In contrast, *PMload* loads them directly into the Global environment, which
#' is not best-practice programming.
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
