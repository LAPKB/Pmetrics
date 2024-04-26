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
# #' @param remote Default is `FALSE`.  Set to `TRUE` if loading results of an NPAG run on remote server.
# #' See [NPrun]. Currently remote runs are not configured for IT2B or the Simulator.
# #' @param server_address If missing, will use the default server address returned by getPMoptions().
# #' Pmetrics will prompt the user to set this address the first time the `remote` argument is set to `TRUE`
#' in [NPrun].
#' @return An R6 [PM_result].
#' @author Michael Neely and Julian Otalvaro
#' @seealso [NPparse], [ITparse],
#' [makeFinal], [makeCycle], [makeOP], [makeCov],
#' [makePop], [makePost]
#' @export


PM_load <- function(run, file) {

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
  
  # if (remote) { # only look on server - this needs to be updated
  #   if (missing(server_address)) server_address <- getPMoptions("server_address")
  #   status <- .remoteLoad(thisrun, server_address)
  #   if (status == "finished") {
  #     result <- output2List(Out = NPAGout)
  #     return(PM_result$new(result, quiet = T)) # no errors
  #   } else {
  #     sprintf("Warning: Remote run #%d has not finished yet.\nCurrent status: \"%s\"\n", thisrun, status) %>%
  #       cat()
  #     return(invisible(NULL))
  #   }
  # } else 
    
    if (!missing(file)) { # not remote, file supplied, so look for it
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
    #file_list <- c("NPcore.Rdata", "PMout.Rdata", "NPAGout.Rdata", "IT2Bout.Rdata")
    file_list <- c("PMout.Rdata", "NPAGout.Rdata", "IT2Bout.Rdata")
    
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

