#Use menu item Code -> Jump To... for rapid navigation
#Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ---------------------------------------------------------------


#' @title Results of a Pmetrics run
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object contains all of the results after a Pmetrics runs. It is created
#' by using the [PM_load] function.
#'
#' @details After a run completes, results are stored on your hard drive. They are loaded
#' back into R with [PM_load] to create the [PM_result] object, which contains both
#' the results and functions to analyze or plot the result.
#'
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_result <- R6::R6Class(
  "PM_result",
  public <- list(
    #' @field NPdata List with all output from NPAG, made by [NPparse]
    NPdata = NULL,
    #' @field ITdata List with all output from IT2B, made by [ITparse]
    ITdata = NULL,
    #' @field pop A [PM_pop] object
    pop = NULL,
    #' @field post A [PM_post] object
    post = NULL,
    #' @field final A [PM_final] object
    final = NULL,
    #' @field cycle A [PM_cycle] object
    cycle = NULL,
    #' @field op A [PM_op] object
    op = NULL,
    #' @field cov A [PM_cov] object
    cov = NULL,
    #' @field data [PM_data] object representing the original .csv data file used in the run. The predictions contained in the `$data` fields from `$pop` and `$post` will be added to this [PM_data] object to permit easy addition of such predictions to raw data plots. See [plot.PM_data] for more details.
    data = NULL,
    #' @field model text string representing the original model file used in the run
    model = NULL,
    #' @field errfile Name of error file if it exists
    errfile = NULL,
    #' @field success Boolean if successful run
    success = NULL,
    #' @field valid If the `$validate` method has been executed after a run, this object will be added to
    #' the `PM_result` object.  It contains the information required to plot visual predictive checks and normalized prediction
    #' error discrepancies via the npde code developed by Comets et al. Use the
    #' `$save` method on the augmented `PM_result` object to save it with the
    #' new validation results.
    valid = NULL,
    
    #' @description
    #' Create new object populated with data from previous run
    #' @details
    #' Creation of new `PM_result` objects is via [PM_load].
    #' @param out The parsed output from [PM_load], which is
    #' automatically generated. This is not a user-modifiable.
    #' @param quiet Quietly validate. Default is `FALSE`.
    initialize = function(out, quiet = TRUE) {
      if (!is.null(out$NPdata)) {
        self$NPdata <- out$NPdata
        class(self$NPdata) <- c("NPAG", "list")
        allData <- "NPdata"
      } else {
        self$NPdata <- NULL
      }
      if (!is.null(out$ITdata)) {
        self$ITdata <- out$ITdata
        class(self$ITdata) <- c("IT2B", "list")
        allData <- "ITdata"
      } else {
        self$ITdata <- NULL
      }
      if(is.null(out$NPdata)) {
        self$NPdata <- out
        class(self$NPdata) <- c("NPAG", "rust", "list")
        allData <- "NPdata"
      }
      #the following were saved as R6 objects
      purrr::walk(c("pop", "post", "final", "cycle", "op", "cov", "data", "model", "valid"),
                  \(x){
                    self[[x]] <- NULL
                    if(!is.null(out[[x]])){ #if the object is loaded...
                      if(!inherits(out[[x]], "R6")){ #older save
                        if(inherits(out[[x]], paste0("PM",x))){
                          self[[x]] <- get(paste0("PM_",x))$new(out[[x]]) #...make the R6 from old PMxxx
                        } else {
                          self[[x]] <- get(paste0("PM_",x))$new(out[[allData]]) #...make the R6 from raw
                        }
                      } else {
                        self[[x]] <- get(paste0("PM_",x))$new(out[[x]], quiet = TRUE) #was saved in R6 format, but remake to update if needed
                      }
                    }
                  })
      # self$pop <- out$pop
      # self$post <- out$post
      # self$final <- out$final
      # self$cycle <- out$cycle
      # self$op <- out$op
      # self$cov <- out$cov
      # self$data <- out$data
      # self$model <- out$model 
      # if (!is.null(out$valid)) {
      #   self$valid <- out$valid
      # } else {
      #   self$valid <- NULL
      # }
      #these are diagnostics, not R6
      self$errfile <- out$errfile
      self$success <- out$success
      
      #add the pop/post data to data
      if(is.null(self$data$pop) | is.null(self$data$post)){
        self$data <- PM_data$new(self$data$data, quiet = TRUE)
        self$data$pop <- self$pop$data
        self$data$post <- self$post$data
      }
      
      return(self)
    },
    
    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param ... Plot-specific arguments
    plot = function(type, ...) {
      if (is.null(type)) {
        stop("Please provide the type of plot.")
      } else {
        self[[type]]$plot(...)
      }
    },
    
    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param ... Summary-specific arguments
    summary = function(type, ...) {
      if (is.null(type)) {
        stop("please provide the type of summary you want to obtain")
      } else {
        self[[type]]$summary(...)
      }
    },
    
    #' @description
    #' AUC generic function based on type
    #' @param type Type of AUC based on class of object
    #' @param ... Summary-specific arguments
    auc = function(type, ...) {
      if (!type %in% c("op", "pop", "post", "sim")) {
        stop("makeAUC is defined only for PM_op, PM_pop, PM_post, PM_sim objects.\n")
      }
      self[[type]]$auc(...)
    },
    
    #' @description
    #' Perform non-compartmental analysis
    #' @details
    #' See [makeNCA].
    #' @param ... Arguments passed to [makeNCA].
    nca = function(...) {
      make_NCA(self, ...)
    },
    
    #' @description
    #' Simulates using the self$final object.
    #' For parameter information refer to [SIMrun]. It will return a `PM_sim` object
    #' by running [SIMparse] at the end of the simulation.
    #' @param ... Parameters passed to [SIMrun]
    sim = function(...) {
      # store copy of the final object
      bk_final <- self$final$clone()
      sim <- PM_sim$new(poppar = self, ...)
      self$final <- bk_final
      return(sim)
    },
    
    #' @description
    #' Save the current PM_result object to an .Rdata file.
    #' @details
    #' This is useful if you have updated the result in some way, for example you
    #' have run the `$make_valid` method on the `PM_result` object, which returns
    #' an internal simulation based validation as a new `valid` field. To save this
    #' validation, use this `$save` method. Note that unless a `file` name is provided,
    #' the changes will overwrite the
    #' previous run results, although unchanged items will be preserved. This is the
    #' usual workflow. However, a custom file name may be useful to share the run
    #' results with someone.
    #'
    #' The saved object is an .Rdata file. When loaded, it should be assigned to an R
    #' object, e.g. `run2 <- PM_result$new("filename")`. An equivalent statement would
    #' be `run2 <- PM_load(file = "filename")`.
    #' @param run The run output folder number to save the revised result. If missing,
    #' will save in the current working directory. For example, if folder "1" is in
    #' your current working directory, specify `run = 1` to save the result to the "outputs"
    #' subfolder of the "1" folder.
    #' @param file Custom file name. Default is "PMout.Rdata".
    save = function(run, file) {
      if (missing(run)) {
        outputfolder <- getwd()
      } else {
        if (is.na(suppressWarnings(as.numeric(run)))) {
          stop("The run argument is not numeric. Do you need to say 'file = '? See help for PM_result.")
        }
        outputfolder <- paste0(run, "/outputs")
        if (!file.exists(outputfolder)) {
          stop(paste0(outputfolder, " does not exist from the current working directory./n"))
        }
      }
      if (missing(file)) {
        # if(is.null(self$NPdata$backend)){
        file <- "PMout.Rdata"
        # } else {
        #   file <- "NPcore.Rdata"
        # }
      } 
      PMout <- list(
        NPdata = self$NPdata,
        ITdata = self$ITdata,
        pop = self$pop$data, post = self$post$data,
        final = self$final$data, cycle = self$cycle$data,
        op = self$op$data, cov = self$cov$data, data = self$data$data,
        model = self$model, errfile = self$errfile,
        success = self$success,
        valid = self$valid
      )
      save(PMout, file = paste0(outputfolder, "/", file))
    },
    
    #' @description
    #' Validate the result by internal simulation methods.
    #' @param ... Arguments passed to [make_valid].
    validate = function(...) {
      self$valid <- PM_valid$new(self, ...)
      self$valid
    },
    
    #' @description
    #' Conduct stepwise linear regression of Bayesian posterior parameter values
    #' and covariates.
    #' @param ... Arguments passed to [PMstep].
    step = function(...) {
      PMstep(self$cov$data, ...)
    },
    
    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' Method to compute 1 or more MM-optimal sampling times.
    #' @details
    #' Based on the multiple-model optimization algorithm, this method computes *nsamp* times where the concentration time profiles are the most separated, thereby minimizing the risk of choosing the incorrect Bayesian posterior for an individual. The algorithm was published as 
    #' Bayard, David S., and Michael Neely. "Experiment Design for Nonparametric
    #' Models Based on Minimizing Bayes Risk: Application to Voriconazole."
    #' Journal of Pharmacokinetics and Pharmacodynamics 44, no. 2 (April 2017):
    #' 95–111. https://doi.org/10.1007/s10928-016-9498-5.
    #'
    #' @param data One of two choices which will provide the template dosage and sampling regimens:
    #' * [PM_data] object
    #' * Character vector with the filename of a Pmetrics data file
    #' that contains template regimens and observation times.   
    #' 
    #' If `data` is missing, the default value of "data.csv" will be used and Pmetrics will try to open a so-named filed in teh current working directory. In either choice, the value for outputs
    #' can be coded as any number(s) other than -99.  The number(s) will be replaced in the simulator output with the simulated values.
    #' @param nsamp The number of MM-optimal sample times to compute; default is 1, but can be any number.  Values >4 will take an exponentially longer time.
    #' @param weight List whose names indicate the type of weighting, and values indicate the relative weight. Values should sum to 1.  Names can be any of the following:
    #' * **none** The default. MMopt times will be chosen to maximally discriminate all responses at all times.
    #' * **AUC** MMopt times will be chosen to maximally discriminate AUC, regardless of the shape of the response profile.
    #' * **max** MMopt times will be chosen to maximally discriminate maximum, regardless of the shape of the response profile.
    #' * **min** MMopt times will be chosen to maximally discriminate minimum, regardless of the shape of the response profile.
    #'
    #' Any combination of AUC, max, and min can be chosen.  If "none" is specified, other
    #' weight types will be ignored and the relative value will be set to 1.
    #' For example,`list(auc = 0.5,max = 0.5)` or `list(auc = 0.2, min = 0.8)`.
    #' The default is `list(none = 1)`.
    #' @param predInt The interval in fractional hours for simulated predicted outputs at times other than those specified in the template `data`.
    #' The default is 0.5, which means there will be simulated outputs every 30 minutes from time 0 up
    #' to the maximal time in the template file.  You may also specify `predInt`
    #' as a vector of 3 values, e.g. `c(1,4,1)`, similar to the R command [seq], where the
    #' first value is the start time, the second is the stop time, and the third is the
    #' step value.  Outputs for times specified in the template file will also be simulated.
    #' To simulate outputs *only* at the output times in the template data (i.e. EVID=0 events), use `predInt = 0`.
    #' Note that the maximum number of predictions total is 594, so the interval must be sufficiently large to accommodate this for a given
    #' number of output equations and total time to simulate over.  If `predInt` is set so that this cap is exceeded, predictions will be truncated.
    #' @param mmInt Specify the time intervals from which MMopt times can be selected.
    #' These should only include simulated times specified by `predInt`.
    #' @param outeq Output equation to optimize
    #' @param clean Boolean parameter to specify whether temporary files made in the
    #' course of the simulation run should be deleted. Defaults to `TRUE`.
    #' This is primarily used for debugging.
    #' @param ... Other parameters to pass to [SIMrun], which are not usually necessary.
    #' @return A object of class *MM_opt* with 3 items.
    #' * **sampleTime** The MM-optimal sample times
    #' * **bayesRisk** The Bayesian risk of mis-classifying a subject based on the sample times.  This
    #' is more useful for comparisons between sampling strategies, with minimization the goal.
    #' * **simdata** A [PM_sim] object with the simulated profiles
    #' * **mmInt** A list with the `mmInt` values, `NULL` if `mmInt` is missing.
    #' @examples
    #' library(PmetricsData)
    #' mmopt1 <- NPex$mmopt(data = exData, limits = NA)
    #' mmopt1$summary()
    #' mmopt1$plot()
    #' 
    mmopt = function(data = NULL, nsamp = 1, weight = list(none = 1),
                     predInt = 0.5, mmInt = NULL, outeq = 1, clean = TRUE,...) {
      make_mmopt(poppar = self, data = data, nsamp = nsamp, weight = weight,
                 predInt = predInt, mmInt = mmInt, outeq = outeq, 
                 clean = clean,...)
    },
    
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load prior results saved with the `$save` method.
    #' Replaced by [PM_load].
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "PM_result$load() is deprecated. Please use PM_load() instead.")
    }
  ) # end public
) # end PM_result

#' @keywords internal
#' @name PM_result
#' @export
PM_result$load <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "Please use PM_load() instead. ?PM_load for details.")
}









# LOAD --------------------------------------------------------------------
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










