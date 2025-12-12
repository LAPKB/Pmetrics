# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ----------------------------------------------------------------------


#' @title Create Probability of Target Attainment (PTA) object
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object class contains results of simulations and a probability of
#' target attainment analysis.
#'
#' @details
#' There are two ways of creating a *PM_pta* object.
#'
#' * **PM_sim$pta()** This way uses the simulation method directly from
#' a [PM_sim] object.
#' * **PM_pta$new()** This way takes an external [PM_sim] result as an argument
#' and creates the PTA. It is described here.
#'
#' Both methods require the prior creation of a simulation of
#' appropriate regimens.
#'
#' @author Julian Otalvaro and Michael Neely
#' @export
PM_pta <- R6::R6Class(
  "PM_pta",
  public <- list(
    #' @field data Contains the raw results.
    data = NULL,
    #' @description
    #' Create a new `PM_pta` object.
    #' @details
    #' This function will calculate the probability of target attainment (PTA)
    #' for any number of simulations, targets and definitions of success.
    #' Simulations typically differ by dose, but may differ by other features
    #' such as children vs. adults.
    #'
    #' @param simdata Can be one of multiple inputs as shown in the examples below using.
    #'
    #' * `simEx` *PM_sim*
    #' * `simEx$data` *PM_simlist*
    #' * `simEx$data[[1]]` *PM_sim_data*
    #' * `NPex$post` *PM_post*
    #' * `NPex$post$data` *PM_post_data*
    #' * `NPex$data` *PM_data*
    #' * `NPex$data$standard_data` *PM_data_data*
    #' * `"simout.txt"` matches files with wildcard ability, see [PM_sim]
    #'
    #' @param simlabels Optional character vector of labels for each simulation.  Default is `c('Regimen 1', 'Regimen 2',...)`.
    #' @param target One of several options.
    #'
    #' * A vector of pharmacodynamic targets, such as Minimum Inhibitory Concentrations (MICs), e.g. `c(0.25, 0.5, 1, 2, 4, 8, 16, 32)`.
    #' * A single numerical value such as a concentration, e.g. 10.
    #' * A sampled distribution using  [makePTAtarget].
    #' * A list of multiple targets combining the above if multiple `target_type`s are used. If so, the first `target` can be a vector,
    #' but subsequent targets must be single values to avoid factorial expansion of combinations. For example, the first target could be a vector of MICs corresponding
    #' to a `target_type` of "time", the second target a value of 10 corresponding to a `target_type` of "min", and the third target a value of 50
    #' corresponding to a `target_type` of "max" as in this example:
    #'  `target = list(c(0.25, 0.5, 1, 2, 4, 8, 16, 32), 10, 50)`. The first value can also be a sampled
    #' distribution made with [makePTAtarget].
    #'
    #' @param target_type A vector of the type for each `target`.  For any, place a minus sign
    #' in front to make the success less than the target ratio, e.g. `target_type = c("min", "-min")`.
    #' Available types:
    #'
    #' * "time" is percent time above `target` within the time range specified by `start` and `end`.
    #' Use "-time" for percent time below `target`.
    #' * "auc" is ratio of area under the curve within the time range to `target`
    #' * "peak" or "max", ratio of peak or max (synonymous) concentration to `target` within the time range.
    #' Use "-max" or "-peak" to make the success less than the target ratio.
    #' * "min", is the ratio of minimum concentration to `target` within the time range.
    #' Use "-min" to make the success less than the target ratio.
    #' * A single numeric value, which must correspond to an observation time common to all PMsim objects in
    #' `simdata`, rounded to the nearest hour.  In this case, the target statistic will be the ratio of observation at that time to `target`.
    #'
    #' This enables testing of a specific timed concentration (e.g. one hour after a dose or C1).  Be sure that the time in the simulated data is used,
    #' e.g., 122 after a dose given at 120. As for the other target types, make the number negative
    #' to make the success less than the target ratio, eg -122.
    #' @param success A vector specifying the success statistics, e.g. 0.4 for proportion time (end-start) above target, and/or 100 for max:target.
    #' For example `success = 0.4` or `success = c(0.4, 100)`. The length must be the same as for `target` and `target_type`.
    #' @param outeq An integer specifying the number of the simulated output equation to use. Default is 1.
    #' @param free_fraction Proportion of free, active drug, expressed as a numeric value >=0 and <=1.  Default is 1, i.e.,
    #' 100% free drug or 0% protein binding.
    #' @param start Specify the time to begin PTA calculations. Default is a vector with the first observation time for subjects
    #' in each element of `simdata`, e.g. dose regimen. If specified as a vector, values will be recycled as necessary.
    #' @param end Specify the time to end PTA calculations so that PTA is calculated
    #' from `start` to `end`.  Default for end is the maximum observation
    #' time for subjects in each element of `simdata`, e.g. dose regimen.  If specified as a vector, values will be recycled
    #' as necessary. Subjects with insufficient data (fewer than 5 simulated observations) for a specified interval will trigger a warning.
    #' Ideally then, the simulated datset should contain sufficient observations within the interval specified by `start` and `end`.
    #' @param icen Can be either "median" for the predictions based on medians of `pred.type` parameter value
    #' distributions, or "mean".  Default is "median".
    #' @param block Which block to plot, where a new block is defined by dose resets (evid = 4); default is 1.
    #' @param ... Not currently used
    #' @return A list of class *PM_pta_data*, included in the `data` field of the [PM_pta] object.
    #' The list contains each `target_type` as an element, followed by a final `intersection`
    #' element showing the results for profiles which meet ALL the conditions (intersection)
    #' or `NA` if only one `target_type` was specified.
    #' The individual elements are tibbles with all possible combinations
    #' of `target`s and simulated regimens for a given `target_type`. The tibbles have the following columns:
    #' * **reg_num** The simulation number in `simdata`.
    #' * **label** Annotation of the simulation, supplied by the `simlabels` argument.
    #' * **target** is the specified `target` for the results row. If a distribution created by [makePTAtarget],
    #' this will be a tibble with  the simulated targets
    #' * **type** is the specified `target_type` for the results row
    #' * **success_ratio** The specified `success` metric for the results row
    #' * **prop_success** The proportion of profiles meeting the `success_ratio` for the results row
    #' * **success** A tibble of success (1) or not (0) for each profile for the results row
    #' * **pdi** A tibble of the pharmacodynamic index, i.e. the ratio or time above for each profile for the results row
    #' * **start** The start time used for the results row
    #' * **end** The end time used for the results row.
    #' For the `$intersect` item in the return list, the columns are the same, but the
    #' `target` and `target_type` will reflect all requested values expressed in
    #' parenthetical multiplication format to emphasize intersection, e.g., (auc)(min).
    #' Simulated (rather than discrete) targets made with [makePTAtarget] will be
    #' abbreviated as "(sim)", e.g. (sim)(5) for a combination of simulated targets and
    #' a single concentration target of 5.
    #'
    #' @author Michael Neely and Jan Strojil
    #' @seealso [plot.PM_pta], [PM_sim]
    #' @examples
    #' \dontrun{
    #' pta1 <- PM_pta$new(simEx,
    #'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
    #'                  target = list(2^(-2:6), 1, 50),
    #'                  target_type = c("time", 144, "-max"),
    #'                  success = c(0.6, 1, 1),
    #'                  start = 120, end = 144)
    #'
    #' pta2 <- PM_pta$new(simEx,
    #'                  target = c(2^(-2:6)),
    #'                  target_type = "time",
    #'                  success = 0.6,
    #'                  start = 120, end = 144)
    #'
    #' pta3 <- PM_pta$new(simEx,
    #'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
    #'                  target = list(5,10),
    #'                  target_type = c("min", "-min"),
    #'                  success = c(1,1),
    #'                  start = 120, end = 144)
    #'
    #' pta4 <- PM_pta$new(simEx,
    #'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
    #'                  target = makePTAtarget(mic1),
    #'                  target_type = "auc",
    #'                  success = 200,
    #'                  start = 120, end = 144)
    #'
    #' pta5 <- PM_pta$new(simdata = simEx,
    #'                  simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
    #'                  target = list(makePTAtarget(mic1),5),
    #'                  target_type = c("auc","min"),
    #'                  success = c(200,1),
    #'                  start = 120, end = 144)
    #' }
    
    initialize = function(simdata, simlabels, target, target_type, success, outeq = 1,
      free_fraction = 1, start = 0, end = Inf, icen = "median", block = 1,
      ...) { # dots are for deprecated arguments
        
        # handle deprecated arguments
        extraArgs <- list(...)
        deprecated_args <- c("targets", "target.type", "free.fraction")
        new_args <- c("target", "target_type", "free_fraction")
        which_deprecated <- which(deprecated_args %in% names(extraArgs))
        if (length(which_deprecated) > 0) {
          cli::cli_abort(c(
            "x" = "The following argument{?s} {?has/have} been deprecated: {paste0(deprecated_args[which_deprecated], collapse = ', ')}.",
            "i" = "Instead, use {?this/these instead}: {paste0(new_args[which_deprecated], collapse = ', ')}."
          ))
        }
        
        
        # initial check
        if (missing(simdata) | missing(target) | missing(target_type) | missing(success)) {
          cli::cli_abort(c("x" = "Simulation output ({.code simdata}), {.code target}, {.code target_type}, and {.code success} are all mandatory."))
        }
        
        if (missing(simlabels)) {
          simlabels <- NULL
        }
        
        # what kind of object is simdata?
        if (!inherits(simdata, "character")) {
          # lists, characters are assumed to be simulations
          dataType <- switch(EXPR = class(simdata)[1],
          PM_sim = 0,
          PM_sim_data = 1,
          PM_simlist = 2,
          list = 3,
          character = 4,
          PM_post = 5,
          PM_post_data = 6,
          PM_data_data = 7,
          PM_data = 8,
          -1
        )
        if (dataType == -1) {
          cli::cli_abort(c(
            "x" = "You must specify a {.cls PM_sim}, {.cls PM_sim_data}, {.cls PM_simlist}, {.cls PM_post},
          {.cls PM_post_data},
          {.cls PM_data}, {.cls PM_data_data}, or character vector of simulator output files.",
            "i" = "See help for {.fn PM_pta}."
          ))
        }
        
        ########### new PTA calculation ##################
        # need to get it into a list of PMsim objects
        if (dataType == 0) { # PM_sim object
          if (inherits(simdata$data, "PM_simlist")) { # multiple sims
            simdata <- simdata$data
          } else { # just one sim
            simdata <- split(simdata$data$obs, as.factor(simdata$data$obs$id))
          }
        }
        
        if (dataType == 1) { # PM_sim_data object
          simdata <- split(simdata, as.factor(simdata$id)) # split by id
        }
        
        if (dataType == 2) { # PM_simlist
          simdata <- purrr::map(simdata$data, \(x) x$data) # extract data
        }
        
        # nothing to do for dataType=3 already in right format
        
        if (dataType == 4) { # character vector of simulator output files
          simfiles <- Sys.glob(simdata)
          if (length(simfiles) == 0) {
            cli::cli_abort(c("x" = "There are no files matching \"{simdata}\"."))
          }
          
          simdata <- PM_sim$new(simfiles)$data
        }
        
        if (dataType == 5) { # PM_post object
          simdata <- simdata$data %>% filter(icen == !!icen & block == !!block)
          temp <- list(obs = data.frame(id = simdata$id, time = simdata$time, out = simdata$pred, outeq = simdata$outeq))
          simdata <- list(temp)
        }
        
        if (dataType == 6) { # PM_post_data object
          simdata <- simdata %>% filter(icen == !!icen & block == !!block)
          temp <- list(obs = data.frame(id = simdata$id, time = simdata$time, out = simdata$pred, outeq = simdata$outeq))
          simdata <- list(temp)
        }
        
        if (dataType == 7 | dataType == 8) { # PM_data_data or PM_data object
          if (dataType == 8) {
            simdata <- simdata$data
          }
          simdata <- makePMmatrixBlock(simdata)
          simdata <- simdata %>% filter(evid == 0 & block == !!block)
          temp <- list(obs = data.frame(id = simdata$id, time = simdata$time, out = simdata$out, outeq = simdata$outeq))
          simdata <- list(temp)
        }
        
        
        pta <- private$make(
          simdata = simdata,
          simlabels = simlabels,
          target = target,
          target_type = target_type,
          success = success,
          outeq = outeq,
          free_fraction = free_fraction,
          start = start,
          end = end,
          icen = icen,
          block = block, ...
        )
        private$populate(pta)
      } else { # try simdata as a filename
        pta <- readRDS(simdata)
        private$populate(pta)
      }
    },
    #' @description
    #' Save the current PM_pta object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMpta.rds
    save = function(file_name = "PMpta.rds") {
      saveRDS(self, file_name)
    },
    #' @description
    #' Summarize the `PM_pta` object. See [summary.PM_pta].
    #' @param ... Arguments passed to [summary.PM_pta]
    summary = function(...) {
      summary.PM_pta(self, ...)
    },
    #' @description
    #' Plot the `PM_pta` object. See [plot.PM_pta].
    #' @param ... Arguments passed to [plot.PM_pta]
    plot = function(...) {
      plot.PM_pta(self, ...)
    },
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load a prior PTA Replaced by `PM_pta$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_pta$load()", details = "Please use PM_pta$new() instead. ?PM_pta for details.")
    }
  ), # end public
  private = list(
    make = function(simdata, simlabels, target, target_type, success, outeq = 1,
      free_fraction = 1, start = 0, end = Inf, icen = "median", block = 1) {
        if (is.numeric(target) | inherits(target, "PMpta.targ")) {
          target <- list(target) # make a list
        }
        
        # define some global variables
        n_reg <- length(simdata) # number of regimens
        n_sim <- sapply(simdata, function(x) length(unique(x$nsim))) # number of sims per regimen
        n_type <- length(target_type)
        n_success <- length(success)
        if(n_type == 1 && !is.list(target)) {
          target <- list(target) # make a list if only one target_type
        }
        n_target <- length(target) # length of the whole list, should correspond with n_type, success
        if (inherits(target[[1]], "PMpta.targ")) {
          simTarg <- TRUE
        } else {
          simTarg <- FALSE
        }
        
        # fill in start and end times for each regimen
        if (length(start) < n_reg) {
          start <- rep(start, n_reg)[1:n_reg]
        }
        if (length(end) < n_reg) {
          end <- rep(end, n_reg)[1:n_reg]
        }
        
        # check for valid arguments
        target_type <- stringr::str_replace_all(target_type, "peak", "max")
        invalid_types <- purrr::map_lgl(target_type, \(x){
          !x %in% c("time", "auc", "max", "peak", "min", "-time", "-auc", "-max", "-peak", "-min") &&
          suppressWarnings(is.na(as.numeric(x)))
        })
        
        if (any(invalid_types)) {
          cli::cli_abort(c(
            "x" = "Please specify {.code target_type} as a numerical value corresponding to a common time in
                         all simulated datasets,\for a character value of 'time', 'auc', 'max' or 'min'.",
            "i" = "See help for {.fn PM_pta}."
          ))
        }
        
        # adjust start and end for any specific times
        start <- unlist(map(1:n_type, \(x) {
          if (suppressWarnings(!is.na(as.numeric(target_type[x])))) {
            abs(as.numeric(target_type[x]))
          } else {
            start[x]
          }
        }))
        end <- unlist(map(1:n_type, \(x) {
          if (suppressWarnings(!is.na(as.numeric(target_type[x])))) {
            abs(as.numeric(target_type[x]))
          } else {
            end[x]
          }
        }))
        
        
        # check to make sure secondary targets are only length 1
        if (n_type > 1) {
          invalid_sec_type <- purrr::map_lgl(target[2:n_target], \(x){
            length(x) > 1
          })
          if (any(invalid_sec_type)) {
            cli::cli_abort(c(
              "x" = "For multiple {.code target_type}, types after the first cannot have more than one target",
              "i" = "They typically are {.code min}, {.code max}, or a specific time."
            ))
          }
        }
        
        if (!identical(n_target, n_type, n_success)) {
          cli::cli_abort(c("x" = "{.code target}, {.code target_type}, and {.code success} vectors must all be the same length for discrete targets."))
        }
        
        if (stringr::str_detect(free_fraction, "%")) { # if passed as percents convert to a number
          free_fraction <- as.numeric(stringr::str_replace(free_fraction, "%", "")) / 100
        }
        while (free_fraction <= 0 | free_fraction > 1) {
          free_fraction <- as.numeric(readline(cat("Invalid free fraction, please specify a fraction > 0 and <= 1.\n")))
        }
        if (stringr::str_detect(success[1], "%")) { # if passed as percents convert to a number
          success[1] <- as.numeric(stringr::str_replace(success[1], "%", "")) / 100
        }
        if (success[1] <= 0 | ((target_type[1] == "time" | target_type[1] == "-time") & success[1] > 100)) cli::cli_abort(c("x" = "Invalid success threshold value. Aborting."))
        if (target_type[1] == "time" & success[1] > 1 & success[1] <= 100) {
          cat("Your specified success threshold for time above target of ", success[1], " is bigger than 1.", sep = "")
          ans <- readline(cat("\nWhat would you like to do?\n1) set success to ", success[1] / 100, " (i.e. ", success[1], "% of time relative to target)\n2) end ", sep = ""))
          if (ans == 1) {
            success[1] <- success[1] / 100
            cat("Success threshold for time was set to ", success[1], ".", sep = "")
          } else {
            cli::cli_abort(c("x" = "Function aborted."))
          }
        }
        
        
        
        #### PREPARE DATA
        
        # check outeq
        if (!outeq %in% simdata[[1]]$outeq) {
          cli::cli_abort(c("x" = "There are no simulated outputs for output equation {outeq}. Aborting."))
        }
        
        # filter and multiply free fraction
        simdata <- purrr::map(1:n_reg, \(x) {
          simdata[[x]] <- simdata[[x]] %>%
          filter(outeq == !!outeq, !is.na(out)) %>%
          mutate(outeq = 1) # after filter, change to 1
          simdata[[x]]$out <- simdata[[x]]$out * free_fraction
          simdata[[x]]
        })
        
        
        # Check the simulation labels
        sim_labels <- paste("Regimen", 1:n_reg)
        
        if (length(simlabels) > 0) { # replace generic labels with user labels
          n_reglabels <- length(simlabels)
          if (n_reglabels < n_reg) warning("There are more simulated regimens (n=", n_reg, ") than labels (n=", n_reglabels, ").", call. = FALSE, immediate. = TRUE)
          if (n_reglabels > n_reg) warning("There are fewer simulated regimens (n=", n_reg, ") than labels (n=", n_reglabels, "); some labels will be ignored.", call. = FALSE, immediate. = TRUE)
          sim_labels[1:min(n_reglabels, n_reg)] <- simlabels[1:min(n_reglabels, n_reg)]
        }
        
        # calculate number of iterations for progress bar
        if (!simTarg) {
          cat("\nCalculating PTA for each simulated regimen and target...\n")
        } else {
          cat("\nCalculating PTA for each simulated regimen using simulated targets...\n")
        }
        flush.console()
        
        # create the progress bar
        maxpb <- sum(unlist(purrr::map(target, \(x) ifelse(inherits(x, "PMpta.targ"), 1, length(x))))) * n_reg # target * simulations
        pb <- txtProgressBar(min = 0, max = maxpb, style = 3)
        
        
        ###### MAKE THE PTA OBJECT
        master_pta <- purrr::map(1:n_reg, \(x){
          purrr::map(1:n_type, \(y) tidyr::expand_grid(
            reg_num = x,
            target = if (simTarg) { # simulated targets
              if (y == 1) {
                list(
                  tidyr::tibble(
                    id = unique(simdata[[x]]$id),
                    target = sample(
                      x = target[[y]]$target,
                      size = n_sim[x], replace = TRUE,
                      prob = target[[y]]$n
                    )
                  )
                )
              } else {
                target[y]
              }
            } else {
              target[[y]]
            }, # discrete targets
            this_type = y,
            type = target_type[[y]],
            success_ratio = success[[y]],
            start = start[y],
            end = end[y]
          )) %>% # end inner map
          purrr::list_rbind()
        }) %>% # end outer map
        purrr::list_rbind() %>%
        mutate(type = stringr::str_replace_all(type, "\\d+", "specific")) %>%
        rowwise() %>%
        mutate(pdi = list(do.call(
          paste0("pta_", stringr::str_replace_all(type, "-", "")), # call the appropriate pta function below
          list( # arguments to the pta function
            sims = simdata[[reg_num]],
            .target = target,
            .simTarg = simTarg,
            .start = start,
            .end = end,
            .pb = pb
          )
        ))) %>%
        mutate(success = list(purrr::map_dbl(pdi, \(x) {
          if (stringr::str_detect(type, "-")) {
            x <= success_ratio # will return NA is x is NA
          } else {
            x >= success_ratio
          }
        }))) %>%
        mutate(prop_success = sum(success) / length(success)) %>%
        mutate(label = sim_labels[reg_num]) %>%
        dplyr::relocate(
          reg_num, label, target, type,
          success_ratio, prop_success, success, pdi,
          start, end
        ) %>%
        ungroup() # remove rowwise
        
        # add intersection if multiple target types
        # browser()
        if (n_type > 1) {
          master_pta <- split(master_pta, master_pta$this_type) %>% # split by target type number
          .[order(match(names(.), target_type))]
          master_pta <- map(master_pta, \(x) {
            x$this_type <- NULL
            x
          })
          names(master_pta) <- NULL
          master_pta$intersect <- master_pta[[1]] %>% select(reg_num, target, success_1 = success, label) # get primary success
          for (i in 2:n_type) { # add additional success
            master_pta$intersect[[paste0("success_", i)]] <- master_pta[[i]]$success[match(master_pta[[1]]$reg_num, master_pta[[i]]$reg_num)]
          }
          all_success <- master_pta$intersect %>%
          select(tidyr::starts_with("success"))
          total_success <- lapply(apply(all_success, 1, function(x) as.data.frame(x)), rowSums) # sum all success matrices for each sim/target
          master_pta$intersect$prop_success <- purrr::map_dbl(total_success, \(x) sum(x == n_type) / length(x))
          master_pta$intersect <- master_pta$intersect %>%
          rowwise() %>%
          mutate(target = paste0("(", c(target, !!target[2:n_type]), ")", collapse = "")) %>%
          mutate(target = stringr::str_replace(target, "\\(1:(.|\\n)*\\){2}", "(sim)")) %>%
          mutate(type = paste0("(", target_type, ")", collapse = "")) %>%
          mutate(success_ratio = paste0("(", success, ")", collapse = "")) %>%
          mutate(success = list(total_success)) %>%
          select(
            reg_num, label, target, type,
            success_ratio, prop_success, success
          ) %>%
          ungroup()
        } else { # only one target_type
          master_pta <- list(data = master_pta %>%
            select(-this_type),
            intersect = NA)
          }
          
          class(master_pta) <- c("PM_pta_data", "list")
          return(master_pta)
        },
        populate = function(pta) {
          self$data <- pta
          return(self)
        }
      ) # end private
    )
    
    #' @keywords internal
    #' @name PM_pta
    #' @export
    PM_pta$load <- function(file_name = "PMpta.rds") {
      lifecycle::deprecate_warn("2.1.0", "PM_pta$load()", details = "Please use PM_pta$new() instead. ?PM_pta for details.")
    }
    
    
    # ACCESSORY INTERNAL FUNCTIONS --------------------------------------------
    
    pta_auc <- function(sims, .target, .simTarg, .start, .end, .pb) {
      cycle <- utils::getTxtProgressBar(.pb)
      utils::setTxtProgressBar(.pb, cycle + 1)
      
      auc <- rlang::try_fetch(makeAUC(sims, out ~ time | nsim, start = .start, end = .end),
      error = function(e) {
        cli::cli_warn("Unable to generate AUC.", parent = e)
        return(NA)
      }
    )
    
    if (nrow(auc) > 0) {
      if (.simTarg & length(.target) > 1) {
        .target$nsim <- 1:nrow(.target)
        auc <- dplyr::left_join(auc, .target, by = "nsim")
      } else {
        auc$target <- .target
      }
      pdi <- auc$tau / auc$target
    } else {
      pdi <- NA # filtered to 0 rows or other AUC error
    }
    return(pdi)
  }
  
  pta_min <- function(sims, .target, .simTarg, .start, .end, .pb) {
    cycle <- utils::getTxtProgressBar(.pb)
    utils::setTxtProgressBar(.pb, cycle + 1)
    mins <- sims %>%
    group_by(nsim) %>%
    filter(time >= .start, time <= .end)
    if (.simTarg & length(.target) > 1) {
      .target$nsim <- 1:nrow(.target)
      mins <- dplyr::left_join(mins, .target, by = c("id", "nsim"))
    } else {
      mins$target <- .target
    }
    if (nrow(mins) > 0) {
      mins <- mins %>%
      dplyr::summarize(min = min(out, na.rm = TRUE), target = target[1]) %>%
      ungroup()
      pdi <- mins$min / mins$target
    } else {
      pdi <- NA # filtered to 0 rows
    }
    return(pdi)
  }
  
  pta_max <- function(sims, .target, .simTarg, .start, .end, .pb) {
    cycle <- utils::getTxtProgressBar(.pb)
    utils::setTxtProgressBar(.pb, cycle + 1)
    
    maxes <- sims %>%
    group_by(nsim) %>%
    filter(time >= .start, time <= .end)
    if (.simTarg & length(.target) > 1) {
      .target$nsim <- 1:nrow(.target)
      maxes <- dplyr::left_join(maxes, .target, by = c("id", "nsim"))
    } else {
      maxes$target <- .target
    }
    if (nrow(maxes) > 0) {
      maxes <- maxes %>%
      dplyr::summarize(max = max(out, na.rm = TRUE), target = target[1]) %>%
      ungroup()
      pdi <- maxes$max / maxes$target
    } else {
      pdi <- NA # filtered to 0 rows
    }
    return(pdi)
  }
  
  pta_specific <- function(sims, .target, .simTarg, .start, .end, .pb) {
    cycle <- utils::getTxtProgressBar(.pb)
    utils::setTxtProgressBar(.pb, cycle + 1)
    
    concs <- sims %>%
    group_by(nsim) %>%
    filter(time == .start)
    if (.simTarg & length(.target) > 1) {
      .target$nsim <- 1:nrow(.target)
      concs <- dplyr::left_join(concs, .target, by = c("id", "nsim"))  
    } else {
      concs$target <- .target
    }
    if (nrow(concs) > 0) {
      pdi <- concs$out / concs$target
    } else {
      pdi <- NA # filtered to 0 rows
    }
    return(pdi)
  }
  
  pta_time <- function(sims, .target, .simTarg, .start, .end, .pb) {
    cycle <- utils::getTxtProgressBar(.pb)
    utils::setTxtProgressBar(.pb, cycle + 1)
    
    interval <- diff(sims$time)[1] # will be regular for sims
    
    times <- sims %>%
    group_by(nsim) %>%
    filter(time >= .start, time <= .end)
    if (.simTarg & length(.target) > 1) {
      .target$nsim <- 1:nrow(.target)
      times <- dplyr::left_join(times, .target, by = c("id", "nsim"))
    } else {
      times$target <- .target
    }
    if (nrow(times) > 0) {
      times <- times %>%
      mutate(above = interval * (out > target)) %>%
      mutate(cross = above - dplyr::lag(above, n = 1))
      
      crossing_rows <- which(times$cross == 0.5 | times$cross == -0.5)
      if (length(crossing_rows) > 0) {
        crossing1 <- times[c(crossing_rows - 1, crossing_rows), ] %>%
        arrange(id, time) %>%
        ungroup() %>%
        mutate(pair = rep(seq_along(1:(0.5 * dplyr::n())), each = 2)) %>%
        group_by(pair)
        
        crossing2 <- crossing1 %>%
        summarize(above = interval / (out[2] - out[1]) *
        (target[1] - out[1]))
        
        crossing3 <- crossing1 %>%
        filter(above == interval) %>%
        ungroup() %>%
        mutate(above = crossing2$above)
        
        times2 <- times %>%
        filter(is.na(cross) | cross == 0)
        
        pdi <- bind_rows(times2, crossing3) %>%
        group_by(nsim) %>%
        summarize(sum = sum(above[-1]) / 24) %>%
        pull(sum)
      } else {
        pdi <- times %>%
        group_by(nsim) %>%
        summarize(sum = sum(above[-1]) / 24) %>%
        pull(sum)
      }
      pdi[pdi > 1] <- 1 # in case of rounding errors
    } else {
      pdi <- NA # filtered to 0 rows
    }
    
    return(pdi)
  }
  
  
  # MAKE TARGET -------------------------------------------------------------
  #' @title Make a Percent Target Attainment (PTA) Target
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' Generates an object of class *PMpta.targ* which can
  #' be used in the `$new()` method for [PM_pta] or `$pta()` method for [PM_sim] for targets sampled from a distribution.
  #'
  #' @title Make PTA target object
  #' @param x A data.frame or name of .csv file in working directory whose first two
  #' columns are targets and the number of samples for each target.  An example can be
  #' seen for Staphylococcus aureus susceptibility to vancomycin at
  #' [EUCAST](http://mic.eucast.org/Eucast2/regShow.jsp?Id=1214).
  #' @return A data frame with two columns named targets and n, of class *PMpta.targ*.
  #' @seealso [PM_pta]
  #' @examples
  #' \dontrun{
  #' makePTAtarget(mic1)
  #' }
  #' @export
  
  makePTAtarget <- function(x) {
    ptaTarg <- x
    if (is.character(x)) {
      ptaTarg <- read.table(x, sep = ",")
    } else {
      if (!is.data.frame(ptaTarg)) cli::cli_abort(c("x" = "PTA target must be a data frame."))
    }
    names(ptaTarg)[1:2] <- c("target", "n")
    class(ptaTarg) <- c("PMpta.targ", "data.frame")
    return(ptaTarg)
  }
  
  # PLOT --------------------------------------------------------------------
  #' @title Plot PM_pta Percent Target Attainment objects
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' This function will plot the percent target attainment for associated with
  #' simulations.
  #'
  #' @details
  #' [PM_pta] objects are made with the `$pta` method for [PM_sim]  or
  #' with `PM_pta$new()`. 
  #'
  #' @method plot PM_pta
  #' @param x The name of an *PM_pta* data object 
  #' @param at Which object in the *PM_pta* result list to plot. By default "intersect" if
  #' an intersection is present due to creation of the object with multiple target types, or
  #' 1 if no intersection is present, which means only 1 target type was selected. If
  #' "intersect" is present in the object, the default can be overridden with a number to
  #' plot one of the individual PTAs, e.g. `at = 2` to plot the second PTA rather than the
  #' intersection of all the PTAs.
  #' @param include `r template("include")`
  #' @param exclude `r template("exclude")`
  #' @param type Character vector controlling type of plot.
  #' Default is "pta", which plots proportion with success on the y-axis and target on the x-axis.
  #' The other choice is "pdi", which plots the median pdi (pharmacodynamic index), e.g. AUC/MIC, on the
  #' y-axis, and target on the x-axis.
  #' @param mult `r template("mult")`
  #' @param log Boolean operator to plot the x axis in log base 10. This argument maps to   
  #' the xaxis type value in the layout object in plotly. Many other Pmetrics plots map this
  #' argument to the y axis, but for PTA plots, the x axis is the more common axis to log transform.
  #' Use the plotly `plotly::schema()` command in the console and navigate to layout > layoutAttributes > xaxis > type.  
  #' Example: `log = T` 
  #' @param outeq `r template("outeq")`
  #' @param line Controls characteristics of lines.
  #' This argument maps to the plotly line object.
  #' It can be boolean or a list.
  #' `TRUE` will plot the line with default characteristics for each simulated regimen.
  #' `FALSE` will suppress line plotting.
  #' If a list, it functions a little differently than other Pmetrics plotly functions.
  #' Rather than controlling individual line characteristics, for this plot,
  #' the `line` argument should be a list of the options for group based plotting,
  #' where each group corresponds to a simulated regimen. The possible elements of the
  #' `line` list should be exactly named:
  #' * color Maps to the [plot_ly] `colors` argument to override default colors
  #' applied to the lines for each regimen. This can be a named palette, which
  #' can be obtained with `RColorBrewer::display.brewer.all()` or a vector of hexadecimal
  #' color names. One way to ensure reliable color palettes is to use the
  #' [ColorBrewer](https://colorbrewer2.org/#type=qualitative&scheme=Accent&n=6) site.
  #' Choosing the number of data classes to correspond to regimens, and qualitative data
  #' results in a distinct palette. Easiest importing into R is to copy/paste the Export
  #' of JavaScript on the ColorBrewer website. The default is "Set1". Palettes
  #' with fewer colors than regimens will be recycled. A color can also be a character
  #' vector of color names, recycled as needed. For example, a print-friendly choice
  #' is `line = list(color = "black")`.
  #' * width Maps to the [plot_ly] `width` argument to override default widths
  #' applied to the lines for each regimen. All lines will have the same width.
  #' The default value is 2.
  #' * dash Maps to the [plot_ly] `linetypes` argument to override default styles
  #' applied to the lines for each regimen. If numeric, will map to `lty` [par] values.
  #' It can also be a character vector of dash names as listed in [plot_ly].
  #' Example: `line = list(color = "Blues", width = 1, dash = 2)`, whicb will result
  #' in dotted lines (dash  = 2) all with width 1 but in different shades of blue.
  #' @param marker Controls the plotting symbol.
  #' This argument maps to the plotly marker object.
  #' It can be boolean or a list.
  #' `TRUE` will plot the profiles with default characteristics for each simulated regimen.
  #' `FALSE` will suppress line plotting.
  #' If a list, it functions a little differently than other Pmetrics plotly functions.
  #' Rather than controlling individual marker characteristics, for this plot,
  #' the `marker` argument should be a list of the options for group based plotting,
  #' where each group corresponds to a simulated regimen. The possible elements of the
  #' `marker` list should be exactly named:
  #' * color Default marker color is the same as the line color. If line color is specified,
  #' marker color does not need to also be specified. Even if line plotting is suppressed
  #' with `line = F`, the default color value of "Set1" will be applied to markers,
  #' unless specified, e.g. `marker = list(color = "Blues")`.
  #' * symbol Maps to the [plot_ly] `symbols` argument to override default symbols
  #' applied to the markers for each regimen. If only one value is supplied for this,
  #' it will be recycled for each regimen, i.e. all will have the same symbol.
  #' See `plotly::schema()`, traces > scatter > attributes > marker > symbol > values
  #' for options.
  #' * size Maps to the [plot_ly] `size` argument to override default size
  #' applied to the markers for each regimen. All markers will have the same size.
  #' The default value is 12.
  #' @param grid `r template("grid")`
  #' @param legend `r template("legend")` Default will be the labeled regimen names as an argument 
  #' when creating a [PM_pta] object,
  #' or if missing, "Regimen 1, Regimen 2,...Regimen n", where *n* is the number of
  #' regimens in the PM_pta object.
  #' @param ci Confidence interval around curves on `type = "pdi"` plot, on scale of 0 to 1. Default is 0.9.
  #' @param xlab `r template("xlab")`  Default is "Target" when targets are discrete,
  #' and "Regimen" when targets are sampled.
  #' @param ylab `r template("ylab")`  Default is "Proportion with success" for
  #' plot `type = "pta"` and "Pharmacodynamic Index" for plot `type = "pdi"`.
  #' @param title `r template("title")` Default is to have no title.
  #' @param xlim `r template("xlim")`
  #' @param ylim `r template("ylim")`
  #' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
  #' @param ... `r template("dotsPlotly")`
  #' @return Plots the object.
  #' @author Michael Neely
  #' @importFrom plotly plotly_build
  #' @export
  #' @examples
  #' \dontrun{
  #' pta1 <- simEx$pta(
  #'   simlabels <- c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
  #'   targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), target.type = "time",
  #'   success = 0.6, start = 120, end = 144
  #' )
  #' pta1$summary()
  #' pta1$plot()
  #' }
  #' @family PMplots
  
  plot.PM_pta <- function(x,
    at = "intersect",
    include, exclude,
    type = "pta",
    mult = 1,
    outeq = 1,
    line = TRUE,
    marker = TRUE,
    ci = 0.9,
    legend = TRUE,
    log = FALSE,
    grid = TRUE,
    xlab, ylab,
    title,
    xlim, ylim,
    print = TRUE, ...) {
      # clone to avoid changes
      pta <- x$clone()$data
      
      # select correct object to plot
      if (at == "intersect") {
        if (length(pta$intersect) > 1) {
          pta <- pta$intersect 
          #%>%
          #mutate(target = as.numeric(stringr::str_extract(target, "\\d+\\.*\\d*")))
        } else {
          pta <- pta[[1]] # no intersect, plot first
        }
      } else {
        at <- suppressWarnings(tryCatch(as.numeric(at), error = function(e) NA))
        if (!is.na(at)) {
          if (at > length(pta)) {
            cli::cli_abort(c("x" = "at = {at} is greater than the number of PTAs."))
          } else if (length(pta[[at]]) == 0 || all(is.na(pta[[at]]))) {
            cli::cli_abort(c("x" =  "at = {at} does not return a valid PTA."))
          } else {
            pta <- pta[[at]]
          }
        } else {
          cli::cli_abort(c("x" = "{.code at} should be either \"intersect\" or the number of one of the objects to plot."))
        }
      }
      
      # vector of regimens
      simnum <- 1:max(pta$reg_num)
      
      # names of regimens
      simLabels <- unique(pta$label)
      
      # check input
      if (!missing(include)) {
        if (any(include > max(simnum))) {
          cli::cli_abort(c("x" = "PM_pta object does not have {max(simnum)} simulations."))
        } else {
          simnum <- simnum[include]
          simLabels <- simLabels[include]
        }
      }
      if (!missing(exclude)) {
        if (any(exclude > max(simnum))) {
          cli::cli_abort(c("x" = "PMpta object does not have {max(simnum)} simulations."))
        } else {
          simnum <- simnum[-exclude]
          simLabels <- simLabels[-exclude]
        }
      }
      
      # filter by include/exclude
      pta <- pta %>% filter(reg_num %in% simnum)
      
      n_reg <- length(simnum)
      
      # parse line
      line <- amendLine(line, default = list(
        color = "Set1",
        width = 2,
        dash = 1:n_reg
      ))
      
      # parse marker
      marker <- amendMarker(marker, default = list(
        color = line$color,
        size = 12,
        symbol = 1:n_reg
      ))
      
      # simulated or discrete targets
      if (is.list(pta$target) || all(stringr::str_detect(pta$target, "sim"))) {
        simTarg <- 2
      } else {
        simTarg <- 1
      }
      
      
      # process dots
      layout <- amendDots(list(...))
      
      # legend
      legendList <- amendLegend(legend, default = list(xanchor = "right", title = list(text = "<b>Regimen</b>")))
      layout <- modifyList(layout, list(showlegend = legendList$showlegend))
      if (length(legendList) > 1) {
        layout <- modifyList(layout, list(legend = within(legendList, rm(showlegend))))
      }
      
      # grid
      layout$xaxis <- setGrid(layout$xaxis, grid)
      layout$yaxis <- setGrid(layout$yaxis, grid)
      
      # axis labels if needed
      xtitle <- purrr::pluck(layout$xaxis, "title")
      ytitle <- purrr::pluck(layout$yaxis, "title")
      if (is.null(xtitle)) {
        if (missing(xlab)) {
          # choose xlab as Target if targets were set or Regimen if targets were simulated
          
          xlab <- switch(simTarg,
            "Target",
            "Regimen"
          )
        }
        layout$xaxis$title <- amendTitle(xlab)
      }
      if (is.null(ytitle)) {
        if (missing(ylab)) {
          ylab <- switch(type,
            pdi = "Pharmacodynamic Index",
            pta = "Proportion with success",
            "Proportion with success"
          )
        }
        if (is.character(ylab)) {
          layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
        } else {
          layout$yaxis$title <- amendTitle(ylab)
        }
      }
      
      # axis ranges
      if (!missing(xlim)) {
        layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
      }
      if (!missing(ylim)) {
        layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
      }
      
      # log x axis
      if (log) {
        layout$xaxis <- modifyList(layout$xaxis, list(type = "log"))
      }
      
      # title
      if (missing(title)) {
        title <- ""
      }
      layout$title <- amendTitle(title, default = list(size = 20))
      
      
      # PLOTS
      if (type == "pdi") { # pdi plot
        if (at == "intersect") {
          cli::cli_abort(c(
            "x" = "PDI plot not possible on intersection.",
            "i" = "Choose an individual PTA with {.code at = 1}, for example."
          ))
        }
        if (simTarg == 1) { # discrete targets
          p <- pta %>%
          group_by(reg_num, target) %>%
          rowwise() %>%
          mutate(
            lower = quantile(pdi, probs = 0.5 - ci / 2, na.rm = TRUE),
            median = median(pdi),
            upper = quantile(pdi, probs = 0.5 + ci / 2, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          mutate(label = factor(label)) %>%
          group_by(label) %>%
          plotly::plot_ly(
            x = ~target, y = ~median,
            type = "scatter", mode = "markers+lines",
            colors = marker$color,
            symbols = marker$symbol,
            linetypes = line$dash,
            strokes = line$color,
            color = ~label,
            stroke = ~label,
            linetype = ~label,
            symbol = ~label,
            marker = list(size = marker$size),
            line = list(width = line$width)
          ) %>%
          plotly::add_ribbons(
            ymin = ~lower, ymax = ~upper,
            opacity = 0.5, line = list(width = 0),
            marker = list(size = .01), showlegend = FALSE
          )
          
          p <- p %>% plotly::layout(
            xaxis = layout$xaxis,
            yaxis = layout$yaxis,
            showlegend = layout$showlegend,
            legend = layout$legend,
            title = layout$title
          )
        } else { # random targets
          
          p <- pta %>%
          mutate(simnum = factor(reg_num, labels = simLabels)) %>%
          group_by(simnum) %>%
          rowwise() %>%
          mutate(
            lower = quantile(pdi, probs = 0.5 - ci / 2, na.rm = TRUE),
            median = median(pdi),
            upper = quantile(pdi, probs = 0.5 + ci / 2, na.rm = TRUE)
          ) %>%
          plotly::plot_ly(x = ~simnum, y = ~median) %>%
          plotly::add_markers(
            error_y = list(
              symmetric = FALSE,
              array = ~ (upper - median),
              arrayminus = ~ (median - lower),
              color = line$color
            ),
            marker = list(color = marker$color, size = marker$size, symbol = marker$symbol)
          )
          
          layout$xaxis <- modifyList(layout$xaxis, list(tickvals = ~simnum, ticktext = ~simLabels))
          p <- p %>% plotly::layout(
            xaxis = layout$xaxis,
            yaxis = layout$yaxis,
            showlegend = FALSE,
            legend = layout$legend,
            title = layout$title
          )
        }
      } else { # pta plot
        
        if (simTarg == 1) { # set targets
          
          p <- pta %>%
          mutate(simnum = factor(reg_num, labels = simLabels)) %>%
          group_by(simnum) %>%
          plotly::plot_ly(
            x = ~target, y = ~prop_success,
            type = "scatter", mode = "lines+markers",
            colors = marker$color,
            symbols = marker$symbol,
            linetypes = line$dash,
            strokes = line$color,
            color = ~simnum,
            stroke = ~simnum,
            linetype = ~simnum,
            symbol = ~simnum,
            marker = list(size = marker$size),
            line = list(width = line$width)
          )
          
          p <- p %>% plotly::layout(
            xaxis = layout$xaxis,
            yaxis = layout$yaxis,
            showlegend = layout$showlegend,
            legend = layout$legend,
            title = layout$title
          )
        } else { # random targets
          
          p <- pta %>%
          mutate(simnum = factor(reg_num, labels = simLabels)) %>%
          plotly::plot_ly(
            x = ~simnum, y = ~prop_success,
            type = "scatter", mode = "lines+markers",
            line = list(color = line$color, width = line$width, dash = line$dash),
            marker = list(color = marker$color, symbol = marker$symbol, size = marker$size)
          )
          layout$xaxis <- modifyList(layout$xaxis, list(tickvals = ~simnum, ticktext = ~simLabels))
          p <- p %>% plotly::layout(
            xaxis = layout$xaxis,
            yaxis = layout$yaxis,
            showlegend = FALSE,
            legend = layout$legend,
            title = layout$title
          )
        }
      }
      p <- suppressMessages(plotly::plotly_build(p))
      if (print) print(p)
      return(invisible(p))
    }
    
    # SUMMARY ------------------------------------------------------------------
    
    #' @title Summarize Percent Target Attainment
    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' Summarize a Pmetrics Percent Target Attainment Object
    #' @details
    #' Summarize Pharmacodynamic Index (PDI) statistics and success proportions in a [PM_pta] object. The PDI is the metric calculated by the target type and target, e.g. AUC/Target,
    #' or %time>target. Since a PDI cannot be calculated for intersections, summarizing the intersection object only provides the success proportion per simulation/target.
    #'
    #' @method summary PM_pta
    #' @param object A [PM_pta] object
    #' @param at Which object in the [PM_pta] result list to summarize. By default "intersect" if
    #' an intersection is present due to creation of the object with multiple target types, or
    #' 1 if no intersection is present, which means only 1 target type was selected. If
    #' "intersect" is present in the object, the default can be overridden with a number to
    #' summarize one of the individual PTAs, e.g. `at = 2` to summarize the second PTA rather than the
    #' intersection of all the PTAs.
    #' @param ci Width of the interval for pharmacodynamic index reporting.  Default is 0.95, i.e. 2.5th to 97.5th percentile.
    #' @param ... Not used.
    #' @return A tibble with the following columns (only the first five if `at = "intersect"`):
    #'
    #' * **reg_num** is the number of the simulation regimen
    #' * **label** is the simulation label, for reference
    #' * **target** is the target for the row, if targets are discrete, not used for simulated targets
    #' * **type** is the target type for the row, e.g. "auc", "time", "-min", etc.
    #' * **prop_success** is the proportion of simulated profiles that met the success definition
    #' * **median** is the median parmacodynamic index (PDI), i.e. the proportion or ratio depending on the target type
    #' * **lower** is the lower bound of the interval defined by `ci`
    #' * **upper** is the upper bound of the interval defined by `ci`
    #' * **mean** is the mean of the PDI
    #' * **sd** is the standard deviation of the PDI
    #' * **min** is the minimum PDI
    #' * **max** is the maximum PDI
    #'
    #' @author Michael Neely
    #' @seealso [PM_pta]
    #' @examples
    #' \dontrun{
    #' ptaEx$summary()
    #' }
    
    #' @export
    
    summary.PM_pta <- function(object, at = "intersect", ci = 0.95, ...) {
      pta <- object$clone()$data
      
      if (at == "intersect") {
        if (length(pta$intersect) > 1) {
          pta <- pta$intersect 
        } else {
          pta <- pta[[1]] # no intersect, summarize first
        }
      } else {
        at <- suppressWarnings(tryCatch(as.numeric(at), error = function(e) NA))
        if (!is.na(at)) {
          if (at > length(pta)) {
            cli::cli_abort(c("x" = "at = {at} is greater than the number of PTAs."))
          } else if (length(pta[[at]]) == 0 || all(is.na(pta[[at]]))) {
            cli::cli_abort(c("x" =  "at = {at} does not return a valid PTA."))
          } else {
            pta <- pta[[at]]
          }
        } else {
          cli::cli_abort(c("x" = "{.code at} should be either \"intersect\" or the number of one of the objects to plot."))
        }
      }
      
      simTarg <- 1 + as.numeric(inherits(pta$target[[1]], "tbl_df")) # 1 if missing or set, 2 if random
      if (length(simTarg) == 0) simTarg <- 1
      
      if (simTarg == 1) { # discrete targets
        
        pdi <- pta %>% group_by(reg_num, target)
      } else { # random targets
        pdi <- pta %>% group_by(reg_num)
      }

      if (at != "intersect") {
        pdi <- pdi %>% mutate(
          label = label, type = type, prop_success = prop_success,
          median = median(unlist(pdi), na.rm = TRUE),
          lower = quantile(unlist(pdi), probs = 0.5 - ci / 2, na.rm = TRUE),
          upper = quantile(unlist(pdi), probs = 0.5 + ci / 2, na.rm = TRUE),
          mean = mean(unlist(pdi), na.rm = TRUE),
          sd = sd(unlist(pdi), na.rm = TRUE),
          min = min(unlist(pdi), na.rm = TRUE),
          max = max(unlist(pdi), na.rm = TRUE),
          .keep = "none"
        )
      } else {
        pdi <- pdi %>% mutate(type = type, label = label, prop_success = prop_success, .keep = "none")
      }
      
      pdi <- pdi %>% ungroup()
      
      return(pdi)
    }
    