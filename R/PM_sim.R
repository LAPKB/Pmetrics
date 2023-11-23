#' @title
#' Object to contain results of simulation
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object is created after a successful run of the simulator.
#'
#' @details
#' There are two methods of creating a PM_sim object.
#' * **PM_result$sim()**
#' * **PM_sim$new()**
#'
#' These both call [SIMrun] to execute the simulation and [SIMparse] to process
#' the results and return the PM_sim objects. See help on both of these functions
#' for further details.
#'
#' @export
PM_sim <- R6::R6Class(
  "PM_sim",
  public = list(
    #' @field obs Observations for each output
    obs = NULL,
    #' @field amt Amounts in each model compartment
    amt = NULL,
    #' @field parValues Retained simulated parameter values after discarding
    #' any due to truncation limits
    parValues = NULL,
    #' @field totalSets Number of all simulated parameter values
    totalSets = NULL,
    #' @field totalMeans Mean of all simulated parameter values
    totalMeans = NULL,
    #' @field totalCov Covariance of all simulated parameter values
    totalCov = NULL,
    #' @field data A list that contains all the above elements
    data = NULL,

    #' @description
    #' This is a wrapper function that combines [SIMrun] and [SIMparse] in R6.
    #' It can be called directly
    #' or via the `$sim` method for [PM_result] objects.
    #' @param poppar One of four things:
    #' * Population prior parameters as a [PM_final] object found in
    #' `PM_result$final`. Normally these would be supplied by calling the
    #' `$sim` method for a [PM_result] object, e.g. `NPex$sim(...)`.
    #' * A manually specified prior, see [SIMrun] for more details.
    #' * The name of a previously saved simulation via the `$save` method. The
    #' file will be loaded.
    #' * The results of running [SIMparse] on a prior simulation.
    #' @param ... Additional parameters to be passed to [SIMrun] and optionally,
    #' `combine = TRUE` as an argument will be passed to [SIMparse].
    #' @return A `PM_sim` object created by calling [SIMparse] at the completion of the
    #' simulation.
    initialize = function(poppar, ...) {
      dots <- list(...)
      combine <- if (exists("combine", where = dots)) {
        dots$combine
      } else {
        FALSE
      }
      clean <- if (exists("clean", where = dots)) {
        dots$clean
      } else {
        TRUE
      }
      outname <- if (exists("outname", where = dots)) {
        dots$outname
      } else {
        "simout"
      }
      if (inherits(poppar, c("PM_result", "PM_final", "PMfinal"))) {
        poppar <- poppar # SIMrun will handle any of these
      } else if (inherits(poppar, "PMsim")) { # from SIMparse
        private$populate(poppar)
        return(self)
      } else if (inherits(poppar, "list")) {
        poppar <- poppar # PMfinal and PMsim are lists, so needs to be after those for manual list
      } else { # try it as a filename
        if (file.exists(poppar)) {
          private$populate(readRDS(poppar)) # open and return saved object
          return(self)
        } else {
          stop(paste0(poppar, " does not exist in the current working directory.\n"))
        }
      }

      system("echo 347 > SEEDTO.MON") # TODO: look to fix the simulator without this
      SIMrun(poppar, ...)

      # TODO: read files and fix the missing E problem
      sim_files <- list.files() %>% .[grepl(paste0(outname, "*"), .)]
      if (length(sim_files) == 1) {
        parseRes <- SIMparse(file = paste0(outname, "*")) %>% private$populate()
      } else {
        if (combine) {
          parseRes <- SIMparse(file = paste0(outname, "*"), combine = T) %>% private$populate()
        } else {
          parseRes <- list()
          for (i in seq_len(length(sim_files))) {
            parseRes[[i]] <- SIMparse(file = sprintf("simout%i.txt", i))
          }
          private$populate(parseRes, list = TRUE)
        }
      }
      if (clean) system(paste0("rm ", outname, "*"))
      return(self)
    },
    #'
    #' @description
    #' Save the current PM_sim object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMsim.rds
    save = function(file_name = "PMsim.rds") {
      saveRDS(self, file_name)
    },
    #' @description
    #' Plot `PM_sim` object.
    #' @param at Index of the PM_sim object to be plotted. Default is 1.
    #' result.
    #' @param ... Arguments passed to [plot.PM_sim].
    plot = function(at = 1, ...) {
      if (inherits(self$data, "PM_simlist")) {
        if (at > length(self$data)) {
          stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
        }
        plot.PM_sim(self$data[[at]], ...)
      } else {
        plot.PM_sim(self$data, ...)
      }
    },
    #' @description
    #' Estimates the Probability of Target Attaintment (PTA), based on the results
    #' of the current Simulation.
    #' @param ... Additional parameters, refer to [makePTA]
    pta = function(...) {
      PM_pta$new(self, ...)
    },
    #' @description
    #' Calculates the AUC of the specified simulation
    #' @param at Index of the PM_sim object to use. Default is 1.
    #' @param ... Arguments passed to [makeAUC].
    auc = function(at = 1, ...) {
      if (inherits(self$data, "PM_simlist")) {
        if (at > length(self$data)) {
          stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
        }
        makeAUC(self$data[[at]], ...)
      } else {
        makeAUC(self$data, ...)
      }
    },
    #' @description
    #' Summarize simulation
    #' @param at Index of the PM_sim object to be summarized. Default is 1.
    #' @param field Quoted character value, one of
    #' * obs for simulated observations
    #' * amt for simulated amounts in each compartment
    #' * parValues for simulated parameter values
    #' @param by Optional quoted column name(s) to group by, e.g. `by = "id"` or
    #' `by = c("id", "outeq")`.
    #' @param individual If "id" is included within `by`, the summary statistics
    #' will be calculated for each individual, i.e. each individual's mean, sd, median,
    #' min and max observations. If `individual` is `FALSE` (the default), then those
    #' summaries will again be summarized to present, for example, the mean of
    #' all subjects' mean observations, or the mean of their maximum observations.
    #' This argument will be ignored if "id" is not in `by`.
    #' @return If `by` is ommitted, a data frame with rows for each data element except ID,
    #' and columns labeled as mean, sd, median, min and max. If `by` is specified,
    #' return will be a list with named elements mean, sd, median, min and max, each containing
    #' the corresponding value for each group in `by`.
    summary = function(at = 1, field, by, individual = FALSE) {
      summaries <- c("mean", "sd", "median", "min", "max")
      # get the right data
      if (inherits(self$data, "PM_simlist")) {
        if (at > length(self$data)) {
          stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
        }
        dat <- self$data[[at]][[field]]
      } else {
        dat <- self$data[[field]]
      }

      if (!missing(by)) {
        summ <- purrr::map(
          summaries,
          \(x) dplyr::summarize(dat,
            dplyr::across(everything(), !!!dplyr::syms(x)),
            .by = by
          )
        )
        names(summ) <- summaries
        if ("id" %in% by) {
          if (!individual) {
            # group by individual, then summarize
            by <- by %>% purrr::discard(\(x) x == "id") # remove id
            if (length(by) == 0) {
              by <- NULL
            } # set to NULL if character(0)
            summ <- purrr::map(summ, \(x){
              purrr::map_df(summaries, \(y) {
                dplyr::summarize(x,
                  dplyr::across(everything(), !!!dplyr::syms(y)),
                  .by = by
                ) %>%
                  select(-id)
              }) %>%
                t() %>%
                as.data.frame() %>%
                rename_with(~summaries)
            })
          }
        } else {
          # id is not in grouping, so just remove id column
          summ <- purrr::map(summ, \(x) x %>% select(-id))
        }
      } else { # no grouping
        summ <- purrr::map_df(summaries, \(x) summarize(dat, across(everything(), !!!syms(x))))
        if ("id" %in% names(summ)) {
          summ <- summ %>% select(-id)
        }
        summ <- summ %>%
          t() %>%
          as.data.frame()
        names(summ) <- summaries
      }

      return(summ)
    },
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to run a simulation. Replaced by `PM_sim$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    run = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_sim$run()", details = "PM_sim$run() is deprecated. Please use PM_sim$new() instead.")
    },
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load a prior simulation. Replaced by `PM_sim$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_sim$load()", details = "PM_sim$load() is deprecated. Please use PM_sim$new() instead.")
    }
  ), # end public
  private = list(
    # Create new simulation objects with results of simulation
    populate = function(simout, list = FALSE) {
      if (!list) {
        self$obs <- simout$obs
        self$amt <- simout$amt
        self$parValues <- simout$parValues
        self$totalMeans <- simout$totalMeans
        self$totalCov <- simout$totalCov
        self$data <- simout
        class(self$data) <- c("PMsim", "list")
        return(self)
      } else {
        purrr::map(1:length(simout), \(x){
          self$obs[[x]] <- simout[[x]]$obs
          self$amt[[x]] <- simout[[x]]$amt
          self$parValues[[x]] <- simout[[x]]$parValues
          self$totalMeans[[x]] <- simout[[x]]$totalMeans
          self$totalCov[[x]] <- simout[[x]]$totalCov
          self$data[[x]] <- simout[[x]]
          class(self$data[[x]]) <- c("PMsim", "list")
        })
        class(self$data) <- c("PM_simlist", "list")
        return(self)
      }
    }
  ) # end private
)


#' @keywords internal
#' @name PM_sim
#' @export
PM_sim$run <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_sim$run()", details = "Please use PM_sim$new() instead. ?PM_sim for details.")
}

#' @keywords internal
#' @name PM_sim
#' @export
PM_sim$load <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_sim$load()", details = "Please use PM_sim$new() instead. ?PM_sim for details.")
}
