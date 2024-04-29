#Summary methods
#Use menu item Code -> Jump To... for rapid navigation


# PM_cov ------------------------------------------------------------------


#' @title Summarize Covariates and Bayesian Posterior Parameter Values
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Summarize a Pmetrics Covariate object
#' 
#' @details This is a function usually called by the `$summary()` method for [PM_cov] objects
#' with a [PM_result] to summarize covariates and Bayesian posterior parameter 
#' values for each subject. The function can
#' be called directly on a [PM_cov] object. See examples.Summarize .
#'
#' @method summary PM_cov
#' @param object A PM_cov object
#' @param icen Summary function for covariates with time dependent values and posterior parameters. 
#' Default is "median", but can specify "mean". 
#' @param ... Not used.
#' @return A data frame with the summary of the PM_cov object for each subject's covariates and
#' Bayesian posterior parameter values.
#' @author Michael Neely
#' @seealso [PM_cov]
#' @examples
#' library(PmetricsData)
#' NPex$cov$summary() #preferred
#' summary(NPex$cov) #alternative
#' NPex$cov$summary(icen = "mean") #use mean as summary
#' 
#' @export

summary.PM_cov <- function(object, icen = "median", ...) {
  
  if(inherits(object, "PM_cov")){ #user called summary(PM_cov)
    object <- object$data
  }
  
  if ("icen" %in% names(object)) {
    data <- object[object$icen == icen, ]
    data <- subset(data, select = -icen)
  } else {
    data <- object
  }
  # get order of ID in case non-numeric
  allID <- unique(data$id)
  orderID <- rank(allID)
  sumCov <- aggregate(data[, -1], list(data$id), match.fun(icen), na.rm = T)
  # reorder in ID order
  sumCov <- sumCov[orderID, ]
  # replace the first grouping column with ID again
  sumCov[, 1] <- allID
  names(sumCov)[1] <- "id"
  # set the attribute to be the type of summary
  attr(sumCov, "icen") <- icen
  return(sumCov)
}


# PM_data -----------------------------------------------------------------

#' @title Summarize PM_data objects
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Summarize the raw data used for a Pmetrics run.
#'
#' @method summary PM_data
#' @param object A [PM_data] object.
#' @param formula Optional formula for specifying custom summaries.  See [aggregate]
#' and [formula] for details on how to specify formulae in R. If, for example, the data contain
#' a covariate for weight named 'wt', then to summarize the mean dose in mg/kg per subject specify
#' `formula = dose/wt ~ id` and  `FUN = mean`.
#' @param FUN The summary function to apply to [formula], if specified. This is not
#' quoted, and usual choices will be [mean], [median], [max], or [min].
#' @param include A vector of subject IDs to include in the summary, e.g. `c(1:3,5,15)`
#' @param exclude A vector of subject IDs to exclude in the summary, e.g. `c(4,6:14,16:20)`
#' @param ... Additional arguments to `FUN`, e.g. `na.rm = TRUE`
#' @return A list of class *summary.PM_data* with the following items:
#' * **nsub** Number of subjects
#' * **ndrug** Number of drug inputs
#' * **numeqt** Number of outputs
#' * **nobsXouteq** Number of observations by outeq
#' * **missObsXouteq** Number of missing observations by outeq
#' * **ncov** Number of covariates
#' * **covnames** Covariate names
#' * **ndoseXid** Number of doses per input per subject
#' * **nobsXid** Number of observations per outeq per subject
#' * **doseXid** Doses per input per subject
#' * **obsXid** Observations per outeq per subject
#' * **formula** Results of including [formula]
#' @author Michael Neely
#' @seealso [aggregate]
#' @export

summary.PM_data <- function(object, formula, FUN, include, exclude, ...) {
  
  if(inherits(object, "PM_data")){ #user called summary(PM_data)
    object <- object$standard_data
  }
  # filter data if needed
  if (!missing(include)) {
    object <- subset(object, sub("[[:space:]]+", "", as.character(object$id)) %in% as.character(include))
  }
  if (!missing(exclude)) {
    object <- subset(object, !sub("[[:space:]]+", "", as.character(object$id)) %in% as.character(exclude))
  }
  
  # make results list
  results <- list()
  idOrder <- rank(unique(object$id))
  
  results$nsub <- length(unique(object$id))
  results$ndrug <- max(object$input, na.rm = T)
  results$numeqt <- max(object$outeq, na.rm = T)
  results$nobsXouteq <- tapply(object$evid, object$outeq, function(x) length(x == 0))
  results$missObsXouteq <- by(object, object$outeq, function(x) length(x$out[x$evid == 0 & x$out == -99]))
  covinfo <- getCov(object)
  ncov <- covinfo$ncov
  results$ncov <- ncov
  results$covnames <- covinfo$covnames
  results$ndoseXid <- tapply(object$evid, list(object$id, object$input), function(x) length(x != 0))[idOrder, ]
  results$nobsXid <- tapply(object$evid, list(object$id, object$outeq), function(x) length(x == 0))[idOrder, ]
  results$doseXid <- tapply(object$dose, list(object$id, object$input), function(x) x[!is.na(x)])[idOrder, ]
  results$obsXid <- tapply(object$out, list(object$id, object$outeq), function(x) x[!is.na(x)])[idOrder, ]
  if (ncov > 0) {
    # get each subject's covariate values
    results$cov <- lapply(1:ncov, function(y) {
      tapply(
        object[[covinfo$covstart + y - 1]], object$id,
        function(z) z[!is.na(z)]
      )[idOrder]
    })
    names(results$cov) <- covinfo$covnames
  }
  if (!missing(formula)) {
    results$formula <- aggregate(formula, object, FUN, ...)
  }
  
  class(results) <- c("summary.PM_data", "list")
  return(results)
} # end function


# PM_final ----------------------------------------------------------------

#' @title Summary Statistics for Final Cycle
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Generates summary statistics of final population model parameters.
#'
#' @details
#' For NPAG runs, this function will generate weighted medians as central tendencies of the
#' population points with a 95% confidence interval (95% CI) around the median,
#' and the median absolute weighted deviation (MAWD) from the median as a measure
#' of the variance, with its 95% CI.  These estimates correspond to weighted mean,
#' 95% CI of the mean, variance, and 95% CI of the variance, respectively, for a
#' sample from a normal distribution.  
#' 
#' To estimate these non-parametric summaries,
#' the function uses a Monte Carlo simulation approach, creating  1000 x npoint samples
#' with replacement from the weighted marginal distribution of each parameter,
#' where npoint is the number of support points in the model.  As an example,
#' if there are 100 support points, npoint = 100, and for Ka, there will be
#' 1000 sets of 100 samples drawn from the weighted marginal distribution of the
#' values for Ka.  For each of the 1,000 sets of npoint values, the median and MAWD are
#' calculated, with MAWD equal to the median absolute difference between each point
#' and the median of that set.  The output is npoint estimates of the weighted median
#' and npoint estimates of the MAWD for each parameter, from which the median, 2.5th,
#' and 97.5th percentiles can be found as point estimates and 95% confidence
#' interval limits, respectively, of both the weighted median and MAWD.
#'
#' For IT2B runs, the function will return the mean and variance of each parameter,
#' and the standard errors of these terms, using
#' \deqn{SE_mean = SD/\sqrt(nsub)} 
#' \deqn{SE_var = var * \sqrt(2/(nsub-1))}.
#'
#' @method summary PM_final
#' @param object The PM_final object made after an NPAG or IT2B run
#' @param lower Desired lower confidence interval boundary.  Default is 0.025. Ignored for IT2B objects.
#' @param upper Desired upper confidence interval boundary.  Default is 0.975. Ignored for IT2B objects.
#' @param ... Not used.
#' @return The output is a data frame.
#' For NPAG this has 4 columns:
#' * **value** The value of the summary statistic
#' * **par** The name of the parameter
#' * **type** Either *WtMed* for weighted median, or *MAWD* for MAWD (see details)
#' * **percentile** Requested `lower`, 0.5 (median), and `upper` quantiles
#' For IT2B this has 6 columns:
#' * **mean** Parameter mean value
#' * **se.mean** Standard error of the mean
#' * **cv.mean** Error of the mean divided by mean
#' * **var** Variance of the parameter values
#' * **se.var** Standard error of the variance
#' * **summary** Name of the summary statistic
#' @author Michael Neely
#' @seealso [PM_final]
#' @examples
#' library(PmetricsData)
#' NPex$final$summary() #preferred
#' ITex$final$summary()
#' summary(NPex$final) #alternate
#' @export

summary.PM_final <- function(object, lower = 0.025, upper = 0.975, ...) {
  
  if(inherits(object, "PM_final")){ #user called summary(PM_final)
    object <- object$data
  }
  
  if (inherits(object, "IT2B")) { # IT2B object
    if (is.null(object$nsub)) {
      nsub <- as.numeric(readline("Your IT2B run is very old. Please re-run.\nFor now, enter the number of subjects. "))
    } else {
      nsub <- object$nsub
    }
    mean <- object$popMean
    se.mean <- object$popSD / sqrt(nsub)
    cv.mean <- se.mean / mean
    var <- object$popVar
    se.var <- object$popVar * sqrt(2 / (nsub - 1))
    sumstat <- dplyr::bind_rows(
      mean, se.mean,
      cv.mean,
      var, se.var
    ) %>%
      dplyr::mutate(summary = c("Mean", "StdErr Mean", "CV% Mean", "Variance", "StdErr Var"))
    return(sumstat)
  } else { # NPAG object
    medMAD <- function(x) {
      med <- median(x)
      MAD <- median(abs(x - med))
      # MAD <- sqrt(sum((x-med)^2)/length(x))
      return(list(med, MAD))
    }
    
    mcsim <- function(x, prob) {
      set.seed(17)
      sim <- apply(matrix(sample(x, replace = T, 10^3 * length(x), prob = prob), nrow = 10^3), 1, medMAD)
      ciMed <- quantile(sapply(sim, function(x) x[[1]]), c(lower, 0.5, upper))
      ciMAD <- quantile(sapply(sim, function(x) x[[2]]), c(lower, 0.5, upper))
      return(list(ciMed, ciMAD))
    }
    if (inherits(object, "NPAG") || inherits(object, "PM_final")) {
      popPoints <- object$popPoints
    } else {
      popPoints <- object
    }
    
    nvar <- ncol(popPoints) - 1
    
    # trick it if there is only one point
    if (nrow(popPoints) == 1) {
      popPoints <- rbind(popPoints, popPoints)
      popPoints$prob <- c(0.5, 0.5)
    }
    
    sumstat <- apply(popPoints[, 1:nvar], 2, function(x) mcsim(x, popPoints[, nvar + 1]))
    
    
    sumstat2 <- sumstat %>%
      dplyr::as_tibble() %>%
      unnest(cols = names(sumstat))
    sumstat2$percentile <- rep(c(lower, 0.5, upper), 2)
    sumstat2$parameter <- rep(c("WtMed", "MAWD"), each = 3)
    
    
    # sumstat2 <- melt(sumstat)[,c(1,3)]
    # names(sumstat2) <- c("value","par")
    # sumstat2$type <- rep(c("WtMed","MAWD"),each=3,times=nvar)
    # sumstat2$quantile <- rep(c(lower,0.5,upper),times=2*nvar)
    # sumstat2 <- sumstat2[,c("par","type","quantile","value")]
    class(sumstat2) <- c("summary.PM_final", "tbl_df", "tbl", "data.frame")
    return(sumstat2)
  }
}



# PM_op -------------------------------------------------------------------


#' @title Summarize Observations and Predictions
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Summarize a Pmetrics Observed vs. Predicted object
#'
#' @details This is a function usually called by the `$summary()` method for [PM_op] objects
#' with a [PM_result] to summarize observations, predictions and errors. The function can
#' be called directly on a [PM_op] object. See examples.
#'
#' @method summary PM_op
#' @param object A [PM_op] object 
#' @param digits Integer, used for number of digits to print.
#' @param pred.type Either 'post' for a posterior object or 'pop' for a population object.  Default is 'post'.
#' @param icen Can be either "median" for the predictions based on medians of `pred.type` parameter value
#' distributions, or "mean".  Default is "median".
#' @param outeq Output equation number.  Default is 1.
#' @param ... Not used.
#' @return A list with three elements of class *summary.PM_op*.  
#' * sumstat A data frame with the minimum, first quartile, median, third quartile, maximum,
#' mean and standard deviation for times, observations and predictions in `x`.
#' * pe A named vector with mean prediction error (mpe),
#' the mean weighted prediction error (mwpe), the mean squared prediction error (mspe), root mean sqaured error (rmse),
#' percent root mean squared error (percent_rmse), the mean weighted
#' squared prediction error (mwspe), the bias-adjusted mean squared prediction error (bamspe), and the bias-
#' adjusted mean weighted squared prediction error (bamwspe).  The mwpe is bias and the bamwspe is imprecision on 
#' plots of PM_op objects.
#' * wtd.t A list of 6 elements based on a t test that the weighted mean prediction bias is different than zero
#'  - estimate: the weighted mean of the prediction bias for each observation
#'  - se: the standard error of the estimate
#'  - conf.int: the 95% confidence interval of the mean
#'  - statistic: the t statistic of the standardized difference between mean and zero
#'  - df: degrees of freedom equal to number of observations minus one
#'  - p.value: the probability that the weighted mean is different than zero
#' @author Michael Neely
#' @examples
#' library(PmetricsData)
#' NPex$op$summary() #preferred
#' summary(NPex$op) #alternative
#' @seealso [PM_op]
#' @export

summary.PM_op <- function(object, digits = max(3, getOption("digits") - 3),
                         pred.type = "post", icen = "median",
                         outeq = 1, ...) {
  argList <- list(...)
  if ("type" %in% names(argList)) {
    cat("The 'type' argument has been updated to 'pred.type'.\nPlease update your script.\n")
    return(invisible())
  }

  sumPMopWrk <- function(data) {
    sumstat <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("Min", "25%", "Median", "75%", "Max", "Mean", "SD"), c("Time", "Obs", "Pred")))
    # min
    sumstat[1, ] <- round(apply(data[, 2:4], 2, min, na.rm = T), digits)
    # 25th percentile
    sumstat[2, ] <- round(apply(data[, 2:4], 2, quantile, 0.25, na.rm = T), digits)
    # median
    sumstat[3, ] <- round(apply(data[, 2:4], 2, median, na.rm = T), digits)
    # 75th percentil
    sumstat[4, ] <- round(apply(data[, 2:4], 2, quantile, 0.75, na.rm = T), digits)
    # max
    sumstat[5, ] <- round(apply(data[, 2:4], 2, max, na.rm = T), digits)
    # mean
    sumstat[6, ] <- round(apply(data[, 2:4], 2, mean, na.rm = T), digits)
    # SD
    sumstat[7, ] <- round(apply(data[, 2:4], 2, sd, na.rm = T), digits)
    sumstat <- data.frame(sumstat)
    # N
    N <- length(data$obs[!is.na(data$obs)])
    # mean prediction error
    mpe <- sum(data$d, na.rm = T) / N
    # wt = 1/sd, so mwpe = sum(wd)/sum(wt)
    # mean weighted prediction error or BIAS
    mwpe <- sum(data$wd, na.rm = T) / N
    # mean squared prediction error
    mspe <- sum(data$ds, na.rm = T) / N
    # root mean squared error (RMSE)
    rmse <- sqrt(mspe)
    # %rmse
    percent_rmse <- rmse * 100 * N / sum(data$obs, na.rm = T)
    # mean weighted squared prediction error
    mwspe <- sum(data$wds, na.rm = T) / N
    # bias-adjusted squared prediction error
    bamspe <- mspe - mpe**2
    # imprecision - bias-adjusted mean weighted squared error
    bamwspe <- mwspe - mwpe**2

    pe <- data.frame(mpe = mpe, mwpe = mwpe, mspe = mspe, rmse = rmse, percent_rmse = percent_rmse, mwspe = mwspe, bamspe = bamspe, bamwspe = bamwspe)
    wtd.t <- weighted.t.test(data)

    result <- list(sumstat = sumstat, pe = pe, wtd.t = wtd.t)
    return(result)
  } # end sumPMopWrk

  # make summary
  if(inherits(object, "PM_op")){ #user called summary(PM_op)
    object <- object$data
  }
  
  if (inherits(object, "list")) { # we are dealing with the old list PMop
    if (missing(outeq)) {
      outeq <- 1:length(object)
    } else {
      outeq <- c(sapply(outeq, function(x) c(2 * x - 1, 2 * x)))
    }
    sumresult <- list()
    for (i in outeq) {
      if (i > length(object)) {
        sumresult[[i]] <- NA
      } else {
        print(object[[i]])
        sumresult[[i]] <- sumPMopWrk(object[[i]])
        names(sumresult)[i] <- names(object)[i]
      }
      sumresult <- sumresult[lapply(sumresult, length) != 0]
    }
  } else {
    object <- object %>% filter(outeq == !!outeq, pred.type == !!pred.type, icen == !!icen)
    if (all(is.na(object$obs))) {
      sumstat <- NA
      pe <- data.frame(mpe = NA, mwpe = NA, mspe = NA, rmse = NA, percent_rmse = NA, mwspe = NA, bamspe = NA, bamwspe = NA)
      wtd.t <- NA
      result <- list(sumstat = sumstat, pe = pe, wtd.t = wtd.t)
      class(result) <- c("summary.PMop", "list")
      return(result)
    } else {
      sumresult <- sumPMopWrk(object)
    }
  }

  class(sumresult) <- c("summary.PM_op", "list")
  attr(sumresult, "pred.type") <- pred.type
  sumresult
}


# PM_pta ------------------------------------------------------------------

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
#' @return A tibble with the following columns (only the first three if `at = "intersect"`):
#' 
#' * **sim_num** is the number of the simulation
#' * **target** is the target for the row, if targets are discrete, not used for simulated targets
#' * **label** is the simulation label, for reference
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
#' library(PmetricsData)
#' ptaEx$summary()
#' @export

summary.PM_pta <- function(object, at = "intersect", ci = 0.95, ...){
  
  pta <- object$clone()$data
  
  if(at == "intersect"){
    if(length(pta$intersect)>1){
      pta <- pta$intersect %>%
        mutate(target = as.numeric(stringr::str_extract(target, "\\d+\\.*\\d*")))
    } else {
      pta <- pta[[1]] #no intersect, summarize first
    }
  } else {
    at <- suppressWarnings(tryCatch(as.numeric(at), error = function(e) NA))
    if(!is.na(at)){
      if(at > length(pta)){
        stop("'at' is greater than the number of PTAs.")
      } else {
        pta <- pta[[at]]
      }
    } else {
      stop("'at' should be either \"intersect\" or the number of one of the objects to summarize.")
    }
    
  }
  
  simTarg <- 1+as.numeric(inherits(pta$target[[1]],"tbl_df")) #1 if missing or set, 2 if random
  if(length(simTarg)==0) simTarg <- 1
  
  if(simTarg==1){ #set targets
    
    pdi <- pta %>% group_by(.data$sim_num, .data$target) 
    
  } else { #random targets
    pdi <- pta %>% group_by(.data$sim_num)
    
  }
  
  if(at != "intersect"){
    pdi <- pdi %>% transmute(
      label = .data$label, prop_success = .data$prop_success,
      median = median(unlist(pdi), na.rm = TRUE),
      lower = quantile(unlist(pdi), probs = 0.5 - ci/2, na.rm = TRUE),
      upper = quantile(unlist(pdi), probs = 0.5 + ci/2, na.rm = TRUE),
      mean = mean(unlist(pdi), na.rm = TRUE),
      sd = sd(unlist(pdi), na.rm = TRUE),
      min = min(unlist(pdi), na.rm = TRUE),
      max = max(unlist(pdi), na.rm = TRUE)
    )
  } else {
    pdi <- pdi %>% transmute(label = label, prop_success = prop_success)
  }
  
  pdi <- pdi %>% ungroup()
  
  return(pdi)
}

