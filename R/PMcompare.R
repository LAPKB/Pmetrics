#' @title Compare NPAG or IT2B runs
#' @description
#' ` r lifecycle::badge("stable")`
#'
#' Compare parameters, convergence, -2*log likelihood, AIC and  bias
#' and imprecision of population and posterior predictions.
#' @details
#' Objects can be specified separated by commas, e.g. `PM_compare(run1, run2, run3)`
#' followed by any arguments you wish to [plot.PMop], [mtsknn.eq].
#' P-values are based on comparison using the nearest neighbors
#' approach if all models are non-parametrics.  Models may only be compared on
#' parameters that are included in the first model.  The P-value is the
#' comparison between each model and the first model in
#' the list.  Missing P-values are when a model has no parameter names
#' in common with the first model, and for the first model compared to itself,
#' or when models from IT2B runs are included.  Significant P-values indicate
#' that the null hypothesis should be rejected, i.e. the joint distributions
#' between the two compared models for that parameter are significantly different.
#'
#' @param x The first [PM_result] object you wish to compare. Unlike the legacy
#' [PMcompare] this function only uses objects already loaded with [PM_load].
#' This will serve as the reference output for P-value testing (see details).
#' @param y The second [PM_result] object to compare.
#' @param ... Additional [PM_result] objects to compare.  See details.
#' Also, parameters to be passed to [plot.PM_op]
#' if `plot` is true as well as to [mtsknn.eq].  Order does not matter.
#' @param icen Can be either "median" for the predictions based on medians of
#' `pred.type` parameter value distributions, or "mean".  Default is "median".
#' @param outeq Number of the output equation to compare; default is 1
#' @param plot Boolean operator selecting whether to generate observed vs.
#' predicted plots for each data object as in [plot.PM_op].
#' @return A data frame with the following objects for each model to analyze:
#'  \item{run }{The run number of the data}
#'  \item{type }{NPAG or IT2B data}
#'  \item{nsub }{Number of subjects in the model}
#'  \item{nvar }{Number of random parameters in the model}
#'  \item{par }{Names of random parameters}
#'  \item{cycles }{Number of cycles run}
#'  \item{converge }{Boolean value if convergence occurred.}
#'  \item{ll }{Final cycle -2*Log-likelihood }
#'  \item{aic }{Final cycle Akaike Information Criterion}
#'  \item{bic }{Final cycle Bayesian (Schwartz) Information Criterion }
#'  \item{popBias }{Bias, or mean weighted prediction error of predictions based on population parameters minus observations}
#'  \item{popImp }{Imprecision, or bias-adjusted mean weighted squared error of predictions based on population parameters minus observations }
#'  \item{popPerRMSE}{Percent root mean squared error of predictions based on population parameters minus observations}
#'  \item{postBias }{Bias, or mean weighted prediction error of predictions - observations  based on posterior parameters}
#'  \item{postImp }{Imprecision, or bias-adjusted mean weighted squared error of predictions - observations based on posterior parameters}
#'  \item{postPerRMSE}{Percent root mean squared error of predictions based on posterior parameters minus observations}
#'  \item{pval }{P-value for each model compared to the first. See details.}
#' @author Michael Neely
#' @seealso [PM_load], [plot.PM_op], [mtsknn.eq]
#' @export

PM_compare <- function(x, y, ..., icen = "median", outeq = 1, plot = F) {
  if (missing(x) | missing(y)) stop("You must specify at least two PM_result objects for PM_compare.\n")
  stopifnot("Please specify your PM_result objects.  See help." = inherits(x, "PM_result"))


  # parse dots
  arglist <- list(...)
  namesPlot <- names(formals(plot.PM_op))
  namesMTSKNN <- names(formals(mtsknn.eq))
  # get the args to plot.PMop and set defaults if missing
  plotArgs <- which(names(arglist) %in% namesPlot)
  argsPlot <- arglist[plotArgs]
  MTSKNNargs <- which(names(arglist) %in% namesMTSKNN)
  argsMTSKNN <- arglist[MTSKNNargs]
  if (!"k" %in% names(argsMTSKNN)) argsMTSKNN$k <- 3
  if (!"print" %in% names(argsMTSKNN)) argsMTSKNN$print <- FALSE
  # get the others if there and assume that they are PMdata objects for now
  if ((length(arglist) - length(c(plotArgs, MTSKNNargs))) > 0) {
    if (length(c(plotArgs, MTSKNNargs)) == 0) {
      argsPM <- 1:length(arglist)
    } else {
      argsPM <- (1:length(arglist))[-c(plotArgs, MTSKNNargs)]
    }
  } else {
    argsPM <- NULL
  }

  if (length(argsPM) == 0) obj <- list(x, y)
  if (length(argsPM) >= 1) obj <- c(list(x, y), arglist[argsPM])

  # declare global variables to avoid problems with R CMD Check
  # NPAGout <- NULL
  # get each obj
  nobj <- length(obj)
  allObj <- purrr::map(obj, function(x) {
    if (!is.null(x$NPdata)) {
      x$NPdata
    } else {
      x$ITdata
    }
  })

  objClass <- purrr::map(allObj, function(x) class(x)[1])
  # check for non-Pmetrics data objects and remove them if found
  yesPM <- which(objClass %in% c("NPAG", "IT2B"))
  allObj <- allObj[yesPM]
  objClass <- objClass[yesPM]

  # check for zero cycle objects
  cycles <- unlist(sapply(allObj, function(x) x$icyctot))
  if (any(cycles == 0)) stop(paste("Do not include 0-cycle runs: item(s) ", paste(which(cycles == 0), collapse = ", "), "\n", sep = ""))

  op <- purrr::map(obj, function(x) {
    x$op$data
  })

  if (plot) {
    if (!"resid" %in% names(argsPlot)) {
      if (nobj <= 3) {
        par(mfrow = c(nobj, 2))
      } else {
        par(mfrow = c(3, 2))
        devAskNewPage(ask = T)
      }
      for (i in 1:length(op)) {
        do.call(plot.PM_op, args = c(list(
          x = op[[i]], pred.type = "pop", icen = icen, outeq = outeq,
          title = paste("Model", i, "Population")
        ), argsPlot))
        do.call(plot.PM_op, args = c(list(
          x = op[[i]], pred.type = "post", icen = icen, outeq = outeq,
          title = paste("Model", i, "Posterior")
        ), argsPlot))
      }
    } else {
      devAskNewPage(ask = T)
      for (i in 1:length(op)) {
        do.call(plot.PM_op, args = c(list(
          x = op[[i]], pred.type = "post", icen = icen, outeq = outeq,
          title = paste("Model", i)
        ), argsPlot))
      }
    }
    par(mfrow = c(1, 1))
    devAskNewPage(ask = F)
  }

  # get summaries of op for outeq
  sumobjPop <- mapply(summary.PMop, op, MoreArgs = list(outeq = outeq, pred.type = "pop", icen = icen), SIMPLIFY = F)
  sumobjPost <- mapply(summary.PMop, op, MoreArgs = list(outeq = outeq, pred.type = "post", icen = icen), SIMPLIFY = F)


  popBias <- sapply(sumobjPop, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$mwpe))
  postBias <- sapply(sumobjPost, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$mwpe))
  popImp <- sapply(sumobjPop, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$bamwspe))
  postImp <- sapply(sumobjPost, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$bamwspe))
  popPercent_RMSE <- sapply(sumobjPop, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$percent_rmse))
  postPercent_RMSE <- sapply(sumobjPost, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$percent_rmse))

  # get population points
  final <- purrr::map(obj, function(x) {
    x$final$data
  })
  # find intersecting parameters
  popPointsRef <- final[[1]]$popPoints
  namesRef <- names(popPointsRef)
  popPointsOther <- lapply(2:nobj, function(x) final[[x]]$popPoints)
  t <- sapply(2:nobj, function(x) {
    thisPopPoints <- popPointsOther[[x - 1]]
    namesThis <- names(thisPopPoints)
    intersect <- namesRef[namesRef %in% namesThis]
    if (length(intersect) > 0) {
      popPoints1 <- popPointsRef[, intersect]
      popPoints2 <- thisPopPoints[, intersect]
      t <- do.call(mtsknn.eq, args = c(list(x = popPoints1, y = popPoints2), argsMTSKNN))$pval
    } else {
      t <- NA
    }
    signif(t, 3)
  })

  t <- c(NA, t)
  results <- data.frame(
    run = seq(1:nobj),
    type = unlist(objClass),
    nsub = unlist(purrr::map(obj, function(x) {
      x$final$nsub
    })),
    nvar = mapply(function(x) x$nvar, allObj),
    par = mapply(function(x) paste(x$par, collapse = " "), allObj),
    converge = mapply(function(x) x$converge == 1, allObj),
    ll = mapply(function(x) -2 * x$ilog[length(x$ilog)], allObj),
    aic = mapply(function(x) tail(x$iic[, 1], 1), allObj),
    bic = mapply(function(x) tail(x$iic[, 2], 1), allObj),
    popBias = popBias,
    popImp = popImp,
    popPer_RMSE = popPercent_RMSE,
    postBias = postBias,
    postImp = postImp,
    postPer_RMSE = postPercent_RMSE,
    pval = t
  )
  names(results)[7] <- "-2*LL"
  results[, 7:15] <- format(results[, 7:15], digits = 4)
  row.names(results) <- 1:nobj
  results
}



