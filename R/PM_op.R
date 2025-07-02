# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 -------------------------------------------------------------------

#' @title Observed vs. predicted data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains observed vs. predicted data after a run, typically a field in a [PM_result]
#'
#' @details
#' The [PM_op] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for analysis and plotting of
#' observed vs. population or individual predicted outputs.
#'
#' Because [PM_op] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_op] objects
#' themselves.
#'
#' The main results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' You can use this `$data` field for custom manipulations, e.g. `trough <- run1$op$data %>% filter(time == 24)`.
#' If you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#'
#' To provide a more traditional experience in R,
#' the `$data` field is also separated by columns into the other data fields within the R6 object,
#' e.g. `id` or `time`. This
#' allows you to access them in an S3 way, e.g. `run1$op$time` if `run1` is a
#' [PM_result] object.
#'
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_op <- R6::R6Class(
  "PM_op",
  public = list(
    #' @field id subject identification
    id = NULL,
    #' @field time observation time in relative units, usually hours
    time = NULL,
    #' @field obs observation
    obs = NULL,
    #' @field pred prediction
    pred = NULL,
    #' @field pred.type Population predictions based on Bayesian prior parameter value distribution,
    #' or individual predictions based on Bayesian posterior parameter value distributions
    pred.type = NULL,
    #' @field icen Predictions based on mean or median of Bayesian `pred.type`parameter values
    icen = NULL,
    #' @field outeq output equation number
    outeq = NULL,
    #' @field block dosing block number for each subject, as defined by dose resets (evid=4).
    block = NULL,
    #' @field obsSD standard deviation of the observation based on the assay error polynomial
    obsSD = NULL,
    #' @field d prediction error, `pred` - `obs`
    d = NULL,
    #' @field ds squared prediction error
    ds = NULL,
    #' @field wd weighted prediction error, which is the prediction error divided by the `obsSD`
    wd = NULL,
    #' @field wds weighted squared prediction error
    wds = NULL,
    #' @field data A data frame of class **PM_op_data** combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with observed vs. predicted data
    #' @details
    #' Creation of new `PM_op` object is automatic at the end of a run and not generally necessary
    #' for the user to do.
    #' @param PMdata include `r template("PMdata")`.
    #' @param ... Not currently used.
    initialize = function(PMdata = NULL, ...) {
      op <- private$make(PMdata)
      self$data <- op
      if (length(op) > 1) { # all the objects were made
        self$id <- op$id
        self$time <- op$time
        self$obs <- op$obs
        self$pred <- op$pred
        self$pred.type <- op$pred.type
        self$icen <- op$icen
        self$outeq <- op$outeq
        self$block <- op$block
        self$obsSD <- op$obsSD
        self$d <- op$d
        self$ds <- op$ds
        self$wd <- op$wd
        self$wds <- op$wds
      }
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_op].
    #' @param ... Arguments passed to [plot.PM_op]
    plot = function(...) {
      plot.PM_op(self, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_op].
    #' @param ... Arguments passed to [summary.PM_op]
    summary = function(...) {
      summary.PM_op(self, ...)
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC]
    #' @param data The object to use for AUC calculation
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      rlang::try_fetch(makeAUC(self, ...),
      error = function(e) {
        cli::cli_warn("Unable to generate AUC.", parent = e)
        return(NULL)
      }
    )
  }
), # end public
private = list(
  make = function(data) {
    if (file.exists("op.csv")) {
      op_raw <- readr::read_csv(file = "op.csv", show_col_types = FALSE)
    } else if (inherits(data, "PM_op")) { # file not there, and already PM_op
      class(data$data) <- c("PM_op_data", "data.frame")
      return(data$data)
    } else {
      cli::cli_warn(c(
        "!" = "Unable to generate obs-pred information.",
        "i" = "Result does not have valid {.code PM_op} object, and {.file {getwd()}/op.csv} does not exist."
      ))
      return(NULL)
    }
    
    if (file.exists("settings.json")) {
      config <- jsonlite::fromJSON("settings.json", simplifyVector = FALSE)
    } else {
      cli::cli_warn(c(
        "!" = "Unable to generate obs-pred information.",
        "i" = "Result does not have valid {.code PM_op} object, and {.file {getwd()}/settings.json} does not exist."
      ))
      return(NULL)
    }
    
    poly <- map(config$errormodels$models, \(x) x %>% pluck(1, "poly") %>% unlist()) %>% bind_rows()
    op <- op_raw %>%
    # left_join(pred_raw, by = c("id", "time", "outeq")) %>%
    pivot_longer(cols = c(pop_mean, pop_median, post_mean, post_median)) %>%
    mutate(
      icen = case_when(
        name == "pop_mean" ~ "mean",
        name == "pop_median" ~ "median",
        name == "post_mean" ~ "mean",
        name == "post_median" ~ "median",
      )
    ) %>%
    mutate(
      pred.type = case_when(
        name == "pop_mean" ~ "pop",
        name == "pop_median" ~ "pop",
        name == "post_mean" ~ "post",
        name == "post_median" ~ "post",
      )
    ) %>%
    select(-name) %>%
    dplyr::rename(pred = value) %>%
    dplyr::mutate(outeq = outeq + 1) %>%
    dplyr::mutate(obs = na_if(obs, -99)) %>%
    dplyr::rowwise() %>%
    mutate(d = pred - obs) %>%
    mutate(ds = d * d) %>%
    mutate(obsSD = map(1:4, \(x) poly[outeq, x] * obs^(x-1)) %>% unlist() %>% sum()) %>%
    mutate(wd = d / obsSD) %>%
    mutate(wds = wd * wd) %>%
    dplyr::ungroup()
    class(op) <- c("PM_op_data", "data.frame")
    return(op)
  } # end make
) # end private
)



# PLOT --------------------------------------------------------------------

#' @title Plot Pmetrics Observed vs. Predicted Objects
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plot PM_op objects
#' @details This is a function usually called by the `$plot()` method for [PM_op] objects
#' within a [PM_result] to generate a plot of Observed vs. Predicted observations.
#' The function can be called directly on a [PM_op] object. The default
#' is to generate an observed vs. predicted plot of predictions based on the
#' median of the Bayesian posterior distributions for each subject.
#' @method plot PM_op
#' @param x The name of a [PM_op] data object
#' and loaded with [PM_load] as a field in a [PM_result], e.g. `PM_result$op`.
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param icen `r template("icen")`
#' @param pred.type Either 'post' for a posterior object or 'pop' for a population object.  Default is 'post'.
#' @param outeq `r template("outeq")`
#' @param block `r template("block")` Default is missing, which results in all blocks included.
#' @param marker `r template("marker")` Default is
#' `marker = list(color = orange, shape = "circle", size = 10, opacity = 0.5, line = list(color = black, width = 1))`.
#' @param line Controls characteristics of lines. Unlike
#' some other Pmetrics plots, for plot.PM_op, `line` is a list of
#' three elements:
#' * `lm`  If set to `TRUE` or a list of plotly line attributes,
#' will generate a linear regression of the form obs ~ pred.
#' Line attributes will control the appearance of the regression
#' line and the confidence interval around the line. If set to
#' `FALSE`, no linear regression will be generated. The default
#' values for the elements of the `lm` list, all of which can be
#' overriden are:
#'     - `ci` Confidence interval around the regression, default 0.95.
#'     - `color` Color of the regression line and the confidence area around
#' the line, but at opacity = 0.2. Default is "dodgerblue".
#'     - `width `Width of the regression line, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#' Example: `line = list(lm = list(color = "red", dash = "longdash", width = 2))`
#' * `loess` If set to `TRUE` or a list of plotly line attributes,
#' will generate a loess regression of the form obs ~ pred.
#' The list elements and default values in the `loess` list are the
#' same as for `lm` except the default style is "dash".
#' Example: `line = list(lm = FALSE, loess = TRUE)`
#' * `ref` If set to `TRUE` or a list of plotly line attributes,
#' will generate a reference line with slope = 1 and intercept = 0.
#' The default values for the elements of the `ref` list are:
#'     - `color` "grey".
#'     - `width` 1.
#'     - `dash` "dot".
#' Note that there is no *ci* argument for the *ref* list.
#' Example: `line = list(lm = FALSE, loess = TRUE, ref = list(color = "lightgrey"))`
#' If the `line` argument is missing, it will be set to
#' `line = list(lm = TRUE, loess = FALSE, ref = TRUE)`, i.e. there will be a linear
#' regression with reference line, but no loess regression. However, if `resid = T`,
#' the default will become `line = list(lm = FALSE, loess = TRUE, ref = TRUE)`, i.e.,
#' loess regression with reference line, but no linear regression.
#' @param mult `r template("mult")`
#' @param legend Ignored for this plot.
#' @param resid Boolean operator to generate a plot of weighted prediction error vs. time,
#' a plot of weighted prediction error vs. prediction. Prediction error is
#' pred - obs. By default a loess regression will indicate deviation from
#' zero prediction error.
#' @param log `r template("log")`
#' @param grid `r template("grid")`
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param xlab `r template("xlab")`   If missing, will default to "Predicted" for
#' plots when `resid = F` and either "Time" or "Predicted" for residual plots.
#' @param ylab `r template("ylab")`   If missing, will default to "Observed" for
#' plots when `resid = F` and either "Individual weighted residuals" or
#' "Population weighted residuals" for residual plots, depending on the value of
#' `pred.type`.
#' @param title `r template("title")` Default is to have no title.
#' @param stats Add the statistics from linear regression to the plot. If
#' `FALSE`, will be suppressed. Default is `TRUE` which results in default format of
#' `list(x= 0.8, y = 0.1, font = list(color = "black", family = "Arial", size = 14, bold = FALSE))`.
#' The coordinates are relative to the plot with lower left = (0,0), upper right = (1,1). This
#' argument maps to `plotly::add_text()`.
#' @param ... `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_result], [PM_op], [schema]
#' @export
#' @examples
#' \dontrun{
#' NPex$op$plot()
#' NPex$op$plot(pred.type = "pop")
#' NPex$op$plot(line = list(lm = TRUE, ref = TRUE, loess = FALSE))
#' NPex$op$plot(line = list(loess = list(ci = 0.9, color = "green")))
#' NPex$op$plot(marker = list(color = "blue"))
#' NPex$op$plot(resid = TRUE)
#' NPex$op$plot(stats = list(x = 0.5, y = 0.2, font = list(size = 7, color = "blue")))
#' }
#' @family PMplots
plot.PM_op <- function(x,
  line = list(lm = NULL, loess = NULL, ref = NULL),
  marker = TRUE,
  resid = FALSE,
  icen = "median", pred.type = "post", outeq = 1, block,
  include, exclude,
  mult = 1,
  legend,
  log = FALSE,
  grid = TRUE,
  xlab, ylab,
  title,
  stats = TRUE,
  xlim, ylim, ...) {
    if (inherits(x, "PM_op")) {
      x <- x$data
    }
    
    # include/exclude
    if (missing(include)) include <- unique(x$id)
    if (missing(exclude)) exclude <- NULL
    if (missing(block)) {
      block <- unique(x$block)
    }
    
    if (max(outeq) > max(x$outeq)) {
      cli_abort(c(
        "x" = "{.cls PM_op} object does not have {outeq} output equation{?s}.",
        "i" = "Choose {max(x$outeq)} or fewer for {.code outeq}."
      ))
    }
    if (max(block) > max(x$block)) {
      cli_abort(c(
        "x" = "{.cls PM_op} object does not have {block} blocks.",
        "i" = "Choose {max(x$block)} or fewer for {.code block}."
      ))
    }
    
    sub1 <- x %>%
    dplyr::filter(
      icen == !!icen, outeq == !!outeq, pred.type == !!pred.type,
      block %in% !!block
    ) %>%
    includeExclude(include, exclude) %>%
    dplyr::filter(!is.na(obs)) %>%
    dplyr::mutate(pred = pred * mult, obs = obs * mult) %>%
    dplyr::arrange(id, time)
    
    if (nrow(sub1) == 0) {
      cli_abort(c("x" = "You have selected <0> rows in your {.cls PM_op} object.", "i" = "Check the values of {.code include}, {.code exclude}, {.code outeq}, and {.code block}."))
    }
    
    
    # unnecessary arguments for consistency with other plot functions
    if (!missing(legend)) {
      notNeeded("legend", "plot.PM_op")
    }
    
    # process reference lines
    if (any(!names(line) %in% c("lm", "loess", "ref"))) {
      cli::cli_warn(c("!" = "{.code line} should be a list with at most three named elements: {.code lm}, {.code loess}, and/or {.code ref}.", "i" = "See {.fn Pmetrics::plot.PM_op}."))
    }
    if (!is.list(line)) {
      cli::cli_warn(c("!" = "{.code line} should be a list.", "i" = "See {.fn Pmetrics::plot.PM_op}."))
      line <- list()
    }
    
    if (!resid) { # defaults
      if (is.null(line$lm)) {
        line$lm <- T
      }
      if (is.null(line$loess)) {
        line$loess <- F
      }
      if (is.null(line$ref)) {
        line$ref <- T
      }
    } else { # defaults for residual plot
      if (is.null(line$lm)) {
        line$lm <- F
      }
      if (is.null(line$loess)) {
        line$loess <- T
      }
      if (is.null(line$ref)) {
        line$ref <- T
      }
    }
    marker <- amendMarker(marker, default = list(color = "orange"))
    lmLine <- amendLine(line$lm, default = list(color = "dodgerblue", dash = "solid"))
    loessLine <- amendLine(line$loess, default = list(color = "dodgerblue", dash = "dash"))
    refLine <- amendLine(line$ref, default = list(color = "grey", dash = "dot"))
    
    if (is.logical(line$lm)) {
      lmLine$plot <- line$lm
    } else {
      lmLine$plot <- T
    }
    
    if (is.logical(line$loess)) {
      loessLine$plot <- line$loess
    } else {
      loessLine$plot <- T
    }
    
    if (is.logical(line$ref)) {
      refLine$plot <- line$ref
    } else {
      refLine$plot <- T
    }
    
    
    # process dots
    layout <- amendDots(list(...))
    
    # legend - not needed for this function
    layout <- modifyList(layout, list(showlegend = FALSE))
    
    # grid
    layout$xaxis <- setGrid(layout$xaxis, grid)
    layout$yaxis <- setGrid(layout$yaxis, grid)
    
    # axis ranges
    if (!missing(xlim)) {
      layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
    }
    if (!missing(ylim)) {
      layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
    }
    
    # title
    if (missing(title)) {
      title <- ""
    }
    layout$title <- amendTitle(title, default = list(size = 20))
    
    
    
    # PLOTS -------------------------------------------------------------------
    if (!resid) { # default plot
      
      # axis labels
      xlab <- if (missing(xlab)) {
        "Predicted"
      } else {
        xlab
      }
      ylab <- if (missing(ylab)) {
        "Observed"
      } else {
        ylab
      }
      
      layout$xaxis$title <- amendTitle(xlab)
      if (is.character(ylab)) {
        layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
      } else {
        layout$yaxis$title <- amendTitle(ylab)
      }
      
      # log axes
      if (log) {
        layout$xaxis <- modifyList(layout$xaxis, list(type = "log"))
        layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
      }
      
      # make square
      # define anchor axis as the one with largest
      if (max(x$obs, na.rm = TRUE) > max(x$pred, na.rm = TRUE)) { # anchor is y axis
        layout$xaxis <- modifyList(layout$xaxis, list(matches = "y"))
      } else { # anchor is x axis
        layout$yaxis <- modifyList(layout$yaxis, list(matches = "x"))
      }
      
      
      p <- sub1 %>%
      plotly::plot_ly(x = ~pred) %>%
      plotly::add_markers(
        y = ~obs,
        marker = marker,
        text = ~id,
        hovertemplate = "Pred: %{x:.2f}<br>Obs: %{y:.2f}<br>ID: %{text}<extra></extra>"
      )
      
      if (lmLine$plot) { # linear regression
        lmLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(lmLine$ci))) {
          ci <- 0.95
        } else {
          ci <- lmLine$ci
          lmLine$ci <- NULL # remove to allow only formatting arguments below
        }
        
        p <- p %>% add_smooth(ci = ci, line = lmLine, stats = stats)
      }
      
      if (loessLine$plot) { # loess regression
        loessLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(loessLine$ci))) {
          ci <- 0.95
        } else {
          ci <- loessLine$ci
          loessLine$ci <- NULL # remove to allow only formatting arguments below
        }
        p <- p %>% add_smooth(ci = ci, line = loessLine, method = "loess")
      }
      
      if (refLine$plot) { # reference line
        refLine$plot <- NULL # remove to allow only formatting arguments below
        layout$refLine <- list(
          type = "line",
          x0 = 0,
          y0 = 0,
          x1 = 1,
          y1 = 1,
          xref = "paper",
          yref = "paper",
          line = refLine
        )
      }
      
      # set layout
      p <- p %>%
      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend,
        shapes = layout$refLine,
        title = layout$title
      )
      
      print(p)
      return(p)
    } else { # residual plot
      # Y axis and point labels
      if (pred.type == "post") {
        ylab <- "Individual weighted residuals (pred - obs)"
        pointLab <- "IWRES"
      } else {
        ylab <- "Population weighted residuals (pred - obs)"
        pointLab <- "PWRES"
      }
      
      layout$yaxis$title <- amendTitle(ylab)
      # amend xaxis title later
      
      
      # res vs. time
      p1 <- sub1 %>%
      plotly::plot_ly(x = ~time) %>%
      plotly::add_markers(
        y = ~wd,
        marker = marker,
        text = ~id,
        hovertemplate = paste0("Time: %{x:.2f}<br>", pointLab, ": %{y:.2f}<br>ID: %{text}<extra></extra>")
      )
      
      
      # res vs. pred
      p2 <- sub1 %>%
      plotly::plot_ly(x = ~pred) %>%
      plotly::add_markers(
        y = ~wd,
        marker = marker,
        text = ~id,
        hovertemplate = paste0("Pred: %{x:.2f}<br>", pointLab, ": %{y:.2f}<br>ID: %{text}<extra></extra>")
      )
      
      # add reference lines
      if (lmLine$plot) {
        lmLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(lmLine$ci))) {
          ci <- 0.95
        } else {
          ci <- lmLine$ci
          lmLine$ci <- NULL # remove to allow only formatting arguments below
        }
        p1 <- p1 %>% add_smooth(ci = ci, line = lmLine, stats = stats)
        p2 <- p2 %>% add_smooth(ci = ci, line = lmLine, stats = stats)
      }
      
      if (loessLine$plot) {
        loessLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(loessLine$ci))) {
          ci <- 0.95
        } else {
          ci <- loessLine$ci
          loessLine$ci <- NULL # remove to allow only formatting arguments below
        }
        p1 <- p1 %>% add_smooth(ci = ci, line = loessLine, method = "loess")
        p2 <- p2 %>% add_smooth(ci = ci, line = loessLine, method = "loess")
      }
      # set layout
      layout$xaxis$title <- amendTitle("Time")
      p1 <- p1 %>%
      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend
      )
      
      layout$xaxis$title <- amendTitle("Predicted")
      p2 <- p2 %>%
      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend,
        title = layout$title
      )
      
      # final residual plot
      p <- subplot(p1, p2,
        nrows = 1,
        shareY = TRUE, shareX = FALSE, titleX = TRUE
      )
      print(p)
      return(p)
    } # end resid plot
  }
  
  # SUMMARY -------------------------------------------------------------------
  
  
  #' @title Summarize Observations and Predictions
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' Summarize a Pmetrics Observed vs. Predicted object
  #'
  #' @details This is a function usually called by the `$summary()` method for [PM_op] objects
  #' within a [PM_result] to summarize observations, predictions and errors. The function can
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
  #' \dontrun{
  #' NPex$op$summary() # preferred
  #' summary(NPex$op) # alternative
  #' }
  
  #' @seealso [PM_op]
  #' @export
  
  summary.PM_op <- function(object, digits = max(3, getOption("digits") - 3),
  pred.type = "post", icen = "median",
  outeq = 1, ...) {
    argList <- list(...)
    if ("type" %in% names(argList)) {
      cli::cli_inform(c("i" = "{.code type} has been updated to {.code pred.type}.\fPlease update your script."))
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
    if (inherits(object, "PM_op")) {
      object <- object$data
    }
    
    object <- object %>% filter(outeq == !!outeq, pred.type == !!pred.type, icen == !!icen)
    if (all(is.na(object$obs))) {
      sumstat <- NA
      pe <- data.frame(mpe = NA, mwpe = NA, mspe = NA, rmse = NA, percent_rmse = NA, mwspe = NA, bamspe = NA, bamwspe = NA)
      wtd.t <- NA
      result <- list(sumstat = sumstat, pe = pe, wtd.t = wtd.t)
      class(result) <- c("summary.PM_op", "list")
      return(result)
    } else {
      sumresult <- sumPMopWrk(object)
    }
    
    class(sumresult) <- c("summary.PM_op", "list")
    attr(sumresult, "pred.type") <- pred.type
    return(sumresult)
  }
  # PRINT SUMMARY -------------------------------------------------------------------
  
  #' @title Print Summary of Observations and Predictions
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' Print a Pmetrics Observed vs. Predicted Summary Object
  #'
  #' @details
  #' Print a summary of observations, predictions and errors in a [summary.PM_op] object
  #' made by [summary.PM_op]. Users do not normally need to call this
  #' function directly, as it will be the default method to display the object.
  #'
  #' @title Print Summary of Observations and Predictions
  #' @method print summary.PM_op
  #' @param x An object made by [summary.PM_op].
  #' @param digits Integer, used for number of digits to print.
  #' @param ... Not used.
  #' @return A printed object.
  #' @author Michael Neely
  #' @seealso [summary.PM_op]
  #' @examples
  #' \dontrun{
  #' NPex$op$summary()
  #' }
  #' @export
  
  print.summary.PM_op <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    printSumWrk <- function(data, dataname) {
      cat(paste("\n", dataname, "\n", sep = ""))
      print(data$sumstat)
      cat(paste("\nPrediction type:", attr(x, "pred.type"), "\n"))
      cat("\n\n")
      cat(paste("Mean prediction error:", round(data$pe$mpe, digits), "\n"))
      cat(paste("Mean weighted prediction error (bias): ", round(data$pe$mwpe, digits), " (P=", round(data$wtd.t$p.value, digits), " different than 0)\n", sep = ""))
      cat(paste("Mean squared prediction error:", round(data$pe$mspe, digits), "\n"))
      cat(paste("Root mean squared error (RMSE):", round(data$pe$rmse, digits), "\n"))
      cat(paste("Percent root mean squared error (%RMSE):", round(data$pe$percent_rmse, digits), "\n"))
      cat(paste("Mean weighed squared prediction error:", round(data$pe$mwspe, digits), "\n"))
      cat(paste("Bias-adjusted mean squared prediction error:", round(data$pe$bamspe, digits), "\n"))
      cat(paste("Bias-adjusted mean weighted squared prediction error (imprecision):", round(data$pe$bamwspe, digits), "\n\n"))
    }
    # function to make summary
    if (inherits(x[[1]], "data.frame")) { # we just have one
      if (!is.na(x$pe[1])) {
        printSumWrk(x, "")
      } else {
        cat("NA\n")
      }
    } else {
      if (all(unlist(sapply(x, is.na)))) {
        cat("No observations.\n")
      } else {
        for (i in 1:length(x)) {
          if (!is.na(x[[i]][1])) {
            printSumWrk(x[[i]], paste("$", names(x)[i], sep = ""))
          } else {
            cat("NA\n")
          }
        }
      }
    }
  }
  