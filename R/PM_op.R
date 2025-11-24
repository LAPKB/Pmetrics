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
    #' @field cens censoring information: "none" for observed, "bloq" for below limit of quantification, "aloq" for above limit of quantification
    cens = NULL,
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
    #' @param path include `r template("path")`.
    #' @param ... Not currently used.
    initialize = function(PMdata = NULL, path = ".", ...) {
      op <- private$make(PMdata, path)
      self$data <- op
      if (length(op) > 1) { # all the objects were made
        self$id <- op$id
        self$time <- op$time
        self$obs <- op$obs
        self$cens <- op$cens
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
  make = function(data, path) {
    if (file.exists(file.path(path, "pred.csv"))) {
      op_raw <- readr::read_csv(file = file.path(path, "pred.csv"), 
      col_types = list(
        time = readr::col_double(),
        outeq = readr::col_integer(),
        block = readr::col_integer(),
        obs = readr::col_double(),
        cens = readr::col_character(),
        pop_mean = readr::col_double(),
        pop_median = readr::col_double(),
        post_mean = readr::col_double(),
        post_median = readr::col_double()
      ), show_col_types = FALSE) %>% filter(!is.na(obs))
      
      if(!"cens" %in% names(op_raw)){
        op_raw <- op_raw %>% mutate(cens = "none") # if cens column missing, assume all observed
      } 
      
      
    } else if (inherits(data, "PM_op") & !is.null(data$data)) { # file not there, and already PM_op
      if(!"cens" %in% names(data$data)){
        data$data <- data$data %>% mutate(cens = "none") # if cens column missing, assume all observed
      } 
      class(data$data) <- c("PM_op_data", "data.frame")
      return(data$data)
    } else {
      cli::cli_warn(c(
        "!" = "Unable to generate obs-pred information.",
        "i" = "{.file {file.path(path, 'pred.csv')}} does not exist, and result does not have valid {.code PM_op} object ."
      ))
      return(NULL)
    }
    
    
    
    if (file.exists(file.path(path, "settings.json"))) {
      config <- jsonlite::fromJSON(file.path(path, "settings.json"))
    } else if (inherits(data, "PM_op")) { # file not there, and already PM_op
      class(data$data) <- c("PM_op_data", "data.frame")
      return(data$data)
    } else {
      cli::cli_warn(c(
        "!" = "Unable to generate obs-pred information.",
        "i" = "{.file {file.path(path, 'settings.json')}} does not exist, and result does not have valid {.code PM_op} object."
      ))
      return(NULL)
    }
    poly <- map(config$errormodels$models, \(x) x %>% pluck("poly")) %>% bind_rows()
    op <- op_raw %>%
    # left_join(pred_raw, by = c("id", "time", "outeq")) %>%
    pivot_longer(cols = c(pop_mean, pop_median, post_mean, post_median)) %>%
    mutate(
      icen = dplyr::case_when(
        name == "pop_mean" ~ "mean",
        name == "pop_median" ~ "median",
        name == "post_mean" ~ "mean",
        name == "post_median" ~ "median",
      )
    ) %>%
    mutate(
      pred.type = dplyr::case_when(
        name == "pop_mean" ~ "pop",
        name == "pop_median" ~ "pop",
        name == "post_mean" ~ "post",
        name == "post_median" ~ "post",
      )
    ) %>%
    select(-name) %>%
    dplyr::rename(pred = value) %>%
    dplyr::mutate(outeq = outeq + 1) %>%
    dplyr::mutate(block = block + 1) %>%
    # dplyr::mutate(obs = dplyr::na_if(obs, -99)) %>% # obsolete
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
#' Missing observations are excluded. Observations reported as BLQ (below the limit of quantification)
#' are indicated as downward triangles, and colored differently from other observations. They are plotted
#' at the reported LOQ on the observed axis and the predicted value on the predicted axis. They are not
#' included in any regression lines or statistics.
#' 
#' Clicking on a point in the plot will highlight all points from that subject. The color of the highlight
#' is 180 degrees different on the color wheel from the color of the other points ([opposite_color]), ensuring good contrast.
#' Clicking on the plot background will remove the highlighting.
#' 
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
#' The color of any BLQ points is set to a color 90 degrees different on the color wheel from the
#' color of the other points using [opposite_color] to ensure good contrast. The symbol for BLQ points is fixed to "triangle-down", and
#' the opacity is fixed to 1. Size is the same as for other points.
#' @param line Controls characteristics of lines. Unlike
#' some other Pmetrics plots, for plot.PM_op, `line` is a list of
#' three elements:
#' * `lm`  If set to `TRUE` (default) or a list of plotly line attributes,
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
#' * `loess` If set to `TRUE` (default is `FALSE`) or a list of plotly line attributes,
#' will generate a loess regression of the form obs ~ pred.
#' The list elements and default values in the `loess` list are the
#' same as for `lm` except the default style is "dash".
#' Example: `line = list(lm = FALSE, loess = TRUE)`
#' * `ref` If set to `TRUE` (default) or a list of plotly line attributes,
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
#' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
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
  print = TRUE,
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
      cli::cli_abort(c(
        "x" = "{.cls PM_op} object does not have {outeq} output equation{?s}.",
        "i" = "Choose {max(x$outeq)} or fewer for {.code outeq}."
      ))
    }
    if (max(block) > max(x$block)) {
      cli::cli_abort(c(
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
      cli::cli_abort(c("x" = "You have selected <0> rows in your {.cls PM_op} object.", "i" = "Check the values of {.code include}, {.code exclude}, {.code outeq}, and {.code block}."))
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
    
    # markers
    
    normal_color <- blue()
    
    marker <- amendMarker(marker, default = list(color = normal_color))
    highlight_color <- opposite_color(marker$color) # in plotly_Utils.R
    
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
      
      # Split into traces
      traces <- sub1 %>%
      dplyr::group_split(id)
      
      # Build plot
      p <- plot_ly()
      
      for (i in seq_along(traces)) {
        trace_data <- traces[[i]]
        group_name <- trace_data$id[1]
        
        # Add group label and CENS coloring/symbol/opacity directly to the data
        trace_data <- trace_data %>% 
        rowwise() %>%
        mutate(
          text_label = dplyr::case_when(
            cens == "bloq" | cens == "1" ~ glue::glue("ID: {group_name}\nPred: {round2(pred)}\nBLOQ: {round2(obs)}"),
            cens == "none" | cens == "0" ~ glue::glue("ID: {group_name}\nPred: {round2(pred)}\nObs: {round2(obs)}"),
            cens == "aloq" | cens == "-1" ~ glue::glue("ID: {group_name}\nPred: {round2(pred)}\nALOQ: {round2(obs)}"),
            .default = glue::glue("ID: {group_name}\nPred: {round2(pred)}\nObs: {round2(obs)}")
          ),
          color = dplyr::if_else(cens != "none", opposite_color(marker$color, degrees = 90), marker$color),
          symbol = dplyr::case_when(
            cens == "bloq" | cens == "1" ~ "triangle-down", 
            cens == "none" | cens == "0" ~ as.character(marker$symbol),
            cens == "aloq" | cens == "-1" ~ "triangle-up",
            .default = as.character(marker$symbol)
          ),
          opacity = dplyr::if_else(cens != "none" & cens != "0", 1, marker$opacity)
        ) %>% ungroup()
        p <- add_trace(
          p,
          data = trace_data,
          x = ~pred,
          y = ~obs,
          type = "scatter",
          mode = "markers",
          name = group_name,
          marker = list(color = ~color, symbol = ~symbol, size = marker$size, opacity = ~opacity, line = marker$line),
          hoverinfo = "text",
          text = ~text_label,
          showlegend = FALSE
        )
        
        
      }
      
      
      if (lmLine$plot) { # linear regression
        lmLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(lmLine$ci))) {
          ci <- 0.95
        } else {
          ci <- lmLine$ci
          lmLine$ci <- NULL # remove to allow only formatting arguments below
        }
        
        p <- p %>% add_smooth(data = sub1 %>% filter(cens == "none"), x = ~pred, y = ~obs, ci = ci, line = lmLine, stats = stats)
      }
      
      if (loessLine$plot) { # loess regression
        loessLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(loessLine$ci))) {
          ci <- 0.95
        } else {
          ci <- loessLine$ci
          loessLine$ci <- NULL # remove to allow only formatting arguments below
        }
        p <- p %>% add_smooth(data = sub1 %>% filter(cens == "none" | cens == "0"), x = ~pred, y = ~obs, ci = ci, line = loessLine, method = "loess")
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
      
      if (print) print(click_plot(p, highlight_color))
      return(invisible(p))
      
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
      
      # Split into traces
      traces <- sub1 %>% filter(cens == "none" | cens == "0") %>%
      group_split(id)
      
      
      ##### Build plots
      # res vs. time
      p1 <- plot_ly()
      
      # res vs. pred
      p2 <- plot_ly()
      
      for (i in seq_along(traces)) {
        trace_data <- traces[[i]]
        group_name <- trace_data$id[1]
        
        # Add group label directly to the data
        trace_data$text_label_1 <- glue::glue("ID: {group_name}\nTime: {round2(trace_data$time)}\n{pointLab}: {round2(trace_data$wd)}")
        trace_data$text_label_2 <- glue::glue("ID: {group_name}\nPred: {round2(trace_data$pred)}\n{pointLab}: {round2(trace_data$wd)}")
        
        p1 <- add_trace(
          p1,
          data = trace_data,
          x = ~time,
          y = ~wd,
          type = "scatter",
          mode = "markers",
          name = group_name,
          marker = marker,
          hoverinfo = "text",
          text = ~text_label_1,
          showlegend = FALSE
        )
        
        p2 <- add_trace(
          p2,
          data = trace_data,
          x = ~pred,
          y = ~wd,
          type = "scatter",
          mode = "markers",
          name = group_name,
          marker = marker,
          hoverinfo = "text",
          text = ~text_label_2,
          showlegend = FALSE
        )
      }
      
      
      # add regression lines
      if (lmLine$plot) {
        lmLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(lmLine$ci))) {
          ci <- 0.95
        } else {
          ci <- lmLine$ci
          lmLine$ci <- NULL # remove to allow only formatting arguments below
        }
        p1 <- p1 %>% add_smooth(data = sub1, x = ~time, y = ~wd, ci = ci, line = lmLine, stats = stats)
        p2 <- p2 %>% add_smooth(data = sub1, x = ~pred, y = ~wd, ci = ci, line = lmLine, stats = stats)
      }
      
      if (loessLine$plot) {
        loessLine$plot <- NULL # remove to allow only formatting arguments below
        if (is.null(purrr::pluck(loessLine$ci))) {
          ci <- 0.95
        } else {
          ci <- loessLine$ci
          loessLine$ci <- NULL # remove to allow only formatting arguments below
        }
        p1 <- p1 %>% add_smooth(data = sub1, x = ~time, y = ~wd, ci = ci, line = loessLine, method = "loess")
        p2 <- p2 %>% add_smooth(data = sub1, x = ~pred, y = ~wd, ci = ci, line = loessLine, method = "loess")
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
      if (print) print(click_plot(p, highlight_color))
      return(invisible(p))
    } # end resid plot
  }
  
  
  #' @title Plot PM_op_data objects
  #' @description
  #' `r lifecycle::badge("stable")`
  #' Plots the raw data (`class: PM_op_data`) from a [PM_op] object in the same way as plotting a [PM_op] object.
  #' Both use [plot.PM_op].
  #' @method plot PM_op_data
  #' @param x A `PM_op_data`` object
  #' @param ... Additional arguments passed to [plot.PM_op]
  #' @examples
  #' \dontrun{
  #' NPex$op$data %>%
  #' dplyr::filter(pred > 5) %>%
  #' dplyr::filter(pred < 10) %>%
  #' plot()
  #' }
  #' @export
  #' 
  plot.PM_op_data <- function(x,...){
    plot.PM_op(x, ...)
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
  #' the mean weighted prediction error (mwpe), the percent mean weighted prediction error (percent_mwpe),
  #' the mean squared prediction error (mspe), root mean sqaured error (rmse),
  #' percent root mean squared error (percent_rmse), the mean weighted
  #' squared prediction error (mwspe), the bias-adjusted mean squared prediction error (bamspe), the bias-
  #' adjusted mean weighted squared prediction error (bamwspe), the percent root mean bias-
  #' adjusted weighted squared prediction error (percent_rmbawspe).  The percent_mwpe is bias and the percent_rmbawspe is imprecision on
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
      
      # N
      N <- length(data$obs[!is.na(data$obs)])
      
      sumstat <- data %>% select(time, obs, pred) %>%
      dplyr::summarize(across(
        .cols = everything(),  
        .fns = list(
          min = ~min(.x, na.rm = TRUE),
          q1 = ~quantile(.x, 0.25, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          q3 = ~quantile(.x, 0.75, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE)
        )
      )) %>%
      pivot_longer(everything(), names_sep = "_", names_to = c("type", "statistic")) %>%
      pivot_wider(names_from = type, values_from = value) 
      
      
      # BIAS
      
      # mean absolute error
      mae <- sum(data$d, na.rm = T) / N
      # % mae
      percent_mae <- mean(data$d / data$obs, na.rm = TRUE)  * 100
      
      # mean weighted error or old BIAS
      mwe <- sum(data$wd, na.rm = T) / N
      # % mean weighted error or new BIAS
      percent_mwe <- sum(data$wd, na.rm = TRUE) / sum(data$obs / data$obsSD, na.rm = TRUE) * 100
      
      # IMPRECISION
      
      mean_obs <- mean(data$obs, na.rm = TRUE)
      wmean_obs <- sum(data$obs / data$obsSD, na.rm = TRUE) / N
      
      # mean squared  error
      mse <- sum(data$ds, na.rm = T) / N
      # % mean squared  error
      percent_mse <- mean(data$ds, na.rm = TRUE) / (mean(data$obs, na.rm = TRUE)^2) * 100
      
      # mean weighted squared error
      mwse <- sum(data$wds, na.rm = T) / N
      # % mean weighted squared error
      percent_mwse <- mwse / (wmean_obs^2) * 100
      
      # root mean squared error (RMSE)
      rmse <- sqrt(mse)
      # %rmse
      percent_rmse <- rmse / mean_obs * 100
      
      # mean bias-adjusted squared error
      mbase <- mse - mae**2
      # % mean bias-adjusted squared error
      percent_mbase <- (mse - mae^2) / (mean_obs^2) * 100
      
      
      # OLD imprecision - mean bias-adjusted weighted squared error
      mbawse <- mwse - mwe**2
      # % mean bias-adjusted weighted squared error
      percent_mbawse <- (mwse - mwe^2) / (wmean_obs^2) * 100
      
      # root mean bias-adjusted, weighted squared error
      rmbawse <- sqrt(mbawse)
      # NEW imprecision - % root mean bias-adjusted, weighted squared error
      percent_rmbawse <- sqrt(mbawse) * 100 / (sum(data$obs / data$obsSD, na.rm = TRUE) / N)
      
      
      pe <- data.frame(
        type = c("mae", "mwe", "mse", "mwse", "rmse", "mbase", "mbawse", "rmbawse"),
        absolute = c(mae, mwe, mse, mwse, rmse, mbase, mbawse, rmbawse),
        percent = c(percent_mae, percent_mwe, percent_mse, percent_mwse, percent_rmse, percent_mbase, percent_mbawse, percent_rmbawse)
      )    
      
      wtd.t <- weighted.t.test(data)
      
      result <- list(sumstat = sumstat, pe = pe, wtd.t = wtd.t)
      return(result)
    } # end sumPMopWrk
    
    #### make summary #####
    if (inherits(object, "PM_op")) {
      object <- object$data
    } else if (!inherits(object, "PM_op_data")) {
      cli::cli_abort(c("x" = "{.code object} must be a {.cls PM_op} or {.cls PM_op_data} object.", "i" = "See {.fn Pmetrics::summary.PM_op}."))
    }
    
    object <- object %>% filter(outeq == !!outeq, pred.type == !!pred.type, icen == !!icen)
    if (all(is.na(object$obs))) {
      sumstat <- NA
      pe <- data.frame(
        type = rep(NA, 8),
        absolute = rep(NA, 8),
        percent = rep(NA, 8)
      )
      wtd.t <- NA
      sumresult <- list(sumstat = sumstat, pe = pe, wtd.t = wtd.t)
    } else {
      sumresult <- sumPMopWrk(object)
    }
    
    class(sumresult) <- c("summary.PM_op", "list")
    attr(sumresult, "pred.type") <- pred.type
    attr(sumresult, "outeq") <- outeq
    attr(sumresult, "icen") <- icen
    return(sumresult)
  }
  
  #' @title Summarize PM_op_data objects
  #' @description
  #' `r lifecycle::badge("stable")`
  #' Summarizes the raw data (`class: PM_op_data`) from a [PM_op] object in the same way as summarizing a [PM_op] object.
  #' Both use [summary.PM_op].
  #' @method summary PM_op_data
  #' @param object A `PM_op_data` object
  #' @param ... Additional arguments passed to [summary.PM_op]
  #' @examples
  #' \dontrun{
  #' NPex$op$data %>%
  #' dplyr::filter(pred > 5) %>%
  #' dplyr::filter(pred < 10) %>%
  #' summary()
  #' }
  #' @export
  #' 
  summary.PM_op_data <- function(object,...){
    summary.PM_op(object, ...)
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
  #' @param embed If `TRUE`, will embed the summary in the R Markdown document. Default is `FALSE`.
  #' @param ... Not used.
  #' @return A printed object.
  #' @author Michael Neely
  #' @seealso [summary.PM_op]
  #' @examples
  #' \dontrun{
  #' NPex$op$summary()
  #' }
  #' @export
  
  print.summary.PM_op <- function(x, digits = getPMoptions("digits"), embed = FALSE, ...) {
    printSumWrk <- function(data, dataname) {
      
      # Summary statistics
      ft1 <- data$sumstat %>% mutate(across(-statistic, ~round2(.x, digits = digits))) %>%
      mutate(statistic = c("Minimum", "Q1", "Median", "Q3", "Maximum", "Mean", "SD")) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(
        time = "Time", obs = "Observed", pred = "Predicted"
      ) %>%
      flextable::set_table_properties(width = 0.8, layout = "autofit") %>%
      # "Hide" the variable column from header and treat it as row label
      flextable::compose(j = "statistic", value = flextable::as_paragraph(statistic)) %>%
      # Remove header label for row names
      flextable::set_header_labels(statistic = "") %>%
      # Optional: align left
      flextable::align(j = "statistic", align = "right", part = "body") %>%
      flextable::theme_zebra() %>%
      flextable::bg(part = "header", bg = blue()) %>%
      flextable::autofit()
      
      # Bias and imprecision
      
      metric_info <- data$pe %>% get_metric_info()
      with_percent <- getPMoptions("bias_method") %>% stringr::str_detect("percent_")
      
      ft2 <- metric_info$metric_vals %>% 
      mutate(across(everything(), ~if(with_percent) paste0(.x, "%") else .x)) %>%
      flextable::flextable() %>% 
      flextable::theme_zebra() %>%
      flextable::align_text_col(header = TRUE) %>%
      flextable::bg(part = "header", bg = blue()) %>%
      flextable::autofit()
      
      pred.type <- if (attr(data, "pred.type") == "post") "posterior" else "population"
      outeq <- attr(data, "outeq")
      icen <- attr(data, "icen")
      return(list(ft1 = ft1, ft2 = ft2, metric_types = metric_info$metric_types, pred.type = pred.type, outeq = outeq, icen = icen))
    }
    
    # function to print the summary
    if (!all(is.na(x$pe))) {
      obj <- printSumWrk(x, "")
      temp <- if (embed) {"images"} else {tempdir()} # will embed in Quarto document
      
      out <- rmarkdown::render(
        input = system.file("report/templates/summary_op.Rmd", package = "Pmetrics"),
        output_file = "summary_op.html",
        output_dir = temp,
        params = list(object = obj),
        quiet = TRUE
      )
      
      if (!embed){
        suppressWarnings(htmltools::html_print(htmltools::includeHTML(out)))
      }
      return(invisible(NULL))
      
    } else {
      cat("NA\n")
    }
    
  }
  
  
  
  # Internal functions
  
  # Extracts the relevant metrics from the summary.PM_op object based on curren user options
  get_metric_info <- function(pe){
    if(!all(names(pe) %in% c("type", "absolute", "percent"))) {
      return(list(metric_types = tibble(bias = NA, imprecision = NA), metric_vals = tibble(Bias = NA, Imprecision = NA)))
    }
    current_bias <- getPMoptions("bias_method")
    with_percent <- stringr::str_detect(current_bias, "percent_")
    type_options <- pe$type
    bias_row <- which(type_options == stringr::str_replace(current_bias, "percent_", "")) 
    bias_imp_col <- ifelse(with_percent, 3, 2)
    current_imp <- getPMoptions("imp_method")
    imp_row <- which(type_options == stringr::str_replace(current_imp, "percent_", ""))
    
    metric_types <- tibble(
      bias = stringr::str_replace(current_bias, "percent_", "") %>%
      case_match(
        "mae" ~ "Mean absolute error (MAE)",
        "mwe" ~ "Mean weighted error (MWE)"
      ),
      imprecision = stringr::str_replace(current_imp, "percent_", "") %>%
      case_match(
        "mse" ~ "Mean squared error (MSE)",
        "mwse" ~ "Mean weighted squared error (MWSE)",
        "rmse" ~ "Root mean squared error (RMSE)",
        "mbase" ~ "Mean, bias-adjusted, squared error (MBASE)",
        "mbawse" ~ "Mean, bias-adjusted, weighted, squared error (MBAWSE)",
        "rmbawse" ~ "Root mean, bias-adjusted, weighted, squared error (RMBAWSE)"
      )
    ) %>% mutate(across(everything(), ~if(with_percent) paste0("% ", .x) else .x))
    
    metric_vals <- tibble(Bias = pe[bias_row, bias_imp_col],
      Imprecision = pe[imp_row, bias_imp_col]) %>% 
      mutate(across(everything(), ~round2(.x, digits = getPMoptions("digits")))) 
      
      return(list(metric_types = metric_types,
        metric_vals = metric_vals))  
        
      }
      
      