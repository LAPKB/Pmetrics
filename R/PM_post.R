# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 -----------------------------------------------------------------


#' @title Individual Bayesian posterior predictions at short intervals
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains the Bayesian posterior predictions at short intervals
#' specified as an argument to the $run method of [PM_fit]. Default is every 12 minutes.
#'
#' @details
#' #' The [PM_post] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for
#' analysis and plotting of posterior predictions generated during the run.
#'
#' Because [PM_post] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_post] objects
#' themselves.
#'
#' The main results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' data frame with population predicted outputs for all subjects.
#'
#' To provide a more traditional experience in R,
#' the data frame is separated by columns into fields, e.g. `id` or `time`. This
#' allows you to access them in an S3 way, e.g. `run1$post$time` if `run1` is a
#' `PM_result` object.
#'
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `trough <- run1$post$data %>% filter(time == 24)`. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_post <- R6::R6Class(
  "PM_post",
  public = list(
    #' @field data A data frame with the following columns:
    #' * **id** Subject id
    #' * **time** Time of predictions in decimal hours
    #' * **icen** Prediction based on mean or median of Bayesian posterior parameter distribution
    #' * **outeq** Output equation number
    #' * **pred** Predicted output for each outeq
    #' * **block** Observation blocks within subjects as defined by *EVID=4* dosing events
    data = NULL,
    #' @description
    #' Create new object populated with Bayesian posterior predicted data at
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_post` object is automatic and not generally necessary
    #' for the user to do.
    #' @param PMdata include `r template("PMdata")`.
    #' @param path include `r template("path")`.
    #' @param ... Not currently used.
    initialize = function(PMdata = NULL, path = ".", ...) {
      self$data <- private$make(PMdata, path)
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_pop].
    #' @param ... Arguments passed to [plot.PM_pop]
    plot = function(...) {
      plot.PM_post(self, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_post].
    #' @param ... Arguments passed to [summary.PM_pop]
    summary = function(...) {
      summary.PM_pop(self, ...)
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
    } else if (inherits(data, "PM_post") & !is.null(data$data)) { # file not there, and already PM_post
      class(data$data) <- c("PM_post_data", "data.frame")
      return(data$data)
    } else {
      cli::cli_warn(c(
        "!" = "Unable to generate post pred information.",
        "i" = "{.file {file.path(path, 'pred.csv')}} does not exist, and result does not have valid {.code PM_post} object."
      ))
      return(NULL)
    }
    
    if (is.null(op_raw)) {
      return(NA)
    }
    
    post <- op_raw %>%
    pivot_longer(
      cols = c(post_median, post_mean),
      values_to = "pred"
    ) %>%
    dplyr::rename(icen = name) %>%
    mutate(icen = dplyr::case_when(
      icen == "post_median" ~ "median",
      icen == "post_mean" ~ "mean"
    )) %>%
    mutate(block = block + 1) %>%
    mutate(outeq = outeq + 1) %>%
    relocate(id, time, icen, outeq, pred, block)
    
    class(post) <- c("PM_post_data", "data.frame")
    return(post)
  }
) # end private
)




# PLOT --------------------------------------------------------------------

#' @title Plot PM_post Prediction Data
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots [PM_post] objects
#' @details
#' This is a function usually called by the `$plot()` method for [PM_post] objects
#' within a [PM_result] to generate the plot.
#' However, the function can be called directly on a [PM_post] object.
#' This function will plot time and population predictions with a variety of options.
#' By default markers are included and  have the following plotly properties:
#' `list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))`.
#' Markers can be joined by lines, default is `TRUE`. If `TRUE`,
#' the joining lines will have the following properties:
#' `list(color = "dodgerblue", width = 1, dash = "solid"`.
#' The grid and legend are omitted by default.
#'
#' @method plot PM_post
#' @param x The name of a [PM_post]  object, e.g. `NPex$post`.
#' in a [PM_result] object
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param line Controls characteristics of lines as for all plotly plots.
#' It can either be a boolean or a list. If set to `TRUE` or
#' a list of plotly line attributes, it
#' will generate line segments joining observations. If set to
#' `FALSE`, no segments will be generated. The default
#' values for the elements of formatting list, all of which can be
#' overriden are:
#'     - `color` Color of the segments. Default is "dodgerblue".
#'     - `width `Width of the segments, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#' Example: `line = list(color = "red", dash = "longdash", width = 2)`
#' @param marker Formats the symbols plotting observations. `r template("marker")`
#' @param color Character vector naming a column to **group** by, e.g. "outeq" or "icen".
#' @param colors to use for **groups** when there are multiple `outeq`, `block`, or `icen`.
#' This can be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors as groups within `color`, as colors
#' will be interpolated to generate the correct number. The default when `color`
#' is specified is the "Set1" palette.
#' @param names A character vector of names to label the **groups** if `legend = TRUE`.
#' This vector does need to be the same length as the number of groups.
#' Example: `c("Mean", "Median")` if `icen = c("mean", "median")`.
#' @param mult `r template("mult")`
#' @param icen Can be "median" for the predictions based on median of
#' parameter value distributions, "mean", or both, e.g. `c("mean", "median")`.  Default is "median".
#' @param outeq `r template("outeq")` Default is 1, but can be multiple if present in the data, e.g. `1:2` or `c(1, 3)`.
#' @param block `r template("block")` Default is 1, but can be multiple if present in the data, as for `outeq`.
#' @param overlay Operator to overlay all time prediction profiles in a single plot.
#' The default is `TRUE`. If `FALSE`, will trellisplot subjects one at a time. Can also be
#' specified as a vector with number of rows and columns, e.g. `c(3, 2)` for 3 rows and
#' 2 columns of subject splots to include in each trellis.
#' @param legend Not used for this plot.
#' @param log `r template("log")`
#' @param grid `r template("grid")`
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param xlab `r template("xlab")` Default is "Time".
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
#' @param ... `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_post], [PM_result]
#' @export
#' @examples
#' \dontrun{
#' # basic spaghetti plot
#' NPex$post$plot()
#' # format line and marker
#' NPex$post$plot(
#'   marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
#'   line = list(color = "orange")
#' )
#' }

#' @family PMplots

plot.PM_post <- function(x,
  include = NULL,
  exclude = NULL,
  line = TRUE,
  marker = TRUE,
  color = NULL,
  colors = "Set1",
  names = NULL,
  mult = 1,
  icen = "median",
  outeq = 1,
  block = 1,
  overlay = TRUE,
  legend = FALSE,
  log = FALSE,
  grid = FALSE,
  xlab = "Time",
  ylab = "Output",
  title = "",
  print = TRUE,
  xlim, ylim, ...) {
    # Plot parameters ---------------------------------------------------------
    
    x <- if (inherits(x, "PM_post")) {
      x$data
    }
    
    # process marker
    marker <- amendMarker(marker)
    
    # process line
    line <- amendLine(line)
    
    
    # get the rest of the dots
    layout <- amendDots(list(...))
    
    # legend
    legendList <- amendLegend(legend)
    layout <- modifyList(layout, list(showlegend = legendList$showlegend))
    if (length(legendList) > 1) {
      layout <- modifyList(layout, list(legend = within(legendList, rm(showlegend))))
    }
    
    # grid
    layout$xaxis <- setGrid(layout$xaxis, grid)
    layout$yaxis <- setGrid(layout$yaxis, grid)
    
    # axis labels if needed
    layout$xaxis$title <- amendTitle(xlab)
    if (is.character(ylab)) {
      layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
    } else {
      layout$yaxis$title <- amendTitle(ylab)
    }
    
    
    # axis ranges
    if (!missing(xlim)) {
      layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
    }
    if (!missing(ylim)) {
      layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
    }
    
    # log y axis
    if (log) {
      layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
    }
    
    # title
    layout$title <- amendTitle(title, default = list(size = 20))
    
    # overlay
    if (is.logical(overlay)) { # T/F
      if (!overlay) { # F,default
        nrows <- 1
        ncols <- 1
      } # if T, no need to set nrows or ncols
    } else { # specified as c(rows, cols)
      nrows <- overlay[1]
      ncols <- overlay[2]
      overlay <- FALSE
    }
    
    # Data processing ---------------------------------------------------------
    
    # filter
    presub <- x %>%
    filter(outeq %in% !!outeq, block %in% !!block, icen %in% !!icen) %>%
    mutate(group = "") %>%
    includeExclude(include, exclude)
    
    # group
    if (outeq[1] != 1 | length(outeq) > 1) {
      presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", outeq: ", outeq))
    }
    if (block[1] != 1 | length(block) > 1) {
      presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", block: ", block))
    }
    
    if (length(icen) > 1) {
      presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", ", icen))
    }
    
    presub$group <- stringr::str_replace(presub$group, "^\\s*,*\\s*", "")
    if (!is.null(names)) {
      presub$group <- factor(presub$group, labels = names)
    } else {
      presub$group <- factor(presub$group)
    }
    
    # select relevant columns
    sub <- presub %>%
    select(id, time, pred, outeq, group) %>%
    ungroup()
    sub$group <- factor(sub$group)
    
    # remove missing
    # sub <- sub %>% filter(pred != -99) # obsolete now
    
    
    
    # Plot function ----------------------------------------------------------
    
    dataPlot <- function(allsub, overlay) {
      # set appropriate pop up text
      if (!overlay) {
        hovertemplate <- "Time: %{x}<br>Pred: %{y}<extra></extra>"
        text <- ""
      } else {
        hovertemplate <- "Time: %{x}<br>Pred: %{y}<br>ID: %{text}<extra></extra>"
        text <- ~id
      }
      
      if (!all(is.na(allsub$group)) && any(allsub$group != "")) { # there was grouping
        n_colors <- length(levels(allsub$group))
        if (checkRequiredPackages("RColorBrewer")) {
          palettes <- RColorBrewer::brewer.pal.info %>% mutate(name = rownames(.))
          if (length(colors) == 1 && colors %in% palettes$name) {
            max_colors <- palettes$maxcolors[match(colors, palettes$name)]
            colors <- colorRampPalette(RColorBrewer::brewer.pal(max_colors, colors))(n_colors)
          }
        } else {
          cli::cli_inform(c("i" = "Group colors are better with RColorBrewer package installed."))
          colors <- getDefaultColors(n_colors) # in plotly_Utils
        }
        
        marker$color <- NULL
        line$color <- NULL
      } else { # no grouping
        allsub$group <- factor(1, labels = "Predicted")
      }
      
      p <- allsub %>%
      plotly::plot_ly(
        x = ~time, y = ~ pred * mult,
        color = ~group,
        colors = colors,
        name = ~group
      ) %>%
      plotly::add_markers(
        marker = marker,
        text = text,
        hovertemplate = hovertemplate
      ) %>%
      plotly::add_lines(
        line = line,
        showlegend = FALSE
      )
      p <- p %>% plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        title = layout$title,
        showlegend = layout$showlegend,
        legend = layout$legend
      )
      return(p)
    } # end dataPlot
    
    
    # Call plot ---------------------------------------------------------------
    
    # call the plot function and display appropriately
    if (overlay) {
      sub <- sub %>% dplyr::group_by(id)
      p <- dataPlot(sub, overlay = TRUE)
      if (print) print(p)
    } else { # overlay = FALSE, ie. split them
      
      if (!checkRequiredPackages("trelliscopejs")) {
        cli::cli_abort(c("x" = "Package {.pkg trelliscopejs} required to plot when {.code overlay = FALSE}."))
      }
      sub_split <- x %>%
      nest(data = -id) %>%
      mutate(panel = trelliscopejs::map_plot(data, \(x) dataPlot(x, overlay = FALSE)))
      p <- sub_split %>%
      ungroup() %>%
      trelliscopejs::trelliscope(name = "Data", nrow = nrows, ncol = ncols)
      if (print) print(p)
    }
    
    return(invisible(p))
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
  #' @method summary PM_post
  #' @param object A [PM_post] object
  #' @param digits Integer, used for number of digits to print.
  #' @param icen Can be either "median" for the predictions based on medians of the posterior parameter value
  #' distributions, or "mean".  Default is "median".
  #' @param outeq Output equation number.  Default is 1.
  #' @param ... Not used.
  #' @return A data frame with the minimum, first quartile, median, third quartile, maximum,
  #' mean and standard deviation for times and predictions in `x`.
  #'
  #' @author Michael Neely
  #' @examples
  #' \dontrun{
  #' NPex$post$summary() # preferred
  #' summary(NPex$post) # alternative
  #' }
  
  #' @seealso [PM_post]
  #' @export
  
  summary.PM_post <- function(object, digits = max(3, getOption("digits") - 3),
  icen = "median",
  outeq = 1, ...) {
    sumWrk <- function(data) {
      sumstat <- matrix(NA, nrow = 7, ncol = 2, dimnames = list(c("Min", "25%", "Median", "75%", "Max", "Mean", "SD"), c("Time", "Pred")))
      # min
      sumstat[1, ] <- round(apply(data[, c(2, 5)], 2, min, na.rm = T), digits)
      # 25th percentile
      sumstat[2, ] <- round(apply(data[, c(2, 5)], 2, quantile, 0.25, na.rm = T), digits)
      # median
      sumstat[3, ] <- round(apply(data[, c(2, 5)], 2, median, na.rm = T), digits)
      # 75th percentil
      sumstat[4, ] <- round(apply(data[, c(2, 5)], 2, quantile, 0.75, na.rm = T), digits)
      # max
      sumstat[5, ] <- round(apply(data[, c(2, 5)], 2, max, na.rm = T), digits)
      # mean
      sumstat[6, ] <- round(apply(data[, c(2, 5)], 2, mean, na.rm = T), digits)
      # SD
      sumstat[7, ] <- round(apply(data[, c(2, 5)], 2, sd, na.rm = T), digits)
      sumstat <- data.frame(sumstat)
      # N
      N <- length(data$pred[!is.na(data$pred)])
      
      return(sumstat)
    } # end sumWrk
    
    # make summary
    if (inherits(object, "PM_post")) {
      object <- object$data
    }
    
    object <- object %>% filter(outeq == !!outeq, icen == !!icen)
    if (all(is.na(object$pred))) {
      result <- NA
    } else {
      result <- sumWrk(object)
    }
    return(result)
  }
  