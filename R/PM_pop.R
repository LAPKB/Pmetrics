# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ------------------------------------------------------------------

#' @title Population predictions at short intervals
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains the population predictions at short intervals
#' specified as an argument to the `$fit` method of [PM_model]. Default is every 12 minutes.
#'
#' @details
#' #' The [PM_pop] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for
#' analysis and plotting of population predictions generated during the run.
#'
#' Because [PM_pop] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_pop] objects
#' themselves.
#'
#' The main results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' data frame with population predicted outputs for all subjects.
#'
#' To provide a more traditional experience in R,
#' the data frame is separated by columns into fields, e.g. `id` or `time`. This
#' allows you to access them in an S3 way, e.g. `run1$pop$time` if `run1` is a
#' `PM_result` object.
#'
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `trough <- run1$pop$data |> filter(time == 24)`. If
#' you are unfamiliar with the base R `|>` pipe function, type `help("|>")`
#' into the R console for documentation and examples.
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_pop <- R6::R6Class(
  "PM_pop",
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
    #' Create new object populated with population predicted data at
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_pop` object is automatic and not generally necessary
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
      plot.PM_pop(self, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_pop].
    #' @param ... Arguments passed to [summary.PM_pop]
    summary = function(...) {
      summary.PM_pop(self, ...)
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [make_AUC]
    #' @param data The object to use for AUC calculation
    #' @param ... Arguments passed to [make_AUC]
    auc = function(...) {
      rlang::try_fetch(make_AUC(self, ...),
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
        op_raw <- readr::read_csv(
          file = file.path(path, "pred.csv"),
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
          ), show_col_types = FALSE
        )
      } else if (inherits(data, "PM_pop") & !is.null(data$data)) { # file not there, and already PM_pop
        class(data$data) <- c("PM_pop_data", "data.frame")
        return(data$data)
      } else {
        cli::cli_warn(c(
          "!" = "Unable to generate pop pred information.",
          "i" = "{.file {file.path(path, 'pred.csv')}} does not exist, and result does not have valid {.code PM_pop} object."
        ))
        return(NULL)
      }

      if (is.null(op_raw)) {
        return(NA)
      }

      pop <- op_raw |>
        select(-post_median, -post_mean) |>
        pivot_longer(cols = c(pop_median, pop_mean), values_to = "pred") |>
        dplyr::rename(icen = name) |>
        mutate(icen = dplyr::case_when(
          icen == "pop_median" ~ "median",
          icen == "pop_mean" ~ "mean"
        )) |>
        mutate(block = block + 1) |>
        mutate(outeq = normalize_engine_index(outeq)) |>
        relocate(id, time, icen, outeq, pred, block)

      class(pop) <- c("PM_pop_data", "data.frame")
      return(pop)
    }
  ) # end private
)


# PLOT --------------------------------------------------------------------

#' @title Plot PM_pop Prediction Data
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots *PM_pop* objects
#' @details
#' This is a function usually called by the `$plot()` method for [PM_pop] objects
#' within a [PM_result] to generate the plot.
#' However, the function can be called directly on a [PM_pop] object.
#' This function will plot time and population predictions with a variety of options.
#' By default markers are included and  have the following plotly properties:
#' `list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))`.
#' Markers are omitted by default.
#' If enabled, markers can be joined by lines, default is `line = list(join = TRUE)`.
#' If joined,
#' the joining lines will have the following properties:
#' `list(color = "dodgerblue", width = 1, dash = "solid"`.
#' The grid and legend are omitted by default.
#'
#' @method plot PM_pop
#' @param x The name of a [PM_pop]  object, e.g. `NPex$pop`.
#' in a [PM_result] object
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param line Controls characteristics of lines as for all plotly plots.
#' It can either be a boolean or a list. If set to `TRUE` or
#' a list of plotly line attributes, it
#' will generate line segments joining observations. If set to
#' `FALSE`, no segments will be generated.
#' Line colors are fixed to match marker colors for group displays and cannot be overridden.
#' Other customizable line properties are:
#'     - `width` Width of the segments, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#'     - `join` Set to `TRUE` (default) to join markers with lines, or `FALSE` for markers only.
#' Example: `line = list(width = 2, dash = "longdash", join = FALSE)`
#' @param marker Formats the symbols plotting observations. `r template("marker")`
#' Marker colors control group display colors. When multiple output equations are displayed
#' (via `outeq`), `marker$color` can be a palette name from `RColorBrewer::brewer.pal.info` or
#' a vector of colors. Line colors are automatically matched to marker colors.
#' @param out_names A character vector of names to label the output equations if `legend = TRUE`.
#' Must be at least as long as the maximum value in `outeq`.
#' Example: `c("Conc A", "Conc B")` if `outeq = c(1, 2)`.
#' @param mult `r template("mult")`
#' @param icen Can be `"median"` for predictions based on the median of the parameter
#' value distributions, or `"mean"`. Default is `"median"`.
#' Only a single value is accepted; `outeq` is the sole grouping dimension.
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
#' @seealso [PM_pop], [PM_result]
#' @export
#' @examples
#' \dontrun{
#' # basic spaghetti plot
#' NPex$pop$plot()
#' # format line and marker
#' NPex$pop$plot(
#'   marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
#'   line = list(color = "orange")
#' )
#' }

#' @family PMplots

plot.PM_pop <- function(
  x,
  include = NULL,
  exclude = NULL,
  line = list(join = TRUE),
  marker = FALSE,
  out_names = NULL,
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
  xlim, ylim,
  print = TRUE, ...
) {
  # Plot parameters ---------------------------------------------------------

  x <- if (inherits(x, "PM_pop")) {
    x$data
  }

  user_color <- if (is.list(marker) && !is.null(marker$color)) marker$color else NULL

  line_join <- TRUE
  line_input <- line
  if (is.list(line_input) && "join" %in% names(line_input)) {
    join_val <- line_input$join
    line_input$join <- NULL
    if (is.logical(join_val)) {
      line_join <- isTRUE(join_val)
    } else if (is.list(join_val)) {
      line_input <- modifyList(line_input, join_val)
      line_join <- TRUE
    } else {
      line_join <- isTRUE(join_val)
    }
  }
  line_color_specified <- is.list(line_input) && "color" %in% names(line_input)

  # process marker
  marker <- amendMarker(marker)

  # process line
  line <- amendLine(line_input)
  if (!line_join) {
    line <- amendLine(FALSE)
  }

  if (line_color_specified) {
    cli::cli_warn(c(
      "!" = "Line colors are fixed to match marker colors for PM_pop groups.",
      "i" = "Use {.code marker$color} to control PM_pop group colors."
    ))
  }
  line$color <- NULL


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


  # filter — icen is a single-value filter only; outeq is the sole grouping dimension
  presub <- x |>
    filter(outeq %in% !!outeq, block %in% !!block, icen == !!icen[1]) |>
    mutate(group = "") |>
    includeExclude(include, exclude)

  # group by outeq only
  if (length(outeq) > 1) {
    if (is.null(out_names)) {
      out_names_vec <- paste0("Output ", seq_len(max(outeq)))
    } else if (length(out_names) < max(outeq)) {
      cli::cli_abort(c("x" = "The number of names in {.var out_names} must be at least as long as the maximum value in {.var outeq}."))
    } else {
      out_names_vec <- out_names
    }
    presub$group <- out_names_vec[presub$outeq]
  }

  presub$group <- factor(presub$group)

  # select relevant columns
  sub <- presub |>
    select(id, time, pred, outeq, group) |>
    ungroup()
  sub$group <- factor(sub$group)

  # remove missing
  # sub <- sub |> filter(pred != -99) # obsolete now


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
      if (!is.null(user_color)) {
        if (length(user_color) == 1 && requireNamespace("RColorBrewer", quietly = TRUE) &&
          user_color %in% rownames(RColorBrewer::brewer.pal.info)) {
          max_colors <- RColorBrewer::brewer.pal.info[user_color, "maxcolors"]
          colors <- colorRampPalette(RColorBrewer::brewer.pal(max_colors, user_color))(n_colors)
        } else {
          colors <- rep(as.character(user_color), length.out = n_colors)
        }
      } else if (n_colors == 1) {
        colors <- rep(marker$color[[1]], n_colors)
      } else if (requireNamespace("RColorBrewer", quietly = TRUE)) {
        n_pal <- min(max(n_colors, 3L), 9L)
        colors <- rep(RColorBrewer::brewer.pal(n_pal, "Set1"), length.out = n_colors)
      } else {
        cli::cli_inform(c("i" = "Group colors are better with RColorBrewer package installed."))
        colors <- getDefaultColors(n_colors) # in plotly_Utils
      }

      marker$color <- NULL
    } else { # no grouping
      allsub$group <- factor(1, labels = "Predicted")
      colors <- marker$color[[1]]
      marker$color <- NULL
    }

    p <- allsub |>
      plotly::plot_ly(
        x = ~time, y = ~ pred * mult,
        color = ~group,
        colors = colors,
        name = ~group
      ) |>
      plotly::add_markers(
        marker = marker,
        text = text,
        hovertemplate = hovertemplate
      ) |>
      plotly::add_lines(
        line = line,
        showlegend = FALSE
      )
    p <- p |> plotly::layout(
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
    sub <- sub |> dplyr::group_by(id)
    p <- dataPlot(sub, overlay = TRUE)
    if (print) print(p)
  } else { # overlay = FALSE, ie. split them

    if (!requireNamespace("trelliscopejs", quietly = TRUE)) {
      cli::cli_abort(c("x" = "Package {.pkg trelliscopejs} required to plot when {.code overlay = FALSE}."))
    }
    sub_split <- x |>
      nest(data = -id) |>
      mutate(panel = trelliscopejs::map_plot(data, \(x) dataPlot(x, overlay = FALSE)))
    p <- sub_split |>
      ungroup() |>
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
#' @method summary PM_pop
#' @param object A [PM_pop] object
#' @param digits Integer, used for number of digits to print.
#' @param icen Can be either "median" for the predictions based on medians of the population parameter value
#' distributions, or "mean".  Default is "median".
#' @param outeq Output equation number.  Default is 1.
#' @param ... Not used.
#' @return A data frame with the minimum, first quartile, median, third quartile, maximum,
#' mean and standard deviation for times and predictions in `x`.
#'
#' @author Michael Neely
#' @examples
#' \dontrun{
#' NPex$pop$summary() # preferred
#' summary(NPex$pop) # alternative
#' }

#' @seealso [PM_pop]
#' @export

summary.PM_pop <- function(
  object, digits = max(3, getOption("digits") - 3),
  icen = "median",
  outeq = 1, ...
) {
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
  if (inherits(object, "PM_pop")) {
    object <- object$data
  }

  object <- object |> filter(outeq == !!outeq, icen == !!icen)
  if (all(is.na(object$pred))) {
    result <- NA
  } else {
    result <- sumWrk(object)
  }
  return(result)
}
