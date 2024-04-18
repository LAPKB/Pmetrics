#' @title Plot PM_data Time-Output Data
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots *PM_data* objects
#' @details
#' This function will plot raw and fitted time and concentration data with a variety of options.
#' By default markers are included and  have the following plotly properties:
#' `list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))`.
#' Markers can be joined by lines, default is `FALSE`. If chosen to be `TRUE`,
#' the joining lines will have the following properties:
#' `list(color = "dodgerblue", width = 1, dash = "solid"`.
#' The grid and legend are omitted by default.
#'
#' @method plot PM_data
#' @param x The name of an `PM_data` data object or loaded as a field
#' in a [PM_result] object
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param line Controls characteristics of lines as for all plotly plots.
#' Here  `line` is a list of two elements:
#' * `join`  Can either be a boolean or a list. If set to `TRUE` or
#' a list of plotly line attributes, it
#' will generate line segments joining observations. If set to
#' `FALSE`, no segments will be generated. The default
#' values for the elements of the `join` list, all of which can be
#' overriden are:
#'     - `color` Color of the segments. Default is "dodgerblue".
#'     - `width `Width of the segments, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#' Example: `line = list(join = list(color = "red", dash = "longdash", width = 2))`
#' * `pred` Default is `FALSE`, which means that predictions will not be included
#' in the plot. To include predictions, supply the name of a population or
#' posterior prediction object in a [PM_result] object, eg.
#' `run1$post` or `run1$pop`. To format the predictions, supply `pred` as
#' a list, with the prediction object first, followed by named options to control the
#' prediction plot:
#' * icen Chooses the median or mean of each
#' subject's Bayesian posterior parameter distribution.  Default is "median",
#' but could be "mean".
#' * Other parameters to pass to plotly to control line characteristics that join
#' the predictions, including `color`, `width`, and `dash`.
#' For example: `pred = list(run1$post, icen = "mean", color = "red", width = 2)`.
#' Default formats are the same as for the `join` argument, since normally one would not plot
#' both lines joining observations and prediction lines, i.e., typical use would be
#' `line = list(join = F, pred = run1$post)`.
#' @param marker Formats the symbols plotting observations. `r template("marker")`
#' @param color Character vector naming a column in `x` to **group** by, e.g. "id" or
#' a covariate like "gender"
#' @param colors to use for **groups**. This can be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors as groups within `color`, as colors
#' will be interpolated to generate the correct number. The default when `color`
#' is specified is the "Set1" palette.
#' @param names A character vector of names to label the **groups** if `legend = T`.
#' This vector does need to be the same length as the number of groups within `color`.
#' Example: `c("Male", "Female")` if `color = "gender"` and "gender" is a covariate
#' in the data.
#' @param mult `r template("mult")`
#' @param outeq `r template("outeq")` Default is 1, but can be multiple if present in the data, e.g. `1:2` or `c(1, 3)`.
#' @param block `r template("block")` Default is 1, but can be multiple if present in the data, as for `outeq`.
#' @param tad `r template("tad")`
#' @param overlay Operator to overlay all time concentration profiles in a single plot.
#' The default is `TRUE`. If `FALSE`, will trellisplot subjects one at a time. Can also be
#' specified as a vector with number of rows and columns, e.g. `c(3, 2)` for 3 rows and
#' 2 columns of subject splots to include in each trellis.
#' @param legend `r template("legend")` Default is `FALSE` unless groups are specified with `color`above.
#' @param log `r template("log")` 
#' @param grid `r template("grid")` 
#' @param xlim `r template("xlim")` 
#' @param ylim `r template("ylim")` 
#' @param xlab `r template("xlab")` Default is "Time".
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param \dots `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_data], [PM_result]
#' @export
#' @examples
#' library(PmetricsData)
#' # basic spaghetti plot
#' dataEx$plot()
#' # format line and marker
#' dataEx$plot(
#'   marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
#'   line = list(join = list(color = "orange"))
#' )
#' # include predictions with default format and suppress joining lines
#' dataEx$plot(
#'   line = list(join = FALSE, pred = NPex$post),
#'   xlim = c(119, 146)
#' )
#' # customize prediction lines
#' dataEx$plot(
#'   line = list(
#'     pred = list(NPex$post, color = "slategrey", dash = "dash"),
#'     join = FALSE
#'   )
#' )
#' @family PMplots

plot.PM_data <- function(x,
                         include = NA,
                         exclude = NA,
                         line = list(join = TRUE, pred = FALSE),
                         marker = TRUE,
                         color = NULL,
                         colors = "Set1",
                         names = NULL,
                         mult = 1,
                         outeq = 1,
                         block = 1,
                         tad = FALSE,
                         overlay = TRUE,
                         legend, 
                         log = FALSE, 
                         grid = FALSE,
                         xlab = "Time",
                         ylab = "Output",
                         title = "",
                         xlim, ylim, ...) {
  # Plot parameters ---------------------------------------------------------
  
  # process marker
  marker <- amendMarker(marker)
  
  # process line
  if (any(!base::names(line) %in% c("join", "pred"))) {
    cat(paste0(crayon::red("Warning: "), "<line> should be a list with at most two named elements: ", crayon::blue("<join>"), " and/or ", crayon::blue("<pred>"), ".\n See help(\"plot.PM_data\")."))
  }
  if (is.null(line$join)) {
    line$join <- FALSE
  }
  if (is.null(line$pred)) {
    line$pred <- FALSE
  }
  
  join <- amendLine(line$join)
  if (is.logical(line$pred) && !line$pred) { # if line$pred is FALSE
    line$pred <- NULL
  }
  pred <- line$pred # process further later
  
  
  # get the rest of the dots
  layout <- amendDots(list(...))
  
  #legend
  if(missing(legend)){
    if(is.null(color)){
      legend <- FALSE
    } else {legend <- TRUE}
  }
  
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
  # make blocks
  x$standard_data <- makePMmatrixBlock(x$standard_data)
  
  # time after dose
  if (tad) {
    x$standard_data$time <- calcTAD(x$standard_data)
  }
  
  # filter
  presub <- x$standard_data %>%
    filter(outeq %in% !!outeq, block %in% !!block, evid == 0) %>%
    includeExclude(include, exclude)
  
  
  
  # make group column for colors
  if (!is.null(color)) {
    if (!color %in% base::names(x$standard_data)) {
      stop(paste0(crayon::red(color), " is not a column in the data.\n"))
    }
    if (!is.null(names)) {
      presub$group <- factor(presub[[color]], labels = names)
    } else {
      presub$group <- presub[[color]]
    }
  } else {
    presub <- presub %>% mutate(group = "")
  }
  if (outeq[1] != 1 | length(outeq) > 1) {
    presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", outeq ", outeq))
  }
  if (block[1] != 1 | length(block) > 1) {
    presub <- presub %>%
      rowwise() %>%
      mutate(group = paste0(group, ", block ", block))
  }
  
  presub$group <- stringr::str_replace(presub$group, "^\\s*,*\\s*", "")
  
  
  # select relevant columns
  sub <- presub %>%
    select(id, time, out, outeq, group) %>%
    ungroup()
  sub$group <- factor(sub$group)
  
  # add identifier
  sub$src <- "obs"
  
  # remove missing
  sub <- sub %>% filter(out != -99)
  
  # now process pred data if there
  if (!is.null(pred)) {
    if (inherits(pred, c("PM_post", "PM_pop"))) {
      pred <- list(pred)
    } # only PM_post/pop was supplied, make into a list of 1
    if (!inherits(pred[[1]], c("PM_post", "PM_pop"))) {
      cat(paste0(crayon::red("Warning: "), "The first element of ", crayon::blue("pred"), " must be a PM_pop or PM_post object.\n"))
    } else {
      predData <- pred[[1]]$data
      if (length(pred) == 1) { # default
        predArgs <- TRUE
        icen <- "median"
      } else { # not default, but need to extract icen if present
        icen <- purrr::pluck(pred, "icen") # check if icen is in list
        if (is.null(icen)) { # not in list so set default
          icen <- "median"
        } else {
          purrr::pluck(pred, "icen") <- NULL
        } # was in list, so remove after extraction
        predArgs <- pred[-1]
      }
      
      predArgs <- amendLine(predArgs, default = list(color = NULL))
    }
    
    # filter and group by id
    predsub <- predData %>%
      filter(outeq %in% !!outeq, block %in% !!block, icen == !!icen) %>%
      includeExclude(include, exclude) %>%
      group_by(id)
    
    # time after dose
    if (tad) {
      predsub$time <- calcTAD(predsub)
    }
    
    # select relevant columns and filter missing
    predsub <- predsub %>%
      select(id, time, out = pred, outeq) %>%
      filter(out != -99)
    
    # add group
    lookup <- dplyr::distinct(sub, id, outeq, group)
    predsub <- predsub %>% dplyr::left_join(lookup, by = c("id", "outeq"))
    
    # add identifier
    predsub$src <- "pred"
  } else {
    predsub <- NULL
  } # end pred processing
  
  
  
  # Plot function ----------------------------------------------------------
  
  dataPlot <- function(allsub, overlay, includePred) {
    # set appropriate pop up text
    if (!overlay) {
      hovertemplate <- "Time: %{x}<br>Out: %{y}<extra></extra>"
      text <- ""
    } else {
      hovertemplate <- "Time: %{x}<br>Out: %{y}<br>ID: %{text}<extra></extra>"
      text <- ~id
    }
    
    if (!all(is.na(allsub$group)) && any(allsub$group != "")) { # there was grouping
      n_colors <- length(levels(allsub$group))
      if(requireNamespace("RColorBrewer", quietly = TRUE)){
        palettes <- RColorBrewer::brewer.pal.info %>% mutate(name = rownames(.))
        if (length(colors) == 1 && colors %in% palettes$name) {
          max_colors <- palettes$maxcolors[match(colors, palettes$name)]
          colors <- colorRampPalette(RColorBrewer::brewer.pal(max_colors, colors))(n_colors)
        }
      } else {
        cat(paste0(crayon::green("Note: "), "Group colors are better with RColorBrewer package installed.\n"))
        colors <- getDefaultColors(n_colors) #in plotly_Utils
      }
      
      marker$color <- NULL
      join$color <- NULL
    } else { # no grouping
      allsub$group <- factor(1, labels = "Observed")
    }
    
    p <- allsub %>%
      plotly::filter(src == "obs") %>%
      plotly::plot_ly(
        x = ~time, y = ~ out * mult,
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
        line = join,
        showlegend = FALSE
      )
    
    
    if (includePred) {
      # if(!is.null(color)){ predArgs$color <- NULL}
      p <- p %>%
        plotly::add_lines(
          data = allsub[allsub$src == "pred", ], x = ~time, y = ~out,
          color = ~group,
          line = predArgs,
          name = "Predicted",
          showlegend = FALSE
        )
    }
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
  
  
  # if pred present, need to combine data and pred for proper display
  
  if (!is.null(predsub)) {
    allsub <- dplyr::bind_rows(sub, predsub) %>% dplyr::arrange(id, time)
    includePred <- T
  } else {
    allsub <- sub
    includePred <- F
  }
  
  
  # call the plot function and display appropriately
  if (overlay) {
    allsub <- allsub %>% dplyr::group_by(id)
    p <- dataPlot(allsub, overlay = TRUE, includePred)
    print(p)
  } else { # overlay = FALSE, ie. split them
    if(!requireNamespace("trelliscopejs", quietly = TRUE)){
      stop(paste0("Package trelliscopejs required to plot when overlay = ", crayon::red("FALSE")))
    }
    sub_split <- allsub %>%
      nest(data = -id) %>%
      mutate(panel = trelliscopejs::map_plot(data, \(x) dataPlot(x, overlay = F, includePred = includePred)))
    p <- sub_split %>%
      ungroup() %>%
      trelliscopejs::trelliscope(name = "Data", nrow = nrows, ncol = ncols)
    print(p)
  }
  
  return(p)
}


#' @title Plot PMmatrix Time-Output Data
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Plots *PMmatrix* objects. It is largely now a
#' legacy plotting function, replaced
#' by [plot.PM_data].
#' @details
#' This function will plot raw and fitted time and concentration data, which can be
#' accessed as the `$data` object within the `$data` field of a [PM_data] object, e.g.
#' `PM_data$data$data`.
#' For the legend, defaults that are different that the standard are:
#' \itemize{
#'   \item x Default \dQuote{topright}
#'   \item legend Default will be factor label names if \code{group} is specified and valid; otherwise
#'   \dQuote{Output 1, Output 2,...Output n}, where \emph{n} is the number of output equations.  This default
#'   can be overridden by a supplied character vector of output names.
#'   \item fill The color of each group/output as specified by the default color scheme or \code{col}
#'   \item bg Default \dQuote{white}
#' }
#'
#' @method plot PMmatrix
#' @param x The name of an \emph{PMmatrix} data object read by \code{\link{PMreadMatrix}}
#' @param include A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)
#' @param exclude A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)
#' @param pred The name of a population or posterior prediction object read by \code{\link{makePop}} or
#' \code{\link{makePost}}, respectively
#' @param icen Only relevant for PMpost or PMpop objects which have predictions based on median or mean of each
#' subject's Bayesian posterior parameter distribution.  Default is "median", but could be "mean".
#' @param mult Multiplication factor for y axis, e.g. to convert mg/L to ng/mL
#' @param outeq A vector of output equation(s) to plot; if missing, plot all.  E.g. outeq=1, outeq=2, outeq=c(1,3).
#' @param group Quoted name of a covariate in \code{data} by which
#' to distinguish groups with color in the plot. Note that if covariates do not have values on observation
#' rows, those observations will be unable to be grouped.  Grouping is only applicable if \code{outeq} is
#' specified; otherwise there would be a confusing mix of colors for groups and output equations.
#' @param block Which block to plot, where a new block is defined by dose resets (evid=4); default is 1.
#' @param layout If \code{overlay} is \code{False}, this parameter specifies the number of plots per page.
#' @param log Boolean operator to plot in log-log space; the default is \code{False}
#' @param pch Controls the plotting symbol for observations; default is NA which results in no symbol.
#' Use 0 for open square, 1 for open circle, 2 for open triangle, 3 for cross, 4 for X, or 5 for a diamond.
#' Other alternatives are \dQuote{*} for asterisks, \dQuote{.} for tiny dots, or \dQuote{+} for a smaller,
#' bolder cross.  These plotting symbols are standard for R (see \code{\link{par}}).
#' @param errbar Either boolean (true/false) or a list.  If assay error coefficients are included
#' in the data file, setting this to \code{True} will plot error bars around each observation
#' according to the standard deviation calculated from C0, C1, C2 and C3 in the data file.
#' If C0, C1, C2, and C3 are missing in the data file, you can specify \code{errbar} to be a named list,
#' i.e. \code{list(c0=,c1=,c2=,c3=)}, where each value is a vector of length equal to the number of
#' output equations.  For example, with two output equations having coefficients of
#' 0.1, 0.15, 0, 0 and 0.2, 0.1, -0.001, and 0, specify as \code{errbar=list(c0=c(0.1,0.2),
#' c1=c(0.15,0.1),c2=c(0,-0.001),c3=c(0,0))}.
#' @param doses Boolean operator to include doses as small lines at the bottom of the plot.
#' Infusions are correctly represented according to their duration.  The default is \code{False}.
#' This parameter is ignored if \code{overlay} is \code{True}.
#' @param tad Boolean operator to use time after dose rather than time after start.  Default is \code{False}.
#' @param join Boolean operator to join observations by a straight line; the default is \code{True}.
#' @param grid Either a boolean operator to plot a reference grid, or a list with elements x and y,
#' each of which is a vector specifying the native coordinates to plot grid lines; the default is \code{False}.
#' For example, grid=list(x=seq(0,24,2),y=1:10).  Defaults for missing x or y will be calculated by \code{\link{axTicks}}.
#' @param ident Boolean operator to plot points as ID numbers in overlay plots; the default is \code{False}.  Ignored if \code{overlay} is false.
#' This option is useful to identify outliers.
#' @param overlay Boolean operator to overlay all time concentration profiles in a single plot.
#' The default is \code{True}.
#' @param main An optional parameter to specify the title for plot(s).  If \code{overlay} is \code{False},
#' the default will be the subject identification. If \code{overlay} is \code{True}, the default is blank.
#' To omit a title from a non-overlaid plot, use the syntax \code{main=}\dQuote{}.
#' @param xlim Optional to specify the limits for the x axis.
#' @param ylim Optional to specify the limits for the y axis.
#' @param xlab Label for the x axis.  Default is \dQuote{Time (h)}
#' @param ylab Label for the y axis.  Default is \dQuote{Observation}
#' @param col A vector of color names to be used for output equation or group coloring.  If the
#' length of \code{col} is too short, values will be recycled.
#' @param col.pred  A vector of color names to be used for prediction (post or pop) coloring.  Default is the same
#' as \code{col}.
#' @param cex Size of the plot symbols.
#' @param legend Either a boolean operator or a list of parameters to be supplied to the \code{\link{legend}}
#' function (see its documentation).  If \code{False} or missing, a legend will not be plotted.
#' If \code{True}, the default legend parameters will be used, as documented in that function, with exceptions
#' as noted in \emph{Details}.
#' @param out Direct output to a PDF, EPS or image file.  Format is a named list whose first argument,
#' \code{type} is one of the following character vectors: \dQuote{pdf}, \dQuote{eps} (maps to \code{postscript}),
#' \dQuote{\code{png}}, \dQuote{\code{tiff}}, \dQuote{\code{jpeg}}, or \dQuote{\code{bmp}}.  Other named items in the list
#' are the arguments to each graphic device. PDF and EPS are vector images acceptable to most journals
#' in a very small file size, with scalable (i.e. infinite) resolution.  The others are raster images which may be very
#' large files at publication quality dots per inch (DPI), e.g. 800 or 1200. Default value is \code{NA} which means the
#' output will go to the current graphic device (usually the monitor). For example, to output an eps file,
#' out=list(\dQuote{eps}) will generate a 7x7 inch (default) graphic.
#' @param \dots Other parameters as found in \code{\link{plot.default}}.
#' @return Plots the object.
#' @author Michael Neely
#' @seealso \code{\link{PMreadMatrix}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{axis}}
#' @export
#' @examples
#' library(PmetricsData)
#' plot(NPex$data$data)
plot.PMmatrix <- function(x, include, exclude, pred = NULL, icen = "median", mult = 1, outeq, group, block = 1,
                          layout = c(3, 3), log = F, pch = NA, errbar = F, doses = F, tad = F,
                          join = T, grid, ident = F, overlay = T, main, xlim, ylim,
                          xlab = "Time (h)", ylab = "Observation", col, col.pred, cex = 1, legend, out = NA, ...) {
  # error bar function
  add.ebar <- function(x, y, upper, lower = upper, length = 0.05, ...) {
    if (length(x) != length(y) | length(y) != length(lower) | length(lower) != length(upper)) {
      stop("vectors must be same length")
    }
    arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
  }
  
  # choose output
  if (inherits(out, "list")) {
    if (out$type == "eps") {
      setEPS()
      out$type <- "postscript"
    }
    if (length(out) > 1) {
      do.call(out$type, args = out[-1])
    } else {
      do.call(out$type, list())
    }
  }
  
  data <- x
  
  # switch to time after dose if requested
  if (tad) {
    for (i in 1:nrow(data)) {
      if (data$evid[i] != 0) {
        doseTime <- data$time[i]
        prevDose <- data$dose[i]
      }
      data$orig.time[i] <- data$time[i]
      data$time[i] <- data$time[i] - doseTime
      data$prevDose[i] <- prevDose
      if (xlab == "Time (h)") xlab <- "Time After Dose (h)"
    }
    data <- data[order(data$id, data$time, -data$evid), ]
  }
  
  if (!missing(include)) {
    data <- subset(data, sub("[[:space:]]+", "", as.character(data$id)) %in% as.character(include))
    if (!is.null(pred)) pred <- pred[sub("[[:space:]]+", "", as.character(pred$id)) %in% as.character(include), ]
  }
  if (!missing(exclude)) {
    data <- subset(data, !sub("[[:space:]]+", "", as.character(data$id)) %in% as.character(exclude))
    if (!is.null(pred)) pred <- pred[!sub("[[:space:]]+", "", as.character(pred$id)) %in% as.character(exclude), ]
  }
  
  if (missing(col)) col <- c("black", "red", "blue", "green", "purple", "orange", "pink", "gray50")
  
  # make error bar object
  obsdata <- data[data$evid == 0, ]
  if (is.logical(errbar)) {
    ebar <- list(
      plot = errbar, id = obsdata$id, sd = obsdata$c0 + obsdata$c1 * obsdata$out + obsdata$c2 * obsdata$out^2 + obsdata$c3 * obsdata$out^3,
      outeq = obsdata$outeq
    )
  } else {
    if (!identical(names(errbar), c("c0", "c1", "c2", "c3"))) stop("\nSee plot.PMmatrix help for structure of errbar argument.\n")
    if (any(sapply(errbar, length) != max(obsdata$outeq, na.rm = TRUE))) stop("\nSee plot.PMmatrix help for structure of errbar argument.\n")
    ebar <- list(
      plot = T, id = obsdata$id, sd = errbar$c0[obsdata$outeq] + errbar$c1[obsdata$outeq] * obsdata$out + errbar$c2[obsdata$outeq] * obsdata$out^2 + errbar$c3[obsdata$outeq] * obsdata$out^3,
      outeq = obsdata$outeq
    )
  }
  
  if (!missing(legend)) {
    if (inherits(legend, "list")) {
      legend$plot <- T
      if (is.null(legend$x)) legend$x <- "topright"
      if (is.null(legend$bg)) legend$bg <- "white"
    } else {
      if (legend) {
        legend <- {
          list(plot = T, x = "topright", bg = "white")
        }
      } else {
        legend <- list(plot = FALSE)
      }
    }
  } else {
    legend <- list(plot = FALSE)
  }
  
  if (length(grep("SIM", data$id)) > 0) data$id <- as.numeric(gsub("[[:alpha:]]", "", data$id))
  
  if (!is.null(pred)) {
    # remove SIM prefix from simulated IDs if necessary
    if (length(grep("SIM", pred$id)) > 0) pred$id <- as.numeric(gsub("[[:alpha:]]", "", pred$id))
    # check to make sure updated
    if ("pred1" %in% names(pred)) {
      stop("\nPlease update your pred object using makePost or makePop and then PMsave.\n")
    }
    # filter to icen
    pred <- pred[pred$icen == icen, ]
    # multiply by mult
    pred$pred <- pred$pred * mult
  }
  
  data$out[data$out == -99] <- NA
  data$out <- data$out * mult
  if (log) {
    logplot <- "y"
    yaxt <- "n"
    if (any(data$out <= 0, na.rm = TRUE)) {
      cat("Observations <= 0 omitted from log plot.\n")
      data$out[data$out <= 0] <- NA
    }
  } else {
    logplot <- ""
    yaxt <- "s"
  }
  if (join) {
    jointype <- "o"
  } else {
    jointype <- "p"
  }
  ndrug <- max(data$input, na.rm = TRUE)
  numeqt <- max(data$outeq, na.rm = TRUE)
  # make sure pch is long enough
  pch <- rep(pch, numeqt)
  
  # make event blocks, delimited by evid=4
  data <- makePMmatrixBlock(data)
  
  # filter data and predictions (if present) by block
  data <- data[data$block == block, ]
  if (!is.null(pred)) {
    pred <- pred[pred$block == block, ]
  }
  
  # set outeq if missing to all
  if (missing(outeq)) outeq <- 1:numeqt
  
  # if length of outeq is only 1
  if (length(outeq) == 1) {
    omit <- which(!is.na(data$outeq) & data$outeq != outeq)
    if (length(omit) > 0) data <- data[-omit, ]
    numeqt <- 1
    # filter predicted if outeq supplied
    if (!is.null(pred)) {
      pred <- pred[pred$outeq == outeq, ]
    }
    # if group supplied, color data by group
    if (!missing(group)) {
      groupindex <- which(names(data) == group)
      if (length(groupindex) == 1) group <- data[, groupindex]
      if (length(groupindex) > 1) stop("Group must be a single factor.\n")
      if (length(groupindex) == 0) stop("Group must be a factor in the data.\n")
      group <- as.factor(group)
      # filter group
      if (length(omit) > 0) group <- group[-omit]
      # make sure there are enough colors; if not, recycle
      if (length(col) < length(levels(group))) col <- rep(col, ceiling(length(levels(group)) / length(col)))
      data$col <- col[as.numeric(group)]
      if (is.null(legend$legend)) legend$legend <- levels(group)
      if (is.null(legend$fill)) legend$fill <- col[1:length(legend$legend)]
      if (is.na(legend$fill[1])) legend$fill <- NULL
      # color predicted if supplied
      if (!is.null(pred)) {
        if (!missing(col.pred)) {
          # make sure there are enough colors; if not, recycle
          if (length(col.pred) < length(levels(group))) col.pred <- rep(col.pred, ceiling(length(levels(group)) / length(col.pred)))
          pred$col <- col.pred[as.numeric(group)]
        } else {
          pred$col <- data$col[match(as.character(pred$id), data$id[data$outeq == outeq])]
        }
      }
    } else {
      # no group, so color the single output equqtion
      data$col <- ifelse(!is.na(data$outeq), col[data$outeq], NA)
      if (is.null(legend$legend)) legend$legend <- paste("Output", outeq)
      if (is.null(legend$fill)) legend$fill <- col[outeq]
      if (is.na(legend$fill[1])) legend$fill <- NULL
      
      # color predicted if supplied
      if (!is.null(pred)) {
        if (!missing(col.pred)) {
          # make sure there are enough colors; if not, recycle
          if (length(col.pred) < outeq) col.pred <- rep(col.pred, ceiling(outeq / length(col.pred)))
          pred$col <- col.pred[outeq]
        } else {
          pred$col <- col[outeq]
        }
      }
    }
  } else {
    # outeq length>1, so groups won't apply and color by outeq
    numeqt <- length(outeq)
    if (!missing(group)) warning("Group factor ignored with multiple outputs.\n")
    if (length(col) < numeqt) col <- rep(col, ceiling(numeqt / length(col)))
    data$col <- ifelse(!is.na(data$outeq), col[data$outeq], NA)
    if (is.null(legend$legend)) legend$legend <- paste("Output", outeq)
    if (is.null(legend$fill)) legend$fill <- col[outeq]
    if (is.na(legend$fill[1])) legend$fill <- NULL
    
    # color predicted if supplied
    if (!is.null(pred)) {
      if (!missing(col.pred)) {
        # make sure there are enough colors; if not, recycle
        if (length(col.pred) < numeqt) col.pred <- rep(col.pred, ceiling(numeqt / length(col.pred)))
        for (i in outeq) {
          pred$col[pred$outeq == i] <- col.pred[i]
        }
      } else {
        for (i in outeq) {
          pred$col[pred$outeq == i] <- col[i]
        }
      }
    }
  }
  
  if (missing(xlim)) {
    xlim.flag <- T
  } else {
    xlim.flag <- F
  }
  if (missing(ylim)) {
    ylim.flag <- T
  } else {
    ylim.flag <- F
  }
  if (missing(main)) {
    main.flag <- T
  } else {
    main.flag <- F
  }
  
  # PLOTS -------------------------------------------------------------------
  
  # don't overlay
  if (!overlay) {
    par(mfrow = layout)
    devAskNewPage(ask = TRUE)
    
    # predicted is supplied
    if (!is.null(pred)) {
      for (i in unique(data$id)) {
        if (xlim.flag) {
          xlim <- base::range(c(data$time[data$id == i], pred$time[pred$id == i]), na.rm = TRUE)
          if (abs(xlim[1]) == Inf | abs(xlim[2]) == Inf) xlim <- base::range(data$time, na.rm = TRUE)
        }
        if (ylim.flag) {
          ylim <- base::range(c(data$out[data$id == i], pred$pred[pred$id == i]), na.rm = TRUE)
          if (abs(ylim[1]) == Inf | abs(ylim[2]) == Inf) ylim <- base::range(data$out, na.rm = TRUE)
          if (log) {
            ylim[1][ylim[1] == 0] <- 0.5 * min(data$out[data$id == i], na.rm = TRUE)
          }
        }
        if (main.flag) main <- paste("ID", i)
        plot(out ~ time, data = subset(data, data$id == i), xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = ylim, log = logplot, type = "n", yaxt = yaxt, ...)
        if (missing(grid)) {
          grid <- list(x = NA, y = NA)
        } else {
          if (inherits(grid, "logical")) {
            if (grid) {
              grid <- list(x = axTicks(1), y = axTicks(2))
            } else {
              grid <- list(x = NA, y = NA)
            }
          }
          if (inherits(grid, "list")) {
            if (is.null(grid$x) | all(!is.na(grid$x))) grid$x <- axTicks(1)
            if (is.null(grid$y) | all(!is.na(grid$y))) grid$y <- axTicks(2)
          }
        }
        if (yaxt == "n") logAxis(2, grid = !all(is.na(grid$y)))
        abline(v = grid$x, lty = 1, col = "lightgray")
        abline(h = grid$y, lty = 1, col = "lightgray")
        for (j in outeq) {
          tempdata <- subset(data, data$id == i & data$outeq == j)
          temppred <- pred[pred$id == i & pred$outeq == j, ]
          points(out ~ time, data = tempdata, type = jointype, pch = pch[j], col = tempdata$col, cex = cex, ...)
          lines(pred ~ time, data = temppred, col = temppred$col, ...)
          if (ebar$plot) {
            add.ebar(
              x = tempdata$time[data$evid == 0],
              y = tempdata$out[data$evid == 0],
              upper = ebar$sd[ebar$id == i & ebar$outeq == j], col = subset(data$col, data$id == i & data$outeq == j)
            )
          }
        }
        if (doses) {
          for (k in 1:ndrug) {
            x <- data$time[data$id == i & data$input == k & !is.na(data$input)]
            dur <- data$dur[data$id == i & data$input == k & !is.na(data$input)]
            dur[dur < 0.051 * diff(xlim)] <- 0.05 * diff(xlim)
            ndose <- length(x)
            for (l in 1:ndose) {
              xmin <- x[l]
              xmax <- x[l] + dur[l]
              ymin <- ylim[1]
              ymax <- c(ylim[1] + 0.1 * (ylim[2] - ylim[1]), 10**(log10(ylim[1]) + 0.1 * log10(ylim[2] / ylim[1])))[1 + as.numeric(log)]
              abline(v = x[l], lty = "dotted", lwd = 1, col = "black")
              polygon(x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin), col = col[k], border = NA)
            }
          }
        }
        if (legend$plot) do.call("legend", legend)
      }
    } else {
      # case where there is no predicted
      for (i in unique(data$id)) {
        if (xlim.flag) {
          xlim <- base::range(data$time[data$id == i], na.rm = TRUE)
          if (abs(xlim[1]) == Inf | abs(xlim[2]) == Inf) xlim <- base::range(data$time, na.rm = TRUE)
        }
        if (ylim.flag) {
          ylim <- base::range(data$out[data$id == i], na.rm = TRUE)
          if (abs(ylim[1]) == Inf | abs(ylim[2]) == Inf) ylim <- base::range(data$out, na.rm = TRUE)
          if (log) {
            ylim[1][ylim[1] == 0] <- 0.5 * min(data$out[data$id == i], na.rm = TRUE)
          }
        }
        if (main.flag) main <- paste("ID", i)
        plot(out ~ time, data = subset(data, data$id == i), xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = ylim, log = logplot, type = "n", yaxt = yaxt, ...)
        if (missing(grid)) {
          grid <- list(x = NA, y = NA)
        } else {
          if (inherits(grid, "logical")) {
            if (grid) {
              grid <- list(x = axTicks(1), y = axTicks(2))
            } else {
              grid <- list(x = NA, y = NA)
            }
          }
          if (inherits(grid, "list")) {
            if (is.null(grid$x) | all(!is.na(grid$x))) grid$x <- axTicks(1)
            if (is.null(grid$y) | all(!is.na(grid$y))) grid$y <- axTicks(2)
          }
        }
        
        if (yaxt == "n") logAxis(2, grid = !all(is.na(grid$y)))
        abline(v = grid$x, lty = 1, col = "lightgray")
        abline(h = grid$y, lty = 1, col = "lightgray")
        for (j in outeq) {
          tempdata <- subset(data, data$id == i & data$outeq == j)
          temppred <- pred[pred$id == i & pred$outeq == j, ]
          points(out ~ time, data = tempdata, type = jointype, pch = pch[j], col = tempdata$col, cex = cex, ...)
          if (ebar$plot) {
            add.ebar(
              x = data$time[data$id == i & data$evid == 0 & data$outeq == j],
              y = data$out[data$id == i & data$evid == 0 & data$outeq == j],
              upper = ebar$sd[ebar$id == i & ebar$outeq == j], col = subset(data$col, data$id == i & data$outeq == j)
            )
          }
        }
        if (doses) {
          for (k in 1:ndrug) {
            x <- data$time[data$id == i & data$input == k & !is.na(data$input)]
            dur <- data$dur[data$id == i & data$input == k & !is.na(data$input)]
            dur[dur < 0.01 * xlim[2]] <- 0.01 * xlim[2]
            ndose <- length(x)
            for (l in 1:ndose) {
              xmin <- x[l]
              xmax <- x[l] + dur[l]
              ymin <- ylim[1]
              ymax <- c(ylim[1] + 0.1 * (ylim[2] - ylim[1]), 10**(log10(ylim[1]) + 0.1 * log10(ylim[2] / ylim[1])))[1 + as.numeric(log)]
              abline(v = x[l], lty = "dotted", lwd = 1, col = "black")
              polygon(x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin), col = col[k], border = NA)
            }
          }
        }
        if (legend$plot) do.call("legend", legend)
      }
    }
    devAskNewPage(ask = FALSE)
  } else { # there is overlay
    # predicted suppplied
    if (!is.null(pred)) {
      if (xlim.flag) xlim <- base::range(c(data$time, pred$time), na.rm = TRUE)
      if (ylim.flag) {
        ylim <- base::range(c(data$out, pred$pred), na.rm = TRUE)
        if (log) {
          ylim[1][ylim[1] == 0] <- 0.5 * min(data$out, na.rm = TRUE)
        }
      }
      
      if (main.flag) {
        main <- ""
      }
      plot(out ~ time, data = data, xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = ylim, log = logplot, type = "n", yaxt = yaxt, ...)
      if (missing(grid)) {
        grid <- list(x = NA, y = NA)
      } else {
        if (inherits(grid, "logical")) {
          if (grid) {
            grid <- list(x = axTicks(1), y = axTicks(2))
          } else {
            grid <- list(x = NA, y = NA)
          }
        }
        if (inherits(grid, "list")) {
          if (is.null(grid$x)) grid$x <- axTicks(1)
          if (is.null(grid$y)) grid$y <- axTicks(2)
        }
      }
      if (yaxt == "n") logAxis(2, grid = !all(is.na(grid$y)))
      abline(v = grid$x, lty = 1, col = "lightgray")
      abline(h = grid$y, lty = 1, col = "lightgray")
      for (i in unique(data$id)) {
        for (j in outeq) {
          tempdata <- subset(data, data$id == i & data$outeq == j)
          temppred <- pred[pred$id == i & pred$outeq == j, ]
          if (!ident) {
            points(out ~ time, data = tempdata, type = jointype, pch = pch[j], col = tempdata$col, cex = cex, ...)
          } else {
            lines(out ~ time, data = tempdata, col = col, ...)
            text(out ~ time, data = tempdata, labels = tempdata$id, col = col, cex = cex, ...)
          }
          lines(pred ~ time, data = temppred, col = temppred$col, ...)
          if (ebar$plot) {
            add.ebar(
              x = data$time[data$id == i & data$evid == 0 & data$outeq == j],
              y = data$out[data$id == i & data$evid == 0 & data$outeq == j],
              upper = ebar$sd[ebar$id == i & ebar$outeq == j], col = subset(data$col, data$id == i & data$outeq == j)
            )
          }
        }
      }
      if (legend$plot) do.call("legend", legend)
    } else { # there is no predicted
      if (xlim.flag) xlim <- base::range(data$time, na.rm = TRUE)
      if (ylim.flag) {
        ylim <- base::range(data$out, na.rm = TRUE)
        if (log) {
          ylim[1][ylim[1] == 0] <- 0.5 * min(data$out, na.rm = TRUE)
        }
      }
      if (main.flag) {
        main <- ""
      }
      plot(out ~ time, data = data, xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = ylim, log = logplot, type = "n", yaxt = yaxt, ...)
      if (missing(grid)) {
        grid <- list(x = NA, y = NA)
      } else {
        if (inherits(grid, "logical")) {
          if (grid) {
            grid <- list(x = axTicks(1), y = axTicks(2))
          } else {
            grid <- list(x = NA, y = NA)
          }
        }
        if (inherits(grid, "list")) {
          if (is.null(grid$x)) grid$x <- axTicks(1)
          if (is.null(grid$y)) grid$y <- axTicks(2)
        }
      }
      if (yaxt == "n") logAxis(2, grid = !all(is.na(grid$y)))
      abline(v = grid$x, lty = 1, col = "lightgray")
      abline(h = grid$y, lty = 1, col = "lightgray")
      for (i in unique(data$id)) {
        for (j in outeq) {
          tempdata <- subset(data, data$id == i & data$outeq == j)
          if (!ident) {
            points(out ~ time, data = tempdata, type = jointype, pch = pch[j], col = tempdata$col, cex = cex, ...)
          } else {
            lines(out ~ time, data = tempdata, col = col, ...)
            text(out ~ time, data = tempdata, labels = tempdata$id, col = col, cex = cex, ...)
          }
          if (ebar$plot) {
            add.ebar(
              x = data$time[data$id == i & data$evid == 0 & data$outeq == j],
              y = data$out[data$id == i & data$evid == 0 & data$outeq == j],
              upper = ebar$sd[ebar$id == i & ebar$outeq == j], col = subset(data$col, data$id == i & data$outeq == j)
            )
          }
        }
      }
      if (legend$plot) do.call("legend", legend)
    }
  }
  
  # clean up
  par(mfrow = c(1, 1))
  # close device if necessary
  if (inherits(out, "list")) dev.off()
}
