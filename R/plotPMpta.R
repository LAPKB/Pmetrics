#' Plots PM_pta objects
#'
#' This function will plot the percent target attainment for objects made with the [makePTA] function.
#' 
#' @title Plot PM_pta Percent Target Attainment objects
#' @method plot PM_pta
#' @param x The name of an *PM_pta* data object read by [makePTA]
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param type Character vector controlling type of plot.
#' Default is "pta", which plots proportion with success on the y-axis and target on the x-axis.
#' The other choice is "pdi", which plots the median pdi (pharmacodynamic index), e.g. AUC/MIC, on the
#' y-axis, and target on the x-axis.
#' @param mult `r template("mult")`
#' @param log `r template("log")`
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
#' @param legend `r template("legend")` Default will be the labeled regimen names supplied during [makePTA],
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
#' @param `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [makePTA]
#' @importFrom plotly plotly_build
#' @export
#' @family PMplots

plot.PM_pta <- function(x,
                        include, exclude,
                        type = "pta",
                        mult = 1, 
                        outeq = 1,
                        line = T, 
                        marker = T,
                        ci = 0.9,
                        legend = T, 
                        log = F, 
                        grid = T,
                        xlab, ylab,
                        title,
                        xlim, ylim,...) {
  
  
  #clone to avoid changes
  pta <- x$clone()
  
  #vector of regimens
  simnum <- 1:max(pta$outcome$simnum)
  
  #names of regimens
  simLabels <- attr(x, "simlabels")
  if (is.null(simLabels)) simLabels <- paste("Regimen", simnum)
  
  # check input
  if (!missing(include)) {
    if (any(include > max(simnum))) {
      stop(paste("PMpta object does not have ", max(simnum), " simulations.\n", sep = ""))
    } else {
      simnum <- simnum[include]
      simLabels <- simLabels[include]
    }
  }
  if (!missing(exclude)) {
    if (any(exclude > max(simnum))) {
      stop(paste("PMpta object does not have ", max(simnum), " simulations.\n", sep = ""))
    } else {
      simnum <- simnum[-exclude]
      simLabels <- simLabels[-exclude]
    }
  }
  
  #filter by include/exclude
  pta$outcome <- pta$outcome %>% filter(simnum %in% !!simnum)
  pta$results <- pta$results %>% filter(simnum %in% !!simnum)
  
  nsim <- length(simnum)
  
  
  
  #parse line
  line <- amendLine(line, default = list(color = "Set1", 
                                         width = 2,
                                         dash = 1:nsim))
  
  #parse marker
  marker <- amendMarker(marker, default = list(color = line$color, 
                                               size = 12,
                                               symbol = 1:nsim))
  
  
  #simulated or discrete targets
  simTarg <- 1 + as.numeric(attr(x, "simTarg")) # 1 if missing or set, 2 if random
  if (length(simTarg) == 0) simTarg <- 1
  
  #process dots
  layout <- amendDots(list(...))
  
  
  #legend
  legendList <- amendLegend(legend, default = list(xanchor = "right", title = list(text = "<b>Regimen</b>")))
  layout <- modifyList(layout, list(showlegend = legendList$showlegend))
  if(length(legendList)>1){layout <- modifyList(layout, list(legend = within(legendList,rm(showlegend))))}
  
  #grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)
  
  #axis labels if needed
  xtitle <- pluck(layout$xaxis, "title")
  ytitle <- pluck(layout$yaxis, "title")
  if(is.null(xtitle)){
    if (missing(xlab)) {
      # choose xlab as Target if targets were set or Regimen if targets were simulated
      
      xlab <- switch(simTarg,
                     "Target",
                     "Regimen"
      )
    }
    layout$xaxis$title <- amendTitle(xlab)
  }
  if(is.null(ytitle)){
    if (missing(ylab)) {
      ylab <- switch(type,
                     pdi = "Pharmacodynamic Index",
                     pta = "Proportion with success",
                     "Proportion with success"
      )
    }
    if(is.character(ylab)){
      layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
    } else {
      layout$yaxis$title <- amendTitle(ylab)
    }
  }  
  
  #axis ranges
  if(!missing(xlim)){layout$xaxis <- modifyList(layout$xaxis, list(range = xlim)) }
  if(!missing(ylim)){layout$yaxis <- modifyList(layout$yaxis, list(range = ylim)) }
  
  #log y axis
  if(log){
    layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
  }
  
  #title
  if(missing(title)){ title <- ""}
  layout$title <- amendTitle(title, default = list(size = 20))
  
  
  #PLOTS
  if (type == "pdi") { # pdi plot
    
    if (simTarg == 1) { # set targets
      p <- pta$results %>% 
        nest_by(simnum,target) %>% 
        mutate(lower = quantile(data$pdi, probs = 0.5 - ci / 2, na.rm = T),
               median = median(data$pdi),
               upper = quantile(data$pdi, probs = 0.5 + ci / 2, na.rm = T)) %>%
        ungroup() %>% 
        mutate(simnum = factor(simnum, labels = simLabels)) %>% 
        group_by(simnum) %>%
        plotly::plot_ly(x = ~target, y = ~median, 
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
                        line = list(width = line$width)) %>%
        plotly::add_ribbons(ymin = ~lower, ymax = ~upper,
                            opacity = 0.5, line = list(width = 0), 
                            marker = list(size = .01), showlegend = F)
      
      layout$xaxis <- modifyList(layout$xaxis, list(tickvals = ~target, type = "log"))
      p <- p %>% plotly::layout(xaxis = layout$xaxis,
                                yaxis = layout$yaxis,
                                showlegend = layout$showlegend,
                                legend = layout$legend,
                                title = layout$title) 
      
    } else { # random targets
      
      p <- pta$results %>% nest_by(simnum) %>% 
        mutate(lower = quantile(data$pdi, probs = 0.5 - ci / 2, na.rm = T),
               median = median(data$pdi),
               upper = quantile(data$pdi, probs = 0.5 + ci / 2, na.rm = T)) %>%
        plotly::plot_ly(x = ~simnum, y = ~median) %>%
        add_markers(error_y = list(symmetric = F,
                                   array = ~(upper - median),
                                   arrayminus = ~(median - lower),
                                   color = line$color),
                    marker = list(color = marker$color, size = marker$size, symbol = marker$symbol))
      
      layout$xaxis <- modifyList(layout$xaxis, list(tickvals = ~simnum, ticktext = ~simLabels))
      p <- p %>% plotly::layout(xaxis = layout$xaxis,
                                yaxis = layout$yaxis,
                                showlegend = F,
                                legend = layout$legend,
                                title = layout$title) 
    }
  } else { # pta plot
    
    if (simTarg == 1) { # set targets
      p <- pta$outcome %>% mutate(simnum = factor(simnum, labels=simLabels)) %>% group_by(simnum) %>%
        plotly::plot_ly(x = ~target, y = ~prop.success, 
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
                        line = list(width = line$width))
      
      layout$xaxis <- modifyList(layout$xaxis, list(tickvals = ~target, type = "log"))
      p <- p %>% plotly::layout(xaxis = layout$xaxis,
                                yaxis = layout$yaxis,
                                showlegend = layout$showlegend,
                                legend = layout$legend,
                                title = layout$title) 
      
      
    } else { # random targets
      
      p <- pta$outcome %>%
        plotly::plot_ly(x = ~simnum, y = ~prop.success,
                        type = "scatter", mode = "lines+markers",
                        line = list(color = line$color, width = line$width, dash = line$dash),
                        marker = list(color = marker$color, symbol = marker$symbol, size = marker$size)) 
      layout$xaxis <- modifyList(layout$xaxis, list(tickvals = ~simnum, ticktext = ~simLabels))
      p <- p %>% plotly::layout(xaxis = layout$xaxis,
                                yaxis = layout$yaxis,
                                showlegend = F,
                                legend = layout$legend,
                                title = layout$title) 
      
    }
  }
  p <- suppressMessages(plotly::plotly_build(p))
  print(p)
  return(p)
  
}

#' Plots PMpta objects
#' 
#' This function will plot the percent target attainment for objects made with the \code{\link{makePTA}} function.
#' For the legend, defaults that are different that the standard are:
#' \itemize{
#'   \item x Default \dQuote{topright}
#'   \item legend Default will be the labeled regimen names supplied during \code{\link{makePTA}},
#'   or if missing, \dQuote{Regimen 1, Regimen 2,...Regimen n}, where \emph{n} is the number of
#'   regimens in the PMpta object.
#'   This default can be overridden by a supplied character vector of regimen names.
#'   \item col The color of each Regimen plot as specified by the default color scheme or \code{col}
#'   \item pch The plotting character for each Regimen plot as specified by the default plotting characters or \code{pch}
#'   \item lty The line type of each Regimen plot as specified by the default line types or \code{lty}
#'   \item bg Default \dQuote{white}
#' }
#' 
#' @title Plot PMpta Percent Target Attainment objects
#' @method plot PMpta
#' @param x The name of an \emph{PMpta} data object read by \code{\link{makePTA}}
#' @param include A vector of simulations (regimens) to include in the plot, e.g. c(1,3)
#' @param exclude A vector of simulations (regimens) in the plot, e.g. c(2,4:6)
#' @param plot.type Character vector controlling type of plot.
#' Default is \dQuote{pta}, which plots proportion with success on the y-axis and target on the x-axis.
#' The other choice is \dQuote{pdi}, which plots the median pdi (pharmacodynamic index), e.g. AUC/MIC, on the
#' y-axis, and target on the x-axis.
#' @param log Boolean operator to plot x-axis in logarithmic scale; the default is \code{True}
#' @param pch Vector of integers which control the plotting symbol for each regimen curve; the default is 1:nsim.  NA results in no symbol.
#' Use 0 for open square, 1 for open circle, 2 for open triangle, 3 for cross, 4 for X, or 5 for a diamond.
#' Other alternatives are \dQuote{*} for asterisks, \dQuote{.} for tiny dots, or \dQuote{+} for a smaller,
#' bolder cross.  These plotting symbols are standard for R (see \code{\link{par}}).
#' @param grid Either a boolean operator to plot a reference grid, or a list with elements x and y,
#' each of which is a vector specifying the native coordinates to plot grid lines; the default is \code{False}.
#' For example, grid=list(x=seq(0,24,2),y=1:10).  Defaults for missing x or y will be calculated by \code{\link{axTicks}}.
#' @param xlab Label for the x axis.  Default is \dQuote{MIC}
#' @param ylab Label for the y axis.  Default is \dQuote{Proportion with success}
#' @param col A vector of color names to be used for each regimen plotted.  If the
#' length of \code{col} is too short, values will be recycled.
#' @param lty A vector of line types to be used for each regimen plotted.  If the
#' length of \code{lty} is too short, values will be recycled.
#' @param lwd Line width, with default of 4.
#' @param legend Either a boolean operator or a list of parameters to be supplied to the \code{\link{legend}}
#' function (see its documentation).  If \code{False}, a legend will not be plotted.
#' If \code{True} (the default), the default legend parameters will be used, as documented in that function, with exceptions
#' as noted in \emph{Details}.
#' @param ci Confidence interval around curves on \code{pdi} plot, on scale of 0 to 1. Default is 0.9.
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
#' @seealso \code{\link{makePTA}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{axis}}
#' @export


plot.PMpta <- function(x, include, exclude, plot.type = "pta", log = T, pch,
                       grid, xlab, ylab, col, lty, lwd = 4,
                       legend = T, ci = 0.9, out = NA, ...) {

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

  if (!(inherits(x, "PMpta") || inherits(x, "PM_pta"))) stop("Please supply a PMpta object made by makePTA(), PM_pta$new or PM_sim$pta().\n")

  # check input
  simnum <- 1:max(x$outcome$simnum)
  if (!missing(include)) {
    if (any(include > max(simnum))) {
      stop(paste("PMpta object does not have ", max(simnum), " simulations.\n", sep = ""))
    } else {
      simnum <- simnum[include]
    }
  }
  if (!missing(exclude)) {
    if (any(exclude > max(simnum))) {
      stop(paste("PMpta object does not have ", max(simnum), " simulations.\n", sep = ""))
    } else {
      simnum <- simnum[-exclude]
    }
  }

  # choose xlab as Target if targets were set or Regimen if targets were simulated
  simTarg <- 1 + as.numeric(attr(x, "simTarg")) # 1 if missing or set, 2 if random
  if (length(simTarg) == 0) simTarg <- 1
  if (missing(xlab)) {
    xlab <- switch(simTarg,
                   "Target",
                   "Regimen"
    )
  }
  if (missing(ylab)) {
    ylab <- switch(plot.type,
                   pdi = "Pharmacodynamic Index",
                   pta = "Proportion with success",
                   "Proportion with success"
    )
  }
  if (simTarg == 1) {
    logscale <- c("", "x")[1 + as.numeric(log)]
  } else {
    logscale <- ""
  }
  nsim <- length(simnum)
  if (missing(pch)) {
    pch <- 1:nsim
  } else {
    pch <- rep(pch, nsim)
  }
  if (missing(col)) {
    col <- rep(c("black", "red", "blue", "green", "purple", "orange"), nsim)
  } else {
    col <- rep(col, nsim)
  }
  if (missing(lty)) {
    lty <- 1:nsim
  } else {
    lty <- rep(lty, nsim)
  }
  if (class(legend) == "list") {
    legend$plot <- T
    if (is.null(legend$x)) legend$x <- "topright"
    if (is.null(legend$bg)) legend$bg <- "white"
    if (is.null(legend$col)) legend$col <- col
    if (is.null(legend$pch)) legend$pch <- pch
    if (is.null(legend$lty)) legend$lty <- lty
    if (is.null(legend$legend)) {
      legendText <- attr(x, "simlabels")
      if (is.null(legendText)) legendText <- paste("Regimen", simnum)
      legend$legend <- legendText
    }
  } else {
    if (legend) {
      legendText <- attr(x, "simlabels")
      if (is.null(legendText)) legendText <- paste("Regimen", simnum)
      legend <- list(plot = T, x = "topright", bg = "white", col = col, lty = lty, pch = pch, legend = legendText)
    } else {
      legend <- list(plot = F)
    }
  }

  if (plot.type == "pdi") { # pdi plot


    if (simTarg == 1) { # set targets
      pdi.median <- tapply(x$results$pdi, list(x$results$target, x$results$simnum), median, na.rm = T)
      pdi.lower <- tapply(x$results$pdi, list(x$results$target, x$results$simnum), quantile, probs = 0.5 - ci / 2, na.rm = T)
      pdi.upper <- tapply(x$results$pdi, list(x$results$target, x$results$simnum), quantile, probs = 0.5 + ci / 2, na.rm = T)
      targets <- as.numeric(row.names(pdi.median))
      plot(x = base::range(targets), y = base::range(c(pdi.lower[, simnum], pdi.upper[, simnum]), na.rm = T), type = "n", xlab = xlab, ylab = ylab, log = logscale, xaxt = "n", ...)
      axis(side = 1, at = targets, labels = targets, lwd = 1, ...)
    } else { # random targets
      pdi.median <- tapply(x$results$pdi, x$results$simnum, median, na.rm = T)
      pdi.lower <- tapply(x$results$pdi, x$results$simnum, quantile, probs = 0.5 - ci / 2, na.rm = T)
      pdi.upper <- tapply(x$results$pdi, x$results$simnum, quantile, probs = 0.5 + ci / 2, na.rm = T)
      plot(x = base::range(1:nsim), y = base::range(c(pdi.lower[simnum], pdi.upper[simnum]), na.rm = T), type = "n", xlab = xlab, ylab = ylab, log = logscale, xaxt = "n", ...)
      axisLabels <- attr(x, "simlabels")
      if (is.null(axisLabels)) axisLabels <- paste("Regimen", simnum)
      axis(side = 1, at = 1:nsim, labels = axisLabels, lwd = 1, ...)
    }

    # make grid if necessary
    if (missing(grid)) {
      grid <- list(x = NA, y = NA)
    } else {
      if (inherits(grid, "logical")) {
        if (grid) {
          grid <- list(x = targets, y = axTicks(2))
        } else {
          grid <- list(x = NA, y = NA)
        }
      }
      if (inherits(grid, "list")) {
        if (is.null(grid$x)) grid$x <- targets
        if (is.null(grid$y)) grid$y <- axTicks(2)
      }
    }
    abline(v = grid$x, lty = 1, col = "lightgray")
    abline(h = grid$y, lty = 1, col = "lightgray")

    if (simTarg == 1) { # set targets
      if (ci > 0) {
        for (i in simnum) {
          if (ci > 0) {
            polygon(
              x = c(targets, rev(targets)),
              y = c(pdi.upper[, i], rev(pdi.lower[, i])),
              col = rgb(
                red = col2rgb(col[i])[1, 1],
                green = col2rgb(col[i])[2, 1],
                blue = col2rgb(col[i])[3, 1],
                alpha = 50, maxColorValue = 255
              ), border = NA
            )
          }
        }
      }
      for (i in 1:nsim) {
        lines(x = targets, y = pdi.median[, simnum[i]], type = "o", lty = lty[i], lwd = lwd, col = col[i], pch = pch[i], ...)
      }
      if (legend$plot) do.call("legend", legend)
    } else { # random targets
      for (i in 1:nsim) {
        points(x = i, y = pdi.median[simnum[i]])
        arrows(
          x0 = i, y0 = pdi.lower[simnum[i]],
          x1 = i, y1 = pdi.upper[simnum[i]], angle = 90, code = 3,
          lwd = 1
        ) # draw error bars
      }
    }
  } else { # pta plot

    temp <- x$outcome[x$outcome$simnum %in% simnum, ]
    if (simTarg == 1) { # set targets
      plot(prop.success ~ target, temp, type = "n", xlab = xlab, ylab = ylab, log = logscale, xaxt = "n", ...)
      axis(side = 1, at = unique(temp$target), lwd = 1, ...)
      # make grid if necessary
      if (missing(grid)) {
        grid <- list(x = NA, y = NA)
      } else {
        if (inherits(grid, "logical")) {
          if (grid) {
            grid <- list(x = unique(temp$target), y = axTicks(2))
          } else {
            grid <- list(x = NA, y = NA)
          }
        }
        if (inherits(grid, "list")) {
          if (is.null(grid$x)) grid$x <- unique(temp$target)
          if (is.null(grid$y)) grid$y <- axTicks(2)
        }
      }
      abline(v = grid$x, lty = 1, col = "lightgray")
      abline(h = grid$y, lty = 1, col = "lightgray")

      # draw plot
      for (i in 1:nsim) {
        lines(prop.success ~ target, temp[temp$simnum == simnum[i], ], type = "o", lty = lty[i], lwd = lwd, col = col[i], pch = pch[i], ...)
      }
      # legend
      if (legend$plot) do.call("legend", legend)
    } else { # random targets
      plot(prop.success ~ simnum, temp, type = "n", xlab = xlab, ylab = ylab, log = logscale, xaxt = "n", ...)
      axisLabels <- attr(x, "simlabels")
      if (is.null(axisLabels)) axisLabels <- paste("Regimen", simnum)
      axis(side = 1, at = 1:nsim, labels = axisLabels, lwd = 1, ...)
      # make grid if necessary
      if (missing(grid)) {
        grid <- list(x = NA, y = NA)
      } else {
        if (inherits(grid, "logical")) {
          if (grid) {
            grid <- list(x = simnum, y = axTicks(2))
          } else {
            grid <- list(x = NA, y = NA)
          }
        }
        if (inherits(grid, "list")) {
          if (is.null(grid$x)) grid$x <- simnum
          if (is.null(grid$y)) grid$y <- axTicks(2)
        }
      }
      abline(v = grid$x, lty = 1, col = "lightgray")
      abline(h = grid$y, lty = 1, col = "lightgray")

      # draw plot
      lines(prop.success ~ simnum, temp, type = "o", ...)
    }
  }


  # close device if necessary
  if (inherits(out, "list")) dev.off()
}
