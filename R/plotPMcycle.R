#' @title Plot Cycle Information
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plot PM_cycle objects. These objects are created by [makeCycle] as part of a [PM_result] object
#' when [PM_load] is run.
#'
#' @method plot PM_cycle
#' @param x The name of a [PM_cycle]  object, e.g. `NPex$cycle`.
#' @param line `r template("line")` Default = `list(color = "dodgerblue", width = 2, dash = "solid")`.
#' **Note** The width will apply to all plots, but `color` and `dash` will only apply
#' to the first three plots (log-likelihood, AIC, gamma/lambda). Use `colors` and `linetypes`
#' below to control the appearance
#' of the line traces for the normalized plots, because each of those traces is mapped to
#' a parameter.
#' @param marker `r template("marker")` Here, the observation controlled is the value of a given
#' trace at a specific cycle number. Default = `list(symbol = "circle", color = "dodgerblue", size = 4)`.
#' **Note** the marker color for the normalized parameter value plots will be controlled by the `colors`
#' parameter below, but size and symbol will apply to all plots.
#' @param colors to use for normalized parameter value line traces. This can be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors parameters to be plotted, as colors
#' will be interpolated to generate the correct number. The default when `color`
#' is not specified is the "Set2" palette.
#' @param linetypes to use for normalized parameter value line traces.
#' See `plotly::schema()`, traces > scatter >
#' attributes > line > dash > values.
#' An example vector could be `c("solid", "dash", "longdash")`. It is not
#' necessary to specify the same number of linetype parameters to be plotted, as they
#' will be recycled to generate the correct number. The default when `linetypes`
#' is not specified is "solid".
#' @param omit Decimal between 0 and 1 specifying the proportion of "burn-in" cycles to omit from the plots.  If missing,
#' the first 20% will be omitted.
#' @param grid `r template("grid")`
#' @param xlab Controls the formatting of the x-axis label. The text is fixed by the function and cannot be altered.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to layout > layoutAttributes > xaxis > title to see the ways to customize
#' this axis label.
#' In addition to the plotly attributes, a custom Pmetrics attribute `bold`
#' may be included as a list element, either on its own or within the font
#' list. The default for `bold` is `TRUE`.<br>
#' <br>
#' Examples:
#' \itemize{
#' \item{`xlab = list(bold = F, font = list(color = "red", family = "Arial", size = 10))`}
#' \item{`xlab = list(font = list(bold = T))`}
#' }
#' @param ylab Format for y-axis label.
#' This argument maps to the the yaxis title element of the layout object in plotly.
#' See `xlab` for details. If `xlab` is specified as a list with formatting,
#' then the formatting for the
#' `xlab` will be applied to the `ylab`. To format `ylab` independently,
#' specify a formatting list as for `xlab`.<br>
#' @param \dots Additional R plotting parameters.
#' @return Plots a panel with the following windows: -2 times the log-likelihood at each cycle, gamma/lambda at
#' each cycle; Akaike Information Criterion at each cyle and Bayesian (Schwartz) Information Criterion
#' at each cycle, the mean parameter values at each cycle (normalized to starting values); the normalized
#' standard deviation of the population distribution for each parameter at each cycle; and
#' the normalized median parameter values at each cycle.
#' @author Michael Neely
#' @seealso [makeCycle], [PM_result], [schema]
#' @export
#' @examples
#' NPex$cycle$plot()
#' NPex$cycle$plot(omit = 0, marker = list(symbol = "cross"), line = list(width = 1))
#' NPex$cycle$plot(
#'   linetypes = "dash", colors = "Blues", marker = list(size = 1),
#'   line = list(width = 3)
#' )
#' NPex$cycle$plot(
#'   grid = F,
#'   xlab = list(bold = F, font = list(color = "red", family = "Arial", size = 10))
#' )
#' @family PMplots


plot.PM_cycle <- function(x, 
                          line = TRUE,
                          marker = TRUE,
                          colors,
                          linetypes,
                          omit, 
                          grid = TRUE,
                          xlab, ylab,
                          ...) {
  if (inherits(x, "PMcycle")) {
    data <- x
  } else if (inherits(x, "PM_cycle")) {
    data <- x$data
  } else {
    stop("Please supply a PM_cycle or PMcycle object to plot.\n")
  }
  
  
  # housekeeping
  
  nvar <- ncol(data$mean %>% select(-cycle))
  
  line <- amendLine(line, default = list(color = "dodgerblue", width = 2))
  marker <- amendMarker(marker, default = list(
    symbol = "circle",
    color = "dodgerblue",
    size = 4, line = list(width = 0)
  ))
  if (missing(colors)) {
    colors <- "Set2"
  } else {
    colors <- rep(colors, nvar) #ensure long enough
  }
  
  if (missing(linetypes)) {
    linetypes <- rep("solid", nvar)
  } else {
    linetypes <- rep(linetypes, nvar) # ensure long enough
  }
  
  # process dots
  layout <- amendDots(list(...))
  
  # legend - not needed for this function
  layout <- modifyList(layout, list(showlegend = F))
  
  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)
  
  # axis label formatting if needed
  xlab <- if (missing(xlab)) {
    "Cycle Number"
  } else {
    modifyList(xlab, list(text = "Cycle Number"))
  }
  ylab <- if (missing(ylab)) {
    ""
  } else {
    modifyList(ylab, list(text = ""))
  }
  
  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }
  
  
  numcycles <- nrow(data$mean)
  if (missing(omit)) {
    omit <- floor(0.2 * numcycles)
  } else {
    omit <- floor(omit * numcycles)
  }
  if (omit == 0) omit <- 1
  
  include <- omit:numcycles
  if (length(data$cycnum) == 0) {
    cycnum <- include
  } else {
    cycnum <- data$cycnum[include]
  }
  
  
  # LL
  graph_data <- dplyr::tibble(cycle = include, ll = data$ll[include])
  
  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    "<b>-2 * Log-Likelihood</b>",
                                    "-2 * Log-Likelihood"
  )
  p1 <- graph_data %>%
    plotly::plot_ly(
      x = ~cycle, y = ~ll,
      type = "scatter",
      mode = "markers+lines",
      line = line,
      marker = marker,
      showlegend = F,
      hovertemplate = "Cycle: %{x:i}<br>-2*LL: %{y:.3f}<extra></extra>"
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )
  
  # AIC/BIC
  graph_data$aic <- data$aic[include]
  graph_data$bic <- data$bic[include]
  
  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    "<b>AIC/BIC</b>",
                                    "AIC/BIC"
  )
  
  p2 <- tibble::tibble(cycle = 1:numcycles, aic = data$aic, bic = data$bic) %>%
    pivot_longer(cols = -cycle, names_to = "type", values_to = "value") %>%
    filter(cycle %in% include) %>%
    plotly::plot_ly(
      x = ~cycle, y = ~value, type = "scatter", mode = "lines+markers",
      color = ~type,
      colors = colors,
      line = list(width = line$width),
      linetype = ~type,
      linetypes = linetypes,
      text = ~toupper(type),
      marker = list(size = marker$size, symbol = marker$symbol),
      hovertemplate = "Cycle: %{x:i}<br>%{text}: %{y:.3f}<extra></extra>",
      showlegend = F
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )
  
  # gamma/lambda
  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    "<b>Gamma/Lambda</b>",
                                    "Gamma/Lambda"
  )
  
  if(max(data$gamlam$outeq) > 1){
    all_equal <- data$gamlam %>% group_by(outeq) %>% 
      summarize(checksum = sum(value)) %>% distinct(checksum) %>%
      nrow(.) %>% magrittr::equals(1) 
  } else {
    all_equal <- FALSE
  }
  
  p3 <- data$gamlam %>% filter(cycle %in% include) %>%
    plotly::plot_ly(
      x = ~cycle, y = ~value, type = "scatter", mode = "lines+markers",
      color = ~outeq,
      colors = colors,
      line = list(width = line$width),
      linetype = ~outeq,
      linetypes = linetypes,
      marker = list(size = marker$size, symbol = marker$symbol),
      text = ifelse(all_equal, "All", ~outeq),
      hovertemplate = "Cycle: %{x:i}<br>Gam/Lam: %{y:.3f}<br>Outeq: %{text}<extra></extra>",
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )


# normalized plots

normalized_plot <- function(.par, range) {
  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    paste0("<b>Normalized ", .par, "</b>"),
                                    paste0("Normalized ", .par)
  )
  layout$yaxis$range <- range
  
  .p <- graph_data[[.par]] %>%
    pivot_longer(cols = -cycle, names_to = "par") %>%
    plot_ly(
      x = ~cycle, y = ~value, type = "scatter", mode = "markers+lines",
      color = ~par,
      colors = colors,
      line = list(width = line$width),
      linetype = ~par,
      linetypes = linetypes,
      marker = list(size = marker$size, symbol = marker$symbol),
      text = ~par,
      hovertemplate = paste0("Cycle: %{x:i}<br>Parameter: %{text}<br>", .par, ": %{y:.3f}<br><extra></extra>"),
      showlegend = FALSE,
      legendgroup = "Normalized"
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )
  return(.p)
}
graph_data$Mean <- data$mean[include, ] 
graph_data$Median <- data$median[include, ] 
graph_data$SD <- data$sd[include,  ]

graph_range <- range(graph_data$Mean[,-1], graph_data$Median[,-1], graph_data$SD[,-1])


p4 <- normalized_plot("Mean", graph_range)
p5 <- normalized_plot("Median", graph_range)

if (!all(is.na(data$sd))) {
  p6 <- normalized_plot("SD", graph_range)
} else {
  p6 <- plotly::plotly_empty(type = "scatter", mode = "markers", showlegend = F) %>%
    layout(title = list(
      text = "Initial standard deviation = 0.\nAssay error may be too large.",
      yref = "paper",
      y = 0.5
    ))
}



p_r1 <- plotly::subplot(p1, p2, p3,
                        nrows = 1,
                        titleX = F, titleY = T,
                        margin = c(0.05, 0.05, 0, 0.05)
)
p_r2 <- plotly::subplot(p4, p5, p6,
                        nrows = 1,
                        titleX = T, titleY = T,
                        margin = c(0.05, 0.05, 0, 0.05)
)
p <- plotly::subplot(p_r1, p_r2,
                     nrows = 2,
                     titleX = T, shareX = F,
                     titleY = T,
                     margin = c(0.05, 0.05, 0, 0.05)
) %>%
  layout(legend = list(y = 0.4))

print(p)
return(p)
}

#' @title Plot NPAG Cycle Information
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Plot *PMcycle* objects, which can be
#' accessed as the `$data` object within the `$cycle` field of a [PM_result] object, e.g.
#' `PM_result$cycle$data`.  It is largely now a legacy plotting function and should generally
#' be replaced by [plot.PM_cycle].
#'
#' @method plot PMcycle
#' @param x The name of an *PMcycle* data object generated by [makeCycle].
#' @param x.leg Porportionate location along the X-axis to place legend; 0 (default) is at left, 1 at right.
#' @param y.leg Porportionate location along the X-axis to place legend;  0 is at bottom, 1 (default) at top.
#' @param cex.leg Porportionate size of legend text.
#' @param omit Decimal between 0 and 1 specifying the proportion of "burn-in" cycles to omit from the plots.  If missing,
#' the first 20% will be omitted.
#' @param col A vector of colors for the curves, which will be recycled if too short.  Not mandatory.
#' @param out Direct output to a PDF, EPS or image file.  Format is a named list whose first argument,
#' \code{type} is one of the following character vectors: \dQuote{pdf}, \dQuote{eps} (maps to \code{postscript}),
#' \dQuote{\code{png}}, \dQuote{\code{tiff}}, \dQuote{\code{jpeg}}, or \dQuote{\code{bmp}}.  Other named items in the list
#' are the arguments to each graphic device. PDF and EPS are vector images acceptable to most journals
#' in a very small file size, with scalable (i.e. infinite) resolution.  The others are raster images which may be very
#' large files at publication quality dots per inch (DPI), e.g. 800 or 1200. Default value is \code{NA} which means the
#' output will go to the current graphic device (usually the monitor). For example, to output an eps file,
#' out=list(\dQuote{eps}) will generate a 7x7 inch (default) graphic.
#' @param ... Additional R plotting parameters.
#' @return Plots a panel with the following windows: -2 times the log-likelihood at each cycle, gamma/lambda at
#' each cycle; Akaike Information Criterion at each cyle and Bayesian (Schwartz) Information Criterion
#' at each cycle, the mean parameter values at each cycle (normalized to starting values); the normalized
#' standard deviation of the population distribution for each parameter at each cycle; and
#' the normalized median parameter values at each cycle.
#' @author Michael Neely
#' @seealso [makeCycle], [plot], [par], [axis]
#' @export
#' @examples
#' plot(NPex$cycle$data)
#' plot(NPex$cycle$data, omit = 0)
plot.PMcycle <- function(x, x.leg = 0, y.leg = 1, cex.leg = 1.2, omit, col, out = NA, ...) {
  data <- x
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
  
  # data <- x
  numcycles <- nrow(data$mean)
  if (missing(omit)) {
    omit <- floor(0.2 * nrow(data$mean))
  } else {
    omit <- floor(omit * nrow(data$mean))
  }
  if (omit == 0) omit <- 1
  # if(missing(col)) {col <- rep(c("red","blue","green","black","purple","pink","orange","brown","gold","grey"),3)
  # } else { col <- rep(col,10)}
  # lnty <- rep(1:3,each=10)
  x <- omit:numcycles
  if (length(data$cycnum) == 0) {
    cycnum <- x
  } else {
    cycnum <- data$cycnum[x]
  }
  nvar <- ncol(data$mean)
  nout <- ncol(data$gamlam)
  # establish windows
  # par(mfrow=c(3,2))
  # This is an example
  
  # -2 x log-likelihood
  # plot(y=data$ll[omit:numcycles],x=x,type="l",xlab="Cycle",ylab="",main="-2 x Log likelihood",xaxt="n")
  graph_data <- data.frame(
    x = x,
    m_two_ll = data$ll[omit:numcycles]
  )
  p1 <- ggplot2::ggplot(data = graph_data, ggplot2::aes(x = x, y = m_two_ll)) +
    ggplot2::geom_line() +
    ggplot2::ylab("-2 x Log likelihood") +
    ggplot2::xlab("Cycle")
  # qplot(y=data$ll[omit:numcycles], x=x, geom=c("point", "line"), xlab = "Cycle", ylab = "") + ylab("-2 x Log likelihood")
  
  # axis(1,at=x,labels=cycnum)
  # AIC and BIC
  # aicbic <- c(data$aic[omit:numcycles],data$bic[omit:numcycles])
  # plot(y=aicbic,x=c(x,x),type="n",xlab="Cycle",ylab="",main="AIC/BIC",xaxt="n")
  # axis(1,at=x,labels=cycnum)
  # lines(y=data$bic[omit:numcycles],x=x,type="l",col=col[1])
  # lines(y=data$aic[omit:numcycles],x=x,type="l",col=col[2])
  # legend(x=x.leg*max(x)+omit,y=min(aicbic)+y.leg*(max(aicbic)-min(aicbic)),legend=c("BIC","AIC"),
  #       col=col[1:2],lty=lnty[1:2],lwd=2,bg="white",cex=cex.leg,
  #       x.intersp=0.8,y.intersp=0.8)
  graph_data$aic <- data$aic[omit:numcycles]
  graph_data$bic <- data$bic[omit:numcycles]
  p2 <- ggplot2::ggplot(data = graph_data) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = aic, colour = "aic")) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = bic, colour = "bic")) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::ylab("AIC/BIC") +
    ggplot2::xlab("Cycle")
  
  
  # gamma/lambda
  graph_data$gamma_lambda <- data$gamlam[omit:numcycles, ]
  if (is.null(nout)) {
    # TODO Don't know how to check this scenario, JD-Jul/2020
    # plot(y=data$gamlam[omit:numcycles],x=x,type="l",xlab="Cycle",ylab="",main="Gamma/Lambda",xaxt="n",...)
    # axis(1,at=x,labels=cycnum)
    p3 <- ggplot2::ggplot(data = graph_data) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = gamma_lambda)) +
      ggplot2::ylab("Gamma/Lambda") +
      ggplot2::xlab("Cycle")
  } else {
    # plot(y=max(data$gamlam[omit:numcycles,]),x=max(x),type="n",xlab="Cycle",ylab="",main="Gamma/Lambda",
    #     xlim=range(x),ylim=range(data$gamlam[omit:numcycles,]),xaxt="n")
    # axis(1,at=x,labels=cycnum)
    # for(i in 1:nout){
    #  lines(y=data$gamlam[omit:numcycles,i],x=x,col=col[i])
    # }
    # if(nout>1){
    #  legend(x=x.leg*max(x)+omit,y=min(data$gamlam[omit:numcycles,])+y.leg*(max(data$gamlam[omit:numcycles,])-min(data$gamlam[omit:numcycles,])),
    #         legend=paste("Output",1:nout),col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
    #         x.intersp=0.8,y.intersp=0.8)
    # }
    
    p3 <- ggplot2::ggplot(data = graph_data) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = gamma_lambda)) +
      ggplot2::ylab("Gamma/Lambda") +
      ggplot2::xlab("Cycle")
  }
  
  
  # standardized means
  # plot(y=data$mean[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized Mean",type="n",xaxt="n")
  # axis(1,at=x,labels=cycnum)
  
  
  # for(i in 1:nvar){
  # lines(y=data$mean[omit:numcycles,i],x=x,col=col[i],lty=lnty[i])
  # }
  graph_data$mean <- data$mean[omit:numcycles, ]
  p4 <- purrr::reduce(1:nvar, ~ .x + ggplot2::geom_line(ggplot2::aes(x = x, y = mean[, .y], colour = data$names[.y])), .init = ggplot2::ggplot(data = graph_data) +
                        ggplot2::theme(legend.title = ggplot2::element_blank()) +
                        ggplot2::ylab("Normalized Mean") +
                        ggplot2::xlab("Cycle"))
  # legend(x=x.leg*max(x)+omit,y=min(data$mean[omit:numcycles,])+y.leg*(max(data$mean[omit:numcycles,])-min(data$mean[omit:numcycles,])),
  #       legend=data$names,col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
  #       x.intersp=0.8,y.intersp=0.8)
  
  
  
  
  # standardized SD
  if (!all(is.na(data$sd))) {
    # plot(y=data$sd[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized SD",type="n",xaxt="n",...)
    # axis(1,at=x,labels=cycnum)
    # for(i in 1:nvar){
    #  lines(y=data$sd[omit:numcycles,i],x=x,col=col[i],lty=lnty[i])
    # }
    # legend(x=x.leg*max(x)+omit,y=min(data$sd[omit:numcycles,])+y.leg*(max(data$sd[omit:numcycles,])-min(data$sd[omit:numcycles,])),
    #       legend=data$names,col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
    #       x.intersp=0.8,y.intersp=0.8)
    graph_data$sd <- data$sd[omit:numcycles, ]
    p5 <- purrr::reduce(1:nvar, ~ .x + ggplot2::geom_line(ggplot2::aes(x = x, y = sd[, .y], colour = data$names[.y])), .init = ggplot2::ggplot(data = graph_data) +
                          ggplot2::theme(legend.title = ggplot2::element_blank()) +
                          ggplot2::ylab("Normalized SD") +
                          ggplot2::xlab("Cycle"))
  } else {
    # plot(y=data$median[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized SD",type="n",xaxt="n",...)
    # axis(1,at=x,labels=cycnum)
    # text("Initial standard deviation = 0\nAssay error may be too large.",x=median(c(omit,numcycles)),y=median(data$median[omit:numcycles,]),col="gray50",cex=2)
    p5 <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, size = 4, label = "Initial standard deviation = 0\nAssay error may be too large.") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  
  
  # standardized median
  # plot(y=data$median[omit:numcycles,],x=rep(x,nvar),xlab="Cycle",ylab="",main="Normalized Median",type="n",xaxt="n",...)
  # axis(1,at=x,labels=cycnum)
  # for(i in 1:nvar){
  #  lines(y=data$median[omit:numcycles,i],x=x,col=col[i],lty=lnty[i])
  # }
  # legend(x=x.leg*max(x)+omit,y=min(data$median[omit:numcycles,])+y.leg*(max(data$median[omit:numcycles,])-min(data$median[omit:numcycles,])),
  #       legend=data$names,col=col[1:nvar],lty=lnty[1:nvar],lwd=2,bg="white",cex=cex.leg,
  #       x.intersp=0.8,y.intersp=0.8)
  
  # par(mfrow=c(1,1))
  graph_data$median <- data$median[omit:numcycles, ]
  p6 <- purrr::reduce(1:nvar, ~ .x + ggplot2::geom_line(ggplot2::aes(x = x, y = median[, .y], colour = data$names[.y])), .init = ggplot2::ggplot(data = graph_data) +
                        ggplot2::theme(legend.title = ggplot2::element_blank()) +
                        ggplot2::ylab("Normalized Median") +
                        ggplot2::xlab("Cycle"))
  
  
  # #close device if necessary
  # if(inherits(out,"list")) dev.off()
  p <- plotly::subplot(p1, p2, p3, p4, p5, p6, nrows = 3, titleX = T, titleY = T, margin = c(0.05, 0.05, 0, 0.05))
  p$x$layout$showlegend <- F
  p$x$layout$xaxis$title$text <- ""
  p$x$layout$xaxis2$title$text <- ""
  p$x$layout$xaxis3$title$text <- ""
  p$x$layout$xaxis4$title$text <- ""
  print(p)
  return(p)
}
