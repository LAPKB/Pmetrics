# Common plotly utilities in Pmetrics

#amend markers
amendMarker <- function(.marker, default){
  default_marker <- list(symbol = "circle", 
                         color = "red", 
                         size = 10, 
                         opacity = 0.5,
                         line = list(color = "black", width = 1)
  )
  if(!missing(default)){
    default_marker <- modifyList(default_marker, default)
  }
  
  if(inherits(.marker,"logical")){
    if(!.marker){
      .marker <- default_marker
      .marker$size = 0.1
    } else {
      .marker <- default_marker
    }
  }
  
  if(inherits(.marker,"list")){
    .marker <- modifyList(default_marker, .marker)
  }
  return(.marker)
}

#amend lines
amendLine <- function(.line, default){
  default_line <- list(color = "dodgerblue", width = 1, dash = "solid")
  
  if(!missing(default)){
    default_line <- modifyList(default_line, default)
  }
  
  if(inherits(.line,"logical")){
    if(!.line){
      .line <- default_line
      .line$width = 0
    } else {
      .line <- default_line
    }
  }
  
  if(inherits(.line,"list")){
    .line <- modifyList(default_line, .line)
  }
  return(.line)
}


#amend title
amendTitle <- function(.title, default){
  
  default_font <- list(family = "Arial", color = "black", bold = T, size = 16)
  if(!missing(default)){
    default_font <- modifyList(default_font, default)
  }
  
  errors <- NULL
  error <- F
  
  if(is.list(.title)){
    if(is.null(purrr::pluck(.title,"text"))){
      stop("Missing title text element.\nSee plotly::schema() > layout > layoutAttributes > title for help.\n")
    } 
    if(!is.null(purrr::pluck(.title,"size"))){
      error <- T
      errors <- "size"
      default_font$size <- .title$size
      .title$size <- NULL
    } 
    if(!is.null(purrr::pluck(.title,"color"))){
      error <- T
      errors <- c(errors, "color")
      default_font$color <- .title$color
      .title$color <- NULL
    } 
    if(!is.null(purrr::pluck(.title,"family"))){
      error <- T
      errors <- c(errors, "family")
      default_font$family <- .title$family
      .title$family <- NULL
    } 
    
    if(!is.null(purrr::pluck(.title,"bold"))){
      #don't report error, since not standard plotly
      default_font$bold <- .title$bold
      .title$bold <- NULL
    } 
    
    if(is.null(purrr::pluck(.title, "font"))){
      .title$font <- default_font
    } else {
      .title$font <- modifyList(default_font, .title$font)
    }
    
  } else { #title is simply a name
    .title <- list(text = .title, font = default_font)
  }
  
  if(.title$font$bold){
    .title$text <- paste0("<b>",.title$text,"</b>")
  }
  
  
  if(error){
    cat(paste0(crayon::red("Note: "), paste(errors, collapse = " and "), " should be within a font list.\nSee plotly::schema() > layout > layoutAttributes > title/xaxis/yaxis for help.\n"))
  }
  
  return(.title)
}

#amend CI
amendCI <- function(.ci, default){
  default_ci <- list(color = "dodgerblue", dash = "dash", width = 1, opacity = 0.4)
  
  if(!missing(default)){
    default_ci <- modifyList(default_ci, default)
  }
  
  if(inherits(.ci,"logical")){
    if(!.ci){
      .ci <- modifyList(default_ci, list(opacity = 0, width = 0))
    } else {
      .ci <- default_ci
    }
  }
  
  if(inherits(.ci,"list")){
    .ci <- modifyList(default_ci, .ci)
  }
  return(.ci)
}

#amend bar
amendBar <- function(.bar, color = "dodgerblue", default){
  default_bar <- list(color = color, width = .02, opacity = 0.75)
  
  if(!missing(default)){
    default_bar <- modifyList(default_bar, default)
  }
  
  if(inherits(.bar,"logical")){
    if(!.bar){
      .bar <- default_bar
      .bar$width = 0
    } else {
      .bar <- default_bar
    }
  }
  
  if(inherits(.bar,"list")){
    .bar <- modifyList(default_bar, .bar)
  }
  return(.bar)
}


#make grid lines
setGrid <- function(.axis, grid = F, default){
  
  default_grid <- list(gridcolor = "grey50", gridwidth = 1)
  if(!missing(default)){
    default_grid <- modifyList(default_grid, default)
  }
  
  if(inherits(grid,"logical")){
    if(grid){
      grid <- default_grid
    } else {
      grid <- default_grid
      grid$gridcolor = "white"
        grid$gridwidth = 0
    }
  }
  
  if(inherits(grid,"list")){
    grid <- modifyList(default_grid, grid)
  }
  
  .axis <- modifyList(.axis,grid)
  
  return(.axis)
}



#amend the legend
amendLegend <- function(.legend, default){
  
  default_legend <- list(showlegend = F)
  if(!missing(default)){
    default_legend <- modifyList(default_legend, default)
  }
  
  if(inherits(.legend,"logical")){
    if(!.legend){
      .legend <- default_legend
    } else {
      .legend <- default_legend
      .legend$showlegend <- T
    }
  } else {
    if(inherits(.legend,"list")){
      .legend <- modifyList(default_legend, .legend)
      .legend$showlegend <- T
    }
  }
  return(.legend)
}

#amend dots
amendDots <- function(dots){
  axesAttr <- names(plotly::schema(F)$layout$layoutAttributes$xaxis)
  
  xaxis <- purrr::pluck(dots, "xaxis") #check for additional changes
  if(is.null(xaxis)){
    xaxis <- list()
  } else {
    xaxis <- dots$xaxis
    dots$xaxis <- NULL #take it out of dots
  }
  yaxis <- purrr::pluck(dots, "yaxis")
  if(is.null(yaxis)){
    yaxis <- list()
  } else {
    yaxis <- dots$yaxis
    dots$yaxis <- NULL #take it out of dots
  }
  
  otherArgs <- purrr::map_lgl(names(dots), function(x) x %in% axesAttr)
  if(any(otherArgs)){
    xaxis <- modifyList(xaxis, dots[otherArgs])
    yaxis <- modifyList(yaxis, dots[otherArgs])
  }
  
  if(!all(otherArgs)){ #some are false
    cat(crayon::red("Warning: "),"Attributes other than xaxis/yaxis in dots are currently ignored.")
  }
  
  layout <- list(xaxis = xaxis, yaxis = yaxis)
  return(layout)
  
}

includeExclude <- function(.data, include, exclude){
  if(!is.na(include[1])){
    .data <- .data %>% filter(id %in% include)
  }
  if(!is.na(exclude[1])){
    .data <- .data %>% filter(!id %in% exclude)
  }
  if(nrow(.data)==0){stop("Include/exclude criteria result in zero subjects.")}
  
  return(.data)
}


notNeeded <- function(x, f){
  cat(paste0(crayon::blue(x)," is not required for ",f," and will be ignored."))
}


#' Add lines to plotly plot
#' 
#' Analogous to [abline], draws horizontal, vertical or sloped reference lines.
#' 
#' This function creates a line shape that can be added a plotly plot.
#' See schema() > layout > layoutAttributes > shapes for details. Use only one 
#' of the following:
#' * a and b to specify a line with intercept and slope
#' * h to specify a horizontal line with y-intercept at `h`
#' * v to specify a vertical line with x-intercept at `v`
#' 
#' If using this function to add a line to an existing plot, it must be used
#' with [add_shapes]. If used for a new plot, it can be included as an element
#' in the layout list.
#' 
#' @param a Intercept y value in relative coordinates, i.e. 0 (bottom) to 1 (top).
#' The x value is 0.
#' @param b Slope 
#' @param h Y-intercept of horizontal line, in absolute coordinates
#' @param v X-intercept of vertical line, in absolute coordinates
#' @param line `r template("line")`.  
#' Default is `line = list(color = "black", width = 1, dash = "dash")`.
#' @export
#' @seealso [add_shapes]
#' @examples 
#' #add to an existing plot
#' NPex$op$plot()
#' add_shapes(shapes = ab_line(v = 12))
#' 
#' #add to a new plot
#' plotly::plot_ly(x = 1:10, y=1:10, type = "scatter", mode = "lines+markers") %>%
#' layout(shapes = ab_line(h = 5, line = list(color = "red", dash = "solid")))
ab_line <- function(a = NULL, b = NULL, h = NULL, v = NULL, line = T){
  if(!is.null(a)){
    if(is.null(b)){
      stop(paste0("Specify both ", crayon::red("a"), " (intercept) and ", 
                  crayon::red("b"), " (slope)."))
    }
    x0 <- 0
    y0 <- a
    x1 <- 1
    y1 <- b + a #y1 = b*x1 + a
    xref <- "paper"
    yref <- "paper"
  }
  if(!is.null(b)){
    if(is.null(a)){
      stop(paste0("Specify both ", crayon::red("a"), " (intercept) and ", 
                  crayon::red("b"), " (slope)."))
    }
  }
  if(!is.null(h)){
    x0 <- 0
    x1 <- 1
    xref <- "paper"
    y0 <- h
    y1 <- h
    yref = "y"
  }
  
  if(!is.null(v)){
    x0 <- v
    x1 <- v
    xref <- "x"
    y0 <- 0
    y1 <- 1
    yref = "paper"
  }
  line = amendLine(line, default = list(color = "black", width = 1, dash = "dash"))
  return(list(type = "line", 
              x0 = x0, y0 = y0, x1 = x1, y1 = y1, 
              xref = xref, yref = yref, 
              line = line))
  
}


#' Add shapes to plotly plot
#' 
#' Modifies the layout object of an existing plot to include a new shape.
#' 
#' This function adds a shape to the layout element of a plotly plot.
#' Type `schema()` in the console and expand the list for 
#' layout > layoutAttributes > shapes for details on how
#' to specify a shape. A convenient Pmetrics helper function to add line shapes
#' is [ab_line]. Other shapes such as circles, rectangles and paths can be added,
#' but must be done manually as outlined in the plotly schema documentation.
#' An example of a circle shape is below in examples.
#' 
#' @param p The plot to which the shape should be added. Default is the
#' `last_plot()`.
#' @param shapes A list of attributes that specify a shape. 
#' @export
#' @seealso [ab_line]
#' @examples 
#' NPex$op$plot()
#' add_shapes(shapes = ab_line(v = 12))
#' 
#' NPex$data$plot()
#' add_shapes(shapes = list(type = "circle", x0 = 125, y0 = 10, x1 = 135, y1 = 15))
add_shapes <- function(p = plotly::last_plot(), shape){
  cur_data <- p$x$cur_data
  #try different locations
  if(is.null(p$x$layoutAttrs)){ #no layout attributes
    p$x$layoutAttrs[[cur_data]] <- list(shapes = shape)
  } else {
    nAttrs <- length(p$x$layoutAttrs)
    shapPos <- which(sapply(p$x$layoutAttrs, function(x) which("shapes" %in% names(x))) == 1)
    if(length(shapPos)>0){ #found one
      p$x$layoutAttrs[[shapPos]]$shapes <- append(list(p$x$layoutAttrs[[shapPos]]$shapes), list(shape))
    } else { #didn't find one
      p$x$layoutAttrs[[1]]$shapes <- list(shape)
    }
  }
  return(p)
}

#' Add regression to plotly plot
#' 
#' Modifies an existing plot to include a regression line with  confidence interval.
#' 
#' This function adds a regression line to an existing  plotly plot.
#' The default is to use the x and y values in the plot, but this can be overridden
#' by specifying a data object. If another `data` object is used, values for `x` and `y`
#' must also be specified. Alternatively, the original data can be used and new columns
#' selected for regression by omitting the `data` argument and specifying new `x` and
#' `y` values. The intent of this function is to replicate the behavior of 
#' `ggplot::geom_smooth()`.
#' 
#' @param p The plot to which the shape should be added. Default is the
#' `plotly::last_plot()`.
#' @param x X value for regression if not the original x value of the plot. Be
#' sure to specify as a formula, e.g. `x = ~pred`. 
#' @param y Y value for regression if not the original y value of the plot. Be
#' sure to specify as a formula, e.g. `y = ~obs`. 
#' @param data A secondary data object to use for regression other than the 
#' original plot data.
#' @param method The tegression method, currently either "lm" (the default) for
#' linear regression, or "loess" for loess regression.
#' @param line `r template("line")` Default is `list(color = "blue", width = 2)`.
#' The confidence interval will have the same color but at opacity 0.2.
#' @param ci Confidence interval for regressions. Default is 0.95. It can be suppressed
#' by setting to 0.
#' @param stats Add the statistics from linear regression to the plot. Ignored if 
#' `method = "loess"`. If missing or
#' `FALSE`, will be suppressed. Can be set to `TRUE` which results in default format of 
#' `list(x= 0.8, y = 0.1, bold = F, font = list(color = "black", family = "Arial", size = 14))`.
#' The coordinates are relative to the plot with lower left = (0,0), upper right = (1,1). This
#' argument maps to `plotly::add_text()`. It is also an option that can be set in 
#' [setPMoptions] to avoid specifying this argument for every plot. The default option
#' is `TRUE`. If specified as a
#' Pmetrics option, it can be overridden for specific plots by supplying a value.
#' @export
#' @seealso [add_shapes]
#' @examples 
#' plotly::plot_ly(mtcars, x = ~hp, y = ~mpg, type = "scatter", mode = "markers", showlegend = F) %>%
#'  add_smooth()
#' plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, type = "scatter", mode = "markers", showlegend = F) %>%
#'  add_smooth(method = "loess", ci = 0.9, line = list(color = "red", dash = "dash"))

add_smooth <- function(p = plotly::last_plot(), x = NULL, y = NULL,
                       data = NULL, method = "lm", line = T, ci = 0.95, stats){
  
  line <- amendLine(line, default = list(color = "blue", width = 2))
  
  if(!is.null(data)){
    if(is.null(x) | is.null(y)) stop("Missing x or y with data.\n")
    if(!rlang::is_formula(x) | !rlang::is_formula(y)) stop("Specify x and y as formulae, e.g. x = ~pred.\n")
    x <- model.frame(x, data)
    y <- model.frame(y, data)
  } else { #data is null
    if(!is.null(x) | !is.null(y)){
      if(!rlang::is_formula(x) | !rlang::is_formula(y)) stop("Specify x and y as formulae, e.g. x = ~pred.\n")
      x <- model.frame(x, p$x$visdat[[1]]())
      y <- model.frame(y, p$x$visdat[[1]]())
      
    } else {
      x <- model.frame(p$x$attrs[[2]]$x, p$x$visdat[[1]]())[,1]
      y <- model.frame(p$x$attrs[[2]]$y, p$x$visdat[[1]]())[,1]
    }
    
  }
  if(length(x) != length(y)){
    stop("Regression failed due to unequal x (n = ", length(x), ") and y (n = ", length(y), ").\n")
  }
  vals <- dplyr::bind_cols(x, y) %>% dplyr::rename("x" = 1, "y" = 2)
  mod <- do.call(method, args = list(formula = y ~ x, data = vals))
  
  if(method == "lm"){
    inter <- format(coef(mod)[1],digits=3)
    slope <- format(coef(mod)[2],digits=3)
    if(is.na(summary(mod)$coefficients[1,2])) {ci.inter <- rep("NA",2)} else {ci.inter <- c(format(confint(mod,level=ci)[1,1],digits=3),format(confint(mod,level=ci)[1,2],digits=3)) }
    if(is.na(summary(mod)$coefficients[2,2])) {ci.slope <- rep("NA",2)} else {ci.slope <- c(format(confint(mod,level=ci)[2,1],digits=3),format(confint(mod,level=ci)[2,2],digits=3)) }
    
    regStat <- paste0("R-squared = ",format(summary(mod)$r.squared,digits=3),"<br>",
                      "Inter = ",inter," (",ci*100,"%CI ",ci.inter[1]," to ",ci.inter[2],")","<br>",
                      "Slope = ",slope," (",ci*100,"%CI ",ci.slope[1]," to ",ci.slope[2],")","<br>")
    
    p_data <- plotly::plotly_data(p)
    if(inherits(p_data,c("PM_op", "PMop"))){ #this is a PM_op object
      sumStat <- summary(p_data, outeq = p_data$outeq[1], 
                         pred.type = p_data$pred.type[1])
      regStat <- paste0(regStat,"<br>",
                        "Bias = ",format(sumStat$pe$mwpe,digits=3),"<br>",
                        "Imprecision  = ",format(sumStat$pe$bamwspe,digits=3)
      )
    }
    
    p <- p %>% plotly::add_lines(x = vals$x, y = fitted(mod),
                                 hoverinfo = "text",
                                 text = regStat,
                                 line = line)
  } else { #loess
    p <- p %>% plotly::add_lines(x = vals$x, y = fitted(mod),
                                 hoverinfo = "none",
                                 line = line)
  }
  
  if(ci > 0){
    zVal <- qnorm(0.5 + ci/2)
    seFit <- predict(mod, newdata = vals, se = T)
    upper <- seFit$fit + zVal * seFit$se.fit
    lower <- seFit$fit - zVal * seFit$se.fit
    
    p <- p %>%
      plotly::add_ribbons(x = vals$x, y = vals$y, ymin = ~lower, ymax = ~upper, 
                          fillcolor = line$color,
                          line = list(color = line$color),
                          opacity = 0.2,
                          name = dplyr::case_when(
                            method == "lm" ~"Linear Regression",
                            method == "loess" ~"Loess Regression"
                          ),
                          hovertemplate = paste0("Predicted: %{x:.2f}<br>", 100*ci, 
                                                 "% CI: %{y:.2f}<extra>%{fullData.name}</extra>"))
  }
  if(missing(stats)) stats <- getPMoptions("op_stats")
  if(is.null(stats)){
    setPMoptions(op_stats = T)
    stats <- T
    statPlot <- T
  }
  if(is.logical(stats)){ #default formatting
    if(stats){
      statPlot <- T
    } else {statPlot <- F}
    stats <- amendTitle("", default = list(size = 14, bold = F))
    stats$x <- 0.8
    stats$y <- 0.1
  } else { #formatting supplied, set text to "" (will be replaced later)
    stats$text <- ""
    if(is.null(stats$x)){stats$x <- 0.8}
    if(is.null(stats$y)){stats$y <- 0.1}
    stats <- amendTitle(stats, default = list(size = 14, bold = F))
    statPlot <- T
  }
  
  if(statPlot & method == "lm"){ #add statistics
    p <- p %>%
      plotly::layout(annotations = list(
        x = stats$x, 
        y = stats$y, 
        text = regStat, 
        font = stats$font,
        xref = "paper",
        yref = "paper",
        align = "left",
        showarrow = F)
      )
  }
  
  return(p)
}
