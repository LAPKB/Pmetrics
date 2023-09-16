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
  
  default_font <- list(family = "Arial", color = "black", bold = TRUE, size = 16)
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
      error <- TRUE
      errors <- "size"
      default_font$size <- .title$size
      .title$size <- NULL
    } 
    if(!is.null(purrr::pluck(.title,"color"))){
      error <- TRUE
      errors <- c(errors, "color")
      default_font$color <- .title$color
      .title$color <- NULL
    } 
    if(!is.null(purrr::pluck(.title,"family"))){
      error <- TRUE
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
  
  default_legend <- list(showlegend = FALSE)
  if(!missing(default)){
    default_legend <- modifyList(default_legend, default)
  }
  
  if(inherits(.legend,"logical")){
    if(!.legend){
      .legend <- default_legend
    } else {
      .legend <- default_legend
      .legend$showlegend <- TRUE
    }
  } else {
    if(inherits(.legend,"list")){
      .legend <- modifyList(default_legend, .legend)
      .legend$showlegend <- TRUE
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
#' NPex$op$plot() %>%
#' add_shapes(shapes = ab_line(v = 12))
#' 
#' #add to a new plot
#' plotly::plot_ly(x = 1:10, y=1:10, type = "scatter", mode = "lines+markers") %>%
#' plotly::layout(shapes = ab_line(h = 5, line = list(color = "red", dash = "solid")))
ab_line <- function(a = NULL, b = NULL, h = NULL, v = NULL, line = TRUE){
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
#' @param shapes A list of attributes that specify a shape. Note that only one
#' shape can be added for each call, but to be consistent with the `shapes` argument
#' to [plotly::layout()], we use the same plural.
#' @export
#' @seealso [ab_line]
#' @examples 
#' NPex$op$plot() %>%
#' add_shapes(shapes = ab_line(v = 12))
#' 
#' NPex$data$plot() %>%
#' add_shapes(shapes = list(type = "circle", x0 = 125, y0 = 10, x1 = 135, y1 = 15))
add_shapes <- function(p = plotly::last_plot(), shapes){
  cur_data <- p$x$cur_data
  #try different locations
  if(is.null(p$x$layoutAttrs)){ #no layout attributes
    p$x$layoutAttrs[[cur_data]] <- list(shapes = shapes)
  } else {
    nAttrs <- length(p$x$layoutAttrs)
    shapPos <- which(sapply(p$x$layoutAttrs, function(x) which("shapes" %in% names(x))) == 1)
    if(length(shapPos)>0){ #found one
      p$x$layoutAttrs[[shapPos]]$shapes <- append(list(p$x$layoutAttrs[[shapPos]]$shapes), list(shapes))
    } else { #didn't find one
      p$x$layoutAttrs[[1]]$shapes <- list(shapes)
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
#' plotly::plot_ly(mtcars, x = ~hp, y = ~mpg, 
#' type = "scatter", mode = "markers", showlegend = FALSE) %>%
#'  add_smooth()
#' plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, 
#' type = "scatter", mode = "markers", showlegend = FALSE) %>%
#'  add_smooth(method = "loess", ci = 0.9, line = list(color = "red", dash = "dash"))

add_smooth <- function(p = plotly::last_plot(), x = NULL, y = NULL,
                       data = NULL, method = "lm", line = T, ci = 0.95, stats){
  
  line <- amendLine(line, default = list(color = "blue", width = 2))
  
  if(!is.null(data)){
    if(is.null(x) | is.null(y)) stop("Missing x or y with data.\n")
    if(!purrr::is_formula(x) | !purrr::is_formula(y)) stop("Specify x and y as formulae, e.g. x = ~pred.\n")
    x <- model.frame(x, data)
    y <- model.frame(y, data)
  } else { #data is null
    if(!is.null(x) | !is.null(y)){
      if(!purrr::is_formula(x) | !purrr::is_formula(y)) stop("Specify x and y as formulae, e.g. x = ~pred.\n")
      x <- model.frame(x, p$x$visdat[[1]]())
      y <- model.frame(y, p$x$visdat[[1]]())
      
    } else {
      length_x <- ifelse(length(p$x$attrs)>1,2,1)
      x <- model.frame(p$x$attrs[[length_x]]$x, p$x$visdat[[1]]())[,1]
      y <- model.frame(p$x$attrs[[length_x]]$y, p$x$visdat[[1]]())[,1]
    }
    
  }
  if(length(x) != length(y)){
    stop("Regression failed due to unequal x (n = ", length(x), ") and y (n = ", length(y), ").\n")
  }
  vals <- dplyr::bind_cols(x = x, y = y)
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
                         pred.type = p_data$pred.type[1],
                         icen = p_data$icen[1])
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
    seFit <- predict(mod, newdata = vals, se = TRUE)
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
  if(missing(stats)) stats <- T

  if(is.logical(stats)){ #default formatting
    if(stats){
      statPlot <- T
    } else {statPlot <- F}
    stats <- amendTitle("", default = list(size = 14, bold = FALSE))
    stats$x <- 0.8
    stats$y <- 0.1
  } else { #formatting supplied, set text to "" (will be replaced later)
    stats$text <- ""
    if(is.null(stats$x)){stats$x <- 0.8}
    if(is.null(stats$y)){stats$y <- 0.1}
    stats <- amendTitle(stats, default = list(size = 14, bold = FALSE))
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
        showarrow = FALSE)
      )
  }
  
  return(p)
}

#' Export plotly plot
#' 
#' Wrapper around [plotly::save_image()].
#' 
#' This function improves the experience with the native plotly method of exporting
#' plots to static images. Much of the online documentation points towards using
#' the orca package, but the R help indicates that this method has been superseded
#' by the kaleido python package, made accesible in R via the reticulate package and
#' installation of the miniconda package manager.
#' 
#' These steps are all outlined in the help for [plotly::save_image()], but one step
#' is neglected. It is necessary to execute the following line of code at the end:
#' `reticulate::py_run_string("import sys")`.
#' 
#' This function will check to see that all installations are in place and offer to
#' install if not.
#' 
#' Many of the arguments are the same as for `save_image` and are passed directly
#' to that function.
#' 
#' @param p The plot to which the shape should be added. Default is the
#' `plotly::last_plot()`.
#' @param file A file path with a suitable file extension (png, jpg, jpeg, webp, svg, or pdf).
#' Unlike `save_image`, the `file` argument may include the full path and filename
#' other than in the current working directory.
#' @param width,height  The width/height of the exported image in pixels,
#' mutliplied by `scale`. 
#' @param scale The scale factor to use when exporting the figure. Default is 1.0.
#' A scale factor larger than 1.0 will increase the image size, 
#' and less than 1.0 will decrease the image size. Note that the documentation 
#' for `save_image` says that this argument changes the resolution, but that is
#' not true. The resolution will remain at 72 pixels/inch (28.3 px/cm), which is
#' the default for R. To increase the resolution, export to .pdf or .svg and use
#' an external program, such as Adobe Acrobat, Acrobat Reader, or Mac Preview.
#' @param units Units for `width` and `height`. Default is pixels ("px").
#' Alternatives are inches ("in") or centimeters ("cm")
#' @param show Show the exported image in R via [base::file.show()]. If export
#' format is pdf, it will open the system default pdf viewer. Default is `TRUE`.
#' @return Plot `p` will be exported to `file` with format determined by the
#' extension for `file`.
#' @export
#' @seealso [plotly::save_image()]
#' @examples 
#' \dontrun{
#' NPex$op$plot(stats = list(x = 0.9)) %>% 
#' export_plotly(file = "op.png", width = 12, height = 6, units = "in")
#' }
#' @author Michael Neely


export_plotly <- function(p, file, width = NULL, height = NULL, 
                          scale = NULL, units = "px", show = TRUE){
  
  if(missing(p)) p <- plotly::last_plot()
  if(!inherits(p,"plotly")) stop("Specify a plotly object to be exported.\n")
  if(missing(file)) stop("Provide a file name. The extension will determine the type of export.\n")
  
  if(Sys.which("kaleido")==""){ #not installed
    cat("Pmetrics needs to install/update the kaleido python package.\n")
    cat("See ?kaleido for more information.\n")
    confirm <- readline(cat("Enter:\n<1> to continue\n<2> to abort"))
    if (confirm == 2) {
      return(invisible(NULL))
    }
    if(!"reticulate" %in% utils::installed.packages()[,1]){
      install.packages('reticulate')
    }
    if(reticulate::miniconda_path() == ""){
      reticulate::install_miniconda()
    }
    reticulate::conda_install('r-reticulate', 'python-kaleido')
    reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
    reticulate::use_miniconda('r-reticulate')
    reticulate::py_run_string("import sys")
  }
  
  currwd <- getwd()
  
  if(grepl(.Platform$file.sep, file)){ #file is a path
    setwd(dirname(file))
    filename <- basename(file)
  } else {filename <- file}
  
  correction <- dplyr::case_when(
    units == "px" ~ 1,
    units == "in" ~ 72,
    units == "cm" ~ 28
  )
  width <- if(!is.null(width)) width * correction
  height <- if(!is.null(height)) height * correction
  
  tryCatch(plotly::save_image(p = p, file = filename, width = width, height = height, scale = scale),
           error = function(e){
             cat("The kaleido python package does not appear to be installed properly.\n")
             cat("Try following the installation instructions in the save_image() help page.\n")
             cat(paste0(crayon::red("Note - "),"Add one more line of code after the others: ",crayon::blue("reticulate::py_run_string('import sys')\n")))
             stop("Plot not saved.\n",call. = FALSE)
           }
  )
  if(show){
    if(!grepl("\\.pdf$",file)){ #not pdf
      file.show(file, title = file)
    } else { #pdf
      system(paste("open",file))
    }
  }
  setwd(currwd) #in case changed
}


#' Display multiple plotly plots
#' 
#' Wrapper around [plotly::subplot()].
#' 
#' This function addresses the deficiency with the native plotly method of combining
#' multiple plots that prevents individualized titling of subplots. The function
#' has identical arguments to [plotly::subplot()] with the addition of a `titles`
#' argument. In addition to `subplot`, the behavior of this function is two-fold:
#' * Fetch the titles (text and formatting) from each included plot
#' * Include the titles with placement per the `titles` argument
#' @param \dots One of the following
#' * any number of plotly objects
#' * a list of plotly objects.
#' Note that unlike [plotly::subplot()], ggplots and tibbles cannot be passed to
#' this function.
#' @param nrows number of rows for laying out plots in a grid-like 
#' structure. Default is 1.
#' @param widths,heights Vector of relative column widths or heights
#' on a 0-1 scale. By default all columns have an equal relative width/height, 
#' i.e. `c(0.5, 0.5)` for two columns, `rep(0.25, 4)` for 4 columns. 
#' @param margin either a single value or a vector of four values (all between 0 and 1),
#' e.g. `c(0.05, 0.05, 0.05, 0.1)` 
#' If four values are provided, the first is used as the left margin, 
#' the second is used as the right margin, the third is used as the top margin, 
#' and the fourth is used as the bottom margin. If a single value is provided, 
#' it will be used as all four margins.
#' @param titles Include titles on individual subplots? Default is `NULL`. If
#' specified as vector of length 2, will be rendered as x and y values relative
#' to each plot. For example, `title = c(0,1)` plots the titles in the upper
#' left corner of each subplot and `title = c(1,0)` renders the titles in the 
#' lower right corner. Title text and formatting will be grabbed from each 
#' individual plot. To modify these characteristics, modify the code that 
#' generated the individual plot. 
#' @param shareX,shareY Should the x- or y- axis be shared amongst the subplots?
#' @param titleX,titleY Should x- or y- axis titles be retained?
#' @param which_layout Adopt the layout of which plot? If the default value of 
#' "merge" is used, layout options found later in the sequence of plots 
#' will override options found earlier in the sequence. This argument also 
#' accepts a numeric vector specifying which plots to consider when merging.
#' @return A plot and plotly object combining all the plots in `...`, 
#' which can be further modified.
#' @export
#' @seealso [plotly::subplot()]
#' @examples 
#' plot1 <- NPex$op$plot(title = "Posterior")
#' plot2 <- NPex$op$plot(pred.type = "pop", title = "Population")
#' sub_plot(plot1, plot2, titles = c(0, 0.95), nrows = 2)
#' @author Michael Neely

sub_plot <- function(...,
                     nrows = 1,
                     widths = NULL,
                     heights = NULL,
                     margin = 0.02,
                     titles = NULL, #or (x,y)
                     shareX = FALSE,
                     shareY = FALSE,
                     titleX = shareX,
                     titleY = shareY,
                     which_layout = "merge"){
  
  #number of plots
  plots <- list(...)
  n_plots <- length(plots)
  if(nrows > n_plots) nrows <- n_plots #sanity check
  
  #grab title lists from each plot and convert to annotations
  plot_annotations <- purrr::map(plots, function(p) p$x$layoutAttrs[[length(p$x$layoutAttrs)]]$title) %>%
    purrr::map(function(title){
      list(
        text = title$text,
        font = title$font,
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "left",
        align = "right",
        x = 0, #will be replaced
        y = 0, #will be replaced
        showarrow = FALSE
      )
    })
  
  #remove titles from plots
  plots <- purrr::map(plots, function(p){
    purrr::modify_in(p, list("x","layoutAttrs",length(p$x$layoutAttrs),"title","text"), \(p) "" )
  })
  
  #calculate relative x and y based on plot number and rows
  if(!is.null(titles) && length(titles) == 2){ #we have x and y
    x_pos <- titles[1]
    y_pos <- titles[2]
    plots_per_row <- ceiling(n_plots/nrows)
    x_increment <- 1/plots_per_row
    y_increment <- 1/nrows
    x_adj <- x_pos * x_increment
    y_adj <- y_pos * y_increment
    
    x_list <- if(plots_per_row == 1) {x_adj} else {seq(x_adj, (plots_per_row - 1) * x_increment, x_increment)}
    y_list <- if(nrows == 1) {y_adj} else {rev(seq(y_adj, 1, y_increment))}
    
    plot_coords <- expand.grid(col = x_list, row = y_list) #first arg changes fastest
    
    
    plot_annotations <- purrr::map(1:n_plots, function(i){
      purrr::list_assign(plot_annotations[[i]], x = plot_coords$col[i], y = plot_coords$row[i])
    })
    
  } else {
    plot_annotations <- NULL #we did not have x,y
  }
  
  p <- plotly::subplot(plots, 
                  nrows = nrows,
                  widths = widths,
                  heights = heights,
                  margin = margin,
                  shareX = shareX,
                  shareY = shareY,
                  titleX = shareX,
                  titleY = shareY,
                  which_layout = which_layout) %>% 
    plotly::layout(annotations = plot_annotations)
  print(p)
  return(p)
}
