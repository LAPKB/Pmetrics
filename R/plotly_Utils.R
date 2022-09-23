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

#amend axis labels
amendAxisLabel <- function(.axis, .title, bold = T){
  
  default_font <- list(family = "Arial", color = "black", size = 16)
  
  if(is.list(.title)){
    if(is.null(purrr::pluck(.title,"text"))){
      stop("Axis title missing text element.\nSee plotly::schema() > layout > layoutAttributes > xaxis/yaxis > title for help.\n")
    } else {
      .text <- .title$text
    }
    
    if(is.null(purrr::pluck(.title, "font"))){
      .font <- default_font
    } else {
      .font <- modifyList(default_font, .title$font)
    }
    .title <- list(text = .text, font = .font)
  } else { #title is simply a name
    .title <- list(text = .title, font = default_font)
  }
  
  if(bold){
    .title$text <- paste0("<b>",.title$text,"</b>")
  }
  
  .axis$title <- .title
  return(.axis)
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
  if(length(dots)>0){
    if(!names(dots) %in% c("xaxis", "yaxis")){
      cat(crayon::red("Warning: "),"Only xaxis and yaxis in dots are currently recognized.")
    }
  }
  xaxis <- purrr::pluck(dots, "xaxis") #check for additional changes
  xaxis <- if(is.null(xaxis)){xaxis <- list()} else {xaxis}
  yaxis <- purrr::pluck(dots, "yaxis")
  yaxis <- if(is.null(yaxis)){yaxis <- list()} else {yaxis}
  
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


vline <- function(x = 0, color = "black", width = 1, dash = 1) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, width = width, dash = dash)
  )
}

hline <- function(y = 0, color = "black", width = 1, dash = 1) {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, width = width, dash = dash)
  )
}

notNeeded <- function(x, f){
  cat(paste0(crayon::blue(x)," is not required for ",f," and will be ignored."))
}

