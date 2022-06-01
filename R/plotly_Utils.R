# Common plotly utilities in Pmetrics

#amend markers
amendMarker <- function(.marker, color = "red"){
  default_marker <- list(symbol = "circle", 
                         color = color, 
                         size = 10, 
                         opacity = 0.5,
                         stroke = "black", 
                         span = 1)
  
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
amendLine <- function(.line, color = "dodgerblue"){
  default_line <- list(color = color, width = 1, linetype = 1)
  
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

#amend bar
amendBar <- function(.bar, color = "dodgerblue"){
  default_bar <- list(color = color, width = 0.02, opacity = 0.75)
  
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
setGrid <- function(.axis, grid = F){
  
  default_grid <- list(gridcolor = "grey50", gridwidth = 1)
  
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
amendLegend <- function(.legend){
  
  if(inherits(.legend,"logical")){
    if(.legend){
      showlegend <- T
      legendArgs <- NULL
    } else {
      showlegend <- F
      legendArgs <- NULL
    }
  }
  
  if(inherits(.legend,"list")){
    showlegend <- T
    legendArgs <- .legend
  }
  
  return(list(showlegend=showlegend, legendArgs=legendArgs))
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

