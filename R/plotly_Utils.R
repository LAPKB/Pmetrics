# Common plotly utilities in Pmetrics

#amend markers
amendMarker <- function(.marker){
  default_marker <- list(symbol = "circle", 
                         color = "red", 
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
amendLine <- function(.line){
  default_line <- list(color = "dodgerblue", width = 1, linetype = 1)
  
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
  .data <- .data %>% filter(id %in% include)
  if(!is.na(exclude)){
    .data <- .data %>% filter(!id %in% exclude)
  }
  return(.data)
}



