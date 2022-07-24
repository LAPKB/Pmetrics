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
  default_line <- list(color = "dodgerblue", width = 1, linetype = 1)
  
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

#amend CI
amendCI <- function(.ci, default){
  default_ci <- list(color = "dodgerblue", dash = "dash", opacity = 0.4)
  
  if(!missing(default)){
    default_ci <- modifyList(default_ci, default)
  }
  
  if(inherits(.ci,"logical")){
    if(!.ci){
      .ci <- modifyList(default_ci, list(opacity = 0))
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
  default_bar <- list(color = color, width = 0.02, opacity = 0.75)
  
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

group2NA <- function(data, groupNames = "group", nested = NULL, ordered = NULL,
                     retrace.first = inherits(data, "GeomPolygon")) {
  
  if (NROW(data) == 0) return(data)
  
  # for restoring class information on exit
  datClass <- oldClass(data)
  
  # data.table doesn't play nice with list-columns
  if (inherits(data, "sf")) data <- fortify_sf(data)
  
  # evaluate this lazy argument now (in case we change class of data)
  retrace <- force(retrace.first)
  
  # sanitize variable names (TODO: throw warnings if non-existing vars are referenced?)
  groupNames <- groupNames[groupNames %in% names(data)]
  nested <- nested[nested %in% names(data)]
  ordered <- ordered[ordered %in% names(data)]
  
  dt <- data.table::as.data.table(data)
  
  # if group doesn't exist, just order the rows and exit
  if (!length(groupNames)) {
    keyVars <- c(nested, ordered)
    if (length(keyVars)) data.table::setorderv(dt, cols = keyVars)
    return(structure(dt, class = datClass))
  }
  
  # order the rows
  data.table::setorderv(dt, cols = c(nested, groupNames, ordered))
  
  # when connectgaps=FALSE, inserting NAs ensures each "group" 
  # will be visually distinct https://plotly.com/r/reference/#scatter-connectgaps
  # also, retracing is useful for creating polygon(s) via scatter trace(s)
  keyVars <- c(nested, groupNames)
  keyNum <- length(keyVars) + 1
  idx <- if (retrace) {
    dt[, c(.I, .I[1], NA), by = keyVars][[keyNum]]
  } else {
    dt[, c(.I, NA), by = keyVars][[keyNum]]
  }
  dt <- dt[idx]
  
  # remove NAs that unnecessarily seperate nested groups
  # (at least internally, nested really tracks trace index, meaning we don't need 
  # to seperate them)
  NAidx <- which(is.na(idx))
  for (i in seq_along(keyVars)) {
    dt[[keyVars[[i]]]][NAidx] <- dt[[keyVars[[i]]]][NAidx - 1]
  }
  if (length(nested)) {
    dt <- dt[ dt[, .I[-.N], by = nested][[length(nested) + 1]] ]
  } else {
    dt <- dt[-.N]
  }
  
  structure(dt, class = datClass)
}
