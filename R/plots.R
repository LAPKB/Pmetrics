plot.PMfit <- function(x, icen = "median", outeq = 1, pred.type = "post", block = 1, log = F, 
                   marker = list(), linear, loess, reference, include, exclude, mult = 1){
  
  type <- which(inherits(x,c("PMop"))==1)
  
  if(type == 1){ #PMop
    default_marker <- list(symbol = "circle", color = "dodgerblue", size = 30, opacity = 0.5)
    marker <- modifyList(default_marker,marker)
    if(missing(linear)){
      linearMod <- list(plot = F)
    } else {
      if(is.logical(linear)){linearMod <- list(plot = linear)} else {linearMod <- linear; linearMod$plot <- T}
    } 
    if(missing(loess)){
      loessMod <- list(plot = F)
    } else {
      if(is.logical(loess)){loessMod <- list(plot = loess)} else {loessMod <- loess; loessMod$plot <- T}
    }
    if(missing(reference)){
      reference <- list(plot = F)
    } else {
      if(is.logical(reference)) reference = list(plot = reference); reference$plot <- T
    }
    
    if(missing(include)) include <- unique(x$id)
    if(missing(exclude)) exclude <- NULL
    
    sub1 <- x %>%
      plotly::filter(icen==!!icen, outeq==!!outeq, pred.type==!!pred.type, block==!!block,
             id %in% include, !id %in% exclude) %>%
             filter(!is.na(obs)) %>%
      plotly::mutate(pred = pred* mult, obs = obs * mult)
    
    
    p <- sub1 %>%
      plotly::plot_ly(x = ~pred) %>%
      plotly::add_markers(y = ~obs,
                  symbol = I(marker$symbol), 
                  opacity = I(marker$opacity), 
                  size = I(marker$size), 
                  color = I(marker$color),
                  stroke = I("black"), span = I(1),
                  text = ~id,
                  hovertemplate = "Pred: %{x}<br>Obs: %{y}<br>ID: %{text}<extra></extra>") 
    
    if(linearMod$plot){
      
      if(is.null(linearMod$color)) linearMod$color <- "orange"
      if(is.null(linearMod$width)) linearMod$width <- 2
      if(is.null(linearMod$dash)) linearMod$dash <- "solid"
      lm1 <- lm(obs~pred,sub1)
      p <- p %>% plotly::add_lines(y = fitted(lm1), hoverinfo = "none", 
                           line = list(color = linearMod$color, width = linearMod$width, dash = linearMod$dash))
    } 
    
    if(loessMod$plot){
      
      if(is.null(loessMod$color)) loessMod$color <- "darkgreen"
      if(is.null(loessMod$width)) loessMod$width <- 2
      if(is.null(loessMod$dash)) loessMod$dash <- "solid"
      lo1 <- loess(obs~pred,sub1)
      p <- p %>% plotly::add_lines(y = fitted(lo1), hoverinfo = "none", 
                           line = list(color = loessMod$color, width = loessMod$width, dash = loessMod$dash))
    } 
    
    if(reference$plot){
      
      if(is.null(reference$color)) reference$color <- "grey50"
      if(is.null(reference$width)) reference$width <- 2
      if(is.null(reference$dash)) reference$dash <- "dash"
      p <- p %>% plotly::add_lines(y = ~x, hoverinfo = "none", 
                           line = list(color = reference$color, width = reference$width, dash = reference$dash))
    }
    
    
    if(log){axis_type <- "log"} else {axis_type <- "linear"}
    
    p <- p %>% plotly::layout(xaxis = list(title = "Predicted", type = axis_type), 
                      yaxis = list(title="Observed", type = axis_type), showlegend = F)
  } #end op plot
  
  
  
  
  print(p)
  return(p)
}
