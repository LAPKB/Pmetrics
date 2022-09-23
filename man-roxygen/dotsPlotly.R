#' Other parameters which can be passed to the layout > xaxis or
#' > yaxis attributes
#' in a plotly plot to further control axis formatting. 
#' Note that `legend`, `log`, `xlab`, `ylab`, `xlim`, and `ylim` are 
#' all controlled by the layout object, but are provided throughout Pmetrics
#' plotly function arguments as shortcuts that map to layout elements. 
#' Therefore, the dots argument should be used to specify other aspects of the 
#' xaxis or yaxis See `plotly::schema()` layout > layoutAttributes > xaxis/yaxis for options.
#' Example: `xaxis = list(tickcolor = "black", tickfont = list(family = "Arial", size = 14, color = "black")))`
.
