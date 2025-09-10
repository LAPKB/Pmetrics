#' Other attributes which can be passed to the layout > xaxis/yaxis 
#' in a plotly plot to further control formatting. 
#' Note that `log`, `xlab`, `ylab`, `xlim`, and `ylim` are 
#' all controlled by the layout object, but are provided throughout Pmetrics
#' plotly function arguments as shortcuts that map to layout elements. 
#' Therefore, the dots argument should be used to specify other aspects of the 
#' x axis, y axis, or both. See `plotly::schema()` layout > layoutAttributes 
#' > xaxis/yaxis for options. To add to single axis, name it as a list. 
#' If attributes are specified without an enclosing xaxis or yaxis list, 
#' they will be applied to both axes.<br>
#' <br>
#' Examples: 
#' \itemize{
#' \item{`NPex$data$plot(xaxis = list(tickcolor = "black", tickfont = list(family = "Arial", size = 14, color = "black"))) #applies to x axis only`}
#' \item{`NPex$data$plot(linecolor = "red", ticks = "inside") #applies to both axes`}
#' }

.
