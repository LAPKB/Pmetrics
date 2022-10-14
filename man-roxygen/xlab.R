#' Value for x axis label.
#' This argument maps to the the xaxis title element of the layout object in plotly. 
#' It can simply be a character vector of length 1 that specifies the name of 
#' the axis, or it can be a list for greater control.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to layout > layoutAttributes > xaxis > title to see the ways to customize
#' this axis label.
#' In addition to the plotly attributes, a custom Pmetrics attribute `bold` 
#' may be included as a list element, either on its own or within the font
#' list. The default for `bold` is `TRUE`.<br>
#' <br>
#' Examples:
#' \itemize{
#' \item{`xlab = "Time (h)"`}
#' \item{`xlab = list(text = "Time", bold = F, font = list(color = "red", family = "Arial", size = 10))`}
#' \item{`xlab = list(text = "Time", font = list(bold = T))`}
#' }
  
