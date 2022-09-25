#' Plot title.
#' This argument maps to the the title layout object in plotly. 
#' It can simply be a character vector of length 1 that specifies the name of 
#' the plot title, or it can be a list for greater control.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to layout > layoutAttributes > title to see other ways to customize
#' the title using lists as additional arguments.
#' In addition to the plotly attributes, a custom Pmetrics attribute `bold` 
#' may be included as a list element. The default for `bold` is `TRUE`.<br>
#' <br>
#' Examples:
#' \itemize{
#' \item{`title = "Observed vs. Predicted"`}
#' \item{`title = list(text = "Raw Data", bold = F, font = list(color = "red", family = "Arial", size = 10))`}
#' }


