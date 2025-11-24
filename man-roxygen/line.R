#' Controls characteristics of lines.
#' This argument maps to plotly line attributes.
#' `TRUE` will plot default lines. `FALSE` will suppress lines.
#' If a list, can control many line characteristics, including overriding defaults.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to traces > scatter > attributes > line to see all the ways the line
#' can be formatted. Most common will be: 
#' \itemize{
#' \item{`color`} Line color.
#' \item{`dash`} Plotting character. See `plotly::schema()`, traces > scatter > 
#' attributes > line > dash > values.
#' \item{`width`} Thickness in points.
#' } 
#' <br> 
#' <br>
#' Example: `line = list(color = "red", dash = "longdash", width = 2)`
