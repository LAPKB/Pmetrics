#' Controls the plotting symbol for observations. 
#' This argument maps to the plotly marker object.
#' It can be boolean or a list.
#' `TRUE` will plot the marker with default characteristics.
#' `FALSE` will suppress marker plotting.
#' If a list, can control many marker characteristics, including overriding defaults.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to traces > scatter > attributes > marker to see all the ways the marker
#' can be formatted. Most common will be:
#' \itemize{
#' \item{`color`} Marker color.
#' \item{`symbol`} Plotting character. See `plotly::schema()`, traces > scatter > attributes > marker > symbol > values.
#' \item{`size`} Character size in points.
#' \item{`opacity`} Ranging between 0 (fully transparent) to 1 (fully opaque).
#' \item{`line`} A list of  additional attributes governing the outline for filled shapes, most commonly
#' color and width.
#' }
#' <br>
#' <br>
#' Example: `marker = list(color = "red", symbol = "triangle", opacity = 0.8, line = list(color = "black", width = 2))`
