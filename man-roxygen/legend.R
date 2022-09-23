#' Controls display of legend.
#' This argument maps to the plotly layout showlegend and legend arguments.
#' It is either a boolean operator (most common) or a list of parameters to be supplied to plotly.
#' See `plotly::schema()` > layout > layoutAttributes > legend and showlegend
#' for more details on the available options for formatting. 
#' If legend is supplied as a list,  the plotly layout > layoutAttributes > showlegend value will
#' be set to `TRUE` automatically.
#' Examples:
#' * `legend = T`
#' * `legend = list(orientation = "h", font = list(color = "blue"))`
