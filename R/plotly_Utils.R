# Common plotly utilities in Pmetrics

# amend markers
amendMarker <- function(.marker, default) {
  default_marker <- list(
    symbol = "circle",
    color = red(),
    size = 10,
    opacity = 0.5,
    line = list(color = black(), width = 1)
  )
  if (!missing(default)) {
    default_marker <- modifyList(default_marker, default)
  }
  
  if (inherits(.marker, "logical")) {
    if (!.marker) {
      .marker <- default_marker
      .marker$size <- 0.1
    } else {
      .marker <- default_marker
    }
  }
  
  if (inherits(.marker, "list")) {
    .marker <- modifyList(default_marker, .marker)
  }
  return(.marker)
}

# amend lines
amendLine <- function(.line, default) {
  default_line <- list(color = blue(), width = 1, dash = "solid")
  
  if (!missing(default)) {
    default_line <- modifyList(default_line, default)
  }
  
  if (inherits(.line, "logical")) {
    if (!.line) {
      .line <- default_line
      .line$width <- 0
    } else {
      .line <- default_line
    }
  }
  
  if (inherits(.line, "list")) {
    .line <- modifyList(default_line, .line)
  }
  return(.line)
}


# amend title
amendTitle <- function(.title, default) {
  default_font <- list(family = "Arial", color = black(), bold = TRUE, size = 16)
  if (!missing(default)) {
    default_font <- modifyList(default_font, default)
  }
  
  errors <- NULL
  error <- FALSE
  
  if (is.list(.title)) {
    if (is.null(purrr::pluck(.title, "text"))) {
      stop("Missing title text element.\nSee plotly::schema() > layout > layoutAttributes > title for help.\n")
    }
    if (!is.null(purrr::pluck(.title, "size"))) {
      error <- TRUE
      errors <- "size"
      default_font$size <- .title$size
      .title$size <- NULL
    }
    if (!is.null(purrr::pluck(.title, "color"))) {
      error <- TRUE
      errors <- c(errors, "color")
      default_font$color <- .title$color
      .title$color <- NULL
    }
    if (!is.null(purrr::pluck(.title, "family"))) {
      error <- TRUE
      errors <- c(errors, "family")
      default_font$family <- .title$family
      .title$family <- NULL
    }
    
    if (!is.null(purrr::pluck(.title, "bold"))) {
      # don't report error, since not standard plotly
      default_font$bold <- .title$bold
      .title$bold <- NULL
    }
    
    if (is.null(purrr::pluck(.title, "font"))) {
      .title$font <- default_font
    } else {
      .title$font <- modifyList(default_font, .title$font)
    }
  } else { # title is simply a name
    .title <- list(text = .title, font = default_font)
  }
  
  if (.title$font$bold) {
    .title$text <- paste0("<b>", .title$text, "</b>")
  }
  
  
  if (error) {
    cat(paste0(crayon::red("Note: "), paste(errors, collapse = " and "), " should be within a font list.\nSee plotly::schema() > layout > layoutAttributes > title/xaxis/yaxis for help.\n"))
  }
  
  return(.title)
}

# amend CI
amendCI <- function(.ci, default) {
  default_ci <- list(color = blue(), dash = "dash", width = 1, opacity = 0.4)
  
  if (!missing(default)) {
    default_ci <- modifyList(default_ci, default)
  }
  
  if (inherits(.ci, "logical")) {
    if (!.ci) {
      .ci <- modifyList(default_ci, list(opacity = 0, width = 0))
    } else {
      .ci <- default_ci
    }
  }
  
  if (inherits(.ci, "list")) {
    .ci <- modifyList(default_ci, .ci)
  }
  return(.ci)
}

# amend bar
amendBar <- function(.bar, color = blue(), default) {
  default_bar <- list(color = color, width = .02, opacity = 0.75)
  
  if (!missing(default)) {
    default_bar <- modifyList(default_bar, default)
  }
  
  if (inherits(.bar, "logical")) {
    if (!.bar) {
      .bar <- default_bar
      .bar$width <- 0
    } else {
      .bar <- default_bar
    }
  }
  
  if (inherits(.bar, "list")) {
    .bar <- modifyList(default_bar, .bar)
  }
  return(.bar)
}


# make grid lines
setGrid <- function(.axis, grid = FALSE, default) {
  default_grid <- list(gridcolor = gray(alpha = 0.5), gridwidth = 1)
  if (!missing(default)) {
    default_grid <- modifyList(default_grid, default)
  }
  
  if (inherits(grid, "logical")) {
    if (grid) {
      grid <- default_grid
    } else {
      grid <- default_grid
      grid$gridcolor <- white()
      grid$gridwidth <- 0
    }
  }
  
  if (inherits(grid, "list")) {
    grid <- modifyList(default_grid, grid)
  }
  
  .axis <- modifyList(.axis, grid)
  
  return(.axis)
}



# amend the legend
amendLegend <- function(.legend, default) {
  default_legend <- list(showlegend = FALSE)
  if (!missing(default)) {
    default_legend <- modifyList(default_legend, default)
  }
  
  if (inherits(.legend, "logical")) {
    if (!.legend) {
      .legend <- default_legend
    } else {
      .legend <- default_legend
      .legend$showlegend <- TRUE
    }
  } else {
    if (inherits(.legend, "list")) {
      .legend <- modifyList(default_legend, .legend)
      .legend$showlegend <- TRUE
    }
  }
  return(.legend)
}

# amend dots
amendDots <- function(dots) {
  axesAttr <- names(plotly::schema(F)$layout$layoutAttributes$xaxis)
  
  xaxis <- purrr::pluck(dots, "xaxis") # check for additional changes
  if (is.null(xaxis)) {
    xaxis <- list()
  } else {
    xaxis <- dots$xaxis
    dots$xaxis <- NULL # take it out of dots
  }
  yaxis <- purrr::pluck(dots, "yaxis")
  if (is.null(yaxis)) {
    yaxis <- list()
  } else {
    yaxis <- dots$yaxis
    dots$yaxis <- NULL # take it out of dots
  }
  
  otherArgs <- purrr::map_lgl(names(dots), function(x) x %in% axesAttr)
  if (any(otherArgs)) {
    xaxis <- modifyList(xaxis, dots[otherArgs])
    yaxis <- modifyList(yaxis, dots[otherArgs])
  }
  
  if (!all(otherArgs)) { # some are false
    misplaced_args <- names(dots)[!otherArgs]
    misplaced_values <- as.character(dots[!otherArgs])
    args_to_check <- c("density", "lwd", "col", "lty")
    suggestion_templates <- c(
      "Did you mean {.code line = list(density = {val})}?",
      "Did you mean {.code line = list(width = {val})}?",
      "Did you mean {.code line = list(color = {val})}?",
      "Did you mean {.code line = list(dash = {val})}?"
    )
    
    # Base message
    msg <- c(
      "!" = "{.blue {paste(misplaced_args, collapse = ', ')}} {?is/are} misplaced or invalid and will be ignored."
    )
    
    # Build suggestions with map_chr
    msg <- c(
      msg,
      map_chr(seq_along(args_to_check), function(i) {
        arg <- args_to_check[i]
        if (arg %in% misplaced_args) {
          val <- misplaced_values[[arg]]
          val_expr <- paste0("{.blue ", deparse(val), "}")
          gsub("{val}", val_expr, suggestion_templates[i], fixed = TRUE)
        } else {
          ""  # No suggestion for unmatched arg
        }
      }) %>% purrr::discard(~ .x == "")  # Drop empty strings
    )
    cli::cli_div(theme = list(
      span.blue = list(color = navy())
    ))
    cli::cli_warn(msg)
    cli::cli_end()
  }
  
  layout <- list(xaxis = xaxis, yaxis = yaxis)
  return(layout)
}

includeExclude <- function(.data, include = NULL, exclude = NULL) {
  if (!is.null(include)) {
    .data <- .data %>% filter(id %in% include)
  }
  if (!is.null(exclude)) {
    .data <- .data %>% filter(!id %in% exclude)
  }
  if (nrow(.data) == 0) {
    cli::cli_abort("Include/exclude criteria result in zero subjects.")
  }
  
  return(.data)
}


notNeeded <- function(x, f) {
  cli::cli_inform(c("i" = "{.arg x} is not needed for {.fn f}.",
  "i" = "It will be ignored."))
}


#' @title Add lines to plotly plot
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Analogous to [abline], draws horizontal, vertical or sloped reference lines.
#' @details
#' This function creates a line shape that can be added a plotly plot.
#' See schema() > layout > layoutAttributes > shapes for details. Use only one
#' of the following:
#' * a and b to specify a line with intercept and slope
#' * h to specify a horizontal line with y-intercept at `h`
#' * v to specify a vertical line with x-intercept at `v`
#'
#' If using this function to add a line to an existing plot, it must be used
#' with [add_shapes]. If used for a new plot, it can be included as an element
#' in the layout list.
#'
#' @param a Intercept y value in relative coordinates, i.e. 0 (bottom) to 1 (top).
#' The x value is 0.
#' @param b Slope
#' @param h Y-intercept of horizontal line, in absolute coordinates
#' @param v X-intercept of vertical line, in absolute coordinates
#' @param line `r template("line")`.
#' Default is `line = list(color = "black", width = 1, dash = "dash")`.
#' @export
#' @seealso [add_shapes]
#' @examples 
#' \dontrun{
#' # add to an existing plot
#' NPex$op$plot() %>%
#'   add_shapes(shapes = ab_line(v = 12))
#' # add to a new plot
#' plotly::plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "lines+markers") %>%
#'   plotly::layout(shapes = ab_line(h = 5, line = list(color = "red", dash = "solid")))
#' }
ab_line <- function(a = NULL, b = NULL, h = NULL, v = NULL, line = TRUE) {
  if (!is.null(a)) {
    if (is.null(b)) {
      stop(paste0(
        "Specify both ", crayon::red("a"), " (intercept) and ",
        crayon::red("b"), " (slope)."
      ))
    }
    x0 <- 0
    y0 <- a
    x1 <- 1
    y1 <- b + a # y1 = b*x1 + a
    xref <- "paper"
    yref <- "paper"
  }
  if (!is.null(b)) {
    if (is.null(a)) {
      stop(paste0(
        "Specify both ", crayon::red("a"), " (intercept) and ",
        crayon::red("b"), " (slope)."
      ))
    }
  }
  if (!is.null(h)) {
    x0 <- 0
    x1 <- 1
    xref <- "paper"
    y0 <- h
    y1 <- h
    yref <- "y"
  }
  
  if (!is.null(v)) {
    x0 <- v
    x1 <- v
    xref <- "x"
    y0 <- 0
    y1 <- 1
    yref <- "paper"
  }
  line <- amendLine(line, default = list(color = "black", width = 1, dash = "dash"))
  return(list(
    type = "line",
    x0 = x0, y0 = y0, x1 = x1, y1 = y1,
    xref = xref, yref = yref,
    line = line
  ))
}


#' @title Add shapes to plotly plot
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Modifies the layout object of an existing plot to include a new shape.
#' @details
#' This function adds a shape to the layout element of a plotly plot.
#' Type `schema()` in the console and expand the list for
#' layout > layoutAttributes > shapes for details on how
#' to specify a shape. A convenient Pmetrics helper function to add line shapes
#' is [ab_line]. Other shapes such as circles, rectangles and paths can be added,
#' but must be done manually as outlined in the plotly schema documentation.
#' An example of a circle shape is below in examples.
#'
#' @param p The plot to which the shape should be added. Default is the
#' `last_plot()`.
#' @param shapes A list of attributes that specify a shape. Note that only one
#' shape can be added for each call, but to be consistent with the `shapes` argument
#' to [plotly::layout()], we use the same plural.
#' @export
#' @seealso [ab_line]
#' @examples
#' #'
#' \dontrun{
#' NPex$op$plot() %>%
#'   add_shapes(shapes = ab_line(v = 12))
#'
#' NPex$data$plot() %>%
#'   add_shapes(shapes = list(type = "circle", x0 = 125, y0 = 10, x1 = 135, y1 = 15))
#' }
add_shapes <- function(p = plotly::last_plot(), shapes) {
  cur_data <- p$x$cur_data
  # try different locations
  if (is.null(p$x$layoutAttrs)) { # no layout attributes
    p$x$layoutAttrs[[cur_data]] <- list(shapes = shapes)
  } else {
    nAttrs <- length(p$x$layoutAttrs)
    shapPos <- which(sapply(p$x$layoutAttrs, function(x) which("shapes" %in% names(x))) == 1)
    if (length(shapPos) > 0) { # found one
      p$x$layoutAttrs[[shapPos]]$shapes <- append(list(p$x$layoutAttrs[[shapPos]]$shapes), list(shapes))
    } else { # didn't find one
      p$x$layoutAttrs[[1]]$shapes <- list(shapes)
    }
  }
  return(p)
}

#' @title Add regression to plotly plot
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Modifies an existing plot to include a regression line with  confidence interval.
#' @details
#' This function adds a regression line to an existing  plotly plot.
#' The default is to use the x and y values in the plot, but this can be overridden
#' by specifying a data object. If another `data` object is used, values for `x` and `y`
#' must also be specified. Alternatively, the original data can be used and new columns
#' selected for regression by omitting the `data` argument and specifying new `x` and
#' `y` values. The intent of this function is to replicate the behavior of
#' `ggplot::geom_smooth()`.
#'
#' @param p The plot to which the shape should be added. Default is the
#' `plotly::last_plot()`.
#' @param x X value for regression if not the original x value of the plot. Be
#' sure to specify as a formula, e.g. `x = ~pred`.
#' @param y Y value for regression if not the original y value of the plot. Be
#' sure to specify as a formula, e.g. `y = ~obs`.
#' @param data A secondary data object to use for regression other than the
#' original plot data.
#' @param method The tegression method, currently either "lm" (the default) for
#' linear regression, or "loess" for loess regression.
#' @param span Only used for `method = "loess"`. The span parameter to control
#' the degree of smoothing. Default is 0.75.
#' @param line `r template("line")` Default is `list(color = "blue", width = 2)`.
#' The confidence interval will have the same color but at opacity 0.2.
#' @param ci Confidence interval for regressions. Default is 0.95. It can be suppressed
#' by setting to 0.
#' @param stats Add the statistics from linear regression to the plot. Ignored if
#' `method = "loess"`. If missing or
#' `FALSE`, will be suppressed. Can be set to `TRUE` which results in default format of
#' `list(x= 0.8, y = 0.1, bold = F, font = list(color = "black", family = "Arial", size = 14))`.
#' The coordinates are relative to the plot with lower left = (0,0), upper right = (1,1). This
#' argument maps to `plotly::add_text()`. It is also an option that can be set in
#' [setPMoptions] to avoid specifying this argument for every plot. The default option
#' is `TRUE`. If specified as a
#' Pmetrics option, it can be overridden for specific plots by supplying a value.
#' @export
#' @seealso [add_shapes]
#' @examples
#' \dontrun{
#' plotly::plot_ly(mtcars,
#'   x = ~hp, y = ~mpg,
#'   type = "scatter", mode = "markers", showlegend = FALSE
#' ) %>%
#'   add_smooth()
#' plotly::plot_ly(iris,
#'   x = ~Sepal.Length, y = ~Petal.Length,
#'   type = "scatter", mode = "markers", showlegend = FALSE
#' ) %>%
#'   add_smooth(method = "loess", ci = 0.9, line = list(color = "red", dash = "dash"))
#' }

add_smooth <- function(
  p = plotly::last_plot(),
  x = NULL, y = NULL, data = NULL,
  method = c("lm", "loess"),
  span = 0.75,           # only used for loess
  line = TRUE,           # TRUE -> default line spec; list(...) -> merge; FALSE -> no mean line
  ci = 0.95,             # 0 to disable
  stats = TRUE           # TRUE -> default annotation; list(x=..., y=..., font=...); FALSE -> none
) {
  method <- match.arg(method)
  
  # ---- Resolve data / aesthetics -------------------------------------------------
  get_xy <- function(p, data, x, y) {
    # prefer user-supplied data and formulas
    if (!is.null(data)) {
      if (is.null(x) || is.null(y)) stop("With `data`, supply both `x` and `y` as formulas, e.g. x=~pred, y=~obs.")
      if (!purrr::is_formula(x) || !purrr::is_formula(y)) stop("`x` and `y` must be formulas, e.g. ~pred.")
      xd <- model.frame(x, data)[,1]
      yd <- model.frame(y, data)[,1]
      return(list(df = data.frame(x = xd, y = yd), src = "user"))
    }
    # otherwise use plotly_data and optional override formulas
    pd <- plotly::plotly_data(p)
    if (!is.null(x) || !is.null(y)) {
      if (is.null(x) || is.null(y)) stop("If overriding on-plot data, supply both `x` and `y` as formulas.")
      if (!purrr::is_formula(x) || !purrr::is_formula(y)) stop("`x` and `y` must be formulas, e.g. ~pred.")
      xd <- model.frame(x, pd)[,1]
      yd <- model.frame(y, pd)[,1]
      return(list(df = data.frame(x = xd, y = yd), src = "override"))
    }
    # default: first trace mapping in plotly_data (robust across simple scatters)
    # Try common names; if missing, error
    cand_x <- c("x","X","x__","X__")
    cand_y <- c("y","Y","y__","Y__")
    cx <- cand_x[cand_x %in% names(pd)][1]
    cy <- cand_y[cand_y %in% names(pd)][1]
    if (is.na(cx) || is.na(cy)) stop("Could not infer x/y from `plotly_data(p)`. Supply `data`, `x`, and `y`.")
    data.frame(x = pd[[cx]], y = pd[[cy]]) |> (\(df) list(df=df, src="plot"))()
  }
  
  xy <- get_xy(p, data, x, y)
  vals <- xy$df
  
  # ---- Clean data ----------------------------------------------------------------
  # handle Date/POSIXct by temporarily numeric transform for modeling
  x_is_date <- inherits(vals$x, "Date")
  x_is_time <- inherits(vals$x, "POSIXct")
  x_num <- if (x_is_date) as.numeric(vals$x) else if (x_is_time) as.numeric(vals$x) else vals$x
  
  keep <- is.finite(x_num) & is.finite(vals$y)
  vals <- vals[keep, , drop = FALSE]
  x_num <- x_num[keep]
  
  if (nrow(vals) < 2) {
    warning("Not enough finite points to fit a smoother.")
    return(p)
  }
  
  # order by x for stable lines/ribbons
  o <- order(x_num, method = "radix")
  vals <- vals[o, , drop = FALSE]
  x_num <- x_num[o]
  
  # build prediction grid (unique x to avoid duplicate-vertex artifacts)
  x_pred_num <- unique(x_num)
  newdata <- data.frame(x = x_pred_num)
  
  # ---- Fit model -----------------------------------------------------------------
  fit_obj <- tryCatch({
    if (method == "lm") {
      stats::lm(y ~ x, data = data.frame(x = x_num, y = vals$y))
    } else {
      stats::loess(y ~ x, data = data.frame(x = x_num, y = vals$y), span = span, control = loess.control(surface = "direct"))
    }
  }, error = function(e) NULL)
  
  if (is.null(fit_obj)) {
    cli::cli_inform(c("i" = "Regression failed."))
    return(p)
  }
  
  # ---- Predictions + CI ----------------------------------------------------------
  have_ci <- is.numeric(ci) && ci > 0 && ci < 1
  z <- stats::qnorm(0.5 + ci/2)
  pred_fit <- pred_se <- NULL

  if (method == "lm") {
    # use predict.lm with se.fit for CI
    pr <- stats::predict(fit_obj, newdata = newdata, se.fit = have_ci)
    if (have_ci && is.list(pr)) {
      pred_fit <- as.numeric(pr$fit)
      pred_se  <- as.numeric(pr$se.fit)
    } else {
      pred_fit <- as.numeric(pr)
      pred_se  <- NULL
      have_ci  <- FALSE
    }
  } else {
    # loess: se.fit is available with surface="direct" (we set above); if it fails, drop CI
    pr <- tryCatch(stats::predict(fit_obj, newdata = newdata, se = have_ci), error = function(e) NULL)
    if (is.null(pr)) {
      pred_fit <- as.numeric(stats::predict(fit_obj, newdata = newdata))
      pred_se <- NULL
      have_ci <- FALSE
    } else {
      if (have_ci && is.list(pr)) {
        pred_fit <- as.numeric(pr$fit)
        pred_se  <- as.numeric(pr$se.fit)
      } else {
        pred_fit <- as.numeric(pr)
        pred_se  <- NULL
        have_ci  <- FALSE
      }
    }
  }
  
  if (have_ci) {
    upper <- pred_fit + z * pred_se
    lower <- pred_fit - z * pred_se
  } else {
    upper <- lower <- NULL
  }
  
  # restore x scale for plotting
  x_plot <- if (x_is_date) as.Date(x_pred_num, origin = "1970-01-01") else if (x_is_time) as.POSIXct(x_pred_num, origin = "1970-01-01", tz = attr(vals$x, "tzone")) else x_pred_num
  
  # ---- Line style ----------------------------------------------------------------
  
  line_spec <- amendLine(line, default = list(color = blue(), width = 2))
  
  # ---- Stats text (lm only) ------------------------------------------------------
  regStat <- NULL
  if (identical(method, "lm")) {
    s <- summary(fit_obj)
    r2 <- s$r.squared
    co  <- stats::coef(fit_obj)
    inter <- unname(co[1])
    slope <- unname(co[2])
    ci_inter <- ci_slope <- c(NA_real_, NA_real_)
    if (have_ci) {
      ci_mat <- tryCatch(stats::confint(fit_obj, level = ci), error = function(e) NULL)
      if (!is.null(ci_mat)) {
        ci_inter <- ci_mat[1,]
        ci_slope <- ci_mat[2,]
      }
    }
    regStat <- paste0(
      "R-squared = ", round2(r2), "<br>",
      "Intercept = ", round2(inter), if (have_ci) paste0(" (", ci*100, "% CI ", round2(ci_inter[1]), " to ", round2(ci_inter[2]), ")") else "", "<br>",
      "Slope = ", round2(slope), if (have_ci) paste0(" (", ci*100, "% CI ", round2(ci_slope[1]), " to ", round2(ci_slope[2]), ")") else ""
    )
    
    p_data <- if(!is.null(data)) {data} else {plotly::plotly_data(p)}
    
    if (inherits(p_data, "PM_op_data")) { # this came from a PM_op object
      sumStat <- summary.PM_op(p_data,
        outeq = p_data$outeq[1],
        pred.type = p_data$pred.type[1],
        icen = p_data$icen[1],
        print = FALSE
      )
      uses_percent <- c("","%")[1 + as.numeric(stringr::str_detect(get_metric_info(sumStat$pe)$metric_types$bias, "%"))]
      Bias <- glue::glue(get_metric_info(sumStat$pe)$metric_vals$Bias, uses_percent,"<br>")
      Imprecision <- glue::glue(get_metric_info(sumStat$pe)$metric_vals$Imprecision, uses_percent,"<br>")
      # get_metric_info is in PM_op.R
      
      regStat <- paste0(
        regStat, "<br>",
        "Bias = ", Bias,
        "Imprecision  = ", Imprecision
      )
    }
  }
  
  # ---- Add mean line -------------------------------------------------------------
  if (!identical(line_spec, FALSE)) {
    p <- plotly::add_lines(
      p,
      x = x_plot, y = pred_fit,
      name = if (method == "lm") "Linear Regression" else "Loess Regression",
      hoverinfo = if (!is.null(regStat)) "text" else "x+y",
      text = regStat %||% NULL,
      line = line_spec,
      inherit = FALSE,
      showlegend = TRUE
    )
  }
  
  # ---- Add CI ribbon -------------------------------------------------------------
  if (have_ci) {
    p <- plotly::add_ribbons(
      p,
      x = x_plot,
      y = pred_fit, ymin = lower, ymax = upper,
      name = paste0(round(ci*100), "% CI"),
      fillcolor = if (is.list(line_spec) && !is.null(line_spec$color)) line_spec$color else default_line$color,
      line = list(color = if (is.list(line_spec) && !is.null(line_spec$color)) line_spec$color else default_line$color),
      opacity = 0.2,
      hovertemplate = paste0(
        "%{x}<br>",
        "Fit: %{y:.3f}<br>",
        "CI Lower: %{ymin:.3f}<br>",
        "CI Upper: %{ymax:.3f}<extra>%{fullData.name}</extra>"
      ),
      inherit = FALSE,
      showlegend = TRUE
    )
  }
  
  # ---- Optional stats annotation (lm only) ---------------------------------------
  if (isTRUE(stats) && !is.null(regStat) || (is.list(stats) && !is.null(regStat))) {
    if (isTRUE(stats)) {
      stats <- list(x = 0.8, y = 0.1, font = list(color = "black", family = "Arial", size = 14))
    } else {
      if (is.null(stats$x)) stats$x <- 0.8
      if (is.null(stats$y)) stats$y <- 0.1
      if (is.null(stats$font)) stats$font <- list(family = "Arial", size = 14)
    }
    p <- plotly::layout(
      p,
      annotations = list(
        x = stats$x, y = stats$y, xref = "paper", yref = "paper",
        text = regStat, align = "left", showarrow = FALSE, font = stats$font
      )
    )
  }
  
  p
}


#' @title Export plotly plot
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Wrapper around [plotly::save_image()].
#' @details
#' This function improves the experience with the native plotly method of exporting
#' plots to static images. Much of the online documentation points towards using
#' the orca package, but the R help indicates that this method has been superseded
#' by the kaleido python package, made accesible in R via the reticulate package and
#' installation of the miniconda package manager.
#'
#' These steps are all outlined in the help for [plotly::save_image()], but one step
#' is neglected. It is necessary to execute the following line of code at the end:
#' `reticulate::py_run_string("import sys")`.
#'
#' This function will check to see that all installations are in place and offer to
#' install if not.
#'
#' Many of the arguments are the same as for `save_image` and are passed directly
#' to that function.
#'
#' @param p The plot to which the shape should be added. Default is the
#' `plotly::last_plot()`.
#' @param file A file path with a suitable file extension (png, jpg, jpeg, webp, svg, or pdf).
#' Unlike `save_image`, the `file` argument may include the full path and filename
#' other than in the current working directory.
#' @param width,height  The width/height of the exported image in pixels,
#' mutliplied by `scale`.
#' @param scale The scale factor to use when exporting the figure. Default is 1.0.
#' A scale factor larger than 1.0 will increase the image size,
#' and less than 1.0 will decrease the image size. Note that the documentation
#' for `save_image` says that this argument changes the resolution, but that is
#' not true. The resolution will remain at 72 pixels/inch (28.3 px/cm), which is
#' the default for R. To increase the resolution, export to .pdf or .svg and use
#' an external program, such as Adobe Acrobat, Acrobat Reader, or Mac Preview.
#' @param units Units for `width` and `height`. Default is pixels ("px").
#' Alternatives are inches ("in") or centimeters ("cm")
#' @param show Show the exported image in R via [base::file.show()]. If export
#' format is pdf, it will open the system default pdf viewer. Default is `TRUE`.
#' @return Plot `p` will be exported to `file` with format determined by the
#' extension for `file`.
#' @export
#' @seealso [plotly::save_image()]
#' @examples
#' \dontrun{
#' NPex$op$plot(stats = list(x = 0.9)) %>%
#'   export_plotly(file = "op.png", width = 12, height = 6, units = "in")
#' }
#' @author Michael Neely


export_plotly <- function(p, file, width = NULL, height = NULL,
  scale = NULL, units = "px", show = TRUE) {
    if (missing(p)) p <- plotly::last_plot()
    if (!inherits(p, "plotly")) stop("Specify a plotly object to be exported.\n")
    if (missing(file)) stop("Provide a file name. The extension will determine the type of export.\n")
    
    if (Sys.which("kaleido") == "") { # not installed
      cat("Pmetrics needs to install/update the kaleido python package.\n")
      cat("See ?kaleido for more information.\n")
      confirm <- readline(cat("Enter:\n<1> to continue\n<2> to abort"))
      if (confirm == 2) {
        return(invisible(NULL))
      }
      if (!"reticulate" %in% utils::installed.packages()[, 1]) {
        install.packages("reticulate")
      }
      if (reticulate::miniconda_path() == "") {
        reticulate::install_miniconda()
      }
      reticulate::conda_install("r-reticulate", "python-kaleido")
      reticulate::conda_install("r-reticulate", "plotly", channel = "plotly")
      reticulate::use_miniconda("r-reticulate")
      reticulate::py_run_string("import sys")
    }
    
    currwd <- getwd()
    
    if (grepl(.Platform$file.sep, file)) { # file is a path
      setwd(dirname(file))
      filename <- basename(file)
    } else {
      filename <- file
    }
    
    correction <- dplyr::case_when(
      units == "px" ~ 1,
      units == "in" ~ 72,
      units == "cm" ~ 28
    )
    width <- if (!is.null(width)) width * correction
    height <- if (!is.null(height)) height * correction
    
    tryCatch(plotly::save_image(p = p, file = filename, width = width, height = height, scale = scale),
    error = function(e) {
      cat("The kaleido python package does not appear to be installed properly.\n")
      cat("Try following the installation instructions in the save_image() help page.\n")
      cat(paste0(crayon::red("Note - "), "Add one more line of code after the others: ", crayon::blue("reticulate::py_run_string('import sys')\n")))
      stop("Plot not saved.\n", call. = FALSE)
    }
  )
  if (show) {
    if (!grepl("\\.pdf$", file)) { # not pdf
      file.show(file, title = file)
    } else { # pdf
      system(paste("open", file))
    }
  }
  setwd(currwd) # in case changed
}


#' @title Display multiple plotly plots
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Wrapper around [plotly::subplot()].
#' @details
#' This function addresses the deficiency with the native plotly method of combining
#' multiple plots that prevents individualized titling of subplots. The function
#' has identical arguments to [plotly::subplot()] with the addition of a `titles`
#' argument. In addition to `subplot`, the behavior of this function is two-fold:
#' * Fetch the titles (text and formatting) from each included plot
#' * Include the titles with placement per the `titles` argument
#' @param \dots One of the following
#' * any number of plotly objects
#' * a list of plotly objects.
#' Note that unlike [plotly::subplot()], ggplots and tibbles cannot be passed to
#' this function.
#' @param nrows number of rows for laying out plots in a grid-like
#' structure. Default is 1.
#' @param widths,heights Vector of relative column widths or heights
#' on a 0-1 scale. By default all columns have an equal relative width/height,
#' i.e. `c(0.5, 0.5)` for two columns, `rep(0.25, 4)` for 4 columns.
#' @param margin either a single value or a vector of four values (all between 0 and 1),
#' e.g. `c(0.05, 0.05, 0.05, 0.1)`
#' If four values are provided, the first is used as the left margin,
#' the second is used as the right margin, the third is used as the top margin,
#' and the fourth is used as the bottom margin. If a single value is provided,
#' it will be used as all four margins.
#' @param titles Include titles on individual subplots? Default is `NULL`. If
#' specified as vector of length 2, will be rendered as x and y values relative
#' to each plot. For example, `title = c(0,1)` plots the titles in the upper
#' left corner of each subplot and `title = c(1,0)` renders the titles in the
#' lower right corner. Title text and formatting will be grabbed from each
#' individual plot. To modify these characteristics, modify the code that
#' generated the individual plot.
#' @param shareX,shareY Should the x- or y- axis be shared amongst the subplots?
#' @param titleX,titleY Should x- or y- axis titles be retained?
#' @param which_layout Adopt the layout of which plot? If the default value of
#' "merge" is used, layout options found later in the sequence of plots
#' will override options found earlier in the sequence. This argument also
#' accepts a numeric vector specifying which plots to consider when merging.
#' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
#' @return A plot and plotly object combining all the plots in `...`,
#' which can be further modified.
#' @export
#' @seealso [plotly::subplot()]
#' @examples
#' \dontrun{
#' plot1 <- NPex$op$plot(title = "Posterior")
#' plot2 <- NPex$op$plot(pred.type = "pop", title = "Population")
#' sub_plot(plot1, plot2, titles = c(0, 0.95), nrows = 2)
#' }

#' @author Michael Neely

sub_plot <- function(...,
  nrows = 1,
  widths = NULL,
  heights = NULL,
  margin = 0.02,
  titles = NULL, # or (x,y)
  shareX = FALSE,
  shareY = FALSE,
  titleX = shareX,
  titleY = shareY,
  which_layout = "merge",
  print = TRUE) {
    # number of plots
    plots <- list(...)
    if (length(plots) == 1){
      if (inherits(plots, "list")){ # a list of plotly objects 
        plots <- plots %>% purrr::list_flatten()
        if (!all(purrr::map_lgl(plots, inherits, "plotly"))) {
          cli::cli_abort(c("x" = "All elements in the list must be plotly objects."))
        }
      }
    }
    n_plots <- length(plots)
    if (nrows > n_plots) nrows <- n_plots # sanity check
    
    # grab title lists from each plot and convert to annotations
    plot_annotations <- purrr::map(plots, function(p) p$x$layoutAttrs[[length(p$x$layoutAttrs)]]$title) %>%
    purrr::map(function(title) {
      list(
        text = title$text,
        font = title$font,
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "left",
        align = "right",
        x = 0, # will be replaced
        y = 0, # will be replaced
        showarrow = FALSE
      )
    })
    
    # remove titles from plots
    plots <- purrr::map(plots, function(p) {
      purrr::modify_in(p, list("x", "layoutAttrs", length(p$x$layoutAttrs), "title", "text"), \(p) "")
    })
    
    
    
    # calculate relative x and y based on plot number and rows
    if (!is.null(titles) && length(titles) == 2) { # we have x and y
      x_pos <- titles[1]
      y_pos <- titles[2]
      plots_per_row <- ceiling(n_plots / nrows)
      x_increment <- 1 / plots_per_row
      y_increment <- 1 / nrows
      x_adj <- x_pos * x_increment
      y_adj <- y_pos * y_increment
      
      x_list <- if (plots_per_row == 1) {
        x_adj
      } else {
        seq(x_adj, (plots_per_row - 1) * x_increment, x_increment)
      }
      y_list <- if (nrows == 1) {
        y_adj
      } else {
        rev(seq(y_adj, 1, y_increment))
      }
      
      x_list <- x_list + margin
      y_list <- y_list + margin
      
      plot_coords <- expand.grid(col = x_list, row = y_list) # first arg changes fastest
      
      
      plot_annotations <- purrr::map(1:n_plots, function(i) {
        purrr::list_assign(plot_annotations[[i]], x = plot_coords$col[i], y = plot_coords$row[i])
      })
    } else {
      plot_annotations <- NULL # we did not have x,y
    }
    
    p <- plotly::subplot(plots,
      nrows = nrows,
      widths = widths,
      heights = heights,
      margin = margin,
      shareX = shareX,
      shareY = shareY,
      titleX = shareX,
      titleY = shareY,
      which_layout = which_layout
    ) %>%
    plotly::layout(annotations = plot_annotations)
    if (print) print(p)
    return(invisible(p))
  }
  
  
  #' @title Get color palette
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' Generate list of palettes for plots.
  #' @details
  #' If RColorBrewer package is installed, will return the list of palette names from
  #' RColorBrewer::brewer.pal.info. If not, will return the current list as of April 2024.
  #' @return A character vector of palette names.
  #' @export
  #' @examples
  #' \dontrun{
  #' getPalettes()
  #' }
  #' @author Michael Neely
  
  getPalettes <- function() {
    if (checkRequiredPackages("RColorBrewer")) {
      palettes <- rownames(RColorBrewer::brewer.pal.info)
    } else {
      palettes <- c(
        "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
        "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1",
        "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu",
        "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn",
        "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
        "YlOrBr", "YlOrRd"
      )
    }
    return(palettes)
  }
  
  
  #' @title Get a list of default colors
  #' @description
  #' `r lifecycle::badge("stable")`
  #'
  #' Generate list of default color names.
  #' @details
  #' Used for Pmetrics plots. The following list is recycled as necessary to generate the
  #' requested number of colors: 
  #' `c(red(), green(), blue(), brown(), black(), purple(), pink(), gold(), orange(), gray())`.
  #' Each value is a function that returns a color name.
  #' @param n The number of colors to return from the list.
  #' @return A character vector of color names, which is recycled as needed.
  #'
  #' @export
  #' @examples
  #' \dontrun{
  #' getDefaultColors(6)
  #' }
  #' @author Michael Neely
  getDefaultColors <- function(n) {
    choices <- c(red(), green(), blue(), brown(), black(),
    purple(), pink(), gold(), orange(), gray())
    #choices <- c("red", "green", "blue", "brown", "black", "purple", "pink", "gold", "orange", "grey60")
    selection <- rep(choices, n)[1:n]
    return(selection)
  }
  
  ## CLICK
  #' @title Click on plotly plot to highlight traces
  #' @description
  #' `r lifecycle::badge("stable")`
  #' Adds click functionality to a plotly plot to highlight traces when clicked.
  #' @details
  #' This function modifies a plotly plot to allow clicking on traces to highlight them.
  #' Clicking on a trace will highlight it in orange (default) and dim all other traces.
  #' Clicking on the same trace again will deselect it and restore the original colors.
  #' Clicking on the background will also restore all traces to their original colors.
  #' The function uses the `htmlwidgets::onRender` function from the `htmlwidgets` package to add
  #' JavaScript code that handles the click events on the plotly plot.
  #' @param p The plotly plot to which the click functionality should be added.
  #' Default is the `plotly::last_plot()`.
  #' @param highlight_color The color to use for traces that are highlighted. Colors for non-highlighted 
  #' traces will be preserved but dimmed to 20% opacity. Default highlight color is `orange()`.
  #' @export
  #' @author Michael Neely

click_plot <- function(p, highlight_color = "#1f77b4") {
  js <- "
function(el, x) {
  const fallback = '__FALLBACK__';
  let selectedIndex = null;
  let lastClickWasPoint = false;

  // legend proxy bookkeeping
  let proxyIndex = null;
  let legendTargetIndex = null;   // the single original legend trace we hid

  const originalMarkers = x.data.map(tr => JSON.parse(JSON.stringify(tr.marker || {})));

  // ---------- color helpers ----------
  const __canvas = document.createElement('canvas');
  __canvas.width = __canvas.height = 1;
  const __ctx = __canvas.getContext('2d');

  function resolveToRGBA(colorStr) {
    if (!colorStr) return null;
    try {
      __ctx.clearRect(0,0,1,1);
      __ctx.fillStyle = '#000';
      __ctx.fillStyle = colorStr;
      const norm = __ctx.fillStyle;
      const m = /^rgba?\\(([^)]+)\\)$/i.exec(norm);
      if (!m) return hexToRGBA(colorStr);
      const parts = m[1].split(',').map(s => s.trim());
      const r = parseFloat(parts[0]), g = parseFloat(parts[1]), b = parseFloat(parts[2]);
      const a = parts[3] !== undefined ? parseFloat(parts[3]) : 1;
      return { r, g, b, a };
    } catch(e) { return hexToRGBA(colorStr); }
  }
  function hexToRGBA(hex) {
    if (typeof hex !== 'string') return null;
    let h = hex.replace('#','').trim();
    if (h.length === 3 || h.length === 4) h = h.split('').map(c => c+c).join('');
    if (h.length === 6) h += 'ff';
    if (h.length !== 8) return null;
    return { r:parseInt(h.slice(0,2),16), g:parseInt(h.slice(2,4),16),
             b:parseInt(h.slice(4,6),16), a:parseInt(h.slice(6,8),16)/255 };
  }
  function rgbToHsl(r,g,b){
    r/=255; g/=255; b/=255;
    const max=Math.max(r,g,b), min=Math.min(r,g,b);
    let h=0,s=0,l=(max+min)/2;
    if(max!==min){
      const d=max-min;
      s=l>0.5? d/(2-max-min) : d/(max+min);
      switch(max){
        case r: h=(g-b)/d+(g<b?6:0); break;
        case g: h=(b-r)/d+2; break;
        case b: h=(r-g)/d+4; break;
      }
      h*=60;
    }
    return {h,s,l};
  }
  function hslToRgb(h,s,l){
    const C=(1-Math.abs(2*l-1))*s;
    const X=C*(1-Math.abs(((h/60)%2)-1));
    const m=l-C/2;
    let r1=0,g1=0,b1=0;
    if      (0<=h && h<60)  {r1=C; g1=X; b1=0;}
    else if (60<=h && h<120){r1=X; g1=C; b1=0;}
    else if (120<=h && h<180){r1=0; g1=C; b1=X;}
    else if (180<=h && h<240){r1=0; g1=X; b1=C;}
    else if (240<=h && h<300){r1=X; g1=0; b1=C;}
    else                     {r1=C; g1=0; b1=X;}
    return { r:Math.round((r1+m)*255), g:Math.round((g1+m)*255), b:Math.round((b1+m)*255) };
  }
  function rotateHue90ToRgbaString(colorStr, fb){
    let rgba = resolveToRGBA(colorStr) || resolveToRGBA(fb);
    if(!rgba) return fb;
    const {r,g,b,a} = rgba;
    const {h,s,l} = rgbToHsl(r,g,b);
    const h2=(h+90)%360;
    const {r:rr,g:gg,b:bb} = hslToRgb(h2,s,l);
    return (a===undefined || a>=1) ? `rgb(${rr},${gg},${bb})` : `rgba(${rr},${gg},${bb},${a})`;
  }
  function perPointHighlightColors(trace){
    const mk = trace.marker || {};
    const c = mk.color;
    if (Array.isArray(c)) return c.map(ci => (ci==null)?ci:rotateHue90ToRgbaString(ci,fallback));
    if (typeof c === 'string') return rotateHue90ToRgbaString(c,fallback);
    return rotateHue90ToRgbaString(fallback,fallback);
  }

  // ---------- interactions ----------
  el.on('plotly_click', function(ev){
    lastClickWasPoint = true;
    const i = ev.points[0].fullData.index;

    // Deselect
    if (selectedIndex === i){
      for (let j=0;j<x.data.length;j++){
        Plotly.restyle(el.id, { marker: [originalMarkers[j]] }, [j]);
      }
      selectedIndex = null;
      return;
    }

    // Dim all traces
    for (let j=0;j<x.data.length;j++){
      const dimmed = Object.assign({}, originalMarkers[j], { opacity: 0.2 });
      Plotly.restyle(el.id, { marker: [dimmed] }, [j]);
    }

    // Highlight selected trace (per-point hue rotation)
    const colorHL = perPointHighlightColors(x.data[i]);
    const highlightedMarker = Object.assign({}, originalMarkers[i], { color: colorHL, opacity: 1 });
    Plotly.restyle(el.id, { marker: [highlightedMarker] }, [i]).then(() => {
      return;

    });

    selectedIndex = i;
  });

  // Background click -> restore
  el.addEventListener('click', function(){
    setTimeout(() => {
      if (!lastClickWasPoint){
        for (let j=0;j<x.data.length;j++){
          Plotly.restyle(el.id, { marker: [originalMarkers[j]] }, [j]);
        }
        selectedIndex = null;
      }
      lastClickWasPoint = false;
    }, 0);
  });

  el.addEventListener('contextmenu', e => e.preventDefault());
}
"
  js <- sub("__FALLBACK__", highlight_color, js, fixed = TRUE)
  htmlwidgets::onRender(p, js)
}





#   click_plot <- function(p, highlight_color = highlight_color) {
#     p <- htmlwidgets::onRender(p, sprintf("
# function(el, x) {
#   const highlight = '%s';
#   let selectedIndex = null;
#   let lastClickWasPoint = false;
    
#   // Deep copy of all original marker objects
#   const originalMarkers = x.data.map(trace => JSON.parse(JSON.stringify(trace.marker || {})));
    
#   el.on('plotly_click', function(data) {
#     lastClickWasPoint = true;
#     const i = data.points[0].fullData.index;
    
#     if (selectedIndex === i) {
#       // Deselect: restore all original marker settings
#       for (let j = 0; j < x.data.length; j++) {
#         Plotly.restyle(el.id, { marker: [originalMarkers[j]] }, [j]);
#       }
#       selectedIndex = null;
#     } else {
#       // Dim all traces
#       for (let j = 0; j < x.data.length; j++) {
#         const dimmedMarker = Object.assign({}, originalMarkers[j], { opacity: 0.2 });
#         Plotly.restyle(el.id, { marker: [dimmedMarker] }, [j]);
#       }
    
#       // Highlight selected trace
#       const highlightedMarker = Object.assign({}, originalMarkers[i], {
#         color: highlight,
#         opacity: 1
#       });
#       Plotly.restyle(el.id, { marker: [highlightedMarker] }, [i]);
    
#       selectedIndex = i;
#     }
#   });
    
#   // Background click restores everything
#   el.addEventListener('click', function() {
#     setTimeout(() => {
#       if (!lastClickWasPoint) {
#         for (let j = 0; j < x.data.length; j++) {
#           Plotly.restyle(el.id, { marker: [originalMarkers[j]] }, [j]);
#         }
#         selectedIndex = null;
#       }
#       lastClickWasPoint = false;
#     }, 0);
#   });
    
#   // Optional: disable right-click menu
#   el.addEventListener('contextmenu', function(e) {
#     e.preventDefault();
#   });
# }
# ", highlight_color))
#     return(p)
#   }
  
  
  
  #### RGBA to RGB
  #' @title Convert RGBA to RGB
  #' @description
  #' `r lifecycle::badge("stable")`
  #' Converts a CSS `rgba()` string to an RGB color string.
  #' @param rgba_str A string in the format `rgba(r, g, b, a)` where r, g, and b are
  #' integers between 0 and 255, and a is a float between 0 and 1.
  #' @param alpha Optional numeric value to replace the alpha channel in the rgba string.
  #' @export
  #' 
  rgba_to_rgb <- function(rgba_str, alpha = NULL) {
    # Extract the numeric parts from the rgba() string
    if(length(rgba_str)>1) rgba_str <- rgba_str[1]
    if(is.na(rgba_str) | stringr::str_detect(rgba_str, "#")) return(rgba_str)
    nums <- as.numeric(unlist(regmatches(
      rgba_str,
      gregexpr("[0-9.]+", rgba_str)
    )))
    if (length(nums) == 0) return(rgba_str)
    if (!is.null(alpha)) {
      nums[4] <- alpha #replace alpha if specified
    } 
    
    if (length(nums) != 4) stop("RGBA string must have 4 numbers.")
    
    r <- nums[1] / 255
    g <- nums[2] / 255
    b <- nums[3] / 255
    a <- nums[4]              # already 0-1 in CSS rgba()
    
    grDevices::rgb(r, g, b, alpha = a)
  }
  
  
  #' @title Convert a plotly object to ggplot
  #' @description
  #' `r lifecycle::badge("stable")`
  #' Converts a plotly object to a ggplot object. It is the inverse of [plotly::ggplotly()].
  #' @details
  #' This function extracts the data and layout from a plotly object
  #' and constructs a ggplot object with the same data.
  #' It supports various trace types including scatter, bar, and line traces.
  #' @param p A plotly object to convert.
  #' @param print If `TRUE` (the default), will print the ggplot object and invisibly return it.
  #' @return A ggplot object.
  #' @export
  #' 
  plotlygg <- function(p, print = TRUE) {
    if (inherits(p, "trelliscopejs_widget")) {
      cli::cli_abort(c("x" = "Plotting data with {.arg overlay = TRUE} results in a {.cls treliscopejs_widget} object, which cannot be converted to ggplot.",
      "i" = "Remake your plot with {.arg overlay = FALSE} and use {.arg include} or {.arg exclude} as needed."))
    }
    if (is.list(p) && !is.null(p$p) && inherits(p$p, "plotly")) {
      p <- p$p
    }

    if (!inherits(p, "plotly")){
      cli::cli_abort(c("x" = "The input must be a plotly object."))
    }
    
    # Standardize to a built plotly object that *does* expose $x$data traces
    pb <- plotly::plotly_build(p)
    traces <- pb$x$data
    lay <- pb$x$layout %||% list()
    
    if (length(traces) == 0) stop("No trace data found after plotly_build().")
    
    ####### HELPER FUNCTIONS ##########
    # safe NULL coalesce
    `%||%` <- function(a, b) if (!is.null(a) & length(a)>0) a else b
    
    
    # map plotly shapes to ggplot2
    plotly_shapes_to_gg <- function(shape){
      dplyr::case_when(
        grepl("circle", shape) ~ 21,
        grepl("square", shape) ~ 22,
        grepl("diamond",shape) ~ 23,
        grepl("triangle-up", shape) ~ 24,
        grepl("triangle-down", shape) ~ 25,
        .default = 21
      )
    }
    
    # map plotly line dash to ggplot2
    plotly_line_dash_to_gg <- function(dash) {
      dplyr::case_when(
        grepl("solid", dash) ~ "solid",
        grepl("dash", dash) ~ "dashed",
        grepl("dot",  dash) ~ "dotted",
        grepl("dashdot", dash) ~ "dotdash",
        grepl("longdash", dash) ~ "longdash",
        .default = "solid"
      )
    }
    
    
    
    ####### END HELPER FUNCTIONS ##########
    
    
    # Prepare empty ggplot
    g <- ggplot2::ggplot() + ggplot2::theme_minimal()
    
    # Bar position from layout
    bar_position <- switch(lay$barmode %||% "",
    "stack" = "stack",
    "group" = "dodge",
    "relative" = "stack",
    "overlay")  # ggplot's default is "stack"; "overlay" ~ "identity"
    
    # Title
    if (!is.null(lay$title) && !is.null(lay$title$text)) {
      
      title_text <- lay$title$text %>% stringr::str_replace_all("</*b>", "")
      title_format <- do.call(element_text, list(face = ifelse(lay$title$font$bold, "bold", "plain"),
      size = lay$title$font$size,
      family = lay$title$font$family))
      
    } else if (is.character(lay$title)) {
      title_text <- lay$title %>% stringr::str_replace_all("</*b>", "")
      title_format <- do.call(element_text, list(face = "bold", size = 16, family = "Arial"))
    }
    
    g <- g + ggplot2::ggtitle(title_text) + theme(title = title_format)
    
    # Ranges
    if (!is.null(lay$xaxis) && !is.null(lay$xaxis$range)) {
      range_x <- c(lay$xaxis$range[1], lay$xaxis$range[2])
    } else {
      range_x <- NULL
    }
    if (!is.null(lay$yaxis) && !is.null(lay$yaxis$range)) {
      range_y <- c(lay$yaxis$range[1], lay$yaxis$range[2])
    } else {
      range_y <- NULL
    }
    g <- g + ggplot2::coord_cartesian(
      xlim = range_x,
      ylim = range_y
    )
    
    # log scale the y
    if (!is.null(lay$yaxis) && !is.null(lay$yaxis$type) && lay$yaxis$type == "log") {
      log_y <- TRUE
      g <- g + ggplot2::scale_y_log10()
    } else {
      log_y <- FALSE
    }
    
    
    
    # Iterate traces
    for (i in seq_along(traces)) {
      tr <- traces[[i]]
      ttype <- tr$type %||% "scatter"
      
      # Common aesthetics
      tname <- as.character(tr$name) %||% paste0("trace", i)
      line_opacity <- tr$line$opacity %||% 1
      line_color   <- tr$line$color   %||% tr$marker$color %||% tr$marker$line$color %||% "dodgerblue" %>% rgba_to_rgb()
      line_width <- tr$line$width * 0.5 %||% 0.5
      line_dash <- plotly_line_dash_to_gg(tr$line$dash) %||% "solid"
      marker_opacity <- tr$marker$opacity %||% 1
      marker_fill_color   <- tr$marker$color %||% tr$fillcolor %||% "dodgerblue" %>% rgba_to_rgb() # fill color
      marker_size <- tr$marker$size/3 %||% 3 
      marker_shape <- plotly_shapes_to_gg(tr$marker$symbol)
      marker_line_color <- tr$marker$line$color %||% tr$marker$color %||% "#000000FF" %>% rgba_to_rgb() # stroke color
      marker_line_width <- tr$marker$line$width * 0.5 %||% 0.5 # stroke width
      
      
      
      # Build a small data.frame per trace (avoid recycling issues)
      df <- NULL
      
      if (ttype %in% c("scatter", "scattergl")) {
        # plotly scatter may have x or y missing; handle gracefully
        x <- tr$x %||% seq_along(tr$y %||% numeric())
        y <- tr$y %||% seq_along(tr$x %||% numeric())
        df <- data.frame(x = x, y = y, trace = tname, stringsAsFactors = FALSE) #%>% filter(x >= min_x, x <= max_x, y >= min_y, y <= max_y)
        
        mode <- tr$mode %||% "markers"
        has_lines   <- grepl("lines",   mode)
        has_markers <- grepl("markers", mode)
        
        if (has_lines) {
          if (stringr::str_detect(tname, "CI|ribbon")){
            g <- g + ggplot2::geom_polygon(
              data = df,
              ggplot2::aes(x = x, y = y),
              linewidth = NULL,
              fill = line_color,
              alpha = 0.2 # always set to 0.2 for ribbons
            )
            
          } else {
            g <- g + ggplot2::geom_line(
              data = df,
              ggplot2::aes(x = x, y = y),
              linewidth = line_width,
              color = line_color,
              alpha = line_opacity,
              linetype = line_dash
            )
            
          }
          
        }
        if (has_markers) {
          g <- g + ggplot2::geom_point(
            data = df,
            ggplot2::aes(x = x, y = y), 
            size = marker_size,
            shape = marker_shape,
            fill = marker_fill_color,
            alpha = marker_opacity,
            stroke = marker_line_width,
            color = marker_line_color
          )
        }
        if (!has_lines && !has_markers) {
          # Fallback
          g <- g + ggplot2::geom_point(
            data = df,
            ggplot2::aes(x = x, y = y),
            color = marker_fill_color
          )
        }
        
      } else if (ttype == "bar") {
        x <- tr$x
        y <- tr$y
        if (is.null(x) || is.null(y)) next
        df <- data.frame(x = x, y = y, trace = tname, stringsAsFactors = FALSE)
        
        g <- g + ggplot2::geom_col(
          data = df,
          ggplot2::aes(x = x, y = y, fill = trace),
          alpha = alpha_val,
          position = bar_position
        )
        
        # If a single fixed color is provided, set scale manually
        if (!is.na(fill_col)) {
          g <- g + ggplot2::scale_fill_manual(values = setNames(fill_col, tname))
        }
        
      } else if (ttype == "box") {
        # Plotly box can be vertical (y values, name is group) or horizontal (orientation='h', x values)
        orient <- tr$orientation %||% "v"
        if (orient == "h") {
          vals <- tr$x
          if (is.null(vals)) next
          df <- data.frame(group = tname, val = vals, stringsAsFactors = FALSE)
          g <- g + ggplot2::geom_boxplot(
            data = df,
            ggplot2::aes(x = group, y = val),
            alpha = alpha_val,
            outlier.shape = NA,
            fill = if (is.na(fill_col)) NULL else fill_col,
            color = if (is.na(line_col)) NULL else line_col
          )
        } else {
          vals <- tr$y
          if (is.null(vals)) next
          df <- data.frame(group = tname, val = vals, stringsAsFactors = FALSE)
          g <- g + ggplot2::geom_boxplot(
            data = df,
            ggplot2::aes(x = group, y = val),
            alpha = alpha_val,
            outlier.shape = NA,
            fill = if (is.na(fill_col)) NULL else fill_col,
            color = if (is.na(line_col)) NULL else line_col
          )
        }
        
      } else {
        # Unsupported trace: try to plot points if x/y present
        x <- tr$x; y <- tr$y
        if (!is.null(x) && !is.null(y)) {
          df <- data.frame(x = x, y = y, trace = tname, stringsAsFactors = FALSE)
          g <- g + ggplot2::geom_point(
            data = df,
            ggplot2::aes(x = x, y = y),
            alpha = alpha_val,
            color = if (is.na(fill_col)) NULL else fill_col
          )
        } else {
          warning("Trace ", i, " of type '", ttype, "' not supported; skipping.")
        }
      }
    }
    
    # Axes titles (best effort)
    if (!is.null(lay$xaxis) && !is.null(lay$xaxis$title)) {
      if (is.list(lay$xaxis$title)){
        x_text <- lay$xaxis$title$text %>% stringr::str_replace_all("</*b>", "")
        x_format <- do.call(element_text, list(face = ifelse(lay$xaxis$title$font$bold, "bold", "plain"),
        size = lay$xaxis$title$font$size,
        family = lay$xaxis$title$font$family))
        
      } else {
        x_text <- lay$xaxis$title %>% stringr::str_replace_all("</*b>", "")
        x_format <- do.call(element_text, list(face = "bold", size = 16, family = "Arial"))
      }
      
      g <- g + ggplot2::xlab(x_text) + theme(axis.title.x = x_format)
    }
    
    if (!is.null(lay$yaxis) && !is.null(lay$yaxis$title)) {
      if (is.list(lay$yaxis$title)){
        y_text <- lay$yaxis$title$text %>% stringr::str_replace_all("</*b>", "")
        y_format <- do.call(element_text, list(face = ifelse(lay$yaxis$title$font$bold, "bold", "plain"),
        size = lay$yaxis$title$font$size ,
        family = lay$yaxis$title$font$family))
        
      } else {
        y_text <- lay$yaxis$title %>% stringr::str_replace_all("</*b>", "")
        y_format <- do.call(element_text, list(face = "bold", size = 16, family = "Arial"))
      }
      
      g <- g + ggplot2::ylab(y_text) + theme(axis.title.y = y_format)
    }
    
    
    ####### Legends and annotations
    
    
    # Legend (only used for groups in plot.PM_data)
    if (lay$showlegend){
      group_df <- traces %>% map(~c(
        line_color = .x$line$color   %||% .x$marker$color %||% .x$marker$line$color %||% "dodgerblue" %>% rgba_to_rgb(),
        line_width = .x$line$width * 0.5 %||% 0.5,
        line_dash = plotly_line_dash_to_gg(.x$line$dash),
        line_opacity = .x$line$opacity %||% 1,
        marker_fill_color = .x$marker$color %||% .x$fillcolor %||% "dodgerblue" %>% rgba_to_rgb(), # fill color
        marker_size = .x$marker$size / 3 %||% 3, 
        marker_shape = plotly_shapes_to_gg(.x$marker$symbol),
        marker_opacity = .x$marker$opacity %||% 1,
        marker_line_color = .x$marker$line$color %||% .x$marker$color %||% "#000000FF" %>% rgba_to_rgb(), # stroke color
        marker_line_width = .x$marker$line$width * 0.5 %||% 0.5, # stroke width
        group = as.character(.x$name))) %>% 
        bind_rows() %>% 
        mutate(marker_shape = as.integer(marker_shape)) %>% 
        mutate(across(contains(c("width", "size", "opacity")), as.numeric)) %>%
        distinct()
        
        
        if (is.null(lay$legend)) {
          lay$legend <- list(x = 1, y = 1) # default position
        }
        max_chr <- max(nchar(group_df$group)) + 1
        
        for(i in seq_along(group_df$group)) {
          g <- g +
          ggplot2::annotation_custom(
            grob = grid::pointsGrob(
              x = grid::unit(lay$legend$x - (0.012 * max_chr), "npc"),
              y = grid::unit(lay$legend$y - (i * 0.03), "npc"),
              pch = group_df$marker_shape[i],
              size = grid::unit(group_df$marker_size[i], "mm"),
              gp = grid::gpar(
                col = group_df$marker_line_color[i],
                fill = group_df$marker_fill_color[i],
                alpha = group_df$marker_opacity[i],
                lwd = group_df$marker_line_width[i])
              )
            ) +
            ggplot2::annotation_custom(
              grob = grid::linesGrob(
                x = grid::unit(
                  c(lay$legend$x - (0.012 * max_chr) - 0.012,
                  lay$legend$x - (0.012 * max_chr) + 0.012), 
                  "npc"),
                  y = grid::unit(
                    c(lay$legend$y - (i * 0.03),
                    lay$legend$y - (i * 0.03)), 
                    "npc"),
                    gp = grid::gpar(
                      col = group_df$line_color[i],
                      lwd = group_df$line_width[i],
                      lty = group_df$line_dash[i],
                      alpha = group_df$line_opacity[i]
                    )
                  )
                ) +
                ggplot2::annotation_custom(
                  grob = grid::textGrob(
                    label = group_df$group[i],
                    x = grid::unit(lay$legend$x - (0.01 * max_chr), "npc"),
                    y = grid::unit(lay$legend$y - (i * 0.03), "npc"),
                    hjust = 0,
                    gp = grid::gpar(
                      fontsize = 10,
                      col = "black"
                    )
                  )
                )
                
              }
            }
            
            
            
            
            # grid
            if (is.null(lay$xaxis$gridwidth) || lay$xaxis$gridwidth == 0){
              g <- g + theme(panel.grid = element_blank())
            }
            
            # shapes (reference lines)
            if (!is.null(lay$shapes) && length(lay$shapes) > 0) {
              shapes <- lay$shapes
              for (shape in shapes) {
                if (shape$type == "line") {
                  g <- g + ggplot2::annotation_custom(
                    grid::linesGrob(
                      x = grid::unit(c(shape$x0, shape$x1), "npc"),
                      y = grid::unit(c(shape$y0, shape$y1), "npc"),
                      gp = grid::gpar(
                        col = shape$line$color %||% "grey",
                        lwd = shape$line$width  %||% 1,
                        lty = plotly_line_dash_to_gg(shape$line$dash)
                      )
                    )
                  )
                } else if (shape$type == "rect") {
                  g <- g + ggplot2::annotation_custom(
                    grid::rectGrob(
                      x = grid::unit(shape$x0, "npc"),
                      y = grid::unit(shape$y0), "npc"),
                      width = grid::unit(shape$x1 - shape$x0, "npc"),
                      height = grid::unit(shape$y1 - shape$y0, "npc"),
                      gp = grid::gpar(
                        fill = shape$fillcolor %||% "transparent",
                        col = shape$line$color %||% "black",
                        lwd = shape$line$width %||% 1,
                        alpha = shape$opacity %||% 0.2
                      )
                    )
                  }
                }
              }
              
              # annotations
              if (!is.null(lay$annotations) && length(lay$annotations) > 0) {
                annots <- lay$annotations
                for (annot in annots) {
                  g <- g + ggplot2::annotation_custom(
                    grid::textGrob(
                      label = stringr::str_replace_all(annot$text, "<br>", "\n"),
                      x = grid::unit((annot$x - 0.3) %||% 0.5, "npc"),
                      y = grid::unit(annot$y %||% 0.5, "npc"),
                      hjust = 0,
                      vjust = 0,
                      gp = grid::gpar(
                        fontsize = annot$font$size / 1.4 %||% 10,
                        col = annot$font$color %||% "black",
                        fontfamily = annot$font$family %||% "Arial"
                      )
                    )
                  )
                }
              }
              
              g <- g + theme(axis.text = element_text(size = 10))
              
              if(print) print(g)
              return(invisible(g))
            }



#' @title Find opposite color with max contrast
#' @description
#' `r lifecycle::badge("stable")`
#' Finds an opposite color with maximum contrast to the input color.
#' @details
#' This function takes a color input (name or hex) and returns an opposite color
#' using one of three methods:
#' - "complement": strict 180 degrees hue complement
#' - "complement_maxcontrast": 180 degrees hue complement adjusted for maximum contrast
#' - "bw_maxcontrast": black or white, whichever has higher contrast
#' The function uses the WCAG relative luminance and contrast ratio formulas to determine contrast.
#' @param col A color name or hex string (e.g. "red", "#FF0000", "#FF0000FF").
#' @param method The method to use for finding the opposite color.
#' One of "complement", "complement_maxcontrast", or "bw_maxcontrast".
#' Default is "complement".
#' @param degrees The degree offset for the hue complement. Default is 180.
#' @return A hex color string in the format "#RRGGBBAA".
#' @export
opposite_color <- function(col,
                           method = c("complement", "complement_maxcontrast", "bw_maxcontrast"),
                          degrees = 180) {
  method <- match.arg(method)
  degree_offset <- degrees / 360

  # --- helpers ---
  clamp01 <- function(x) pmax(0, pmin(1, x))
  rgb01_to_hex <- function(r, g, b, a = 1) {
    grDevices::rgb(r, g, b, alpha = a, maxColorValue = 1)
  }

  # parse any color name or hex into rgb 0-1 + alpha
  parse_col <- function(c) {
    if (grepl("^#", c)) {
      hex <- toupper(c)
      hex <- sub("^#", "", hex)
      if (nchar(hex) == 8) {         # RRGGBBAA
        r <- strtoi(substr(hex, 1, 2), 16L)/255
        g <- strtoi(substr(hex, 3, 4), 16L)/255
        b <- strtoi(substr(hex, 5, 6), 16L)/255
        a <- strtoi(substr(hex, 7, 8), 16L)/255
      } else if (nchar(hex) == 6) {  # RRGGBB
        r <- strtoi(substr(hex, 1, 2), 16L)/255
        g <- strtoi(substr(hex, 3, 4), 16L)/255
        b <- strtoi(substr(hex, 5, 6), 16L)/255
        a <- 1
      } else {
        stop("Unsupported hex format: use #RRGGBB or #RRGGBBAA")
      }
      c(r, g, b, a)
    } else {
      rgb <- as.vector(grDevices::col2rgb(c))/255
      c(rgb, 1)
    }
  }

  # WCAG relative luminance & contrast ratio (ignores alpha for contrast)
  rel_lum <- function(rgb01) {
    f <- function(c) ifelse(c <= 0.03928, c/12.92, ((c+0.055)/1.055)^2.4)
    x <- f(rgb01[1:3])
    0.2126*x[1] + 0.7152*x[2] + 0.0722*x[3]
  }
  contrast_ratio <- function(c1, c2) {
    L1 <- rel_lum(parse_col(c1))
    L2 <- rel_lum(parse_col(c2))
    Lh <- max(L1, L2); Ls <- min(L1, L2)
    (Lh + 0.05) / (Ls + 0.05)
  }
  ## cycle through colors
  op_cols <- purrr::map(col, \(x){
      base <- parse_col(x)
      rgb <- base[1:3]; alpha <- base[4]

      hsv <- grDevices::rgb2hsv(matrix(rgb, nrow = 3), maxColorValue = 1)
      h <- hsv[1, 1]; s <- hsv[2, 1]; v <- hsv[3, 1]

      # strict 180 degrees hue complement
      comp <- grDevices::hsv((h + degree_offset) %% 1, s, v, alpha = alpha)
      if(nchar(x)==7) comp <- substr(comp, 1, 7)  # drop alpha if input had none

      if (method == "complement") {
        return(toupper(comp))
      }

      if (method == "bw_maxcontrast") {
        c_black <- contrast_ratio(col, "#000000")
        c_white <- contrast_ratio(col, "#FFFFFF")
        return(if (c_white >= c_black) "#FFFFFFFF" else "#000000FF")
      }

      # method == "complement_maxcontrast"
      best <- list(hex = comp, cr = contrast_ratio(x, comp))

      h2 <- (h + degree_offset) %% 1
      v_grid <- seq(0, 1, by = 0.01)
      s_grid <- clamp01(s * c(0.6, 0.8, 1.0, 1.2))

      for (sv in s_grid) {
        for (vv in v_grid) {
          cand <- grDevices::hsv(h2, sv, vv, alpha = alpha)
          cr <- contrast_ratio(x, cand)
          if (cr > best$cr) best <- list(hex = toupper(cand), cr = cr)
        }
      }
      best$hex
  })
  return(unlist(op_cols))
  
}


