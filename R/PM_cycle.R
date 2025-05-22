# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ----------------------------------------------------------------

#' @title Pmetrics Run Cycle Information
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains the cycle information after a run.
#'
#' @details
#' The [PM_cycle] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for analysis and plotting of
#' cycle information generated during the run.
#'
#' Because [PM_cycle] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_cycle] objects
#' themselves.
#'
#' The main results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' You can use this `$data` field for custom manipulations, e.g. `last <- run1$cycle$data$aic %>% tail(1)`.
#' This will report the last cycle aic.
#' If you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#'
#' To provide a more traditional experience in R,
#' the `$data` field is also separated by list items into the other data fields within the R6 object,
#' e.g. `mean` or `gamlam`. This
#' allows you to access them in an S3 way, e.g. `run1$cycle$mean` if `run1` is a
#' [PM_result] object.
#'
#' @author Michael Neely, Julian Otalvaro
#' @export

PM_cycle <- R6::R6Class(
  "PM_cycle",
  public = list(
    #' @field data A list with the following elements, which can also be extracted by name.
    #' e.e. `run1$cycle$objective`, which is equivalent to `run1$cycle$data$objective`.
    #' **names** Vector of names of the random parameters
    #' **objective** A tibble of -2*Log-likelihood, AIC and BIC at each cycle
    #' **gamlam** A tibble of cycle number and gamma or lambda at each cycle for each output equation
    #' **mean** A tibble of cycle number and the mean of each random parameter at each cycle, normalized to initial mean
    #' **median** A tibble of cycle number and the median of each random parameter at each cycle, normalized to initial median
    #' **sd** A tibble of cycle number and the standard deviation of each random parameter at each cycle, normalized to initial standard deviation
    #' **converged** Boolean value if convergence occurred
    data = NULL,
    #' @description
    #' Create new object populated with  cycle information
    #' @details
    #' Creation of new `PM_cycle` object is automatic and not generally necessary
    #' for the user to do.
    #' @param PMdata include `r template("PMdata")`. 
    #' @param ... Not currently used.
    
    initialize = function(PMdata = NULL, ...) {
      self$data <- private$make(PMdata)
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_cycle].
    #' @param ... Arguments passed to [plot.PM_cycle]
    plot = function(...) {
      plot.PM_cycle(self, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_cycle].
    #' @param ... Arguments passed to [summary.PM_cycle]
    summary = function(...) {
      summary.PM_cycle(self, ...)
    }
  ), # end public
  active = list(
    #' @field names Vector of names of the random parameters
    names = function(){
      self$data$names
    },
    #' @field objective A tibble of -2*Log-likelihood, AIC and BIC at each cycle
    objective = function(){
      self$data$objective
    },
    #' @field gamlam A tibble of cycle number and gamma or lambda at each cycle for each output equation
    gamlam = function(){
      self$data$gamlam
    },
    #' @field mean A tibble of cycle number and the mean of each random parameter
    #' at each cycle, normalized to initial mean
    mean = function(){
      self$data$mean
    },
    #' @field median A tibble of cycle number and the median of each random
    #' parameter at each cycle,  normalized to initial median
    median = function(){
      self$data$median
    },
    #' @field sd A tibble of cycle number and the standard deviation of each random parameter
    #' at each cycle,  normalized to initial standard deviation
    sd = function(){
      self$data$sd
    }
  ), #end active
  private = list(
    make = function(data) {
      if (file.exists("cycles.csv")) {
        raw <- readr::read_csv(file = "cycles.csv", show_col_types = FALSE)
      } else if (inherits(data, "PM_cycle")){ #file not there, and already PM_cycle
        class(data$data) <- c("PM_cycle_data", "list")
        return(data$data)
      } else{
        cli::cli_warn(c("!" = "Unable to generate cycle information.",
                        "i" = "Result does not have valid {.code PM_cycle} object, and {.file {getwd()}/cycles.csv} does not exist."))
        return(NULL)
      }
      
      if (file.exists("obs.csv")) {
        obs_raw <- readr::read_csv(file = "obs.csv", show_col_types = FALSE)
      } else{
        cli::cli_warn(c("!" = "Unable to generate cycle information.",
                        "i" = "Result does not have valid {.code PM_cycle} object, and {.file {getwd()}/obs.csv} does not exist."))
        return(NULL)
      }
      
      if (file.exists("settings.json")) {
        config <- jsonlite::fromJSON("settings.json")
      } else{
        cli::cli_warn(c("!" = "Unable to generate cycle information.",
                        "i" = "Result does not have valid {.code PM_cycle} object, and {.file {getwd()}/settings.json} does not exist."))
        return(NULL)
      }
      
      cycle_data <- raw %>%
        pivot_longer(cols = ends_with(c("mean", "median", "sd"))) %>%
        separate_wider_delim(name, delim = ".", names = c("parameter", "statistic"))
      
      num_params <- length(names(config$random)) + length(names(config$fixed))
      
      aic <- 2 * num_params + raw$neg2ll
      names(aic) <- raw$cycle
      bic <- num_params * log(length(unique(obs_raw$id))) + raw$neg2ll
      names(bic) <- raw$cycle
      
      mean <- cycle_data %>%
        filter(statistic == "mean") %>%
        select(cycle, value, parameter) %>%
        pivot_wider(names_from = parameter, values_from = value) %>%
        arrange(cycle) %>%
        mutate(across(.cols = -cycle, .fns = function(x) {
          x / first(x)
        }))
      
      sd <- cycle_data %>%
        filter(statistic == "sd") %>%
        select(cycle, value, parameter) %>%
        pivot_wider(names_from = parameter, values_from = value) %>%
        arrange(cycle) %>%
        mutate(across(.cols = -cycle, .fns = function(x) {
          x / first(x)
        }))
      
      median <- cycle_data %>%
        filter(statistic == "median") %>%
        select(cycle, value, parameter) %>%
        pivot_wider(names_from = parameter, values_from = value) %>%
        arrange(cycle) %>%
        mutate(across(.cols = -cycle, .fns = function(x) {
          x / first(x)
        }))
      
      n_out <- length(unique(obs_raw$outeq))
      n_cyc <- max(cycle_data$cycle)
      gamlam <- tibble::as_tibble(raw$gamlam, .name_repair = "minimal")
      if (ncol(gamlam) == 1 & n_out > 1) {
        gamlam <- cbind(gamlam, replicate((n_out - 1), gamlam[, 1]))
      }
      names(gamlam) <- as.character(1:ncol(gamlam))
      gamlam <- gamlam %>%
        pivot_longer(
          cols = everything(),
          values_to = "value", names_to = "outeq"
        ) %>%
        mutate(cycle = rep(1:n_cyc, each = n_out)) %>%
        select(cycle, value, outeq)
      
      converged <- any(cycle_data$converged)
      
      res <- list(
        names = c(names(config$random), names(config$fixed), names(config$constant)),
        objective = raw %>% select(cycle, neg2ll) %>% mutate(aic = aic, bic = bic),
        gamlam = gamlam,
        mean = mean,
        sd = sd,
        median = median,
        converged = converged
      )
      class(res) <- c("PM_cycle_data", "list")
      return(res)
    }
  ) # end private
) # end R6

# PLOT --------------------------------------------------------------------

#' @title Plot Cycle Information
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plot PM_cycle objects. These objects are created by as part of a [PM_result] object
#' when [PM_load] is run.
#'
#' @method plot PM_cycle
#' @param x The name of a [PM_cycle]  object, e.g. `PmetricsData::NPex$cycle`.
#' @param line `r template("line")` Default = `list(color = "dodgerblue", width = 2, dash = "solid")`.
#' **Note** The width will apply to all plots, but `color` and `dash` will only apply
#' to the first three plots (log-likelihood, AIC, gamma/lambda). Use `colors` and `linetypes`
#' below to control the appearance
#' of the line traces for the normalized plots, because each of those traces is mapped to
#' a parameter.
#' @param marker `r template("marker")` Here, the observation controlled is the value of a given
#' trace at a specific cycle number. Default = `list(symbol = "circle", color = "dodgerblue", size = 4)`.
#' **Note** the marker color for the normalized parameter value plots will be controlled by the `colors`
#' parameter below, but size and symbol will apply to all plots.
#' @param colors to use for normalized parameter value line traces. This can be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors parameters to be plotted, as colors
#' will be interpolated to generate the correct number. The default when `color`
#' is not specified is the "Set2" palette.
#' @param linetypes to use for normalized parameter value line traces.
#' See `plotly::schema()`, traces > scatter >
#' attributes > line > dash > values.
#' An example vector could be `c("solid", "dash", "longdash")`. It is not
#' necessary to specify the same number of linetype parameters to be plotted, as they
#' will be recycled to generate the correct number. The default when `linetypes`
#' is not specified is "solid".
#' @param omit Decimal between 0 and 1 specifying the proportion of "burn-in" cycles to omit from the plots.  If missing,
#' the first 20% will be omitted.
#' @param grid `r template("grid")`
#' @param xlab Controls the formatting of the x-axis label. The text is fixed by the function and cannot be altered.
#' Use the plotly `plotly::schema()` command in the console and navigate
#' to layout > layoutAttributes > xaxis > title to see the ways to customize
#' this axis label.
#' In addition to the plotly attributes, a custom Pmetrics attribute `bold`
#' may be included as a list element, either on its own or within the font
#' list. The default for `bold` is `TRUE`.<br>
#' <br>
#' Examples:
#' \itemize{
#' \item{`xlab = list(bold = F, font = list(color = "red", family = "Arial", size = 10))`}
#' \item{`xlab = list(font = list(bold = T))`}
#' }
#' @param ylab Format for y-axis label.
#' This argument maps to the the yaxis title element of the layout object in plotly.
#' See `xlab` for details. If `xlab` is specified as a list with formatting,
#' then the formatting for the
#' `xlab` will be applied to the `ylab`. To format `ylab` independently,
#' specify a formatting list as for `xlab`.<br>
#' @param \dots Additional R plotting parameters.
#' @return Plots a panel with the following windows: -2 times the log-likelihood at each cycle, gamma/lambda at
#' each cycle; Akaike Information Criterion at each cyle and Bayesian (Schwartz) Information Criterion
#' at each cycle, the mean parameter values at each cycle (normalized to starting values); the normalized
#' standard deviation of the population distribution for each parameter at each cycle; and
#' the normalized median parameter values at each cycle.
#' @author Michael Neely
#' @seealso [PM_result], [schema]
#' @export
#' @examples
#' NPex$cycle$plot()
#' NPex$cycle$plot(omit = 0, marker = list(symbol = "cross"), line = list(width = 1))
#' NPex$cycle$plot(
#'   linetypes = "dash", colors = "Blues", marker = list(size = 1),
#'   line = list(width = 3)
#' )
#' NPex$cycle$plot(
#'   grid = FALSE,
#'   xlab = list(bold = FALSE, font = list(color = "red", family = "Arial", size = 10))
#' )
#' @family PMplots

plot.PM_cycle <- function(x,
                          line = TRUE,
                          marker = TRUE,
                          colors,
                          linetypes,
                          omit,
                          grid = TRUE,
                          xlab, ylab,
                          ...) {
  
  if (inherits(x, "PM_cycle")) {
    data <- x$data
  }
  
  # housekeeping
  
  nvar <- if (inherits(data$mean, "matrix")) {
    ncol(data$mean)
  } else {
    ncol(data$mean %>% select(-cycle))
  }
  
  line <- amendLine(line, default = list(color = "dodgerblue", width = 2))
  marker <- amendMarker(marker, default = list(
    symbol = "circle",
    color = "dodgerblue",
    size = 4, line = list(width = 0)
  ))
  
  if (missing(colors)) {
    colors <- "Set2"
  } else {
    palettes <- getPalettes() # in plotly_Utils
    if (length(colors) == 1 && !colors %in% palettes) {
      colors <- rep(colors, nvar) # ensure long enough
    }
  }
  
  if (missing(linetypes)) {
    linetypes <- rep("solid", nvar)
  } else {
    linetypes <- rep(linetypes, nvar) # ensure long enough
  }
  
  # process dots
  layout <- amendDots(list(...))
  
  # legend - not needed for this function
  layout <- modifyList(layout, list(showlegend = F))
  
  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)
  
  # axis label formatting if needed
  xlab <- if (missing(xlab)) {
    "Cycle Number"
  } else {
    modifyList(xlab, list(text = "Cycle Number"))
  }
  ylab <- if (missing(ylab)) {
    ""
  } else {
    modifyList(ylab, list(text = ""))
  }
  
  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }
  
  
  numcycles <- nrow(data$mean)
  if (missing(omit)) {
    omit <- floor(0.2 * numcycles)
  } else {
    omit <- floor(omit * numcycles)
  }
  if (omit == 0) omit <- 1
  
  include <- omit:numcycles
  
  # if (length(data$cycnum) == 0) {
  #   cycnum <- include
  # } else {
  #   cycnum <- data$cycnum[include]
  # }
  # 
  
  # LL
  graph_data <- data$objective[include,]

  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    "<b>-2 * Log-Likelihood</b>",
                                    "-2 * Log-Likelihood"
  )
  p1 <- graph_data %>%
    plotly::plot_ly(
      x = ~cycle, y = ~neg2ll,
      type = "scatter",
      mode = "markers+lines",
      line = line,
      marker = marker,
      showlegend = F,
      hovertemplate = "Cycle: %{x:i}<br>-2*LL: %{y:.3f}<extra></extra>"
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )
  
  # AIC/BIC

  
  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    "<b>AIC/BIC</b>",
                                    "AIC/BIC"
  )
  
  p2 <- graph_data %>% select(-neg2ll) %>%
    pivot_longer(cols = -cycle, names_to = "type", values_to = "value") %>%
    plotly::plot_ly(
      x = ~cycle, y = ~value, type = "scatter", mode = "lines+markers",
      color = ~type,
      colors = colors,
      line = list(width = line$width),
      linetype = ~type,
      linetypes = linetypes,
      text = ~ toupper(type),
      marker = list(size = marker$size, symbol = marker$symbol),
      hovertemplate = "Cycle: %{x:i}<br>%{text}: %{y:.3f}<extra></extra>",
      showlegend = F
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )
  
  # gamma/lambda
  layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                    "<b>Gamma/Lambda</b>",
                                    "Gamma/Lambda"
  )
  
  # amend older versions of gamma if needed
  if (is.matrix(data$gamlam)) {
    n_cyc <- max(data$cycnum)
    n_out <- 1
    data$gamlam <- tibble::as_tibble(data$gamlam, .name_repair = "minimal")
    if (ncol(data$gamlam) == 1 & n_out > 1) {
      data$gamlam <- cbind(data$gamlam, replicate((n_out - 1), data$gamlam[, 1]))
    }
    names(data$gamlam) <- as.character(1:ncol(data$gamlam))
    data$gamlam <- data$gamlam %>%
      pivot_longer(
        cols = everything(),
        values_to = "value", names_to = "outeq"
      ) %>%
      mutate(cycle = rep(1:n_cyc, each = n_out)) %>%
      select(cycle, value, outeq)
  }
  

  p3 <- data$gamlam %>%
    filter(cycle %in% include) %>%
    plotly::plot_ly(
      x = ~cycle, y = ~value, type = "scatter", mode = "lines+markers",
      color = ~outeq,
      colors = colors,
      line = list(width = line$width),
      linetype = ~outeq,
      linetypes = linetypes,
      marker = list(size = marker$size, symbol = marker$symbol),
      text = ~outeq,
      hovertemplate = "Cycle: %{x:i}<br>Gam/Lam: %{y:.3f}<br>Outeq: %{text}<extra></extra>",
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = layout$xaxis,
      yaxis = layout$yaxis
    )
  
  
  # normalized plots
  
  normalized_plot <- function(.par, range) {
    layout$yaxis$title$text <- ifelse(layout$yaxis$title$font$bold,
                                      paste0("<b>Normalized ", .par, "</b>"),
                                      paste0("Normalized ", .par)
    )
    layout$yaxis$range <- range
    
    .p <- graph_data[[.par]] %>%
      pivot_longer(cols = -cycle, names_to = "par") %>%
      plot_ly(
        x = ~cycle, y = ~value, type = "scatter", mode = "markers+lines",
        color = ~par,
        colors = colors,
        line = list(width = line$width),
        linetype = ~par,
        linetypes = linetypes,
        marker = list(size = marker$size, symbol = marker$symbol),
        text = ~par,
        hovertemplate = paste0("Cycle: %{x:i}<br>Parameter: %{text}<br>", .par, ": %{y:.3f}<br><extra></extra>"),
        showlegend = FALSE,
        legendgroup = "Normalized"
      ) %>%
      layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis
      )
    return(.p)
  }
  
  # update objects if needed
  if (is.matrix(data$mean)) {
    n_cyc <- max(data$cycnum)
    data$mean <- tibble::tibble(cycle = 1:n_cyc) %>%
      dplyr::bind_cols(tidyr::as_tibble(data$mean))
    data$median <- tibble::tibble(cycle = 1:n_cyc) %>%
      dplyr::bind_cols(tidyr::as_tibble(data$median))
    data$sd <- tibble::tibble(cycle = 1:n_cyc) %>%
      dplyr::bind_cols(tidyr::as_tibble(data$sd))
  }
  
  
  graph_data$Mean <- data$mean[include, ]
  graph_data$Median <- data$median[include, ]
  graph_data$SD <- data$sd[include, ]
  
  graph_range <- range(graph_data$Mean[, -1], graph_data$Median[, -1], graph_data$SD[, -1])
  
  
  p4 <- normalized_plot("Mean", graph_range)
  p5 <- normalized_plot("Median", graph_range)
  
  if (!all(is.na(data$sd))) {
    p6 <- normalized_plot("SD", graph_range)
  } else {
    p6 <- plotly::plotly_empty(type = "scatter", mode = "markers", showlegend = F) %>%
      layout(title = list(
        text = "Initial standard deviation = 0.\nAssay error may be too large.",
        yref = "paper",
        y = 0.5
      ))
  }
  
  
  
  p_r1 <- plotly::subplot(p1, p2, p3,
                          nrows = 1,
                          titleX = F, titleY = T,
                          margin = c(0.05, 0.05, 0, 0.05)
  )
  p_r2 <- plotly::subplot(p4, p5, p6,
                          nrows = 1,
                          titleX = T, titleY = T,
                          margin = c(0.05, 0.05, 0, 0.05)
  )
  p <- plotly::subplot(p_r1, p_r2,
                       nrows = 2,
                       titleX = T, shareX = F,
                       titleY = T,
                       margin = c(0.05, 0.05, 0, 0.05)
  ) %>%
    layout(legend = list(y = 0.4))
  
  print(p)
  return(p)
}

# SUMMARY -----------------------------------------------------------------

#' @title Summarize Final Cycle Statistics
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Summarize a Pmetrics Cycle object
#'
#' @details This is a function usually called by the `$summary()` method for [PM_cycle] objects
#' within a [PM_result] to summarize final cycle statistics. The function can
#' be called directly on a [PM_cycle] object. See examples.
#'
#' @method summary PM_cycle
#' @param object A [PM_cycle] object
#' @param cycle Cycle number to summarize. Default is last cycle.
#' @param digits Number of digits to round to. Default is 3.
#' @param ... Not used.
#' @return A list of class *summary.PM_cycle* whose elements are the last cycle values for the following fields
#' in a [PM_cycle] object.
#' * **cycle** Maximum cycle number
#' * **ll** Log likelihood
#' * **aic** Akaike Information Criterion
#' * **bic** Bayesian Information Criterion
#' * **gamlam** Value of gamma or lambda for each output equation
#' * **mean** Normalized mean parameter values compared to initial value
#' * **sd** Normalized standard deviation of parameter values compared to initial value
#' * **median** Normalized median parameter values compared to initial value
#' @author Michael Neely
#' @examples
#' NPex$cycle$summary() # preferred
#' summary(NPex$cycle) # alternative
#' @seealso [PM_cycle]
#' @export

summary.PM_cycle <- function(object, cycle = NULL, digits = 3, ...) {
  if (inherits(object, "PM_cycle")) {
    object <- object$data
  }
  
  if (is.null(cycle)) {
    cyc <- max(object$objective$cycle)
  } else {
    cyc <- cycle
  }
  if(cyc > max(object$objective$cycle)) {
    cli::cli_abort(c("x" = "Cycle number exceeds maximum cycle number."))
  }
  
  ret <- list(
    cycle = cyc,
    max_cycle = max(object$objective$cycle),
    ll = round(object$objective$neg2ll[cyc], digits),
    aic = round(object$objective$aic[cyc], digits),
    bic = round(object$objective$bic[cyc], digits),
    gamlam = object$gamlam %>% filter(cycle == cyc) %>% mutate(value = round(value, digits)),
    mean = round(object$mean[cyc, ], digits),
    sd = round(object$sd[cyc, ], digits),
    median = round(object$median[cyc, ], digits)
  )
  class(ret) <- c("summary.PM_cycle", "list")
  return(ret)
}

# PRINT SUMMARY -------------------------------------------------------------------

#' @title Print Summary of Cycle Statistics
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Print a Pmetrics Cycle Summary Object
#'
#' @details
#' Print a summary a summary.PM_cycle object
#' made by [summary.PM_cycle]. Users do not normally need to call this
#' function directly, as it will be the default method to display the object.
#'
#' @title Print Summary of Observations and Predictions
#' @method print summary.PM_cycle
#' @param x An object made by [summary.PM_cycle].
#' @param ... Not used.
#' @return A printed object.
#' @author Michael Neely
#' @seealso [summary.PM_cycle]
#' @examples
#' NPex$cycle$summary()
#' @export

print.summary.PM_cycle <- function(x, ...) {
  cat("Cycle number:", crayon::blue(x$cycle), "of", crayon::blue(x$max_cycle), "\n")
  cat("Log-likelihood:", crayon::blue(x$ll), "\n")
  cat("AIC:", crayon::blue(x$aic), "\n")
  cat("BIC:", crayon::blue(x$bic), "\n")
  cat("Gamlam", paste0("outeq ", x$gamlam$outeq, ": ", crayon::blue(x$gamlam$value), collapse = "; "), "\n")
  cat("Normalized parameter values:\n")
  par_tbl <- bind_rows(x$mean, x$sd, x$median) %>% mutate(stat = c("mean", "sd", "median"))
  cat(paste0(rep("------", ncol(par_tbl)), collapse = "-"), "\n")
  print(par_tbl)
  
  # flextable::set_flextable_defaults(font.family = "Arial")
  # ft <- flextable::flextable(par_tbl) %>% 
  # 
  #   add_header_lines(values = list(
  #     glue::glue("Cycle number: {x$cycle} of {x$max_cycle}")
  #   ))
  #   flextable::set_table_properties(width = .5) %>%
  #   flextable::theme_zebra() %>%
  #   flextable::bold(bold = FALSE, part = "footer") %>%
  #   flextable::align_text_col(align = "center", header = TRUE, footer = FALSE) %>%
  #   flextable::autofit()
  
  
  
  
}
