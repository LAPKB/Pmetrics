#' @title Compare runs
#' @description
#' ` r lifecycle::badge("stable")`
#'
#' Compare convergence, -2*log likelihood, AIC/BIC, bias,
#' imprecision, and regression statistics of population and posterior predictions.
#' Additionally, compare distributions of support points between models (see details)
#' @details
#' Objects can be specified separated by commas, e.g. `PM_compare(run1, run2, run3)`.
#' P-values are based on comparison using the nearest neighbors
#' approach if all models are non-parametrics.  Models may only be compared on
#' parameters that are included in the first model.  The P-value is the
#' comparison between each model and the first model in
#' the list.  Missing P-values are when a model has no parameter names
#' in common with the first model, and for the first model compared to itself.  
#' Significant P-values indicate
#' that the null hypothesis should be rejected, i.e. the joint distributions
#' between the two compared models for that parameter are significantly different.
#'
#' @param ... [PM_result] objects to compare.  See details.
#' @param icen Can be either "median" for the predictions based on medians of
#' `pred.type` parameter value distributions, or "mean".  Default is "median".
#' @param outeq Number of the output equation to compare; default is 1.
#' @param plot Boolean indicating whether to generate and open the comparison report; default is FALSE
#' @return A highlighted table comparing the selected models with the following columns. In each metric column,
#' the best (lowest) value is highlighted in red. In the final best column, the red highlighting applies to the model 
#' with the most "best" metrics.
#' * **run** The run number of the data
# #' * **nsub** Number of subjects in the model
#' * **nvar** Number of random parameters in the model
# #' * **par** Names of random parameters
# #' * **cycles** Number of cycles run
#' * **converged** Boolean value if convergence occurred.
#' * **-2*ll** Final cycle -2*Log-likelihood
#' * One of the following, depending on the option set in [setPMoptions]:
#'   * **aic** Final cycle Akaike Information Criterion OR
#'   * **bic** Final cycle Bayesian (Schwartz) Information Criterion
#' * **popBias** Bias, calculated by the method set in [setPMoptions], of the predictions based on `icen` population parameters
#' * **popImp** Imprecision, calculated by the method set in [setPMoptions], of the predictions based on `icen` population parameters
#' * **postBias** Bias, calculated by the method set in [setPMoptions], of the predictions based on `icen` posterior parameters
#' * **postImp** Imprecision, calculated by the method set in [setPMoptions], of the predictions based on `icen` posterior parameters
#' * **popInt** Intercept of observed vs. population predicted values regression
#' * **postInt** Intercept of observed vs. posterior predicted values regression
#' * **popSl** Slope of observed vs. population predicted values regression
#' * **postSl** Slope of observed vs. posterior predicted values regression
#' * **popR2** R-squared of observed vs. population predicted values regression
#' * **postR2** R-squared of observed vs. posterior predicted values regression
#' * **pval** P-value for each model compared to the first. See details.
#' * **best** Number of times each model was the best (lowest) in the above bias/imprecision, likelihood, and regression metrics.
#' @author Michael Neely
#' @seealso [PM_load]
#' @export

PM_compare <- function(..., icen = "median", outeq = 1, plot = FALSE) {
  
  # parse dots
  mc <- match.call(expand.dots = FALSE)
  
  # this is a list of unevaluated expressions passed in ...
  dots_exprs <- mc$...
  
  # turn each expr into text
  objNames <- vapply(dots_exprs, deparse, character(1))
  
  
  arglist <- list(...)
  
  if (length(arglist) < 2) {
    cli::cli_abort(c("x" = "You must specify at least two {.cls PM_result} objects for {.fn PM_compare}."))
  }
  if (!all(purrr::map_lgl(arglist, \(i) inherits(i, "PM_result")))) {
    cli::cli_abort(c(
      "x" = "All objects to compare must be of class {.cls PM_result}",
      "i" = "Load them beforehand with with {.fn PM_load}."
    ))
  }
  
  
  
  # argsPlot <- arglist %>% `[`(names(.) %in% names(formals(plot.PM_op))) # plot arguments
  # argsKnn <- arglist %>% `[`(names(.) %in% names(formals(mtsknn.eq))) # knn argument
  
  # set defaults if missing
  # argsKnn <- modifyList(
  #   list(k = 3, print = FALSE), # default values for mtsknn.eq
  #   argsKnn
  # )
  
  allObj <- arglist 
  nobj <- length(allObj)
  
  # check for zero cycle objects
  cycles <- unlist(sapply(allObj, function(x) max(x$cycle$data$objective$cycle)))
  if (any(cycles == 0)) {
    cli::cli_warn(c("!" = "These objects were excluded because they had no cycles: {which(cycles == 0)}."))
    allObj <- allObj %>% purrr::discard(\(x) max(x$cycle$data$objective$cycle) == 0)
  }
  
  # grab op data
  
  op <- purrr::map(allObj, function(x) {
    x$op$data
  })
  
  
  # get summaries of op for outeq
  
  get_metric <- function(this_obj){
    type <- c("bias", "imp")
    res <- purrr::map(this_obj, \(x){
      metrics <- get_metric_info(x$pe) #defined in PM_op.R
      
      if(all(is.na(metrics$metric_vals))) {
        cli::cli_abort(c("x" = "Your PM_op object is old. Please re-run the model."))
      } else {
        metrics$metric_vals
        
      }
    })
    tibble(
      bias = map_chr(res, 1),
      imp = map_chr(res, 2)
    )
    
    
  }
  
  sumobjPop <- purrr::map(op, \(x) summary.PM_op(x, outeq = outeq, pred.type = "pop", icen = icen)) 
  sumobjPost <- purrr::map(op, \(x) summary.PM_op(x, outeq = outeq, pred.type = "post", icen = icen)) 
  

  #### MAKE BIAS AND IMPRECISION PLOT ####
  
  
  sumobjBoth <- bind_rows(
    sumobjPop %>% get_metric() %>% mutate(pred.type = "pop", run = objNames[1:n()]),
    sumobjPost %>% get_metric() %>% mutate(pred.type = "post", run = objNames[1:n()])
  ) %>%
  pivot_longer(
    c(bias, imp),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = paste(pred.type, metric, sep = "_")) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(color = ifelse(pred.type == "pop", "blue", "red"))
  
  # determine if current method is percent based or not and label y axis accordingly
  if(stringr::str_detect(getPMoptions("bias_method"), "percent_")){
    sumobjBoth$value <- as.numeric(sumobjBoth$value) / 100
    tickformat <- ".0%"
  } else {
    tickformat <- NULL
  }
  
  ### calcuate p-values ###
  
  # get population points
  final <- purrr::map(allObj, function(x) {
    x$final$data
  })
  # find intersecting parameters
  popPointsRef <- final[[1]]$popPoints
  namesRef <- names(popPointsRef)
  popPointsOther <- lapply(2:nobj, function(x) final[[x]]$popPoints)
  t <- sapply(2:nobj, function(x) {
    thisPopPoints <- popPointsOther[[x - 1]]
    namesThis <- names(thisPopPoints)
    intersect <- namesRef[namesRef %in% namesThis]
    if (length(intersect) > 0) {
      popPoints1 <- popPointsRef[, intersect]
      popPoints2 <- thisPopPoints[, intersect]
      t <- do.call(mtsknn.eq, args = c(list(x = popPoints1, y = popPoints2, k = 3, print = FALSE)))$pval
    } else {
      t <- NA
    }
    signif(t, 3)
  })
  
  t <- c(NA, t)
  
  if(plot){
    x_labels <- c("Bias", "Imprecision")
    
    col_map <- c("1" = "blue", "2" = "red")
    col_vec <- col_map[ rank(unique(sumobjBoth$run)) ]
    
    
    p1 <- plot_ly() %>%
    plotly::add_trace(
      data = sumobjBoth,
      x = ~metric,
      y = ~value,
      type = "scatter",
      mode = "markers",
      color = I("black"),
      marker = list(size = 10),
      name = ~run,
      hoverinfo = "none",
      symbol = ~factor(run),
      showlegend = TRUE
    ) %>%
    plotly::add_trace(
      data = sumobjBoth,
      x = ~metric,
      y = ~value,
      type = "scatter",
      mode = "markers",
      name = ~run,
      color = ~I(color),
      symbol = ~factor(run),
      marker = list(size = 12),
      showlegend = FALSE,
      hoverinfo = "text",
      text = ~paste(run, "<br>", metric, sprintf(paste0("%.", getPMoptions("digits"), "f%%"), value * 100)
    )
  ) %>%
  plotly::layout(
    xaxis = list(
      title = "", # remove default axis title,
      tickvals = unique(sumobjBoth$metric),
      ticktext = c(
        sprintf("<span style='color:blue'>%s</span>", x_labels),
        sprintf("<span style='color:red'>%s</span>", x_labels)
      )
    ),  
    yaxis = list(
      title = "",
      tickformat = tickformat  # use percentage format if applicable
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 1.5, x1 = 1.5,
        y0 = 0, y1 = 1,
        xref = "x", yref = "paper",  # "paper" makes y0/y1 go from bottom to top regardless of axis scale
        line = list(color = gray(), dash = "dash")
      )
    ),
    annotations = list(
      list(
        x = 0.5,  # middle of first 3 categories
        y = -0.15,  # position below axis
        text = "Population",
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14),
        xanchor = "center"
      ),
      list(
        x = 2.5,  # middle of last 3 categories
        y = -0.15,
        text = "Posterior",
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14),
        xanchor = "center"
      )
    ),
    margin = list(b = 80)  # extra space at bottom for labels
  )
  
  
  ### OP plots
  
  
  all_op <- op %>% purrr::imap(\(x, y) {
    pop <- x %>% plot.PM_op(
      outeq = outeq,
      icen = icen,
      pred.type = "pop",
      print = FALSE,
      title = list(text = glue::glue("Pop - {objNames[y]}"), font = list(color = black(alpha = 0.3), size = 16)),
      stats = list(font = list(size = 10))
    )
    post <- x %>% plot.PM_op(
      outeq = outeq,
      icen = icen,
      pred.type = "post",
      print = FALSE,
      title = list(text = glue::glue("Post - {objNames[y]}"), font = list(color = black(alpha = 0.3), size = 16)),
      stats = list(font = list(size = 10))
    )
    list(pop = pop, post = post)
  }) 
  
  all_op_flat <- all_op %>% purrr::list_flatten() 
  p2 <- sub_plot(all_op_flat, nrows = nobj, titles = c(0,.8), shareX = FALSE, shareY = FALSE, print = FALSE, margin = 0.02)
  
  ### LIKELIHOOD PLOT ###
  
  
  like <- tibble(
    ll = purrr::map_dbl(allObj, \(x) {
      tail(x$cycle$data$objective$neg2ll, 1) 
    }),
    AIC = purrr::map_dbl(allObj, \(x) {
      tail(x$cycle$data$objective$aic, 1) 
    }),
    BIC = purrr::map_dbl(allObj, \(x) {
      tail(x$cycle$data$objective$bic, 1) 
    })
  )
  names(like)[1] <- "-2*LL"
  
  gamlam <- purrr::imap(allObj, \(x, y) {
    x$cycle$data$gamlam %>% filter(cycle == max(cycle)) %>%
    mutate(Run = y)
  }) %>% bind_rows() 
  
  not_same <- gamlam %>% group_by(outeq) %>%
  summarize(same_val = dplyr::n_distinct(round(value, getPMoptions("digits"))) == 1, same_type = dplyr::n_distinct(type) == 1) %>%
  filter(dplyr::if_any(dplyr::everything(), ~ .x == FALSE))
  
  if(length(not_same$outeq)>0){
    ft1 <- gamlam %>% select(Run, dplyr::everything()) %>%
    purrr::set_names(c("Run", "Cycle", "Value", "Outeq", "Type")) %>%
    mutate(Value = round2(Value)) %>%
    flextable::flextable() %>%
    flextable::theme_zebra() %>%
    flextable::align_text_col(header = TRUE) %>%
    flextable::bg(part = "header", bg = blue()) %>%
    flextable::autofit()
  } else {
    ft1 <- NULL
  }
  
  
  
  
  if (getPMoptions("ic_method") == "aic"){
    like$BIC <- NULL
  } else {
    like$AIC <- NULL
  }
  
  df <- like %>% mutate(run = objNames[1:n()]) %>%
  pivot_longer(
    1:2,
    names_to = "metric",
    values_to = "value"
  )
  
  
  p3 <- plot_ly() %>%
  plotly::add_trace(
    data = df,
    x = ~metric,
    y = ~value,
    type = "scatter",
    mode = "markers",
    color = I("black"),
    marker = list(size = 10),
    name = ~run,
    hoverinfo = "none",
    symbol = ~factor(run),
    showlegend = TRUE
  )
  
  p3 <- p3 %>%
  plotly::add_trace(
    data = df,
    x = ~metric,
    y = ~value,
    type = "scatter",
    mode = "markers",
    colors = c("blue", "red"),
    name = ~run,
    symbol = ~factor(run),
    marker = list(size = 12, color = I(col_vec)),
    showlegend = FALSE,
    hoverinfo = "text",
    text = ~glue::glue("{run}: {round2(value)}")
  )
  
  
  p3 <- p3 %>%
  plotly::layout(
    xaxis = list(
      title = "", # remove default axis title,
      tickfont = list(size = 14)
    ),  
    yaxis = list(
      title = ""
    ),
    margin = list(b = 80, l = 150, r = 150)  # extra space at bottom for labels
  )
  
  
  ### make table of parameter names
  par <- purrr::map_chr(allObj, \(x) paste(names(x$final$popMean), collapse = ", ")) %>% 
  strsplit(",\\s*") 
  
  all_par <- par %>% unlist() %>% unique() %>%
  sort()
  
  
  tbl_par <- map(par, \(x){
    all_par %in% x
    
  })%>% map(~ set_names(., all_par)) %>%
  bind_rows() %>%
  mutate(Run = objNames[1:n()]) %>%
  select(Run, dplyr::everything())
  
  
  ft2 <- tbl_par %>% 
  mutate(across(-Run, \(x){
    dplyr::if_else(row_number() == 1, 
    #row 1
    ifelse(x, "\u2705", "\u274C"),
    #other rows
    ifelse(x, "\u2611\uFE0F", "\u274C")
  )
})) %>% 
mutate(across(-Run, \(x){
  ifelse(x == "\u2611\uFE0F" & x[1] == "\u2705", "\u2705", x )
})) %>%
mutate(P = round2(t)) %>%
mutate(P = ifelse(stringr::str_trim(P) == "NA", "Ref", P)) %>%
flextable::flextable() %>%
flextable::set_header_labels(P = "P-value") %>%
flextable::theme_zebra() %>%
flextable::align_text_col(header = TRUE) %>%
flextable::bg(part = "header", bg = blue()) %>%
flextable::autofit()
} # end if plot

# make results table
results <- data.frame(
  run = objNames[1:nobj],
  # nsub = purrr::map_int(allObj, \(x) {
  #   x$final$nsub
  # }),
  nvar = purrr::map_int(allObj, \(x) length(names(x$final$popMean))),
  # par = purrr::map_chr(allObj, \(x) paste(names(x$final$popMean), collapse = ", ")),
  converged = purrr::map_lgl(allObj, \(x) {
    x$cycle$data$status == "Converged"
  }),
  ll = purrr::map_dbl(allObj, \(x) {
    tail(x$cycle$data$objective$neg2ll, 1) 
  }),
  aic = purrr::map_dbl(allObj, \(x) {
    tail(x$cycle$data$objective$aic, 1) 
  }),
  bic = purrr::map_dbl(allObj, \(x) {
    tail(x$cycle$data$objective$bic, 1) 
  }),
  popBias = sumobjBoth %>% filter(metric == "pop_bias") %>% pull(value),
  postBias = sumobjBoth %>% filter(metric == "post_bias") %>% pull(value),
  popImp = sumobjBoth %>% filter(metric == "pop_imp") %>% pull(value),
  postImp = sumobjBoth %>% filter(metric == "post_imp") %>% pull(value)
)
names(results)[4] <- "-2*LL"
row.names(results) <- 1:nobj
if (getPMoptions("ic_method") == "aic"){
  results$bic <- NULL
} else {
  results$aic <- NULL
}

op_tbl <- op %>% map(\(i) {
  i %>% filter(icen == "median") %>% group_by(pred.type) %>% 
  group_map(~ {
    fit <- lm(obs ~ pred, data = .x)
    tibble(
      int = coef(fit)[1],
      sl     = coef(fit)[2],
      r2        = summary(fit)$r.squared
    ) 
  }) %>% list_rbind()
}) %>% set_names(objNames[1:nobj]) %>% 
map(~ .x %>% mutate(pred.type = c("pop", "post"))) %>%
list_rbind(names_to = "run") %>%
pivot_wider(id_cols = c(run), names_from = c(pred.type), names_glue = "{pred.type}{stringr::str_to_title({.value})}", values_from = c(int, sl, r2))

results <- bind_cols(results, op_tbl %>% select(-run))
results$pval <- t

results$best <- results %>% select(c(-run, -nvar, -converged, -pval)) %>% map(~ which(.x == min(.x))) %>% unlist()  %>% table()
attr(results, "highlight") <- TRUE

class(results) <- c("PM_compare", "data.frame")

if(plot){
  #render and open the report
  report_list <- list(
    p1 = p1, # bias and imprecision comparison
    p2 = p2, # op comparison
    p3 = p3, # likelihood comparison
    ft1 = ft1, # gamlam comparison: NULL if equal within PMoptions digits
    ft2 = ft2, # model parameter comparison
    metric_types = sumobjPop[[1]]$pe %>% get_metric_info() %>% pluck("metric_types")
  )
  
  
  temp <- tempdir()
  rmarkdown::render(
    input = system.file("report/templates/compare.Rmd", package = "Pmetrics"),
    output_file = "compare.html",
    output_dir = temp,
    params = list(results = report_list),
    quiet = TRUE
  )
  browseURL(glue::glue("{temp}/compare.html"))
  
  
}

return(results)
}

#' @title Print method for PM_compare objects
#' @description
#' ` r lifecycle::badge("stable")`
#' Prints a summary of the PM_compare object.
#' @param x A PM_compare object.
#' @param ... Additional arguments (not used).
#' @method print PM_compare
#' @export
print.PM_compare <- function(x, ...){
  cli_df(x) # in PM_utilities
}