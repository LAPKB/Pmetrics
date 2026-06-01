pmetrics_live_session_config <- function() {
  config <- getOption("Pmetrics.live_session", NULL)
  if (!is.list(config)) {
    return(NULL)
  }

  session_id <- config$session_id
  host <- config$host
  port <- suppressWarnings(as.integer(config$port[[1]]))

  if (is.null(session_id) || !length(session_id) || !nzchar(session_id[[1]])) {
    return(NULL)
  }

  if (is.null(host) || !length(host) || !nzchar(host[[1]])) {
    return(NULL)
  }

  if (is.na(port) || port <= 0) {
    return(NULL)
  }

  list(
    session_id = as.character(session_id[[1]]),
    host = as.character(host[[1]]),
    port = port
  )
}

pmetrics_live_default <- function(value, default) {
  if (is.null(value) || !length(value)) {
    default
  } else {
    value
  }
}

pmetrics_same_live_session_config <- function(x, y) {
  is.list(x) &&
    is.list(y) &&
    identical(x$session_id, y$session_id) &&
    identical(x$host, y$host) &&
    identical(as.integer(x$port), as.integer(y$port))
}

pmetrics_live_session_runtime <- local({
  runtime <- new.env(parent = emptyenv())
  runtime$config <- NULL
  runtime$connection <- NULL
  runtime$state <- NULL
  runtime
})

pmetrics_get_live_session_state <- function(config) {
  if (is.null(config)) {
    return(NULL)
  }

  if (!pmetrics_same_live_session_config(pmetrics_live_session_runtime$config, config) || is.null(pmetrics_live_session_runtime$state)) {
    pmetrics_live_session_runtime$config <- config
    pmetrics_live_session_runtime$state <- pmetrics_new_live_session_state(config)
  }

  pmetrics_live_session_runtime$state
}

pmetrics_set_live_session_state <- function(config, state) {
  pmetrics_live_session_runtime$config <- config
  pmetrics_live_session_runtime$state <- state
  invisible(state)
}

pmetrics_open_live_session_connection <- function(config) {
  socketConnection(
    host = config$host,
    port = config$port,
    blocking = FALSE,
    open = "r+",
    timeout = 1
  )
}

pmetrics_close_live_session_connection <- function(connection) {
  if (is.null(connection)) {
    return(invisible(FALSE))
  }

  try(close(connection), silent = TRUE)
  invisible(TRUE)
}

pmetrics_prime_live_session_connection <- function(config) {
  if (is.null(config)) {
    return(NULL)
  }

  if (pmetrics_same_live_session_config(pmetrics_live_session_runtime$config, config) && !is.null(pmetrics_live_session_runtime$connection)) {
    return(pmetrics_live_session_runtime$connection)
  }

  pmetrics_close_live_session_connection(pmetrics_live_session_runtime$connection)
  pmetrics_live_session_runtime$config <- config
  pmetrics_live_session_runtime$state <- pmetrics_new_live_session_state(config)
  pmetrics_live_session_runtime$connection <- tryCatch(
    pmetrics_open_live_session_connection(config),
    error = function(e) NULL
  )

  if (is.null(pmetrics_live_session_runtime$connection)) {
    pmetrics_live_session_runtime$state <- modifyList(
      pmetrics_live_session_runtime$state,
      list(connection = "failed", status = "Unable to connect to live session")
    )
  }

  pmetrics_live_session_runtime$connection
}

pmetrics_parse_live_session_message <- function(line) {
  tryCatch(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

pmetrics_read_live_session_messages <- function(connection, max_messages = 50L) {
  if (is.null(connection)) {
    return(list())
  }

  ready <- tryCatch(socketSelect(list(connection), timeout = 0), error = function(e) list(FALSE))
  if (!length(ready) || !isTRUE(ready[[1]])) {
    return(list())
  }

  lines <- tryCatch(
    readLines(connection, n = as.integer(max_messages), warn = FALSE),
    error = function(e) character()
  )

  Filter(Negate(is.null), lapply(lines, pmetrics_parse_live_session_message))
}

pmetrics_empty_live_objective_history <- function() {
  data.frame(
    cycle = integer(),
    neg2ll = numeric(),
    objective_delta = numeric(),
    cycle_elapsed_ms = numeric(),
    total_elapsed_ms = numeric(),
    nspp = integer(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

pmetrics_empty_live_error_model_history <- function() {
  data.frame(
    cycle = integer(),
    outeq = integer(),
    kind = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )
}

pmetrics_empty_live_parameter_history <- function() {
  data.frame(
    cycle = integer(),
    parameter = character(),
    mean = numeric(),
    median = numeric(),
    sd = numeric(),
    stringsAsFactors = FALSE
  )
}

pmetrics_new_live_session_state <- function(config) {
  list(
    view = "live_monitor",
    run_state = "connecting",
    connection = "connecting",
    session_id = config$session_id,
    status = "Waiting for fit",
    cycle = NA_integer_,
    neg2ll = NA_real_,
    cycle_elapsed_ms = NA_real_,
    total_elapsed_ms = NA_real_,
    nspp = NA_integer_,
    last_event = "waiting_for_fit",
    last_update = as.POSIXct(NA),
    pending_command = NULL,
    command_status = "Pause takes effect before the next cycle. Stop waits for the current cycle to finish.",
    objective_history = pmetrics_empty_live_objective_history(),
    error_model_history = pmetrics_empty_live_error_model_history(),
    parameter_history = pmetrics_empty_live_parameter_history(),
    events = list()
  )
}

pmetrics_append_live_session_event <- function(state, label, level = "info") {
  events <- c(state$events, list(list(label = label, level = level, at = Sys.time())))
  if (length(events) > 20) {
    events <- tail(events, 20)
  }
  state$events <- events
  state$last_update <- Sys.time()
  state
}

pmetrics_normalize_live_status_value <- function(status) {
  if (is.null(status) || !length(status)) {
    return("")
  }

  if (is.character(status)) {
    return(as.character(status[[1]]))
  }

  if (is.list(status)) {
    if (length(status) == 1L && length(names(status)) == 1L && nzchar(names(status)[[1]])) {
      return(trimws(paste(names(status)[[1]], pmetrics_normalize_live_status_value(status[[1]]))))
    }

    values <- vapply(status, pmetrics_normalize_live_status_value, character(1), USE.NAMES = FALSE)
    values <- values[nzchar(values)]
    return(paste(values, collapse = " "))
  }

  as.character(status[[1]])
}

pmetrics_upsert_live_objective_history <- function(history, event) {
  cycle <- as.integer(pmetrics_live_default(event$cycle, NA_integer_))
  history <- history[history$cycle != cycle, , drop = FALSE]

  row <- data.frame(
    cycle = cycle,
    neg2ll = as.numeric(pmetrics_live_default(event$neg2ll, NA_real_)),
    objective_delta = as.numeric(pmetrics_live_default(event$objective_delta, NA_real_)),
    cycle_elapsed_ms = as.numeric(pmetrics_live_default(event$cycle_elapsed_ms, NA_real_)),
    total_elapsed_ms = as.numeric(pmetrics_live_default(event$total_elapsed_ms, NA_real_)),
    nspp = as.integer(pmetrics_live_default(event$nspp, NA_integer_)),
    status = pmetrics_normalize_live_status_value(pmetrics_live_default(event$status, "running")),
    stringsAsFactors = FALSE
  )

  history <- rbind(history, row)
  history[order(history$cycle), , drop = FALSE]
}

pmetrics_upsert_live_error_model_history <- function(history, event) {
  cycle <- as.integer(pmetrics_live_default(event$cycle, NA_integer_))
  history <- history[history$cycle != cycle, , drop = FALSE]

  error_models <- pmetrics_live_default(event$error_models, list())
  if (!length(error_models)) {
    return(history)
  }

  rows <- do.call(
    rbind,
    lapply(error_models, function(model) {
      data.frame(
        cycle = cycle,
        outeq = as.integer(pmetrics_live_default(model$outeq, NA_integer_)),
        kind = tolower(as.character(pmetrics_live_default(model$kind, "gamma"))),
        value = as.numeric(pmetrics_live_default(model$value, NA_real_)),
        stringsAsFactors = FALSE
      )
    })
  )

  history <- rbind(history, rows)
  history[order(history$cycle, history$outeq), , drop = FALSE]
}

pmetrics_upsert_live_parameter_history <- function(history, event) {
  cycle <- as.integer(pmetrics_live_default(event$cycle, NA_integer_))
  history <- history[history$cycle != cycle, , drop = FALSE]

  parameters <- pmetrics_live_default(event$parameters, list())
  if (!length(parameters)) {
    return(history)
  }

  rows <- do.call(
    rbind,
    lapply(parameters, function(parameter) {
      data.frame(
        cycle = cycle,
        parameter = as.character(pmetrics_live_default(parameter$name, "parameter")),
        mean = as.numeric(pmetrics_live_default(parameter$mean, NA_real_)),
        median = as.numeric(pmetrics_live_default(parameter$median, NA_real_)),
        sd = as.numeric(pmetrics_live_default(parameter$sd, NA_real_)),
        stringsAsFactors = FALSE
      )
    })
  )

  history <- rbind(history, rows)
  history[order(history$cycle, history$parameter), , drop = FALSE]
}

pmetrics_live_stage_label <- function(state) {
  if (identical(state$view, "live_monitor")) {
    "Live status"
  } else {
    "Connecting"
  }
}

pmetrics_live_run_state_label <- function(state) {
  switch(pmetrics_live_default(state$run_state, "connecting"),
    connecting = "Connecting",
    running = "Running",
    paused = "Paused",
    stopping = "Stop requested",
    stopped = "Stopped",
    completed = "Completed",
    failed = "Failed",
    closed = "Closed",
    tools::toTitleCase(gsub("_", " ", pmetrics_live_default(state$run_state, "connecting"), fixed = TRUE))
  )
}

pmetrics_live_error_model_label <- function(state) {
  history <- state$error_model_history
  if (!nrow(history)) {
    return("Gamma/Lambda")
  }

  kinds <- unique(stats::na.omit(history$kind))
  if (length(kinds) != 1L) {
    return("Gamma/Lambda")
  }

  if (identical(kinds[[1]], "lambda")) "Lambda" else "Gamma"
}

pmetrics_live_error_model_type <- function(state) {
  if (identical(pmetrics_live_error_model_label(state), "Lambda")) {
    "Additive"
  } else {
    "Proportional"
  }
}

pmetrics_format_live_elapsed <- function(total_elapsed_ms) {
  if (is.null(total_elapsed_ms) || !length(total_elapsed_ms) || is.na(total_elapsed_ms)) {
    return("Waiting")
  }

  seconds <- as.integer(round(as.numeric(total_elapsed_ms) / 1000))
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  secs <- seconds %% 60

  if (hours > 0) {
    sprintf("%02d:%02d:%02d", hours, minutes, secs)
  } else {
    sprintf("%02d:%02d", minutes, secs)
  }
}

pmetrics_live_parameter_metric_table <- function(state, metric) {
  history <- state$parameter_history
  if (!nrow(history)) {
    return(data.frame())
  }

  metric <- match.arg(metric, choices = c("mean", "median", "sd"))
  cycles <- sort(unique(history$cycle))
  parameters <- sort(unique(history$parameter))

  rows <- lapply(cycles, function(cycle) {
    cycle_rows <- history[history$cycle == cycle, , drop = FALSE]
    values <- stats::setNames(as.list(rep(NA_real_, length(parameters))), parameters)

    for (index in seq_len(nrow(cycle_rows))) {
      values[[cycle_rows$parameter[[index]]]] <- cycle_rows[[metric]][[index]]
    }

    data.frame(
      cycle = cycle,
      as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

pmetrics_live_cycle_result <- function(state) {
  error_model_history <- state$error_model_history
  gamlam <- if (nrow(error_model_history)) {
    transform(
      error_model_history[, c("cycle", "outeq", "value"), drop = FALSE],
      type = pmetrics_live_error_model_type(state)
    )
  } else {
    data.frame(cycle = integer(), outeq = integer(), value = numeric(), type = character(), stringsAsFactors = FALSE)
  }

  list(
    cycle = list(
      objective = state$objective_history[, c("cycle", "neg2ll"), drop = FALSE],
      gamlam = gamlam,
      mean = pmetrics_live_parameter_metric_table(state, "mean"),
      median = pmetrics_live_parameter_metric_table(state, "median"),
      sd = pmetrics_live_parameter_metric_table(state, "sd"),
      data = list(gamlam = list(type = pmetrics_live_error_model_type(state)))
    )
  )
}

pmetrics_latest_live_error_model_table <- function(state) {
  history <- state$error_model_history
  if (!nrow(history)) {
    return(data.frame())
  }

  latest_cycle <- max(history$cycle, na.rm = TRUE)
  latest <- history[history$cycle == latest_cycle, , drop = FALSE]

  data.frame(
    Outeq = latest$outeq,
    Kind = tools::toTitleCase(latest$kind),
    Value = latest$value,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pmetrics_latest_live_parameter_table <- function(state) {
  history <- state$parameter_history
  if (!nrow(history)) {
    return(data.frame())
  }

  latest_cycle <- max(history$cycle, na.rm = TRUE)
  latest <- history[history$cycle == latest_cycle, , drop = FALSE]

  data.frame(
    Parameter = latest$parameter,
    Mean = latest$mean,
    Median = latest$median,
    SD = latest$sd,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pmetrics_live_html_table <- function(data, digits = 3) {
  format_cell <- function(x) {
    if (is.numeric(x)) {
      formatC(x, digits = digits, format = "fg", flag = "#")
    } else if (is.logical(x)) {
      ifelse(x, "TRUE", "FALSE")
    } else {
      as.character(x)
    }
  }

  rows <- apply(data, 1, function(row) {
    htmltools::tags$tr(
      lapply(row, function(cell) htmltools::tags$td(format_cell(cell)))
    )
  })

  htmltools::tags$table(
    class = "table table-sm table-striped table-hover",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(data), htmltools::tags$th))),
    htmltools::tags$tbody(rows)
  )
}

pmetrics_build_live_empty_plot <- function(title) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title)
}

pmetrics_build_cycle_objective_plot <- function(res, metric = "neg2ll", gamlam_label = "Gamma") {
  if (is.null(res$cycle$objective)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No cycle objective data available."))
  }

  data <- as.data.frame(res$cycle$objective)
  if (!"cycle" %in% names(data)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Cycle objective data incomplete."))
  }

  metric <- tolower(metric)

  if (metric == "gamlam") {
    if (is.null(res$cycle$gamlam)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No gamma/lambda cycle data available."))
    }

    gamlam_df <- as.data.frame(res$cycle$gamlam)
    if (!all(c("cycle", "value") %in% names(gamlam_df))) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Gamma/lambda cycle data incomplete."))
    }

    ggplot2::ggplot(gamlam_df, ggplot2::aes(x = cycle, y = value)) +
      ggplot2::geom_line(colour = "#2c3e50") +
      ggplot2::geom_point(colour = "#2c3e50", fill = "#e74c3c", shape = 21, size = 2) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = "Cycle", y = gamlam_label)
  } else if (metric %in% c("norm_mean", "norm_median", "norm_sd")) {
    norm_source <- sub("^norm_", "", metric)
    norm_tbl <- res$cycle[[norm_source]]
    if (is.null(norm_tbl)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No normalized cycle data available."))
    }

    norm_df <- as.data.frame(norm_tbl)
    if (!"cycle" %in% names(norm_df)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Normalized cycle data incomplete."))
    }

    param_cols <- setdiff(names(norm_df), "cycle")
    if (!length(param_cols)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No parameter columns available for normalization plot."))
    }

    long_df <- do.call(
      rbind,
      lapply(param_cols, function(par) {
        vals <- norm_df[[par]]
        base_val <- vals[[1]]
        if (is.na(base_val) || identical(base_val, 0)) {
          base_val <- 1
        }

        data.frame(
          cycle = norm_df$cycle,
          parameter = par,
          value = vals / base_val,
          stringsAsFactors = FALSE
        )
      })
    )

    ggplot2::ggplot(long_df, ggplot2::aes(x = cycle, y = value, colour = parameter)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = "Cycle", y = "Normalized value", colour = "Parameter", subtitle = paste0("Normalized by ", tools::toTitleCase(norm_source)))
  } else {
    if (!metric %in% tolower(names(data))) {
      metric <- "neg2ll"
    }

    metric_name <- names(data)[tolower(names(data)) == metric][1]
    if (is.na(metric_name) || !nzchar(metric_name)) {
      metric_name <- "neg2ll"
    }

    data$criterion_value <- data[[metric_name]]
    y_label <- switch(tolower(metric_name), neg2ll = "-2 Log Likelihood", aic = "AIC", bic = "BIC", metric_name)

    ggplot2::ggplot(data, ggplot2::aes(x = cycle, y = criterion_value)) +
      ggplot2::geom_line(colour = "#2c3e50") +
      ggplot2::geom_point(colour = "#2c3e50", fill = "#e74c3c", shape = 21, size = 2) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = "Cycle", y = y_label)
  }
}

pmetrics_build_live_objective_plot <- function(state) {
  if (!nrow(state$objective_history)) {
    return(pmetrics_build_live_empty_plot("Waiting for the first completed cycle."))
  }

  pmetrics_build_cycle_objective_plot(
    res = pmetrics_live_cycle_result(state),
    metric = "neg2ll",
    gamlam_label = pmetrics_live_error_model_label(state)
  )
}

pmetrics_build_live_error_model_plot <- function(state) {
  history <- state$error_model_history
  if (!nrow(history)) {
    return(pmetrics_build_live_empty_plot("Waiting for gamma/lambda cycle data."))
  }

  plot_data <- history
  if (length(unique(plot_data$outeq)) > 1L) {
    plot_data$series <- paste("Outeq", plot_data$outeq)
  } else {
    plot_data$series <- pmetrics_live_error_model_label(state)
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = cycle, y = value, colour = series, group = series)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Cycle", y = pmetrics_live_error_model_label(state), colour = if (length(unique(plot_data$series)) > 1L) "Output" else NULL)
}

pmetrics_build_live_parameter_plot <- function(state, metric = "mean") {
  metric <- match.arg(metric, choices = c("mean", "median", "sd"))

  if (!nrow(state$parameter_history)) {
    return(pmetrics_build_live_empty_plot("Waiting for parameter trend data."))
  }

  pmetrics_build_cycle_objective_plot(
    res = pmetrics_live_cycle_result(state),
    metric = paste0("norm_", metric),
    gamlam_label = pmetrics_live_error_model_label(state)
  )
}

pmetrics_live_control_state <- function(state) {
  run_state <- pmetrics_live_default(state$run_state, "connecting")
  pending <- pmetrics_live_default(state$pending_command, "")
  terminal <- run_state %in% c("completed", "stopped", "failed", "closed") || identical(state$connection, "failed")
  active <- identical(state$view, "live_monitor") && !terminal

  list(
    pause_enabled = active && identical(run_state, "running") && !identical(pending, "pause_after_cycle"),
    resume_enabled = active && identical(run_state, "paused") && !identical(pending, "resume"),
    stop_enabled = active && run_state %in% c("running", "paused") && !identical(pending, "stop_after_cycle")
  )
}

pmetrics_live_command_help <- function(kind) {
  switch(kind,
    pause_after_cycle = "Pause requested. It will take effect before the next cycle starts.",
    resume = "Resume requested. The run will continue at the next cycle boundary.",
    stop_after_cycle = "Stop requested. The current cycle will finish before the run stops.",
    ping = "Ping sent.",
    "Command sent."
  )
}

pmetrics_send_live_session_command <- function(connection, kind) {
  if (is.null(connection)) {
    return(list(ok = FALSE, message = "Live session connection is not available."))
  }

  payload <- jsonlite::toJSON(list(kind = kind), auto_unbox = TRUE)

  tryCatch(
    {
      writeLines(payload, connection, sep = "\n", useBytes = TRUE)
      try(flush(connection), silent = TRUE)
      list(ok = TRUE, message = pmetrics_live_command_help(kind))
    },
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    }
  )
}

pmetrics_describe_live_fit_completed <- function(event) {
  cycles <- pmetrics_live_default(event$cycles, "?")
  status_label <- pmetrics_normalize_live_status_value(event$status)

  if (grepl("stopped", status_label, ignore.case = TRUE)) {
    return(list(run_state = "stopped", label = sprintf("Fit stopped after %s cycles", cycles)))
  }

  if (grepl("converged", status_label, ignore.case = TRUE)) {
    return(list(run_state = "completed", label = sprintf("Fit converged after %s cycles", cycles)))
  }

  if (grepl("max", status_label, ignore.case = TRUE)) {
    return(list(run_state = "completed", label = sprintf("Fit reached the cycle limit after %s cycles", cycles)))
  }

  list(run_state = "completed", label = sprintf("Fit completed after %s cycles", cycles))
}

pmetrics_format_live_message_label <- function(message) {
  kind <- pmetrics_live_default(message$kind, "unknown")

  if (identical(kind, "session_started")) {
    return("Session started")
  }

  if (identical(kind, "fit_failed")) {
    return(paste("Fit failed:", pmetrics_live_default(message$message, "Fit failed")))
  }

  if (identical(kind, "session_closed")) {
    return("Session closed")
  }

  if (identical(kind, "progress") && is.list(message$event)) {
    event_kind <- pmetrics_live_default(message$event$kind, "progress")
    if (identical(event_kind, "nonparametric_cycle")) {
      return(sprintf("Cycle %s completed", pmetrics_live_default(message$event$cycle, "?")))
    }
    if (identical(event_kind, "paused")) {
      return(sprintf("Paused after cycle %s", pmetrics_live_default(message$event$cycle, "?")))
    }
    if (identical(event_kind, "resumed")) {
      return(sprintf("Resumed at cycle %s", pmetrics_live_default(message$event$cycle, "?")))
    }
    if (identical(event_kind, "stop_requested")) {
      return(sprintf("Stop requested after cycle %s", pmetrics_live_default(message$event$cycle, "?")))
    }
    if (identical(event_kind, "fit_completed")) {
      return(sprintf("Fit completed after %s cycles", pmetrics_live_default(message$event$cycles, "?")))
    }
    return(gsub("_", " ", event_kind, fixed = TRUE))
  }

  gsub("_", " ", kind, fixed = TRUE)
}

pmetrics_reduce_live_session_state <- function(state, messages) {
  for (message in messages) {
    state$connection <- "connected"
    state <- pmetrics_append_live_session_event(state, pmetrics_format_live_message_label(message))

    kind <- pmetrics_live_default(message$kind, "unknown")
    state$last_event <- kind

    if (identical(kind, "session_started")) {
      state$status <- "Session started"
      next
    }

    if (identical(kind, "fit_failed")) {
      state$run_state <- "failed"
      state$connection <- "failed"
      state$status <- pmetrics_live_default(message$message, "Fit failed")
      state$pending_command <- NULL
      state$command_status <- "Run failed."
      next
    }

    if (identical(kind, "session_closed")) {
      state$connection <- "closed"
      if (!state$run_state %in% c("completed", "stopped", "failed")) {
        state$run_state <- "closed"
        state$status <- "Session closed"
      }
      state$pending_command <- NULL
      next
    }

    if (!identical(kind, "progress") || !is.list(message$event)) {
      next
    }

    event <- message$event
    event_kind <- pmetrics_live_default(event$kind, "progress")
    state$last_event <- event_kind

    if (identical(event_kind, "fit_started")) {
      state$run_state <- "running"
      state$status <- "Fit started"
      next
    }

    if (identical(event_kind, "paused")) {
      state$run_state <- "paused"
      state$status <- sprintf("Paused after cycle %s", pmetrics_live_default(event$cycle, "?"))
      state$pending_command <- NULL
      state$command_status <- sprintf("Pause acknowledged after cycle %s.", pmetrics_live_default(event$cycle, "?"))
      next
    }

    if (identical(event_kind, "resumed")) {
      state$run_state <- "running"
      state$status <- sprintf("Resumed at cycle %s", pmetrics_live_default(event$cycle, "?"))
      state$pending_command <- NULL
      state$command_status <- sprintf("Resume acknowledged at cycle %s.", pmetrics_live_default(event$cycle, "?"))
      next
    }

    if (identical(event_kind, "stop_requested")) {
      state$run_state <- "stopping"
      state$status <- sprintf("Stop requested after cycle %s", pmetrics_live_default(event$cycle, "?"))
      state$pending_command <- NULL
      state$command_status <- sprintf("Stop acknowledged after cycle %s.", pmetrics_live_default(event$cycle, "?"))
      next
    }

    if (identical(event_kind, "fit_completed")) {
      completion <- pmetrics_describe_live_fit_completed(event)
      state$run_state <- completion$run_state
      state$status <- completion$label
      state$pending_command <- NULL
      state$command_status <- completion$label
      next
    }

    if (identical(event_kind, "nonparametric_cycle")) {
      if (!identical(state$run_state, "stopping")) {
        state$run_state <- "running"
      }
      state$cycle <- as.integer(pmetrics_live_default(event$cycle, NA_integer_))
      state$neg2ll <- as.numeric(pmetrics_live_default(event$neg2ll, NA_real_))
      state$cycle_elapsed_ms <- as.numeric(pmetrics_live_default(event$cycle_elapsed_ms, NA_real_))
      state$total_elapsed_ms <- as.numeric(pmetrics_live_default(event$total_elapsed_ms, NA_real_))
      state$nspp <- as.integer(pmetrics_live_default(event$nspp, NA_integer_))
      state$status <- sprintf("Cycle %s completed", pmetrics_live_default(event$cycle, "?"))
      state$objective_history <- pmetrics_upsert_live_objective_history(state$objective_history, event)
      state$error_model_history <- pmetrics_upsert_live_error_model_history(state$error_model_history, event)
      state$parameter_history <- pmetrics_upsert_live_parameter_history(state$parameter_history, event)
    }
  }

  state
}

pmetrics_live_summary_card <- function(title, output_id, detail_output_id = NULL, classes = NULL) {
  htmltools::tags$div(
    class = paste(c("live-summary-card", classes), collapse = " "),
    htmltools::tags$div(class = "live-summary-card__label", title),
    htmltools::tags$div(class = "live-summary-card__value", shiny::textOutput(output_id)),
    if (!is.null(detail_output_id)) {
      htmltools::tags$div(class = "live-summary-card__detail", shiny::textOutput(detail_output_id))
    }
  )
}

pmetrics_live_add_external_resources <- function() {
  shiny::tags$head(
    shiny::tags$title("Pmetrics Live Status"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('pmetrics-live-close', function(message) { window.close(); });"
    ))
  )
}

pmetrics_live_app_ui <- function(request) {
  shiny::tagList(
    pmetrics_live_add_external_resources(),
    bslib::page_fluid(
      title = "Pmetrics Live Status",
      theme = bslib::bs_theme(
        bootswatch = "flatly",
        primary = "#2c3e50",
        "card-border-radius" = "0.5rem"
      ),
      shiny::div(
        class = "live-report-page",
        shiny::div(
          class = "app-banner",
          shiny::tags$span(class = "app-banner__title", "Pmetrics Live Status"),
          shiny::tags$div(
            class = "app-banner__actions",
            shiny::uiOutput("live_mode_badge"),
            shiny::actionButton(
              "close_app",
              "Close",
              class = "btn-danger app-banner__close",
              onclick = "setTimeout(function(){window.close();}, 100);"
            )
          )
        ),
        shiny::uiOutput("live_banner"),
        htmltools::tags$div(
          class = "live-summary-grid",
          pmetrics_live_summary_card("Mode", "live_mode"),
          pmetrics_live_summary_card("Connection", "live_connection_state", detail_output_id = "live_connection_detail", classes = "live-summary-card--compact"),
          pmetrics_live_summary_card("Run state", "live_run_state"),
          pmetrics_live_summary_card("Cycle", "live_cycle"),
          pmetrics_live_summary_card("Objective", "live_objective"),
          pmetrics_live_summary_card("Elapsed", "live_elapsed"),
          pmetrics_live_summary_card("Support points", "live_nspp")
        ),
        htmltools::tags$div(
          class = "live-command-strip",
          htmltools::tags$div(class = "live-command-strip__label", "Latest status"),
          htmltools::tags$div(class = "live-command-strip__value", shiny::textOutput("live_command_status"))
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            class = "live-panel-card live-panel-card--plot",
            bslib::card_header("Objective by cycle"),
            bslib::card_body(plotly::plotlyOutput("live_objective_plot", height = "420px"))
          ),
          bslib::card(
            class = "live-panel-card live-panel-card--plot",
            bslib::card_header(shiny::textOutput("live_error_model_header")),
            bslib::card_body(plotly::plotlyOutput("live_error_model_plot", height = "420px"))
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          bslib::card(
            class = "live-panel-card live-panel-card--plot",
            bslib::card_header("Parameter trends by cycle"),
            bslib::card_body(
              shiny::uiOutput("live_parameter_controls"),
              plotly::plotlyOutput("live_parameter_plot", height = "460px")
            )
          ),
          bslib::card(
            class = "live-panel-card",
            bslib::card_header("Run controls"),
            bslib::card_body(shiny::uiOutput("live_controls"))
          )
        ),
        bslib::layout_columns(
          col_widths = c(5, 7),
          bslib::card(
            class = "live-panel-card",
            bslib::card_header("Current cycle detail"),
            bslib::card_body(shiny::uiOutput("live_current_cycle_detail"))
          ),
          bslib::card(
            class = "live-panel-card live-panel-card--events",
            bslib::card_header("Recent events"),
            bslib::card_body(shiny::uiOutput("live_events"))
          )
        )
      )
    )
  )
}

pmetrics_live_app_server <- function(input, output, session) {
  config <- pmetrics_live_session_config()
  live_connection <- pmetrics_prime_live_session_connection(config)
  live_state <- shiny::reactiveVal(pmetrics_get_live_session_state(config))

  if (is.null(live_connection)) {
    state <- modifyList(
      pmetrics_get_live_session_state(config),
      list(connection = "failed", status = "Unable to connect to live session")
    )
    pmetrics_set_live_session_state(config, state)
    live_state(state)
  }

  queue_live_command <- function(kind) {
    state <- pmetrics_get_live_session_state(config)
    result <- pmetrics_send_live_session_command(live_connection, kind)

    if (isTRUE(result$ok)) {
      state$pending_command <- kind
      state$command_status <- result$message
      state <- pmetrics_append_live_session_event(
        state,
        switch(kind,
          pause_after_cycle = "Pause requested",
          resume = "Resume requested",
          stop_after_cycle = "Stop requested",
          ping = "Ping sent",
          "Command sent"
        )
      )
    } else {
      state$command_status <- paste("Command failed:", result$message)
      state <- pmetrics_append_live_session_event(state, state$command_status, level = "danger")
    }

    pmetrics_set_live_session_state(config, state)
    live_state(state)
  }

  shiny::observeEvent(input$close_app, {
    shiny::stopApp()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pause_after_cycle, {
    queue_live_command("pause_after_cycle")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$resume_fit, {
    queue_live_command("resume")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$stop_after_cycle, {
    queue_live_command("stop_after_cycle")
  }, ignoreInit = TRUE)

  shiny::observe({
    shiny::invalidateLater(200, session)

    messages <- pmetrics_read_live_session_messages(live_connection)
    if (!length(messages)) {
      live_state(pmetrics_get_live_session_state(config))
      return(invisible(NULL))
    }

    state <- pmetrics_reduce_live_session_state(pmetrics_get_live_session_state(config), messages)
    pmetrics_set_live_session_state(config, state)
    live_state(state)

    if (identical(state$connection, "closed") && state$run_state %in% c("completed", "stopped")) {
      session$sendCustomMessage("pmetrics-live-close", list())
    }
  })

  output$live_mode_badge <- shiny::renderUI({
    state <- live_state()
    htmltools::tags$span(class = "btn btn-primary", pmetrics_live_stage_label(state))
  })

  output$live_banner <- shiny::renderUI({
    state <- live_state()
    banner_class <- if (identical(state$run_state, "failed")) {
      "alert alert-danger"
    } else {
      "alert alert-primary"
    }

    htmltools::tags$div(
      class = banner_class,
      htmltools::tags$strong(paste0(pmetrics_live_stage_label(state), ": ")),
      state$status
    )
  })

  output$live_mode <- shiny::renderText({
    pmetrics_live_stage_label(live_state())
  })

  output$live_connection_state <- shiny::renderText({
    tools::toTitleCase(live_state()$connection)
  })

  output$live_connection_detail <- shiny::renderText({
    paste("Session", live_state()$session_id)
  })

  output$live_run_state <- shiny::renderText({
    pmetrics_live_run_state_label(live_state())
  })

  output$live_cycle <- shiny::renderText({
    cycle <- live_state()$cycle
    if (is.na(cycle)) "Waiting" else as.character(cycle)
  })

  output$live_objective <- shiny::renderText({
    objective <- live_state()$neg2ll
    if (is.na(objective)) "Waiting" else format(round(objective, 4), nsmall = 4)
  })

  output$live_elapsed <- shiny::renderText({
    pmetrics_format_live_elapsed(live_state()$total_elapsed_ms)
  })

  output$live_nspp <- shiny::renderText({
    nspp <- live_state()$nspp
    if (is.na(nspp)) "Waiting" else as.character(nspp)
  })

  output$live_command_status <- shiny::renderText({
    live_state()$command_status
  })

  output$live_objective_plot <- plotly::renderPlotly({
    plotly::ggplotly(pmetrics_build_live_objective_plot(live_state()), tooltip = c("x", "y"))
  })

  output$live_error_model_header <- shiny::renderText({
    paste0(pmetrics_live_error_model_label(live_state()), " by cycle")
  })

  output$live_error_model_plot <- plotly::renderPlotly({
    plotly::ggplotly(pmetrics_build_live_error_model_plot(live_state()), tooltip = c("x", "y", "colour"))
  })

  output$live_parameter_controls <- shiny::renderUI({
    htmltools::tags$div(
      class = "op-controls-row",
      shiny::radioButtons(
        inputId = "live_parameter_metric",
        label = NULL,
        choices = c("Mean" = "mean", "Median" = "median", "SD" = "sd"),
        selected = if (is.null(input$live_parameter_metric) || !input$live_parameter_metric %in% c("mean", "median", "sd")) "mean" else input$live_parameter_metric,
        inline = TRUE
      )
    )
  })

  output$live_parameter_plot <- plotly::renderPlotly({
    metric <- if (is.null(input$live_parameter_metric)) "mean" else input$live_parameter_metric
    plotly::ggplotly(pmetrics_build_live_parameter_plot(live_state(), metric = metric), tooltip = c("x", "y", "colour"))
  })

  output$live_controls <- shiny::renderUI({
    state <- live_state()
    controls <- pmetrics_live_control_state(state)

    htmltools::tagList(
      htmltools::tags$p(state$command_status),
      htmltools::tags$p(
        class = "text-muted",
        "Pause waits for the current cycle to finish. Stop also waits for the current cycle boundary."
      ),
      htmltools::tags$div(
        class = "d-flex gap-2 flex-wrap",
        shiny::actionButton(
          "pause_after_cycle",
          "Pause",
          class = "btn btn-outline-primary",
          disabled = if (!isTRUE(controls$pause_enabled)) "disabled" else NULL
        ),
        shiny::actionButton(
          "resume_fit",
          "Resume",
          class = "btn btn-outline-success",
          disabled = if (!isTRUE(controls$resume_enabled)) "disabled" else NULL
        ),
        shiny::actionButton(
          "stop_after_cycle",
          "Stop After Cycle",
          class = "btn btn-outline-danger",
          disabled = if (!isTRUE(controls$stop_enabled)) "disabled" else NULL
        )
      )
    )
  })

  output$live_current_cycle_detail <- shiny::renderUI({
    state <- live_state()
    error_models <- pmetrics_latest_live_error_model_table(state)
    parameters <- pmetrics_latest_live_parameter_table(state)

    if (!nrow(error_models) && !nrow(parameters)) {
      return(htmltools::tags$p("Waiting for the first completed cycle."))
    }

    htmltools::tagList(
      htmltools::tags$p(htmltools::tags$strong("Status: "), state$status),
      htmltools::tags$p(
        htmltools::tags$strong("Last update: "),
        if (is.na(state$last_update)) "Waiting" else format(state$last_update, "%H:%M:%S")
      ),
      if (nrow(error_models)) {
        htmltools::tagList(
          htmltools::tags$h5(pmetrics_live_error_model_label(state)),
          pmetrics_live_html_table(error_models, digits = 4)
        )
      },
      if (nrow(parameters)) {
        htmltools::tagList(
          htmltools::tags$h5("Parameters"),
          pmetrics_live_html_table(parameters, digits = 4)
        )
      }
    )
  })

  output$live_events <- shiny::renderUI({
    events <- live_state()$events
    if (!length(events)) {
      return(shiny::tags$p("Waiting for live session events."))
    }

    shiny::tags$ul(
      class = "list-unstyled",
      lapply(rev(events), function(event) {
        shiny::tags$li(shiny::tags$strong(format(event$at, "%H:%M:%S")), " ", event$label)
      })
    )
  })
}