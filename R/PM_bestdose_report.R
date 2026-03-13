bd_parse_datetime <- function(x) {
  user_date_fmt <- getPMoptions("date_format", warn = FALSE, quiet = TRUE)
  if (!is.character(user_date_fmt) || length(user_date_fmt) != 1 || !nzchar(user_date_fmt)) {
    user_date_fmt <- if (grepl("en_US", Sys.getlocale("LC_TIME"), fixed = TRUE)) "%m/%d/%y" else "%d/%m/%y"
  }

  user_4yr <- sub("%y", "%Y", user_date_fmt, fixed = TRUE)
  formats <- unique(c(
    paste(user_4yr, "%H:%M:%S"),
    paste(user_4yr, "%H:%M"),
    paste(user_date_fmt, "%H:%M:%S"),
    paste(user_date_fmt, "%H:%M"),
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S",
    "%m/%d/%y %H:%M",
    "%m-%d-%Y %H:%M:%S",
    "%m-%d-%Y %H:%M",
    "%m-%d-%y %H:%M:%S",
    "%m-%d-%y %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d/%m/%y %H:%M:%S",
    "%d/%m/%y %H:%M",
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%d-%m-%y %H:%M:%S",
    "%d-%m-%y %H:%M"
  ))

  for (fmt in formats) {
    dt <- suppressWarnings(as.POSIXct(x, format = fmt, tz = Sys.timezone()))
    if (!is.na(dt)) return(dt)
  }

  suppressWarnings(as.POSIXct(x, tz = Sys.timezone()))
}

bd_plot_start_datetime <- function(x) {
  if (!is.null(x$past) && !is.null(x$past$data) && nrow(x$past$data) > 0 &&
      all(c("date", "time") %in% names(x$past$data))) {
    dt <- bd_parse_datetime(paste(as.character(x$past$data$date[1]), as.character(x$past$data$time[1])))
    if (!is.na(dt)) return(dt)
  }

  if (is.character(x$start) && length(x$start) == 1 && nzchar(x$start)) {
    dt <- bd_parse_datetime(x$start)
    if (!is.na(dt)) return(dt)
  }

  now <- Sys.time()
  ceiling_hour <- as.POSIXct(
    format(now, "%Y-%m-%d %H:00:00"),
    format = "%Y-%m-%d %H:%M:%S",
    tz = Sys.timezone()
  )
  if (ceiling_hour <= now) ceiling_hour <- ceiling_hour + 3600
  ceiling_hour
}

bd_future_time_offset <- function(x) {
  has_past <- !is.null(x$past) && !is.null(x$past$standard_data) && nrow(x$past$standard_data) > 0
  if (has_past) {
    max(x$past$standard_data$time, na.rm = TRUE)
  } else {
    x$start_offset %||% 0
  }
}

bd_expand_dose_events <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }

  rows <- lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, , drop = FALSE]
    addl <- if ("addl" %in% names(row) && !is.na(row$addl[1])) as.integer(row$addl[1]) else 0L
    n_doses <- addl + 1L
    ii <- if ("ii" %in% names(row) && !is.na(row$ii[1])) row$ii[1] else 0
    times <- row$time[1] + seq.int(0, n_doses - 1L) * ii
    expanded <- row[rep(1, n_doses), , drop = FALSE]
    expanded$time <- times
    expanded$addl <- NA
    expanded$ii <- NA
    expanded
  })

  do.call(rbind, rows)
}

bd_future_dose_events <- function(x) {
  if (is.null(x$future) || is.null(x$future$standard_data)) {
    return(NULL)
  }

  dose_rows <- x$future$standard_data |>
    dplyr::filter(evid == 1)

  if (nrow(dose_rows) == 0) {
    return(NULL)
  }

  dose_rows <- bd_expand_dose_events(dose_rows)
  if (nrow(dose_rows) == 0) {
    return(NULL)
  }

  result_doses <- x$result$doses %||% numeric()
  if (length(result_doses) == nrow(dose_rows)) {
    dose_rows$dose <- as.numeric(result_doses)
  } else if (length(result_doses) > 0 && all(is.na(dose_rows$dose) | dose_rows$dose == 0)) {
    dose_rows$dose[seq_len(min(length(result_doses), nrow(dose_rows)))] <- as.numeric(result_doses[seq_len(min(length(result_doses), nrow(dose_rows)))])
  }

  dose_rows |>
    dplyr::mutate(time = time + bd_future_time_offset(x))
}

bd_weight_table <- function(x, nsim_values) {
  nsim_values <- sort(unique(nsim_values))
  tibble::tibble(
    nsim = nsim_values,
    weight = as.numeric(x$posterior$posterior_weights)[seq_along(nsim_values)]
  )
}

bd_weighted_pred_summary <- function(x, sim_data) {
  if (is.null(sim_data) || nrow(sim_data) == 0) {
    return(NULL)
  }

  sim_data <- sim_data |>
    dplyr::mutate(id = as.character(id))

  weights <- bd_weight_table(x, sim_data$nsim)

  sim_data |>
    dplyr::left_join(weights, by = dplyr::join_by(nsim)) |>
    dplyr::group_by(id, time, outeq) |>
    dplyr::summarise(
      pred = stats::weighted.mean(out, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
}

bd_fit_data <- function(x, source = c("past", "future"), outeq = 1) {
  source <- match.arg(source)

  if (source == "past") {
    if (is.null(x$past) || is.null(x$past$standard_data) || is.null(x$past_pred)) {
      return(NULL)
    }

    actual <- x$past$standard_data |>
      dplyr::filter(evid == 0, outeq == !!outeq) |>
      dplyr::transmute(id = as.character(id), time, outeq, obs = out, c0, c1, c2, c3)

    sim <- x$past_pred$data$obs |>
      dplyr::filter(outeq == !!outeq)

    label <- "Measured"
  } else {
    if (is.null(x$future) || is.null(x$future$standard_data) || is.null(x$future_pred)) {
      return(NULL)
    }

    offset <- bd_future_time_offset(x)
    actual <- x$future$standard_data |>
      dplyr::filter(evid == 0, outeq == !!outeq) |>
      dplyr::transmute(id = as.character(id), time = time + offset, outeq, obs = out, c0, c1, c2, c3)

    sim <- x$future_pred$data$obs |>
      dplyr::filter(outeq == !!outeq) |>
      dplyr::mutate(time = time + offset)

    label <- "Target"
  }

  if (nrow(actual) == 0 || nrow(sim) == 0) {
    return(NULL)
  }

  pred <- bd_weighted_pred_summary(x, sim)
  if (is.null(pred) || nrow(pred) == 0) {
    return(NULL)
  }

  fit <- actual |>
    dplyr::left_join(pred, by = dplyr::join_by(id, time, outeq)) |>
    dplyr::mutate(
      obsSD = dplyr::coalesce(c0, 0) + dplyr::coalesce(c1, 0) * obs + dplyr::coalesce(c2, 0) * obs^2 + dplyr::coalesce(c3, 0) * obs^3,
      obsSD = dplyr::if_else(!is.finite(obsSD) | obsSD <= 0, 1, obsSD),
      d = pred - obs,
      ds = d * d,
      wd = d / obsSD,
      wds = wd * wd,
      kind = label
    ) |>
    dplyr::filter(!is.na(obs), !is.na(pred))

  if (nrow(fit) == 0) {
    return(NULL)
  }

  fit
}

bd_fit_metrics <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    pe <- data.frame(type = rep(NA_character_, 8), absolute = rep(NA_real_, 8), percent = rep(NA_real_, 8))
    return(list(pe = pe, metric_info = get_metric_info(pe)))
  }

  N <- nrow(data)
  mae <- sum(data$d, na.rm = TRUE) / N
  percent_mae <- mean(data$d / data$obs, na.rm = TRUE) * 100

  mwe <- sum(data$wd, na.rm = TRUE) / N
  percent_mwe <- sum(data$wd, na.rm = TRUE) / sum(data$obs / data$obsSD, na.rm = TRUE) * 100

  mean_obs <- mean(data$obs, na.rm = TRUE)
  wmean_obs <- sum(data$obs / data$obsSD, na.rm = TRUE) / N

  mse <- sum(data$ds, na.rm = TRUE) / N
  percent_mse <- mean(data$ds, na.rm = TRUE) / (mean_obs^2) * 100

  mwse <- sum(data$wds, na.rm = TRUE) / N
  percent_mwse <- mwse / (wmean_obs^2) * 100

  rmse <- sqrt(mse)
  percent_rmse <- rmse / mean_obs * 100

  mbase <- mse - mae^2
  percent_mbase <- mbase / (mean_obs^2) * 100

  mbawse <- mwse - mwe^2
  percent_mbawse <- mbawse / (wmean_obs^2) * 100

  rmbawse <- sqrt(mbawse)
  percent_rmbawse <- rmbawse * 100 / wmean_obs

  pe <- data.frame(
    type = c("mae", "mwe", "mse", "mwse", "rmse", "mbase", "mbawse", "rmbawse"),
    absolute = c(mae, mwe, mse, mwse, rmse, mbase, mbawse, rmbawse),
    percent = c(percent_mae, percent_mwe, percent_mse, percent_mwse, percent_rmse, percent_mbase, percent_mbawse, percent_rmbawse)
  )

  list(pe = pe, metric_info = get_metric_info(pe))
}

bd_fit_plot <- function(data, title) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  lims <- range(c(data$pred, data$obs), na.rm = TRUE)
  fit <- if (nrow(data) >= 2) stats::lm(obs ~ pred, data = data) else NULL
  ann_text <- NULL
  if (!is.null(fit) && !anyNA(stats::coef(fit))) {
    ann_text <- sprintf(
      "R² = %.3f<br>Intercept = %.3f<br>Slope = %.3f",
      summary(fit)$r.squared,
      stats::coef(fit)[1],
      stats::coef(fit)[2]
    )
  }

  p <- plotly::plot_ly(data = data, x = ~pred, y = ~obs, type = "scatter", mode = "markers",
    marker = list(size = 9, color = "rgba(214, 86, 69, 0.75)", line = list(color = "rgba(120, 40, 30, 0.9)", width = 1)),
    hovertemplate = "Predicted: %{x}<br>Observed: %{y}<extra></extra>") |>
    plotly::layout(
      title = title,
      xaxis = list(title = "Predicted", range = lims),
      yaxis = list(title = "Observed", range = lims),
      shapes = list(list(
        type = "line",
        x0 = lims[1], x1 = lims[2],
        y0 = lims[1], y1 = lims[2],
        line = list(color = "rgba(80, 80, 80, 0.8)", dash = "dash")
      )),
      hovermode = "closest"
    )

  if (!is.null(fit) && !anyNA(stats::coef(fit))) {
    reg_line <- data.frame(x = lims, y = stats::coef(fit)[1] + stats::coef(fit)[2] * lims)
    p <- p |>
      plotly::add_lines(data = reg_line, x = ~x, y = ~y, inherit = FALSE,
        line = list(color = "#2c3e50", width = 2),
        hoverinfo = "skip", showlegend = FALSE) |>
      plotly::layout(annotations = list(list(
        x = 0.02, y = 0.98, xref = "paper", yref = "paper",
        text = ann_text, align = "left",
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.85)",
        bordercolor = "rgba(44,62,80,0.4)",
        font = list(size = 11)
      )))
  }

  p
}

bd_percent_metric <- function(pe, method) {
  if (is.null(pe) || !all(c("type", "percent") %in% names(pe))) {
    return(NA_real_)
  }

  method0 <- gsub("^percent_", "", method)
  idx <- which(pe$type == method0)
  if (length(idx) == 0) {
    return(NA_real_)
  }

  as.numeric(pe$percent[idx[1]])
}

bd_fit_rainbow_plot <- function(past_metrics, future_metrics) {
  bias_method <- getPMoptions("bias_method")
  imp_method <- getPMoptions("imp_method")

  past_bias <- bd_percent_metric(past_metrics$pe, bias_method)
  past_imp <- bd_percent_metric(past_metrics$pe, imp_method)
  future_bias <- bd_percent_metric(future_metrics$pe, bias_method)
  future_imp <- bd_percent_metric(future_metrics$pe, imp_method)

  x_vals <- c(-100, 100, past_bias, future_bias)
  y_vals <- c(0, 100, past_imp, future_imp)
  x_vals <- x_vals[is.finite(x_vals)]
  y_vals <- y_vals[is.finite(y_vals)]

  if (length(x_vals) == 0 || length(y_vals) == 0) {
    return(NULL)
  }

  xlim <- c(min(x_vals), max(x_vals))
  ylim <- c(0, max(y_vals))

  if (diff(xlim) < 40) xlim <- c(xlim[1] - 20, xlim[2] + 20)
  if (ylim[2] < 20) ylim[2] <- 20

  b <- seq(100, 0, -20)
  a <- seq(50, 0, -10)
  cols <- c(
    "rgba(255,0,0,0.7)",
    "rgba(255,165,0,0.7)",
    "rgba(255,255,0,0.7)",
    "rgba(154,205,50,0.7)",
    "rgba(0,128,0,0.7)"
  )

  p <- plotly::plot_ly()

  for (i in 1:5) {
    a1 <- a[i]
    a2 <- a[i + 1]
    b1 <- b[i]
    b2 <- b[i + 1]

    x1 <- seq(-a1, a1, by = 1)
    x2 <- seq(-a2, a2, by = 1)
    y1 <- sqrt(pmax(0, b1^2 * (1 - x1^2 / a1^2)))
    y2 <- sqrt(pmax(0, b2^2 * (1 - x2^2 / a2^2)))

    poly_x <- c(x1, rev(x2))
    poly_y <- c(y1, rev(y2))

    p <- p |>
      plotly::add_trace(
        x = poly_x,
        y = poly_y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = cols[i],
        line = list(color = "rgba(0,0,0,0)", width = 0),
        hoverinfo = "skip",
        showlegend = FALSE
      )
  }

  if (is.finite(past_bias) && is.finite(past_imp)) {
    p <- p |>
      plotly::add_markers(
        x = past_bias,
        y = past_imp,
        marker = list(color = "black", size = 11, line = list(color = "white", width = 2)),
        name = "Past (% bias, % imprecision)",
        hoverlabel = list(bgcolor = bd_rainbow_region_color(past_bias, past_imp, alpha = 1)),
        hovertemplate = "Past<br>%Bias: %{x:.2f}<br>%Imprecision: %{y:.2f}<extra></extra>",
        showlegend = TRUE
      )
  }

  if (is.finite(future_bias) && is.finite(future_imp)) {
    p <- p |>
      plotly::add_markers(
        x = future_bias,
        y = future_imp,
        marker = list(color = "red", size = 11, line = list(color = "white", width = 2)),
        name = "Future (% bias, % imprecision)",
        hoverlabel = list(bgcolor = bd_rainbow_region_color(future_bias, future_imp, alpha = 1)),
        hovertemplate = "Future<br>%Bias: %{x:.2f}<br>%Imprecision: %{y:.2f}<extra></extra>",
        showlegend = TRUE
      )
  }

  p |>
    plotly::layout(
      title = "Fit quality rainbow",
      xaxis = list(title = "% Bias", range = xlim, zeroline = FALSE),
      yaxis = list(title = "% Imprecision", range = ylim, zeroline = FALSE),
      shapes = list(
        list(
          type = "rect",
          x0 = xlim[1],
          x1 = xlim[2],
          y0 = 0,
          y1 = ylim[2],
          xref = "x",
          yref = "y",
          fillcolor = "#808080",
          line = list(width = 0),
          layer = "below"
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = 0,
          y0 = 0,
          y1 = ylim[2],
          line = list(color = "rgba(128,128,128,0.9)", dash = "dash"),
          layer = "below"
        )
      ),
      legend = list(orientation = "h", y = -0.2)
    )
}

bd_rainbow_region_color <- function(bias, imp, alpha = 1) {
  if (!is.finite(bias) || !is.finite(imp)) {
    return(if (alpha >= 1) "#808080" else "rgba(128,128,128,0.7)")
  }

  a <- seq(50, 0, -10)
  b <- seq(100, 0, -20)

  inside_ellipse <- function(x, y, ax, by) {
    if (!is.finite(ax) || !is.finite(by) || ax <= 0 || by <= 0) return(FALSE)
    ((x^2) / (ax^2) + (y^2) / (by^2)) <= 1
  }

  base_cols <- c("#FF0000", "#FFA500", "#FFFF00", "#9ACD32", "#008000")
  rgba_cols <- c(
    "rgba(255,0,0,0.7)",
    "rgba(255,165,0,0.7)",
    "rgba(255,255,0,0.7)",
    "rgba(154,205,50,0.7)",
    "rgba(0,128,0,0.7)"
  )

  # smallest enclosing ellipse determines visible region color
  for (i in 5:1) {
    if (inside_ellipse(bias, imp, a[i], b[i])) {
      return(if (alpha >= 1) base_cols[i] else rgba_cols[i])
    }
  }

  if (alpha >= 1) "#808080" else "rgba(128,128,128,0.7)"
}

bd_fit_rainbow_plot_single <- function(metrics, color = "black", name = "Point", title = "Fit quality") {
  bias_method <- getPMoptions("bias_method")
  imp_method <- getPMoptions("imp_method")

  bias_val <- bd_percent_metric(metrics$pe, bias_method)
  imp_val <- bd_percent_metric(metrics$pe, imp_method)

  x_vals <- c(-100, 100)
  y_vals <- c(0, 100)
  if (is.finite(bias_val)) x_vals <- c(x_vals, bias_val)
  if (is.finite(imp_val)) y_vals <- c(y_vals, imp_val)

  xlim <- c(min(x_vals), max(x_vals))
  ylim <- c(0, max(y_vals))

  if (diff(xlim) < 40) xlim <- c(xlim[1] - 20, xlim[2] + 20)
  if (ylim[2] < 20) ylim[2] <- 20

  b <- seq(100, 0, -20)
  a <- seq(50, 0, -10)
  cols <- c(
    "rgba(255,0,0,0.7)",
    "rgba(255,165,0,0.7)",
    "rgba(255,255,0,0.7)",
    "rgba(154,205,50,0.7)",
    "rgba(0,128,0,0.7)"
  )

  p <- plotly::plot_ly()

  for (i in 1:5) {
    a1 <- a[i]; a2 <- a[i + 1]
    b1 <- b[i]; b2 <- b[i + 1]
    x1 <- seq(-a1, a1, by = 1)
    x2 <- seq(-a2, a2, by = 1)
    y1 <- sqrt(pmax(0, b1^2 * (1 - x1^2 / a1^2)))
    y2 <- sqrt(pmax(0, b2^2 * (1 - x2^2 / a2^2)))
    poly_x <- c(x1, rev(x2))
    poly_y <- c(y1, rev(y2))
    p <- p |> plotly::add_trace(
      x = poly_x, y = poly_y,
      type = "scatter", mode = "lines",
      fill = "toself", fillcolor = cols[i],
      line = list(color = "rgba(0,0,0,0)", width = 0),
      hoverinfo = "skip", showlegend = FALSE
    )
  }

  if (is.finite(bias_val) && is.finite(imp_val)) {
    p <- p |> plotly::add_markers(
      x = bias_val, y = imp_val,
      marker = list(color = color, size = 11, line = list(color = "white", width = 2)),
      name = name,
      hoverlabel = list(bgcolor = bd_rainbow_region_color(bias_val, imp_val, alpha = 1)),
      hovertemplate = paste0(name, "<br>%Bias: %{x:.2f}<br>%Imprecision: %{y:.2f}<extra></extra>"),
      showlegend = TRUE
    )
  }

  p |> plotly::layout(
    title = list(text = ""),
    xaxis = list(title = "% Bias", range = xlim, zeroline = FALSE),
    yaxis = list(title = "% Imprecision", range = ylim, zeroline = FALSE),
    shapes = list(
      list(
        type = "rect",
        x0 = xlim[1], x1 = xlim[2],
        y0 = 0, y1 = ylim[2],
        xref = "x", yref = "y",
        fillcolor = "#808080",
        line = list(width = 0),
        layer = "below"
      ),
      list(
        type = "line",
        x0 = 0, x1 = 0,
        y0 = 0, y1 = ylim[2],
        line = list(color = "rgba(128,128,128,0.9)", dash = "dash"),
        layer = "below"
      )
    ),
    legend = list(orientation = "h", y = -0.2)
  )
}

bd_sim_weighted_conc <- function(x, sim_obj, outeq = 1, time_offset = 0) {
  if (is.null(sim_obj) || is.null(sim_obj$data) || is.null(sim_obj$data$obs)) {
    return(NULL)
  }

  sim <- sim_obj$data$obs |>
    dplyr::filter(outeq == !!outeq) |>
    dplyr::mutate(time = time + time_offset)

  pred <- bd_weighted_pred_summary(x, sim)
  if (is.null(pred) || nrow(pred) == 0) {
    return(NULL)
  }

  pred |>
    dplyr::group_by(time) |>
    dplyr::summarise(conc = mean(pred, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(time)
}

bd_past_dose_events <- function(x) {
  if (is.null(x$past) || is.null(x$past$standard_data)) {
    return(NULL)
  }

  doses <- x$past$standard_data |>
    dplyr::filter(evid == 1)

  if (nrow(doses) == 0) {
    return(NULL)
  }

  doses <- bd_expand_dose_events(doses)
  if (nrow(doses) == 0) {
    return(NULL)
  }

  doses |>
    dplyr::mutate(source = "Past")
}

bd_auc_interval <- function(conc_data, t0, t1) {
  seg <- conc_data |>
    dplyr::filter(time >= t0, time <= t1) |>
    dplyr::arrange(time)

  if (nrow(seg) < 2) {
    return(0)
  }

  dt <- diff(seg$time)
  y0 <- seg$conc[-nrow(seg)]
  y1 <- seg$conc[-1]
  sum((y0 + y1) / 2 * dt, na.rm = TRUE)
}

bd_auc_table <- function(x, outeq = 1, start_datetime = NULL) {
  offset <- bd_future_time_offset(x)

  past_conc <- bd_sim_weighted_conc(x, x$past_pred, outeq = outeq, time_offset = 0)
  future_conc <- bd_sim_weighted_conc(x, x$future_pred, outeq = outeq, time_offset = offset)
  conc <- dplyr::bind_rows(past_conc, future_conc)

  if (is.null(conc) || nrow(conc) == 0) {
    return(NULL)
  }

  conc <- conc |>
    dplyr::group_by(time) |>
    dplyr::summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(time)

  past_doses <- bd_past_dose_events(x)
  future_doses <- bd_future_dose_events(x)
  if (!is.null(future_doses) && nrow(future_doses) > 0) {
    future_doses <- future_doses |>
      dplyr::mutate(source = "Future")
  }

  doses <- dplyr::bind_rows(past_doses, future_doses)
  if (is.null(doses) || nrow(doses) == 0) {
    return(NULL)
  }

  doses <- doses |>
    dplyr::arrange(time)

  n <- nrow(doses)
  postdose <- numeric(n)
  final_time <- max(conc$time, na.rm = TRUE)

  for (i in seq_len(n)) {
    t0 <- doses$time[i]
    t1 <- if (i < n) doses$time[i + 1] else final_time
    if (is.finite(t0) && is.finite(t1) && t1 > t0) {
      postdose[i] <- bd_auc_interval(conc, t0, t1)
    } else {
      postdose[i] <- 0
    }
  }

  out <- doses |>
    dplyr::transmute(
      time = time,
      dose = dose,
      source = source,
      postdose_auc = postdose,
      cumulative_auc = cumsum(postdose)
    )

  bd_add_datetime_column(out, start_datetime)
}

bd_auc_plot <- function(auc_tbl) {
  if (is.null(auc_tbl) || nrow(auc_tbl) == 0) return(NULL)

  date_fmt <- getPMoptions("date_format", warn = FALSE, quiet = TRUE)
  if (!is.character(date_fmt) || length(date_fmt) != 1 || !nzchar(date_fmt)) {
    date_fmt <- if (grepl("en_US", Sys.getlocale("LC_TIME"), fixed = TRUE)) "%m/%d/%y" else "%d/%m/%y"
  }

  df <- auc_tbl |> dplyr::mutate(
    datetime = suppressWarnings(as.POSIXct(datetime, tz = Sys.timezone())),
    dose_idx = seq_len(dplyr::n()),
    dose_amt = ifelse(is.na(dose), NA_character_, formatC(as.numeric(dose), format = "f", digits = 2)),
    date_label = format(datetime, date_fmt),
    time_label = format(datetime, "%H:%M:%S")
  )

  # colors per source
  cols <- ifelse(df$source == "Past", "black", "red")

  p <- plotly::plot_ly()

  # bars for post-dose AUC
  p <- p |> plotly::add_bars(
    x = ~df$datetime, y = ~df$postdose_auc,
    marker = list(color = cols),
    opacity = 0.5,
    name = "Post-dose AUC",
    showlegend = FALSE,
    text = paste0(
      "Dose: ", df$dose_idx,
      "<br>Amount: ", df$dose_amt,
      "<br>Date: ", df$date_label,
      "<br>Time: ", df$time_label,
      "<br>Post-dose AUC: ", sprintf("%.2f", df$postdose_auc)
    ),
    hoverinfo = "text",
    textposition = "none"
  )

  # cumulative line split into past / future to change color
  past_idx <- which(df$source == "Past")
  future_idx <- which(df$source != "Past")

  if (length(past_idx) > 0) {
    p <- p |> plotly::add_trace(
      x = df$datetime[past_idx], y = df$cumulative_auc[past_idx],
      type = "scatter", mode = "lines+markers", name = "Cumulative (Past)",
      line = list(color = "black"), marker = list(color = "black"), yaxis = "y2", showlegend = FALSE,
      text = paste0(
        "Dose: ", df$dose_idx[past_idx],
        "<br>Amount: ", df$dose_amt[past_idx],
        "<br>Date: ", df$date_label[past_idx],
        "<br>Time: ", df$time_label[past_idx],
        "<br>Cumulative AUC: ", sprintf("%.2f", df$cumulative_auc[past_idx])
      ),
      hoverinfo = "text"
    )
  }

  if (length(future_idx) > 0) {
    p <- p |> plotly::add_trace(
      x = df$datetime[future_idx], y = df$cumulative_auc[future_idx],
      type = "scatter", mode = "lines+markers", name = "Cumulative (Future)",
      line = list(color = "red"), marker = list(color = "red"), yaxis = "y2", showlegend = FALSE,
      text = paste0(
        "Dose: ", df$dose_idx[future_idx],
        "<br>Amount: ", df$dose_amt[future_idx],
        "<br>Date: ", df$date_label[future_idx],
        "<br>Time: ", df$time_label[future_idx],
        "<br>Cumulative AUC: ", sprintf("%.2f", df$cumulative_auc[future_idx])
      ),
      hoverinfo = "text"
    )
  }

  # bridge segment from last past to first future cumulative point
  if (length(past_idx) > 0 && length(future_idx) > 0) {
    i_last_past <- max(past_idx)
    i_first_future <- min(future_idx)
    p <- p |> plotly::add_trace(
      x = c(df$datetime[i_last_past], df$datetime[i_first_future]),
      y = c(df$cumulative_auc[i_last_past], df$cumulative_auc[i_first_future]),
      type = "scatter", mode = "lines", yaxis = "y2",
      line = list(color = "black", width = 2),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
  }

  # x-axis tick text: dose number only
  ticktext <- paste0("Dose ", df$dose_idx)

  p |> plotly::layout(
    xaxis = list(title = "", tickvals = df$datetime, ticktext = ticktext),
    yaxis = list(title = "Post-dose AUC", zeroline = FALSE),
    yaxis2 = list(
      overlaying = "y", side = "right",
        title = list(text = "Cumulative AUC", standoff = 40),
      automargin = TRUE,
      zeroline = FALSE
    ),
      margin = list(t = 90),
    bargap = 0.2,
      showlegend = FALSE
  )
}

bd_pd_table <- function(x, outeq = 1, start_datetime = NULL) {
  if (is.null(x$future) || is.null(x$future$standard_data)) {
    return(NULL)
  }

  target_rows <- x$future$standard_data |>
    dplyr::filter(evid == 0, outeq == !!outeq)
  if (nrow(target_rows) == 0) {
    return(NULL)
  }

  target_time <- as.numeric(target_rows$time[1])
  target_value <- as.numeric(target_rows$out[1])
  if (!is.finite(target_time) || !is.finite(target_value)) {
    return(NULL)
  }

  past_doses <- bd_past_dose_events(x)
  future_doses <- bd_future_dose_events(x)
  if (!is.null(future_doses) && nrow(future_doses) > 0) {
    future_doses <- future_doses |>
      dplyr::mutate(source = "Future")
  }

  doses <- dplyr::bind_rows(past_doses, future_doses)
  if (is.null(doses) || nrow(doses) == 0) {
    return(NULL)
  }

  doses <- doses |>
    dplyr::arrange(time) |>
    dplyr::mutate(eval_time = time + target_time)

  offset <- bd_future_time_offset(x)
  past_sim <- NULL
  future_sim <- NULL

  if (!is.null(x$past_pred) && !is.null(x$past_pred$data) && !is.null(x$past_pred$data$obs)) {
    past_sim <- x$past_pred$data$obs |>
      dplyr::filter(outeq == !!outeq)
  }
  if (!is.null(x$future_pred) && !is.null(x$future_pred$data) && !is.null(x$future_pred$data$obs)) {
    future_sim <- x$future_pred$data$obs |>
      dplyr::filter(outeq == !!outeq) |>
      dplyr::mutate(time = time + offset)
  }

  sim <- dplyr::bind_rows(past_sim, future_sim)
  if (is.null(sim) || nrow(sim) == 0) {
    return(NULL)
  }

  sim <- sim |>
    dplyr::mutate(nsim = as.character(nsim))

  weights <- bd_weight_table(x, sim$nsim) |>
    dplyr::mutate(nsim = as.character(nsim))

  sim_split <- split(sim, sim$nsim)
  nsim_keys <- intersect(names(sim_split), weights$nsim)
  if (length(nsim_keys) == 0) {
    return(NULL)
  }

  eval_times <- doses$eval_time
  success_mat <- matrix(FALSE, nrow = length(nsim_keys), ncol = length(eval_times))
  w <- numeric(length(nsim_keys))

  for (j in seq_along(nsim_keys)) {
    key <- nsim_keys[j]
    d <- sim_split[[key]] |>
      dplyr::arrange(time)
    d <- d[!is.na(d$time) & !is.na(d$out), , drop = FALSE]
    if (nrow(d) < 2) next

    pred_at <- stats::approx(
      x = d$time,
      y = d$out,
      xout = eval_times,
      rule = 2,
      ties = mean
    )$y

    success <- pred_at >= target_value
    success_mat[j, ] <- as.logical(success)

    w_row <- weights$weight[weights$nsim == key]
    w[j] <- if (length(w_row) == 0 || !is.finite(w_row[1])) 0 else as.numeric(w_row[1])
  }

  if (sum(w, na.rm = TRUE) <= 0) {
    return(NULL)
  }

  cum_prob <- vapply(seq_along(eval_times), function(i) {
    stats::weighted.mean(as.numeric(success_mat[, i]), w = w, na.rm = TRUE)
  }, numeric(1))

  out <- doses |>
    dplyr::transmute(
      dose_number = dplyr::row_number(),
      time = time,
      source = source,
      cumulative_probability = cum_prob
    )

  bd_add_datetime_column(out, start_datetime)
}

bd_add_datetime_column <- function(data, start_datetime) {
  if (is.null(data) || nrow(data) == 0 || is.null(start_datetime) || is.na(start_datetime) || !"time" %in% names(data)) {
    return(data)
  }

  data |>
    dplyr::mutate(datetime = format(start_datetime + time * 3600, "%Y-%m-%d %H:%M:%S")) |>
    dplyr::relocate(datetime, .after = time)
}

bd_report_build <- function(x, outeq = 1) {
  start_datetime <- bd_plot_start_datetime(x)
  future_offset <- bd_future_time_offset(x)

  overview <- tibble::tibble(
    item = c("Status", "Method", "Objective", "log(Objective)", "Prior weight", "Start", "Start offset (h)", "Future starts at (h)"),
    value = c(
      x$status %||% NA_character_,
      x$method %||% NA_character_,
      signif(x$objf %||% NA_real_, 6),
      if (is.null(x$objf) || is.na(x$objf) || x$objf <= 0) NA_real_ else signif(log(x$objf), 6),
      x$prior_weight %||% NA_real_,
      paste(x$start %||% NA_character_, collapse = ", "),
      x$start_offset %||% NA_real_,
      future_offset
    )
  )

  past_last_dose <- NULL
  past_last_obs <- NULL
  if (!is.null(x$past) && !is.null(x$past$standard_data) && nrow(x$past$standard_data) > 0) {
    past_last_dose <- x$past$standard_data |>
      dplyr::filter(evid == 1) |>
      dplyr::arrange(dplyr::desc(time)) |>
      dplyr::slice(1) |>
      dplyr::select(time, dose, dur, input)

    past_last_obs <- x$past$standard_data |>
      dplyr::filter(evid == 0, outeq == !!outeq) |>
      dplyr::arrange(dplyr::desc(time)) |>
      dplyr::slice(1) |>
      dplyr::select(time, out, outeq)
  }

  # Original requested doses (0 = Optimized, non-zero = Fixed), captured before optimization
  original_requested_doses <- x$future_requested_doses

  future_doses <- bd_future_dose_events(x)
  if (!is.null(future_doses) && nrow(future_doses) > 0) {
    n_fd <- nrow(future_doses)
    status_vec <- rep("Fixed", n_fd)
    if (!is.null(original_requested_doses) && length(original_requested_doses) >= n_fd) {
      status_vec <- dplyr::if_else(
        as.numeric(original_requested_doses[seq_len(n_fd)]) == 0,
        "Optimized", "Fixed"
      )
    }
    future_doses <- future_doses |>
      dplyr::select(time, dose, dur, input) |>
      dplyr::mutate(status = status_vec) |>
      bd_add_datetime_column(start_datetime)
  }

  future_targets <- NULL
  if (!is.null(x$future) && !is.null(x$future$standard_data)) {
    future_targets <- x$future$standard_data |>
      dplyr::filter(evid == 0, outeq == !!outeq) |>
      dplyr::mutate(time = time + future_offset) |>
      dplyr::select(time, out, outeq) |>
      bd_add_datetime_column(start_datetime)
  }

  future_predictions <- x$result$predictions
  if (!is.null(future_predictions) && nrow(future_predictions) > 0) {
    future_predictions <- future_predictions |>
      dplyr::filter(outeq == !!(outeq - 1)) |>
      bd_add_datetime_column(start_datetime)
  }

  auc_table <- bd_auc_table(x, outeq = outeq, start_datetime = start_datetime)
  auc_plot <- bd_auc_plot(auc_table)
  pd_table <- bd_pd_table(x, outeq = outeq, start_datetime = start_datetime)

  pd_target_summary <- NULL
  if (!is.null(x$future_target_info)) {
    ti <- x$future_target_info
    tt  <- ti$target_type %||% "concentration"
    tgt <- ti$target
    ttm <- ti$target_time
    if (!is.null(tt) && tt == "time") {
      pct <- if (!is.null(ttm) && is.numeric(ttm) && !is.na(ttm)) round(ttm * 100) else NA
      pd_target_summary <- paste0(
        "Therapeutic target: Time above ", tgt,
        " for ", pct, "% of each dosing interval."
      )
    } else {
      type_label <- if (!is.null(tt) && tt == "auc") "AUC" else "Concentration"
      ttm_display <- if (!is.null(ttm) && length(ttm) == 1 && is.numeric(ttm) && !is.na(ttm)) ttm else 24
      pd_target_summary <- paste0(
        "Therapeutic target: ", type_label, " of ", tgt,
        ", ", ttm_display, " hours after each dose."
      )
    }
  }

  past_fit <- bd_fit_data(x, source = "past", outeq = outeq)
  future_fit <- bd_fit_data(x, source = "future", outeq = outeq)

  past_fit <- bd_add_datetime_column(past_fit, start_datetime)
  future_fit <- bd_add_datetime_column(future_fit, start_datetime)

  past_fit_metrics <- bd_fit_metrics(past_fit)
  future_fit_metrics <- bd_fit_metrics(future_fit)

  posterior_table <- as.data.frame(x$posterior$theta)
  posterior_table$weight <- as.numeric(x$posterior$posterior_weights)
  posterior_table$population_weight <- as.numeric(x$posterior$population_weights)

  posterior_summary <- tibble::tibble(
    parameter = x$posterior$param_names,
    weighted_mean = vapply(seq_along(x$posterior$param_names), function(i) {
      stats::weighted.mean(x$posterior$theta[, i], x$posterior$posterior_weights, na.rm = TRUE)
    }, numeric(1)),
    weighted_median = vapply(seq_along(x$posterior$param_names), function(i) {
      vals <- x$posterior$theta[, i]
      wts <- x$posterior$posterior_weights
      ord <- order(vals)
      vals <- vals[ord]
      wts <- wts[ord] / sum(wts)
      vals[which.max(cumsum(wts) >= 0.5)]
    }, numeric(1)),
    weighted_sd = vapply(seq_along(x$posterior$param_names), function(i) {
      vals <- x$posterior$theta[, i]
      mu <- stats::weighted.mean(vals, x$posterior$posterior_weights, na.rm = TRUE)
      sqrt(stats::weighted.mean((vals - mu)^2, x$posterior$posterior_weights, na.rm = TRUE))
    }, numeric(1))
  )

  list(
    overview = overview,
    plot = plot.bd(x, quiet = TRUE, print = FALSE, future_region = FALSE),
    past_rainbow_plot = bd_fit_rainbow_plot_single(past_fit_metrics, color = "black", name = "Past"),
    future_rainbow_plot = bd_fit_rainbow_plot_single(future_fit_metrics, color = "red", name = "Future"),
    past_last_dose = bd_add_datetime_column(past_last_dose, start_datetime),
    past_last_obs = bd_add_datetime_column(past_last_obs, start_datetime),
    future_doses = future_doses,
    future_targets = future_targets,
    future_predictions = future_predictions,
    auc_plot = auc_plot,
    auc_table = auc_table,
    pd_table = pd_table,
    pd_target_summary = pd_target_summary,
    past_fit = past_fit,
    future_fit = future_fit,
    past_fit_metrics = past_fit_metrics,
    future_fit_metrics = future_fit_metrics,
    past_fit_plot = bd_fit_plot(past_fit, "Past observations vs predictions"),
    future_fit_plot = bd_fit_plot(future_fit, "Future targets vs predictions"),
    posterior_summary = posterior_summary,
    posterior_table = posterior_table,
    model_summary = paste(capture.output(print(x$posterior$model_info$model)), collapse = "\n")
  )
}

bd_report <- function(x, path, show = TRUE, quiet = TRUE, title = "BestDose Report") {
  if (!inherits(x, "bd")) {
    cli::cli_abort(c("x" = "This function expects a {.cls bd} object."))
  }

  template_file <- system.file("report/templates/bestdose.Rmd", package = "Pmetrics")
  if (!nzchar(template_file) || !file.exists(template_file)) {
    cli::cli_abort(c("x" = "BestDose report template was not found in the package."))
  }

  out_path <- if (missing(path)) tempdir() else normalizePath(path, winslash = "/", mustWork = FALSE)
  fs::dir_create(out_path)

  rmarkdown::render(
    input = template_file,
    output_file = file.path(out_path, "bestdose_report.html"),
    params = list(bd = x, title = title),
    clean = TRUE,
    quiet = quiet
  )

  out_file <- file.path(out_path, "bestdose_report.html")
  if (file.exists(out_file)) {
    if (show) {
      pander::openFileInOS(out_file)
    }
    return(invisible(1))
  }

  invisible(-1)
}
