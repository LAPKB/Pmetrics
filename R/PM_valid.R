# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


# R6 ----------------------------------------------------------------------

#' @title Pmetrics validation object
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains results of internal validation by simulation to permit generation of
#' visual predictive checks (VPCs), prediction corrected visual predictive checks,
#' (pcVPCs), normalized prediction distribution errors (NPDE), and
#' numerical predictive checks. This is typically a field in a [PM_result]
#'
#' @details
#' The [PM_valid] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for analysis and plotting of
#' observed vs. population or individual predicted outputs.
#'
#' Because [PM_valid] objects are automatically added to the [PM_result] by calling the 
#' `$validate()` method of a [PM_result] after a
#' successful run, it is generally not necessary for users to generate [PM_valid] objects
#' themselves.
#' @seealso [PM_result]
#' @export
PM_valid <- R6::R6Class(
  "PM_valid",
  public = list(
    #' @field simdata Simulated data created in the validation process
    simdata = NULL,
    #' @field timeBinMedian Median times for cluster bins
    timeBinMedian = NULL,
    #' @field tadBinMedian Median times after previous doses for cluster bins
    tadBinMedian = NULL,
    #' @field opDF Observed-predicted data frame
    opDF = NULL,
    #' @field npde Data for Normalized Prediction Distribution Error
    npde = NULL,
    #' @field npde_tad Data for Normalized Prediction Distribution Error
    #' using Time After Dose if available
    npde_tad = NULL,
    #' @field npde_stats Captured NPDE console summaries for later replay
    npde_stats = NULL,

    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' This function will create an object suitable for plotting visual predictive
    #' checks (VPCs) and prediction-corrected visual
    #' predictive checks (pcVPCs).
    #'
    #' @details
    #' The function will guide the user
    #' through appropriate clustering of doses, covariates and sample times for
    #' prediction correction using the methods of Bergstrand et al (2011).
    #' *NOTE:* Including `tad` is only
    #' valid if steady state conditions exist for each patient.
    #' This means that dosing is stable and regular
    #' for each patient, without changes in amount or timing, and that
    #' sampling occurs after the average concentrations
    #' are the same from dose to dose.  Otherwise observations are *NOT*
    #' superimposable and `tad` should
    #' *NOT* be used, i.e. should be set to `FALSE`.
    #'
    #' @param result The result of a prior run, usually supplied by calling the 
    #' `$validate()` method of a [PM_result] at the end of a run, or later loaded with [PM_load].
    #' @param tad `r template("tad")`
    #' @param binCov A character vector of the names of covariates which are included in the model, i.e. in the
    #' model equations and which need to be binned.  For example `binCov='wt'` if "wt" is included in a
    #' model equation like V=V0*wt, or `binCov=c( 'wt', 'crcl')` if both "wt" and "crcl"
    #' are included in model equations.
    #' @param doseC An integer with the number of dose/covariate bins to cluster, if known from a previous run of
    #' this function.  Including this value will skip the clustering portion for doses/covariates.
    #' @param timeC An integer with the number of observation time bins to cluster, if known from a previous run of
    #' this function.  Including this value will skip the clustering portion for observation times.
    #' @param tadC An integer with the number of time after dose bins to cluster, if known from a previous run of
    #' this function.  Including this value will skip the clustering portion for time after dose. This argument
    #' will be ignored if `tad=FALSE`.
    #' @param limits Limits on simulated parameters. See [PM_sim].
    #' @param ... Other parameters to be passed to [PM_sim], especially `limits`.
    #' @return An R6 object  of class `PM_valid`, which is a list with the following.
    #' * simdata The combined, simulated files for all subjects using the population mean values and each subject
    #' as a template. This object will be automatically saved to the run, to be loaded with
    #' [PM_load] next time.
    #' * timeBinMedian A data frame with the median times for each cluster bin.
    #' * tadBinMedian A data frame with the median time after dose (tad) for each cluster bin.
    #' This will be `NA` if `tad = FALSE`.
    #' * opDF A data frame with observations, predicitons, and bin-corrected predictions for each subject.
    #' * ndpe An object with results of normalized distrubition of prediction errors analysis.
    #' * npde_tad NPDE with time after dose rather than absolute time, if `tad = TRUE`
    #' * npde_stats Captured NPDE console summary text, replayable with `$show_npde_stats()`.
    #' @author Michael Neely
    #' @examples
    #' \dontrun{
    #' valid <- NPex$validate(limits = c(0, 3))
    #' }
    #'
    #' @export
    initialize = function(result, tad = FALSE, binCov, doseC, timeC, tadC, limits, ...) {
      checkRequiredPackages(c("mclust", "npde"))

      if (!inherits(result, "PM_result")) {
        cli::cli_abort(c(
          "x" = "Please supply a PM_result object to validate.",
          "i" = "PM_result objects are created by the {.fn PM_result$fit() method} or by {.fn PM_load}."
        ))
      }

      # Set default values for missing arguments
      args <- list(
        binCov = if (missing(binCov)) NULL else binCov,
        doseC = if (missing(doseC)) NULL else doseC,
        timeC = if (missing(timeC)) NULL else timeC,
        tadC = if (missing(tadC)) NULL else tadC,
        limits = if (missing(limits)) NULL else limits
      )

      valRes <- private$make(
        result = result,
        tad = tad,
        binCov = args$binCov,
        doseC = args$doseC,
        timeC = args$timeC,
        tadC = args$tadC,
        limits = args$limits,
        ...
      )
      # Assign results to public fields
      purrr::walk2(names(valRes), valRes, \(x, y) self[[x]] <- y)
    },
    #' @description Replay captured NPDE summary statistics.
    #' @param outeq Output equation number to replay.
    #' @param tad Logical; replay TAD-based NPDE stats.
    show_npde_stats = function(outeq = 1, tad = FALSE) {
      source <- if (isTRUE(tad)) self$npde_stats$tad else self$npde_stats$time

      if (is.null(source) || length(source) < outeq || is.null(source[[outeq]]) || length(source[[outeq]]) == 0) {
        cli::cli_abort(c(
          "x" = "No captured NPDE statistics are available for the requested selection.",
          "i" = "Run validation again if you need to regenerate NPDE console summaries."
        ))
      }

      cli::cli_text(source[[outeq]])
      invisible(source[[outeq]])
    },
    #' @description
    #' Summarize a [PM_valid] object. Reports NPDE distribution statistics and a
    #' Numerical Predictive Check (NPC) for each output equation. TAD-based results
    #' are included automatically when TAD validation was performed.
    #' @param probs Numeric vector of quantile cut-points for the NPC.
    #' Default is `c(0.05, 0.5, 0.95)`.
    summary = function(probs = c(0.05, 0.5, 0.95)) {
      # Detect TAD availability from stored fields
      has_tad <- !is.null(self$npde_tad) &&
        length(self$npde_tad) > 0 &&
        !is.null(self$npde_tad[[1]]) &&
        !is.null(self$simdata$data$obs$tad) &&
        any(!is.na(self$simdata$data$obs$tad))

      nout <- length(self$npde)

      # ------------------------------------------------------------------
      # Helpers
      # ------------------------------------------------------------------

      # Linear interpolation of simulation quantile rank for one observation
      sim_interp <- function(t, y, sim_sum, q_probs) {
        if (min(sim_sum$time) > t || max(sim_sum$time) < t) return(NA_real_)
        lo_t <- max(sim_sum$time[sim_sum$time <= t], na.rm = TRUE)
        hi_t <- min(sim_sum$time[sim_sum$time >= t], na.rm = TRUE)
        rank <- 0
        for (p in q_probs) {
          lo_v <- sim_sum$value[sim_sum$time == lo_t & sim_sum$quantile == p]
          hi_v <- sim_sum$value[sim_sum$time == hi_t & sim_sum$quantile == p]
          if (!length(lo_v) || !length(hi_v)) next
          interp_v <- if (lo_t != hi_t) {
            lo_v + (hi_v - lo_v) / (hi_t - lo_t) * (t - lo_t)
          } else {
            lo_v
          }
          if (y >= interp_v) rank <- p
        }
        rank
      }

      # Build a tidy quantile summary from simulated data
      build_sim_quant_df <- function(sim_df, time_col, q_probs) {
        sim_sub <- sim_df %>%
          dplyr::select(time = !!rlang::sym(time_col), out)
        times <- sort(unique(sim_sub$time))
        sim_sub %>%
          dplyr::group_by(time) %>%
          dplyr::group_map(~ quantile(.x$out, probs = q_probs, na.rm = TRUE)) %>%
          dplyr::tibble() %>%
          tidyr::unnest_longer(col = 1, indices_to = "quantile", values_to = "value") %>%
          dplyr::mutate(
            time     = rep(times, each = length(q_probs)),
            quantile = readr::parse_number(quantile) / 100
          ) %>%
          dplyr::select(time, quantile, value)
      }

      # Compute NPC for one output equation
      compute_npc <- function(outeq_idx, use_tad = FALSE) {
        time_col <- if (use_tad) "tad" else "time"

        sim_df <- self$simdata$data$obs %>%
          dplyr::filter(
            outeq == outeq_idx,
            !is.na(.data[[time_col]])
          )
        obs_df <- self$opDF %>%
          dplyr::filter(
            outeq == outeq_idx,
            !is.na(.data[[time_col]])
          )

        if (nrow(sim_df) == 0 || nrow(obs_df) == 0) return(NULL)

        sim_quant_df <- build_sim_quant_df(sim_df, time_col, probs)

        obs_time <- obs_df[[time_col]]
        obs_out  <- obs_df$obs

        q_rank <- vapply(
          seq_along(obs_time),
          \(i) sim_interp(obs_time[[i]], obs_out[[i]], sim_quant_df, probs),
          numeric(1)
        )

        not_miss <- sum(!is.na(q_rank))
        if (not_miss == 0) return(NULL)

        pct_below <- vapply(probs, \(p) sum(q_rank < p, na.rm = TRUE) / not_miss, numeric(1))
        pvals     <- vapply(probs, \(p) {
          s <- sum(q_rank < p, na.rm = TRUE)
          tryCatch(binom.test(s, not_miss, p, "two")$p.value, error = \(e) NA_real_)
        }, numeric(1))

        npc_tbl <- data.frame(
          Quantile    = paste0(formatC(probs * 100, format = "f", digits = 1), "%"),
          Prop_below  = paste0(formatC(pct_below * 100, format = "f", digits = 1), "%"),
          P_value     = formatC(pvals, format = "f", digits = 3),
          stringsAsFactors = FALSE
        )
        names(npc_tbl) <- c("Quantile", "Prop. Below", "P-value")

        # proportion within 5th-95th PI (always computed against 0.05/0.95)
        between_rank <- vapply(
          seq_along(obs_time),
          \(i) sim_interp(obs_time[[i]], obs_out[[i]], sim_quant_df, c(0.05, 0.95)),
          numeric(1)
        )
        in90   <- sum(between_rank >= 0.05 & between_rank < 0.95, na.rm = TRUE)
        pct90  <- in90 / not_miss
        pval90 <- tryCatch(
          binom.test(in90, not_miss, 0.9, "two")$p.value,
          error = \(e) NA_real_
        )

        list(
          tbl      = npc_tbl,
          pct90    = pct90,
          pval90   = pval90,
          not_miss = not_miss,
          total    = nrow(obs_df)
        )
      }

      # Print NPC result block
      print_npc <- function(npc, label) {
        cli::cli_text(
          "{.strong NPC ({label})} \u2014 {npc$not_miss} of {npc$total} observations within simulated range"
        )
        print(npc$tbl, row.names = FALSE, right = FALSE)
        cat(sprintf(
          "  Proportion within 5th\u201395th percentile interval: %.1f%% (p = %.3f)\n",
          npc$pct90 * 100,
          npc$pval90
        ))
      }

      # ------------------------------------------------------------------
      # Output
      # ------------------------------------------------------------------

      cli::cli_h1("PM_valid Summary")

      for (i in seq_len(nout)) {
        cli::cli_h2("Output Equation {i}")

        # NPDE stats (time)
        stats_t <- self$npde_stats$time[[i]]
        if (!is.null(stats_t) && length(stats_t) > 0) {
          cli::cli_text("{.strong NPDE (time)}")
          cat(paste(stats_t, collapse = "\n"), "\n")
        }

        # NPDE stats (TAD)
        if (has_tad) {
          stats_tad <- self$npde_stats$tad[[i]]
          if (!is.null(stats_tad) && length(stats_tad) > 0) {
            cat("\n")
            cli::cli_text("{.strong NPDE (TAD)}")
            cat(paste(stats_tad, collapse = "\n"), "\n")
          }
        }

        # NPC (time)
        npc_t <- compute_npc(i, use_tad = FALSE)
        if (!is.null(npc_t)) {
          cat("\n")
          print_npc(npc_t, "time")
        }

        # NPC (TAD)
        if (has_tad) {
          npc_tad <- compute_npc(i, use_tad = TRUE)
          if (!is.null(npc_tad)) {
            cat("\n")
            print_npc(npc_tad, "TAD")
          }
        }
      }

      invisible(self)
    },
    #' @description
    #' Plot method. Calls [plot.PM_valid].
    #' @param ... Arguments to pass to \[plot.PM_valid].
    plot = function(...) {
      plot.PM_valid(self, ...)
    }
  ), # end public
  private = list(
    make = function(result, tad = F, binCov, doseC, timeC, tadC, limits, ...) {
      # verify packages used in this function
      if (!checkRequiredPackages(c("mclust", "npde"), quietly = FALSE)) {
        return(invisible(NULL))
      } else {
        require(mclust, quietly = TRUE)
        require(npde, quietly = TRUE)
      }

      # save current wd
      currwd <- getwd()


      # parse dots
      arglist <- list(...)
      namesSIM <- names(formals(PM_sim$public_methods$initialize))
      # namesNPDE <- names(formals(autonpde))
      argsSIM <- arglist[which(names(arglist) %in% namesSIM)]

      # Cluster raw data --------------------------------------------------------


      # remove missing observations
      mdata <- result$data$standard_data %>%
        filter(is.na(out) | out != -99) # preserves doses and non-missing


      # Handle include/exclude IDs
      if ("include" %in% names(argsSIM)) {
        mdata <- mdata %>% filter(id %in% argsSIM$include)
        includeID <- argsSIM$include
        argsSIM$include <- NULL
      } else {
        includeID <- NULL
      }
      if ("exclude" %in% names(argsSIM)) {
        mdata <- mdata %>% filter(!id %in% argsSIM$exclude)
        excludeID <- argsSIM$exclude
        argsSIM$exclude <- NULL
      } else {
        excludeID <- NULL
      }

      # Get TAD lazily if needed
      valTAD_cache <- NULL
      get_tad_values <- function() {
        if (is.null(valTAD_cache)) {
          valTAD_cache <<- calcTAD(mdata)
        }
        valTAD_cache
      }

      # number of subjects
      nsub <- length(unique(mdata$id))


      # define covariates in model to be binned
      covData <- getCov(mdata)
      cov_names <- if (covData$ncov > 0) covData$covnames else character(0)

      if (is.null(binCov) || length(binCov) == 0) {
        binCov <- character(0)
      }
      if (length(binCov) > 0 && !all(binCov %in% cov_names)) {
        cli::cli_abort(c("x" = "You have entered covariates which are not valid covariates in your data."))
      }
      binCov <- cov_names[cov_names %in% binCov]

      build_cluster_data <- function(covariates, use_tad = tad) {
        use_cov <- cov_names[cov_names %in% covariates]

        data_sub <- mdata %>%
          select(id, evid, time, out, dose, all_of(use_cov)) %>%
          mutate(tad = if (isTRUE(use_tad)) get_tad_values() else NA) %>%
          dplyr::relocate(tad, .after = time)

        data_sub_dc <- data_sub %>%
          filter(evid > 0) %>%
          select(c("id", "dose", dplyr::all_of(use_cov))) %>%
          mutate(dose = ifelse(dose == 0, NA, dose)) %>%
          group_by(id) %>%
          mutate(dose = {
            first_valid_dose <- dose[!is.na(dose)][1]
            tidyr::replace_na(dose, first_valid_dose)
          }) %>%
          fill(everything(), .direction = "down") %>%
          ungroup()

        data_sub_time <- data_sub %>%
          filter(evid == 0) %>%
          select(time)

        data_sub_tad <- if (isTRUE(use_tad)) data_sub$tad[data_sub$evid == 0] else NULL

        list(
          dataSub = data_sub,
          dataSubDC = data_sub_dc,
          dataSubTime = data_sub_time,
          dataSubTad = data_sub_tad,
          binCov = use_cov,
          use_tad = isTRUE(use_tad)
        )
      }

      cluster_data <- build_cluster_data(binCov, use_tad = tad)
      dataSub <- cluster_data$dataSub
      dataSubDC <- cluster_data$dataSubDC
      dataSubTime <- cluster_data$dataSubTime
      if (tad) {
        dataSubTad <- cluster_data$dataSubTad
      }

      # ELBOW PLOT for clustering if used
      elbow <- function(x) {
        set.seed(123)
        # Compute and plot wss for k = 2 to k = 15.
        # set k.max
        if (is.null(dim(x))) {
          k.max <- min(length(unique(x)), 15)
        } else {
          k.max <- min(nrow(unique(x)), 15)
        }

        wss <- sapply(
          2:k.max,
          function(k) {
            val <- kmeans(x, k, nstart = 50, iter.max = 15)
            val$tot.withinss
          }
        )
        plot(2:k.max, wss,
          type = "b", pch = 19, frame = FALSE,
          xlab = "Number of clusters",
          ylab = "Total within-clusters sum of squares (WSS)"
        )

        invisible(wss)

      }

      suggest_clusters <- function(x) {
        if (is.null(dim(x))) {
          n_unique <- length(unique(x))
        } else {
          n_unique <- nrow(unique(x))
        }
        if (is.na(n_unique) || n_unique < 2) {
          return(1)
        }
        mclust::Mclust(x, verbose = FALSE)$G
      }

      clamp_centers <- function(x, centers) {
        n_unique <- if (is.null(dim(x))) length(unique(x)) else nrow(unique(x))
        if (is.na(n_unique) || n_unique < 1) {
          return(1)
        }
        max(1, min(as.integer(round(centers)), n_unique))
      }

      needs_dose <- length(doseC) == 0
      needs_time <- length(timeC) == 0
      needs_tad <- isTRUE(tad) && length(tadC) == 0

      if (needs_dose || needs_time || needs_tad) {
        if (!interactive()) {
          cli::cli_abort(c(
            "x" = "Interactive clustering requires a Shiny session.",
            "i" = "In non-interactive use, provide {.arg doseC}, {.arg timeC}, and {.arg tadC} (if {.arg tad = TRUE})."
          ))
        }

        if (!requireNamespace("shiny", quietly = TRUE)) {
          cli::cli_abort(c(
            "x" = "Package {.pkg shiny} is required for interactive PM_valid clustering.",
            "i" = "Install {.pkg shiny} or provide {.arg doseC}, {.arg timeC}, and {.arg tadC} directly."
          ))
        }

        default_dose <- if (needs_dose) suggest_clusters(dataSubDC) else as.numeric(doseC)[1]
        default_time <- if (needs_time) suggest_clusters(dataSubTime) else as.numeric(timeC)[1]
        default_tad <- if (length(tadC) > 0) {
          as.numeric(tadC)[1]
        } else {
          suggest_clusters(build_cluster_data(binCov, use_tad = TRUE)$dataSubTad)
        }

        plot_time_with_centers <- function(data_sub, use_data, centers, label = "Time", x_var = c("time", "tad")) {
          x_var <- match.arg(x_var)
          if (all(is.na(use_data))) {
            plot.new()
            title(main = paste(label, "has only missing values"))
            return(invisible(NULL))
          }
          plot_data <- data_sub %>%
            filter(evid == 0, !is.na(out))

          if (identical(x_var, "tad")) {
            plot_data <- plot_data %>% filter(!is.na(tad))
          }

          plot_formula <- if (identical(x_var, "tad")) {
            out ~ tad
          } else {
            out ~ time
          }
          plot(
            plot_formula,
            data = plot_data,
            xlab = label,
            ylab = "Observation",
            xlim = c(min(use_data, na.rm = TRUE), max(use_data, na.rm = TRUE))
          )
          k <- stats::kmeans(use_data, centers = clamp_centers(use_data, centers), nstart = 50)
          abline(v = as.numeric(k$centers), col = "red")
          invisible(NULL)
        }

        cli::cli_bullets(c(
          ">" = "{.strong Step 1}: cluster dosing, selected covariates, and observation times across the population.",
          ">" = "Launching interactive Shiny clustering app..."
        ))

        instruction_panel <- function(title, lines) {
          shiny::tags$details(
            shiny::tags$summary(title, style = "display: list-item; cursor: pointer;"),
            shiny::tags$ul(lapply(lines, shiny::tags$li))
          )
        }

        ui <- shiny::fluidPage(
          shiny::titlePanel("PM_valid clustering"),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              if (covData$ncov > 0) {
                shiny::checkboxGroupInput(
                  "binCov",
                  "Covariates to include in dose/covariate clustering",
                  choices = cov_names,
                  selected = binCov
                )
              },
              shiny::checkboxInput("use_tad", "Include Time After Dose (TAD) clustering", value = isTRUE(tad)),
              if (needs_dose) shiny::numericInput("doseC", "Dose/covariate clusters", value = default_dose, min = 1, step = 1),
              if (needs_time) shiny::numericInput("timeC", "Sample-time clusters", value = default_time, min = 1, step = 1),
              shiny::conditionalPanel(
                condition = "input.use_tad",
                shiny::numericInput("tadC", "TAD clusters", value = default_tad, min = 1, step = 1)
              ),
              shiny::actionButton("apply", "Go", class = "btn-success", style = "color: #fff;"),
              shiny::actionButton("cancel", "Cancel")
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                if (needs_dose) {
                  shiny::tabPanel(
                    "Dose/Covariates",
                    instruction_panel(
                      "How to use the dose/covariate classification plot",
                      c(
                        "Off-diagonal panels show the relationship between the row/column variable values.",
                        "Each point is a dose/covariate record grouped by the current model-based cluster assignment.",
                        "Increase or decrease Dose/covariate clusters to group points as much as possible with the greatest separation between groups.",
                        "Use this plot to decide whether additional clusters reveal meaningful structure or just overly segment the data."
                      )
                    ),
                    shiny::plotOutput("dose_class"),
                    instruction_panel(
                      "How to use the dose/covariate elbow plot",
                      c(
                        "The elbow shows how within-cluster variation decreases as cluster count increases.",
                        "Look for the bend where extra clusters give only small improvement in WSS.",
                        "Choose that elbow value as a practical balance between fit and simplicity."
                      )
                    ),
                    shiny::plotOutput("dose_elbow")
                  )
                },
                if (needs_time) {
                  shiny::tabPanel(
                    "Sample Times",
                    instruction_panel(
                      "How to use the sample-time classification plot",
                      c(
                        "Points are grouped by observation time clusters.",
                        "Adjust Sample-time clusters and confirm whether clusters remain interpretable and well separated.",
                        "Prefer settings that preserve clinically relevant timing structure."
                      )
                    ),
                    shiny::plotOutput("time_class"),
                    instruction_panel(
                      "How to use the observation plot",
                      c(
                        "Observations are plotted relative to cluster times."
                      
                      )
                    ),
                    shiny::plotOutput("time_cluster"),
                    instruction_panel(
                      "How to use the sample-time elbow plot",
                      c(
                        "The elbow summarizes diminishing returns as more time clusters are added.",
                        "A clear bend indicates a reasonable cluster count for time binning.",
                        "Use this together with the classification view before applying your final choice."
                      )
                    ),
                    shiny::plotOutput("time_elbow")
                  )
                },
                shiny::tabPanel(
                  "TAD",
                  shiny::conditionalPanel(
                    condition = "input.use_tad",
                    instruction_panel(
                      "How to use the TAD classification plot",
                      c(
                        "This plot groups observations by model-based clusters of time-after-dose values.",
                        "Adjust TAD clusters to inspect whether the grouping remains stable and clinically interpretable.",
                        "Use this view with the elbow plot before finalizing your TAD cluster count."
                      )
                    ),
                    shiny::plotOutput("tad_class"),
                    instruction_panel(
                      "How to use the observation plot",
                      c(
                        "Observations are plotted relative to clustered times after doses."
                      
                      )
                    ),
                    shiny::plotOutput("tad_cluster"),
                    instruction_panel(
                      "How to use the TAD elbow plot",
                      c(
                        "The elbow reflects how much within-cluster variation drops as TAD cluster count increases.",
                        "Select the bend point where additional clusters have diminishing benefit.",
                        "Prefer the smallest count that still captures meaningful TAD structure."
                      )
                    ),
                    shiny::plotOutput("tad_elbow")
                  ),
                  shiny::conditionalPanel(
                    condition = "!input.use_tad",
                    shiny::tags$p("Enable 'Use TAD clustering' to configure and inspect TAD clusters.")
                  )
                )
              )
            )
          )
        )

        server <- function(input, output, session) {
          selected_cov <- shiny::reactive({
            if (covData$ncov == 0) {
              character(0)
            } else {
              cov_names[cov_names %in% input$binCov]
            }
          })

          use_tad <- shiny::reactive({
            isTRUE(input$use_tad)
          })

          reactive_cluster_data <- shiny::reactive({
            build_cluster_data(selected_cov(), use_tad = use_tad())
          })

          suggested_dose_clusters <- shiny::reactive({
            shiny::req(needs_dose)
            suggest_clusters(reactive_cluster_data()$dataSubDC)
          })

          suggested_time_clusters <- shiny::reactive({
            shiny::req(needs_time)
            suggest_clusters(reactive_cluster_data()$dataSubTime)
          })

          shiny::observeEvent(selected_cov(), {
            if (needs_dose) {
              shiny::updateNumericInput(
                session,
                "doseC",
                value = suggested_dose_clusters()
              )
            }
            if (needs_time) {
              shiny::updateNumericInput(
                session,
                "timeC",
                value = suggested_time_clusters()
              )
            }
          }, ignoreInit = TRUE)

          if (needs_dose) {
            output$dose_class <- shiny::renderPlot({
              dose_data <- reactive_cluster_data()$dataSubDC
              selected_g <- clamp_centers(dose_data, input$doseC)
              dose_mod <- mclust::Mclust(dose_data, G = selected_g, verbose = FALSE)
              plot(dose_mod, "classification")
            })
            output$dose_elbow <- shiny::renderPlot({
              elbow(reactive_cluster_data()$dataSubDC)
            })
          }

          if (needs_time) {
            output$time_class <- shiny::renderPlot({
              mod <- mclust::Mclust(reactive_cluster_data()$dataSubTime, verbose = FALSE)
              plot(mod, "classification")
            })
            output$time_cluster <- shiny::renderPlot({
              cluster_data <- reactive_cluster_data()
              plot_time_with_centers(
                cluster_data$dataSub,
                cluster_data$dataSubTime,
                max(1, as.numeric(input$timeC)),
                "Time",
                x_var = "time"
              )
            })
            output$time_elbow <- shiny::renderPlot({
              elbow(reactive_cluster_data()$dataSubTime)
            })
          }

          output$tad_class <- shiny::renderPlot({
            shiny::req(use_tad())
            mod <- mclust::Mclust(reactive_cluster_data()$dataSubTad, verbose = FALSE)
            plot(mod, "classification")
          })
          output$tad_cluster <- shiny::renderPlot({
            shiny::req(use_tad())
            cluster_data <- reactive_cluster_data()
            plot_time_with_centers(
              cluster_data$dataSub,
              cluster_data$dataSubTad,
              max(1, as.numeric(input$tadC)),
              "Time after dose",
              x_var = "tad"
            )
          })
          output$tad_elbow <- shiny::renderPlot({
            shiny::req(use_tad())
            elbow(reactive_cluster_data()$dataSubTad)
          })

          shiny::observeEvent(input$apply, {
            selected_data <- reactive_cluster_data()
            selected <- list(
              doseC = if (needs_dose) max(1, as.numeric(input$doseC)) else as.numeric(doseC)[1],
              timeC = if (needs_time) max(1, as.numeric(input$timeC)) else as.numeric(timeC)[1],
              tad = selected_data$use_tad,
              tadC = if (isTRUE(selected_data$use_tad)) max(1, as.numeric(input$tadC)) else NA_real_,
              binCov = selected_data$binCov
            )
            shiny::stopApp(selected)
          })

          shiny::observeEvent(input$cancel, {
            shiny::stopApp(NULL)
          })
        }

        app_result <- shiny::runApp(shiny::shinyApp(ui = ui, server = server), launch.browser = TRUE)
        if (is.null(app_result)) {
          cli::cli_abort(c(
            "x" = "Clustering selection was cancelled.",
            "i" = "Re-run and complete the Shiny dialog, or provide cluster settings directly."
          ))
        }

        if (needs_dose) doseC <- as.numeric(app_result$doseC)
        if (needs_time) timeC <- as.numeric(app_result$timeC)
        tad <- isTRUE(app_result$tad)
        tadC <- if (isTRUE(tad)) as.numeric(app_result$tadC) else NULL
        binCov <- cov_names[cov_names %in% app_result$binCov]

        cluster_data <- build_cluster_data(binCov, use_tad = tad)
        dataSub <- cluster_data$dataSub
        dataSubDC <- cluster_data$dataSubDC
        dataSubTime <- cluster_data$dataSubTime
        if (tad) {
          dataSubTad <- cluster_data$dataSubTad
        }
      }

      # now set the cluster bins
      dcClusters <- stats::kmeans(dataSubDC, centers = clamp_centers(dataSubDC, doseC), nstart = 50)
      dataSub$dcBin[dataSub$evid > 0] <- dcClusters$cluster # m=dose,covariate bins

      timeClusters <- stats::kmeans(dataSubTime, centers = clamp_centers(dataSubTime, timeC), nstart = 50)
      # dataSub$timeBin[dataSub$evid == 0] <- sapply(timeClusters$cluster, function(x) which(order(timeClusters$centers) == x)) # n=ordered time bins
      dataSub$timeBin[dataSub$evid == 0] <- timeClusters$cluster

      if (tad) {
        tadClusters <- stats::kmeans(dataSubTad, centers = clamp_centers(dataSubTad, tadC), nstart = 50)
        # dataSub$tadBin[dataSub$evid == 0] <- sapply(tadClusters$cluster, function(x) which(order(tadClusters$centers) == x)) # n=ordered time bins
        dataSub$tadBin[dataSub$evid == 0] <- tadClusters$cluster
      } else {
        dataSub$tadBin <- NA
      }

      # Simulations -------------------------------------------------------------


      # simulate PRED_bin from pop icen parameter values and median of each bin for each subject
      # first, calculate median of each bin
      dcMedian <- dataSub %>%
        dplyr::group_by(bin = dcBin) %>%
        dplyr::filter(!is.na(dose)) %>%
        dplyr::summarize(dplyr::across(c(dose, !!binCov), \(x) median(x, na.rm = TRUE)))

      timeMedian <- dataSub %>%
        group_by(bin = timeBin) %>%
        dplyr::filter(!is.na(timeBin)) %>%
        dplyr::summarize(time = median(time, na.rm = T)) %>%
        dplyr::arrange(time)

      if (tad) {
        tadMedian <- dataSub %>%
          dplyr::group_by(bin = tadBin) %>%
          dplyr::filter(!is.na(tadBin)) %>%
          dplyr::summarize(time = median(tad, na.rm = T)) %>%
          dplyr::arrange(time)
      } else {
        tadMedian <- NA
      }

      # create  datafile based on mdata, but with covariates and doses replaced by medians
      # and sample times by bin times
      mdataMedian <- mdata
      mdataMedian$dcBin <- dataSub$dcBin
      mdataMedian$timeBin <- dataSub$timeBin
      # no need for tadBin as we don't simulate with tad
      mdataMedian$dose <- dcMedian[[2]][match(mdataMedian$dcBin, dcMedian$bin)]
      mdataMedian$time[mdataMedian$evid == 0] <- timeMedian$time[match(mdataMedian$timeBin[mdataMedian$evid == 0], timeMedian$bin)]
      covCols <- which(names(mdataMedian) %in% binCov)
      if (length(covCols) > 0) {
        for (i in covCols) {
          dcMedianCol <- which(names(dcMedian) == names(mdataMedian[i]))
          mdataMedian[, i] <- dcMedian[match(mdataMedian$dcBin, dcMedian$bin), dcMedianCol]
        }
      }
      # write median file
      # MedianDataFileName <- paste(substr(paste("m_", strsplit(datafileName, "\\.")[[1]][1], sep = ""), 0, 8), ".csv", sep = "")
      # FIXME
      # TEMPORARY FIX - @Julian: I have an example of a $valid call that generates dosis at unordered times, just want to get pass that
      fil_data <- mdataMedian[, 1:(ncol(mdataMedian) - 2)]
      fil_data <- fil_data[order(fil_data$id, fil_data$time), ]
      # END TEMPORARY FIX
      medianData <- PM_data$new(fil_data, quiet = TRUE)

      # remove old files
      invisible(file.remove(Sys.glob("sim*.txt")))

      # get poppar and make one with zero covariance
      poppar <- result$final$data
      popparZero <- poppar
      popparZero$popCov[popparZero$popCov != 0] <- 0
      # do the simulation for each subject using the median dose, median covariates and pop parameters
      if ("seed" %in% names(argsSIM)) {
        seed.start <- argsSIM$seed
        argsSIM[[which(names(argsSIM) == "seed")]] <- NULL
      } else {
        seed.start <- -17
      }
      set.seed(seed.start)
      if ("nsim" %in% names(argsSIM)) {
        nsim <- argsSIM$nsim
        argsSIM[[which(names(argsSIM) == "nsim")]] <- NULL
      } else {
        nsim <- 1000
      }
      if ("limits" %in% names(argsSIM)) {
        limits <- argsSIM$limits
        argsSIM[[which(names(argsSIM) == "limits")]] <- NULL
      } else {
        limits <- NA
      }
      argsSIM1 <- c(list(
        poppar = popparZero, data = medianData, model = result$model, nsim = 1,
        seed = runif(nsub, -100, 100),
        limits = limits, quiet = TRUE
      ), argsSIM)
      
      cli::cli_bullets(c(">" = "{.strong Step 2}:Simulating one version of each subject using medians of binned data..."))

      PRED_bin <- do.call(PM_sim$new, argsSIM1)
      PRED_bin$data$obs <- PRED_bin$data$obs %>% filter(!is.na(out))

      # make tempDF subset of PM_op for subject, time, non-missing obs, outeq, pop predictions (PREDij)
      tempDF <- if (inherits(result$op, "PM_op")) {
        result$op$data
      } else {
        result$op
      }

      tempDF <- tempDF %>%
        filter(obs != -99, time > 0, pred.type == "pop", icen == "median") %>%
        includeExclude(includeID, excludeID) %>%
        arrange(id, time, outeq)

      if (tad) {
        tempDF$tad <- dataSub$tad[dataSub$evid == 0]
      } else {
        tempDF$tad <- NA
      }


      # add PRED_bin to tempDF
      tempDF$PRED_bin <- PRED_bin$obs$out

      # add pcYij column to tempDF as obs * PREDbin/PREDij
      tempDF$pcObs <- tempDF$obs * tempDF$PRED_bin / tempDF$pred

      # bin pcYij by time and add to tempDF
      tempDF$timeBinNum <- dataSub$timeBin[dataSub$evid == 0]
      tempDF$timeBinMedian <- timeMedian$time[match(tempDF$timeBinNum, timeMedian$bin)]
      if (tad) {
        tempDF$tadBinNum <- dataSub$tadBin[dataSub$evid == 0]
        tempDF$tadBinMedian <- tadMedian$time[match(tempDF$tadBinNum, tadMedian$bin)]
      } else {
        tempDF$tadBinNum <- NA
        tempDF$tadBinMedian <- NA
      }


      # Now, simulate using full pop model
      # write the adjusted mdata file first
      fullData <- PM_data$new(mdata, quiet = TRUE)
      # fullData$save(datafileName)

      set.seed(seed.start)
      argsSIM2 <- c(
        list(
          poppar = poppar, data = fullData, model = result$model, nsim = nsim,
          seed = runif(nsub, -100, 100), limits = limits,
          quiet = TRUE
        ),
        argsSIM
      )
      if (length(includeID) > 0) {
        argsSIM2$include <- includeID
      }
      if (length(excludeID) > 0) {
        argsSIM2$exclude <- excludeID
      }
      
      cli::cli_bullets(c(">" = "{.strong Step 3}:Simulating {nsim} versions of each subject using original data..."))
      simFull <- do.call(PM_sim$new, argsSIM2)
      # take out observations at time 0 from evid=4 and missing outputs
      simFull$data$obs <- simFull$data$obs %>% filter(time > 0, out != -99)


      # add TAD for plotting options
      if (tad) {
        simFull$data$obs$tad <- dataSub %>%
          dplyr::filter(evid == 0) %>%
          dplyr::group_by(id) %>%
          dplyr::group_map(~ rep(.x$tad, nsim)) %>%
          unlist()
      } else {
        simFull$data$obs$tad <- NA
      }


      # pull in time bins from tempDF; only need median as tempDF contains median and mean,
      # but simulation is only from pop means
      simFull$data$obs$timeBinNum <- dataSub %>%
        dplyr::filter(evid == 0) %>%
        dplyr::group_by(id) %>%
        dplyr::group_map(~ rep(.x$timeBin, nsim)) %>%
        unlist()

      # pull in tad bins from tempDF
      simFull$data$obs$tadBinNum <- dataSub %>%
        dplyr::filter(evid == 0) %>%
        dplyr::group_by(id) %>%
        dplyr::group_map(~ rep(.x$tadBin, nsim)) %>%
        unlist()

      # make simulation number 1:nsim
      simFull$data$obs$simnum <- as.numeric(sapply(strsplit(simFull$data$obs$id, "\\."), function(x) x[1]))
      # class(simFull) <- c("PMsim", "list")

      # NPDE -------------------------------------------------------------------

      # prepare data for npde
      obs <- tempDF %>% select(id, time, tad, out = obs, outeq)


      # remove missing obs
      obs <- obs %>% filter(out != -99)

      simobs <- simFull$data$obs
      # remove missing simulations
      simobs <- simobs %>% filter(out != -99)
      simobs$id <- rep(obs$id, times = nsim)

      simobs <- simobs %>% select(id, time, tad, out, outeq)


      # get number of outeq
      nout <- max(obs$outeq, na.rm = T)
      npde <- vector("list", nout)
      npdeTAD <- vector("list", nout)
      npdeStats <- vector("list", nout)
      npdeTADStats <- vector("list", nout)

      prepare_npde_pair <- function(obs_df, sim_df) {
        id_levels <- unique(obs_df$id)

        obs_clean <- obs_df %>%
          dplyr::mutate(
            id = match(id, id_levels),
            time = suppressWarnings(as.numeric(time)),
            out = suppressWarnings(as.numeric(out))
          ) %>%
          filter(!is.na(id), is.finite(time), is.finite(out)) %>%
          arrange(id, time)

        sim_clean <- sim_df %>%
          dplyr::filter(id %in% id_levels) %>%
          dplyr::mutate(
            id = match(id, id_levels),
            time = suppressWarnings(as.numeric(time)),
            out = suppressWarnings(as.numeric(out))
          ) %>%
          dplyr::filter(!is.na(id), is.finite(time), is.finite(out)) %>%
          dplyr::arrange(id, time)

        list(obs = data.frame(obs_clean), sim = data.frame(sim_clean))
      }

      run_autonpde_silent <- function(...) {
        npde_obj <- NULL
        console_text <- capture.output(
          npde_obj <- suppressMessages(
            suppressWarnings(
              npde::autonpde(...)
            )
          ),
          type = "output"
        )

        list(result = npde_obj, console = console_text)
      }

      for (thisout in 1:nout) {
        obs_sub <- obs %>%
          dplyr::filter(outeq == thisout) %>%
          dplyr::select(id, time, out) %>%
          dplyr::arrange(id, time)
        sim_sub <- simobs %>%
          dplyr::filter(outeq == thisout, id %in% obs_sub$id) %>%
          dplyr::select(id, time, out) %>%
          dplyr::arrange(id, time)
        clean_pair <- prepare_npde_pair(obs_sub, sim_sub)
        obs_sub <- clean_pair$obs
        sim_sub <- clean_pair$sim

        if (tad) {
          obs_sub2 <- obs %>%
            dplyr::filter(outeq == thisout) %>%
            dplyr::select(id, time = tad, out) %>%
            dplyr::arrange(id, time)
          sim_sub2 <- simobs %>%
            dplyr::filter(outeq == thisout, id %in% obs_sub$id) %>%
            dplyr::select(id, time = tad, out) %>%
            dplyr::arrange(id, time)
          clean_pair_tad <- prepare_npde_pair(obs_sub2, sim_sub2)
          obs_sub2 <- clean_pair_tad$obs
          sim_sub2 <- clean_pair_tad$sim
        }
        # get NPDE decorr.method = "inverse",

        npde_run <- tryCatch(
          run_autonpde_silent(
            obs_sub, sim_sub,
            iid = "id", ix = "time", iy = "out",
            detect = FALSE,
            verbose = FALSE,
            boolsave = FALSE
          ),
          error = function(e) e
        )

        if (inherits(npde_run, "error")) {
          npde[[thisout]] <- npde_run
          npdeStats[[thisout]] <- character()
        } else {
          npde[[thisout]] <- npde_run$result
          npdeStats[[thisout]] <- npde_run$console
        }

        if (inherits(npde[[thisout]], "error")) { # error, often due to non pos-def matrix
          npde_run <- tryCatch(
            run_autonpde_silent(
              obs_sub, sim_sub,
              iid = "id", ix = "time", iy = "out",
              detect = FALSE,
              verbose = FALSE,
              boolsave = FALSE,
              decorr.method = "inverse"
            ),
            error = function(e) e
          )

          if (inherits(npde_run, "error")) {
            npde[[thisout]] <- paste0("Unable to calculate NPDE for outeq ", thisout, ": ", npde_run$message)
            npdeStats[[thisout]] <- character()
          } else {
            npde[[thisout]] <- npde_run$result
            npdeStats[[thisout]] <- c(
              npde_run$console,
              paste0(
                "NOTE: Due to numerical instability, for outeq ",
                thisout,
                " inverse decorrelation applied, not Cholesky (the default)."
              )
            )
          }
        }

        # get NPDE for TAD
        if (tad) {
          npde_tad_run <- tryCatch(
            run_autonpde_silent(
              obs_sub2, sim_sub2,
              iid = "id", ix = "time", iy = "out",
              detect = FALSE,
              verbose = FALSE,
              boolsave = FALSE
            ),
            error = function(e) e
          )

          if (inherits(npde_tad_run, "error")) {
            npdeTAD[[thisout]] <- npde_tad_run
            npdeTADStats[[thisout]] <- character()
          } else {
            npdeTAD[[thisout]] <- npde_tad_run$result
            npdeTADStats[[thisout]] <- npde_tad_run$console
          }

          if (inherits(npdeTAD[[thisout]], "error")) { # error, often due to non pos-def matrix
            npde_tad_run <- tryCatch(
              run_autonpde_silent(
                obs_sub2, sim_sub2,
                iid = "id", ix = "time", iy = "out",
                detect = FALSE,
                verbose = FALSE,
                boolsave = FALSE,
                decorr.method = "inverse"
              ),
              error = function(e) e
            )

            if (inherits(npde_tad_run, "error")) {
              npdeTAD[[thisout]] <- paste0("Unable to calculate NPDE with TAD for outeq ", thisout, ": ", npde_tad_run$message)
              npdeTADStats[[thisout]] <- character()
            } else {
              npdeTAD[[thisout]] <- npde_tad_run$result
              npdeTADStats[[thisout]] <- c(
                npde_tad_run$console,
                paste0(
                  "NOTE: Due to numerical instability, for outeq ",
                  thisout,
                  " and TAD, inverse decorrelation applied, not Cholesky (the default)."
                )
              )
            }
          }
        }
      }

      # Finish Up ----------------------------------------------------------------


      valRes <- list(
        simdata = PM_sim$new(simFull),
        timeBinMedian = timeMedian,
        tadBinMedian = tadMedian,
        opDF = tempDF,
        npde = npde,
        npde_tad = npdeTAD,
        npde_stats = list(time = npdeStats, tad = npdeTADStats)
      )

      return(invisible(valRes))
    } # end make function
  ) # end private
)

#' @title Summary Method for PM_valid Objects
#' @description
#' `r lifecycle::badge('stable')`
#' Prints NPDE distribution statistics and a Numerical Predictive Check (NPC)
#' for each output equation in the [PM_valid] object. TAD-based results are
#' included automatically when TAD validation was performed.
#' @param object A [PM_valid] object.
#' @param probs Numeric vector of quantile cut-points for the NPC.
#' Default is `c(0.05, 0.5, 0.95)`.
#' @param ... Ignored; present for S3 compatibility.
#' @export
summary.PM_valid <- function(object, probs = c(0.05, 0.5, 0.95), ...) {
  object$summary(probs = probs)
}


# PLOT --------------------------------------------------------------------
#' @title Plot Pmetrics Validation Objects
#' @description
#' `r lifecycle::badge('stable')`
#' Usually called by the `$plot` method for [PM_valid] objects, which are in turn typically added
#' to a [PM_result] object by the `$validate` method. For example:
#' ```
#'    NPex$validate(limits = c(0, 3)) # creates a PM_valid object and adds it to the $valid field of NPex
#'    NPex$valid$plot(type = "vpc", tad = TRUE, log = TRUE) # now we can plot it
#' ```
#' 
#' Plot [PM_valid] objects.
#' @details
#' Generates a plot of outputs (typically concentrations) on the y axis and time
#' on the x axis. If `tad` was set to `TRUE`
#' when [make_valid] was called, then time may be either absolute (default) or time
#' after dose, controlled by the `tad` argument to this plot function.
#' The following items are included in the plot:
#' * Observed outputs. These may be either as measured for `type = "vpc"` or
#' prediction corrected for `type = "pcvpc"`. Format of the observations is
#' controlled by the `marker` argument. The default is
#' `list(color = "black", symbol = "circle-open", size = 8)`.
#' * Quantiles vs. time for observations. These are plotted by default as dashed
#' blue lines for the 2.5th and 97.5th percentiles and a solid red line for the
#' median. Formatting and the value for each quantile can be controlled
#' with the `upper`, `mid`, and `lower` arguments.
#' * 95% CI around the same quantiles of combined simulations from each subject.
#' The values and formatting for these quantile CIs are the same as for the observations,
#' and also controlled with the `upper`, `mid`, and `lower` arguments.
#'
#' Good vpc/pcvpc plots are considered to be those where the quantile lines for
#' the oberservations lie within the 95%CI quantile regions for simulations,
#' indicated that the model is "centered" on the data and faithfully captures the
#' variability in the data. For an npde plot, one expects to see approximately
#' normally distributed normalized prediction errors.
#' @method plot PM_valid
#' @param x The name of an *PM_valid* data object, which is usually called by 
#' the `$validate` method for [PM_result]
#' objects.
#' @param type Default is "vpc" for a visual predictive check, but could be
#' "pcvpc" for a prediction-corrected visual predictive check, or
#' "npde" for a normalized prediction distribution error analysis/plot.
#' Choosing npde will call [npde::plot.NpdeObject]. To modify this plot,
#' supply argmuents as a named list: `npde = (...)`. Available arguments
#' are in the user manual for the [npde package](https://github.com/ecomets/npde30).
#' @param tad `r template("tad")`
#' @param outeq `r template("outeq")`
#' @param log `r template("log")`
#' @param marker `r template("marker")`
#' @param line A list of three elements `$upper`, `$mid`, and `$lower`,
#' each of which controls characteristics of corresponding quantiles.
#' The arguments to each of these list elements map to several plotly attributes.
#' Each can be a boolean value or a list.
#' `TRUE` will plot default characteristics. `FALSE` will suppress quantile plots.
#' The elements of the list for each argument are as follows:
#' * `value` The quantile value. Default for lower is 0.025, mid is 0.5,
#' and upper is 0.975.
#' * `color` The color for both the 95%CI region around simulated quantile vs. time,
#' and the color of the line for the observation quantile vs. time.
#' Default for lower and upper is "dodgerblue" and for mid it is "red".
#' * `dash` The style of the obervation quantile line.
#' See `plotly::schema()`, traces > scatter > attributes > line > dash > values.
#' Default for lower and upper is "dash" and for mid it is "solid".
#' * `width` Default is 1 for lower, mid, and upper.
#' * `opacity` The opacity of the 95%CI region around simulated quantile vs. time.
#' Default is 0.4 for lower, mid and upper,
#' but can range between 0 (fully transparent) to 1 (fully opaque).
#' Example: `line = list(upper = list(value = 0.9, color = "red", dash = "longdash", opacity = 0.5, width = 2))`
#' @param legend `r template("legend")` Default is `FALSE`
#' @param log `r template("log")`
#' @param grid `r template("grid")`
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param xlab `r template("xlab")` Default is "Time" or "Time after dose" if `tad = TRUE`.
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
#' @param \dots If `type` is not "npde", the following apply. `r template("dotsPlotly")`.
#' However, if `type` is "npde", to modify the appearance of the plot,
#' supply a list of options, `npde = list(...)`. See the documentation
#' for the `type` argument above.
#' @return Plots and returns the plotly object
#' @author Michael Neely
#' @seealso [make_valid]
#' @export
#' @examples
#' \dontrun{
#' # VPC
#' NPex$valid$plot()
#'
#' # pcVPC
#' NPex$valid$plot(type = "pcvpc")
#'
#' # modify median line and marker
#' NPex$valid$plot(
#'   line = list(mid = list(color = "orange", dash = "dashdot")),
#'   marker = list(
#'     color = "blue", size = 12, symbol = "diamond",
#'     line = list(color = "navy")
#'   )
#' )
#' }

#' @family PMplots

plot.PM_valid <- function(x,
                          type = "vpc",
                          tad = FALSE,
                          outeq = 1,
                          line = TRUE,
                          marker = TRUE,
                          legend = FALSE,
                          log = FALSE,
                          grid = TRUE,
                          xlab, ylab,
                          title,
                          xlim, ylim,
                          print = TRUE, ...) {
  # to avoid modifying original object, x
  opDF <- x$opDF
  simdata <- x$simdata$obs


  if (outeq > max(opDF$outeq)) {
    stop(paste("Your data do not contain", outeq, "output equations.\n"))
  }

  opDF <- opDF %>% filter(outeq == !!outeq) # filter to outeq
  simdata <- simdata %>% filter(outeq == !!outeq) # filter to outeq



  # process CI lines
  if (is.logical(line)) {
    if (line) {
      line <- list(lower = TRUE, mid = TRUE, upper = TRUE)
    } else {
      line <- list(lower = FALSE, mid = FALSE, upper = FALSE)
    }
  } else {
    if (is.null(line$lower)) {
      line$lower <- T
    }
    if (is.null(line$mid)) {
      line$mid <- T
    }
    if (is.null(line$upper)) {
      line$upper <- T
    }
  }

  upper <- amendCI(line$upper, default = list(value = 0.975))
  mid <- amendCI(line$mid, default = list(value = 0.5, color = "red", dash = "solid"))
  lower <- amendCI(line$lower, default = list(value = 0.025))

  # process marker
  marker <- amendMarker(marker, default = list(color = "black", symbol = "circle-open", size = 8))

  # process dots
  dots <- list(...)
  npdeOpts <- pluck(dots, "npde")
  if (!is.null(npdeOpts)) {
    dots$npde <- NULL # remove
  }
  layout <- amendDots(dots)

  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)

  # axis labels if needed
  if (missing(xlab)) {
    xlab <- c("Time", "Time after dose")[1 + as.numeric(tad)]
  }
  if (missing(ylab)) {
    ylab <- "Output"
  }

  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }

  # axis ranges
  if (!missing(xlim)) {
    layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
  }
  if (!missing(ylim)) {
    layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
  }

  # log y axis
  if (log) {
    layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
  }

  # title
  if (missing(title)) {
    title <- ""
  }
  layout$title <- amendTitle(title, default = list(size = 20))

  # legend
  legendList <- amendLegend(legend)
  layout <- modifyList(layout, list(showlegend = legendList$showlegend))
  if (length(legendList) > 1) {
    layout <- modifyList(layout, list(legend = within(legendList, rm(showlegend))))
  }

  # select correct time
  if (!tad) {
    use.timeBinMedian <- sort(unique(opDF$timeBinMedian))
    use.optimes <- opDF$time
    use.opTimeBinMedian <- x$timeBinMedian$time
    use.opTimeBinNum <- x$timeBinMedian$bin
    use.simBinNum <- simdata$timeBinNum
  } else {
    if (!all(is.na(opDF$tadBinMedian))) {
      cat("Warning: Using time after dose is misleading if not under steady-state conditions.\n")
      use.timeBinMedian <- sort(unique(opDF$tadBinMedian))
      use.optimes <- opDF$tad
      use.opTimeBinMedian <- x$tadBinMedian$time
      use.opTimeBinNum <- x$tadBinMedian$bin
      use.simBinNum <- simdata$tadBinNum
    } else {
      stop("Rerun makePMvalid and set tad argument to TRUE.\n")
    }
  }

  # calculate lower, mid, and upper percentiles for pcYij by time bins
  groupVar <- if (tad) {
    rlang::quo(tadBinMedian)
  } else {
    rlang::quo(timeBinMedian)
  }
  quant_pcObs <- opDF %>%
    group_by(!!groupVar) %>%
    dplyr::reframe(
      value = quantile(pcObs, probs = c(lower$value, mid$value, upper$value), na.rm = TRUE),
      q = c(lower$value, mid$value, upper$value), .groups = "keep"
    ) %>%
    select(-.groups)
  names(quant_pcObs)[1] <- "time"

  # calculate lower, 50th and upper percentiles for Yij by time bin
  quant_Obs <- opDF %>%
    dplyr::group_by(!!groupVar) %>%
    reframe(
      value = quantile(obs, probs = c(lower$value, mid$value, upper$value), na.rm = TRUE),
      q = c(lower$value, mid$value, upper$value), .groups = "keep"
    ) %>%
    select(-.groups)
  names(quant_Obs)[1] <- "time"

  # calculate median and CI for upper, median, and lower for each bin
  simGroupVar <- if (tad) {
    rlang::quo(tadBinNum)
  } else {
    rlang::quo(timeBinNum)
  }
  simCI <- simdata %>%
    dplyr::group_by(simnum, !!simGroupVar) %>%
    dplyr::reframe(
      value = quantile(out, probs = c(lower$value, mid$value, upper$value), na.rm = TRUE),
      q = c("lower", "mid", "upper"), .groups = "keep"
    ) %>%
    dplyr::group_by(bin = !!simGroupVar, q) %>%
    dplyr::reframe(
      value = quantile(value, probs = c(lower$value, upper$value), na.rm = TRUE),
      ci = c(lower$value, upper$value), .groups = "keep"
    ) %>%
    dplyr::select(-.groups) %>%
    tidyr::pivot_wider(names_from = ci, values_from = value, names_prefix = "q")

  # arrange simCI in order of time, not bin
  if (!tad) {
    simCI$time <- x$timeBinMedian$time[match(simCI$bin, x$timeBinMedian$bin)]
  } else {
    simCI$time <- x$tadBinMedian$time[match(simCI$bin, x$tadBinMedian$bin)]
  }
  simCI <- simCI %>% dplyr::arrange(time, q)



  # combine obs and simCI
  quant_pcObs <- quant_pcObs %>%
    dplyr::select(-q) %>%
    dplyr::bind_cols(simCI[2:4])
  quant_Obs <- quant_Obs %>%
    dplyr::select(-q) %>%
    dplyr::bind_cols(simCI[2:4])



  # type specific options
  if (type == "vpc") {
    timeVar <- if (tad) {
      rlang::quo(tad)
    } else {
      quo(time)
    }
    plotData <- list(
      obsQuant = quant_Obs,
      obs = opDF %>%
        select(id, time = !!timeVar, obs)
    )
  }
  if (type == "pcvpc") {
    timeVar <- if (tad) {
      rlang::quo(tadBinMedian)
    } else {
      rlang::quo(timeBinMedian)
    }
    plotData <- list(
      obsQuant = quant_pcObs,
      obs = opDF %>%
        dplyr::select(id, time = !!timeVar, obs)
    )
  }
  if (type == "vpc" | type == "pcvpc") {
    # GENERATE THE PLOT
    p <- plotData$obsQuant %>%
      dplyr::group_by(q) %>%
      plotly::plot_ly(
        x = ~time, y = ~value,
        colors = c(upper$color, mid$color, lower$color),
        linetypes = c(upper$dash, mid$dash, lower$dash),
        color = ~q,
        linetype = ~q
      ) %>%
      # add simulation quantile CIs
      plotly::add_ribbons(
        ymin = ~q0.025, ymax = ~q0.975,
        opacity = upper$opacity,
        line = list(width = 2, color = "white", dash = "solid"),
        hoverinfo = "none"
      ) %>%
      # add observation quantiles
      plotly::add_lines(hovertemplate = "Time: %{x}<br>Out: %{y}<br><extra></extra>") %>%
      # add observations
      plotly::add_markers(
        data = plotData$obs,
        x = ~time, y = ~obs, marker = marker,
        hovertemplate = "Time: %{x}<br>Out: %{y}<br>ID: %{text}<extra></extra>",
        text = ~id,
        inherit = FALSE
      ) %>%
      # add layout

      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend,
        legend = layout$legend,
        title = layout$title
      )

    # SEND TO CONSOLE
    if (print) print(p)
  }

  if (type == "npde") {
    if (!checkRequiredPackages("npde", quietly = FALSE)) {
      return(invisible(NULL))
    }
    if (!tad) {
      if (is.null(x$npde)) stop("No npde object found.  Re-run $validate or make_valid.\n")
      if (inherits(x$npde[[outeq]], "NpdeObject")) {
        npdeArgs <- c(x = x$npde[[outeq]], npdeOpts)
        if (print) do.call(plot, npdeArgs)
        # do.call(npde:::plot.NpdeObject, npdeArgs)
        par(mfrow = c(1, 1))
      } else {
        cat(paste0("Unable to calculate NPDE for outeq ", outeq))
      }
    } else {
      if (is.null(x$npde_tad)) stop("No npde_tad object found.  Re-run $validate or make_valid with tad = T.\n")
      if (inherits(x$npde_tad[[outeq]], "NpdeObject")) {
        npdeArgs <- c(x = x$npde_tad[[outeq]], npdeOpts)
        do.call(plot, npdeArgs)
        # do.call(npde:::plot.NpdeObject, npdeArgs)
        par(mfrow = c(1, 1))
      } else {
        cat(paste0("Unable to calculate NPDE with TAD for outeq ", outeq))
      }
    }

    p <- NULL
  }
  return(invisible(p))
}




#' @title Plot Pmetrics Validation Objects
#' @description
#' `r lifecycle::badge('superseded')`
#'
#' This is largely now a legacy plotting function,
#' with a variety of options. It has been superseded by [plot.PM_valid].
#' @method plot PMvalid
#' @param x The name of an \emph{PMvalid} data object generated by \code{\link{make_valid}}.
#' @param type Default is \dQuote{vpc} for a visual prective check, but could be \dQuote{pcvpc} for a
#' prediction-corrected visual predictive check.
#' @param tad Plot using time after dose if \code{TRUE}.  Default is \code{FALSE} which plots using standard
#' relative time.  This will be the only option if \code{tad} was not set to \code{TRUE} when making the
#' PMvalid object.
#' @param icen Can be either \dQuote{median} for the predictions based on medians of the population parameter value
#' distributions, or \dQuote{mean}.  Default is \dQuote{median}.
#' @param outeq The number of the output equation to simulate/test.  Default is 1.
#' @param lower The lower quantile displayed for the observed and simulated profiles. Default is 0.025.
#' @param upper The upper quantile displayed for the observed and simulated profiles. Default is 0.975.
#' @param log Boolean operator to plot in semilog space.  The default is \code{FALSE}.
#' @param pch.obs Control the plotting character used for observations.  Default is 1, i.e. an open circle.
#' See \code{\link{points}} for other values of \code{pch}.
#' @param col.obs Color for observations.  Default is black.
#' @param cex.obs Size for observatins.  Default is 1.
#' @param data_theme Default is \dQuote{color}, but could be \dQuote{grey} or \dQuote{gray}.
#' @param plot_theme Default is `theme_grey()` but could be any complete ggplot2 theme, e.g.
#'  [ggplot2::theme_minimal()].
#' @param col.obs.ci Color of the observation confidence interval (set by \code{lower} and \code{upper}).
#' Default is blue.
#' @param col.obs.med Color of the observation median.
#' Default is red.
#' @param col.sim.ci Color of the simulation confidence interval (set by \code{lower} and \code{upper}).
#' Default is dodgerblue.
#' @param col.sim.med Color of the simulation median.
#' Default is lightpink.
#' @param axis.x List of `$name` and `$limits`. Default name is \dQuote{Time}.
#' @param axis.y List of `$name` and `$limits`. Default name is \dQuote{Observation}.
#' @param ... Not currently used
#' @return Plots the object using ggplot2.
#' @author Michael Neely
#' @seealso \code{\link{make_valid}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{points}}
#' @export

plot.PMvalid <- function(x, type = "vpc", tad = FALSE, icen = "median", outeq = 1, lower = 0.025, upper = 0.975,
                         log = FALSE, pch.obs = 1, col.obs = "black", cex.obs = 1, data_theme = "color", plot_theme = theme_grey(),
                         col.obs.ci = "blue", col.obs.med = "red", col.sim.ci = "dodgerblue", col.sim.med = "lightpink",
                         axis.x = NULL,
                         axis.y = NULL, ...) {
  # parse dots
  # arglist <- list(...)
  # names_theme <- names(formals(ggplot2::theme)) #check for elements of ggplot2::theme
  # argsTheme <- arglist[which(names(arglist) %in% names_theme)]

  # checkRequiredPackages("ggplot2")
  if (outeq > max(x$opDF$outeq)) {
    stop(paste("Your data do not contain", outeq, "output equations.\n"))
  }
  if (icen != "mean" & icen != "median") {
    stop(paste("Use \"mean\" or \"median\" for icen.\n", sep = ""))
  }

  x$opDF <- x$opDF[x$opDF$icen == icen & x$opDF$outeq == outeq, ] # filter to icen & outeq
  x$simdata$obs <- x$simdata$obs[x$simdat$obs$outeq == outeq, ] # filter to outeq

  # select correct time
  if (!tad) {
    use.timeBinMedian <- x$timeBinMedian$time
    use.optimes <- x$opDF$time
    use.opTimeBinMedian <- x$opDF$timeBinMedian
    use.opTimeBinNum <- x$opDF$timeBinNum
    use.simBinNum <- x$simdata$obs$timeBinNum
  } else {
    if (!all(is.na(x$tadBinMedian))) {
      cat("Warning: Using time after dose is misleading if not under steady-state conditions.\n")
      use.timeBinMedian <- x$tadBinMedian$time
      use.optimes <- x$opDF$tad
      use.opTimeBinMedian <- x$opDF$tadBinMedian
      use.opTimeBinNum <- x$opDF$tadBinNum
      use.simBinNum <- x$simdata$obs$tadBinNum
      if (axis.x$name == "Time") {
        axis.y$name <- "Time after dose"
      }
    } else {
      stop("Rerun makePMvalid and set tad argument to TRUE.\n")
    }
  }

  # calculate lower, 50th and upper percentiles for pcYij by time bins
  quant_pcObs <- tapply(x$opDF$pcObs, use.opTimeBinNum, quantile, probs = c(lower, 0.5, upper), na.rm = TRUE)
  # calculate lower, 50th and upper percentiles for Yij by time bin
  quant_Obs <- tapply(x$opDF$obs, use.opTimeBinNum, quantile, probs = c(lower, 0.5, upper), na.rm = TRUE)

  # find lower, median, upper percentiles by sim and bin
  simMed <- tapply(x$simdata$obs$out, list(x$simdata$obs$simnum, use.simBinNum), FUN = median, na.rm = TRUE) # nsim row, timeBinNum col
  simLower <- tapply(x$simdata$obs$out, list(x$simdata$obs$simnum, use.simBinNum), FUN = quantile, na.rm = TRUE, lower) # nsim row, timeBinNum col
  simUpper <- tapply(x$simdata$obs$out, list(x$simdata$obs$simnum, use.simBinNum), FUN = quantile, na.rm = TRUE, upper) # nsim row, timeBinNum col

  # calculate median and CI for upper, median, and lower for each bin

  upperLower <- apply(simUpper, 2, quantile, lower, na.rm = TRUE)[order(use.timeBinMedian)]
  upperUpper <- apply(simUpper, 2, quantile, upper, na.rm = TRUE)[order(use.timeBinMedian)]
  medianLower <- apply(simMed, 2, quantile, lower, na.rm = TRUE)[order(use.timeBinMedian)]
  medianUpper <- apply(simMed, 2, quantile, upper, na.rm = TRUE)[order(use.timeBinMedian)]
  lowerLower <- apply(simLower, 2, quantile, lower, na.rm = TRUE)[order(use.timeBinMedian)]
  lowerUpper <- apply(simLower, 2, quantile, upper, na.rm = TRUE)[order(use.timeBinMedian)]

  # calculate time boundaries for each bin
  if (tad) {
    minBin <- tapply(x$opDF$tad, x$opDF$tadBinNum, min)
    maxBin <- tapply(x$opDF$tad, x$opDF$tadBinNum, max)
  } else {
    minBin <- tapply(x$opDF$time, x$opDF$timeBinNum, min)
    maxBin <- tapply(x$opDF$time, x$opDF$timeBinNum, max)
  }
  timeBinNum <- length(minBin)

  # polytime <- c(mitimeBinNum[1],rep(sapply(1:(timeBinNum-1),function(x) mean(c(mitimeBinNum[x+1],maxBin[x]))),each=2),maxBin[timeBinNum])
  polytime <- use.timeBinMedian

  # upperDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(upperUpper,each=2),rev(rep(upperLower,each=2))))
  # medDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(medianUpper,each=2),rev(rep(medianLower,each=2))))
  # lowerDF <- data.frame(time=c(polytime,rev(polytime)),value=c(rep(lowerUpper,each=2),rev(rep(lowerLower,each=2))))
  upperDF <- data.frame(time = c(polytime, rev(polytime)), value = c(upperUpper, rev(upperLower)))
  medDF <- data.frame(time = c(polytime, rev(polytime)), value = c(medianUpper, rev(medianLower)))
  lowerDF <- data.frame(time = c(polytime, rev(polytime)), value = c(lowerUpper, rev(lowerLower)))


  # type specific options
  if (type == "vpc") {
    plotData <- list(
      obsQuant = quant_Obs, obs = x$opDF$obs, binTime = use.timeBinMedian,
      obsTime = use.optimes, upperDF = upperDF, lowerDF = lowerDF,
      medDF = medDF
    )
  }
  if (type == "pcvpc") {
    plotData <- list(
      obsQuant = quant_pcObs, obs = x$opDF$pcObs, binTime = use.timeBinMedian,
      obsTime = use.opTimeBinMedian, upperDF = upperDF, lowerDF = lowerDF,
      medDF = medDF
    )
  }

  # common options
  if (type == "vpc" | type == "pcvpc") {
    # set names if not specified
    if (!"name" %in% names(axis.x)) {
      axis.x$name <- "Time"
    }
    if (!"name" %in% names(axis.y)) {
      axis.y$name <- "Observation"
    }


    # set limits if not specified
    if (!"limits" %in% names(axis.x)) {
      axis.x$limits <- c(range(plotData$obsTime))
    }
    if (!"limits" %in% names(axis.y)) {
      axis.y$limits <- c(
        min(plotData$obs, plotData$lower$value),
        max(plotData$obs, plotData$upperDF$value)
      )
    }


    # override colors to make greyscale
    if (data_theme == "grey" | data_theme == "gray") { # set to grayscale
      col.obs <- "black"
      col.obs.ci <- "grey20"
      col.obs.med <- "grey20"
      col.sim.ci <- "grey75"
      col.sim.med <- "grey50"
    }
    # GENERATE THE PLOT
    p <- with(
      plotData,
      ggplot(mapping = aes(x = binTime, y = unlist(lapply(obsQuant, function(x) x[3])))) +
        geom_line(col = col.obs.ci, lty = 2, lwd = 1) +
        geom_polygon(aes(x = time, y = value), data = upperDF, fill = col.sim.ci, alpha = 0.25) +
        geom_polygon(aes(x = time, y = value), data = medDF, fill = col.sim.med, alpha = 0.25) +
        geom_polygon(aes(x = time, y = value), data = lowerDF, fill = col.sim.ci, alpha = 0.25) +
        geom_line(aes(
          x = binTime,
          y = unlist(lapply(obsQuant, function(x) x[2]))
        ), col = col.obs.med, lty = 1, lwd = 1) +
        geom_line(aes(
          x = binTime,
          y = unlist(lapply(obsQuant, function(x) x[1]))
        ), col = col.obs.ci, lty = 2, lwd = 1) +
        geom_point(aes(x = obsTime, y = obs), col = col.obs, pch = pch.obs, cex = cex.obs) +
        do.call(scale_x_continuous, axis.x) +
        do.call(scale_y_continuous, axis.y) +
        do.call(theme, plot_theme)
    )
    # SEND TO CONSOLE
    print(p)
  }

  if (type == "npde") {
    # cat("NPDE temporarily disabled pending code cleaning.\n")
    if (is.null(x$npde)) stop("No npde object found.  Re-run $validate or make_valid.\n")
    plot(x$npde)
    par(mfrow = c(1, 1))
    p <- NULL
  }
  return(invisible(p))
}
