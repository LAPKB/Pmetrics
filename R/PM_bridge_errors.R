pm_bridge_error_prefix <- "PMETRICS_BRIDGE_ERROR="

pm_bridge_scalar <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }

  x[[1]]
}

pm_bridge_first_or <- function(x, default = NULL) {
  value <- pm_bridge_scalar(x)
  if (is.null(value)) {
    return(default)
  }

  value
}

pm_bridge_stage <- function(stage, default_stage = "runtime") {
  stage <- pm_bridge_scalar(stage)
  if (is.null(stage) || !nzchar(as.character(stage))) {
    return(default_stage)
  }

  stage <- tolower(as.character(stage))
  if (!stage %in% c("compile", "data", "settings", "runtime", "handoff")) {
    return(default_stage)
  }

  stage
}

parse_pm_bridge_error <- function(message) {
  message <- as.character(message[[1]])
  if (!nzchar(message)) {
    return(list(
      structured = FALSE,
      kind = "error",
      schema_version = NULL,
      stage = NULL,
      code = NULL,
      message = "",
      diagnostic = NULL,
      details = list()
    ))
  }

  prefix_loc <- regexpr(pm_bridge_error_prefix, message, fixed = TRUE)[[1]]
  if (is.na(prefix_loc) || prefix_loc < 1) {
    return(list(
      structured = FALSE,
      kind = "error",
      schema_version = NULL,
      stage = NULL,
      code = NULL,
      message = message,
      diagnostic = NULL,
      details = list()
    ))
  }

  display_message <- sub("[\r\n]+$", "", substr(message, 1, prefix_loc - 1))
  payload_text <- substr(message, prefix_loc + nchar(pm_bridge_error_prefix), nchar(message))
  payload <- tryCatch(
    jsonlite::fromJSON(payload_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.list(payload)) {
    return(list(
      structured = FALSE,
      kind = "error",
      schema_version = NULL,
      stage = NULL,
      code = NULL,
      message = if (nzchar(display_message)) display_message else message,
      diagnostic = NULL,
      details = list()
    ))
  }

  details <- payload$details
  if (!is.list(details)) {
    details <- list()
  }

  diagnostic <- pm_bridge_scalar(payload$diagnostic)
  if (!is.null(diagnostic)) {
    diagnostic <- as.character(diagnostic)
  }

  list(
    structured = TRUE,
    kind = as.character(pm_bridge_first_or(payload$kind, "error")),
    schema_version = pm_bridge_scalar(payload$schema_version),
    stage = pm_bridge_stage(payload$stage),
    code = as.character(pm_bridge_first_or(payload$code, "bridge_error")),
    message = if (nzchar(display_message)) {
      display_message
    } else {
      as.character(pm_bridge_first_or(payload$message, message))
    },
    diagnostic = diagnostic,
    details = details
  )
}

pm_bridge_error_classes <- function(stage, structured = TRUE) {
  stage_class <- paste0("pmetrics_bridge_", pm_bridge_stage(stage), "_error")
  if (isTRUE(structured)) {
    return(c(stage_class, "pmetrics_bridge_error"))
  }

  c(stage_class, "pmetrics_bridge_unstructured_error", "pmetrics_bridge_error")
}

abort_pm_bridge_message <- function(
  message,
  stage,
  code,
  diagnostic = NULL,
  details = list(),
  parent = NULL,
  structured = TRUE,
  schema_version = NULL
) {
  message <- as.character(pm_bridge_first_or(message, ""))
  if (!nzchar(message) && !is.null(diagnostic)) {
    message <- as.character(diagnostic)
  }

  if (!is.list(details)) {
    details <- list()
  }

  rlang::abort(
    message = message,
    class = pm_bridge_error_classes(stage, structured = structured),
    stage = pm_bridge_stage(stage),
    code = as.character(pm_bridge_first_or(code, "bridge_error")),
    diagnostic = if (is.null(diagnostic)) NULL else as.character(diagnostic),
    details = details,
    structured = isTRUE(structured),
    schema_version = schema_version,
    parent = parent
  )
}

abort_pm_bridge_error <- function(
  error,
  default_stage = "runtime",
  default_code = "bridge_error",
  details = list()
) {
  bridge_error <- parse_pm_bridge_error(conditionMessage(error))
  bridge_details <- bridge_error$details
  if (!is.list(bridge_details)) {
    bridge_details <- list()
  }
  if (is.list(details) && length(details)) {
    bridge_details <- utils::modifyList(bridge_details, details)
  }

  abort_pm_bridge_message(
    message = bridge_error$message,
    stage = if (is.null(bridge_error$stage)) default_stage else bridge_error$stage,
    code = if (is.null(bridge_error$code)) default_code else bridge_error$code,
    diagnostic = bridge_error$diagnostic,
    details = bridge_details,
    parent = error,
    structured = isTRUE(bridge_error$structured),
    schema_version = bridge_error$schema_version
  )
}

call_pm_bridge <- function(code, default_stage = "runtime", default_code = "bridge_error", details = list()) {
  tryCatch(
    code,
    error = function(error) {
      abort_pm_bridge_error(
        error,
        default_stage = default_stage,
        default_code = default_code,
        details = details
      )
    }
  )
}

utils::globalVariables(c(
  "call_pm_bridge",
  "close_live_session",
  "publish_live_report_failed",
  "publish_live_report_result",
  "start_live_session",
  "wait_live_session_connected"
))