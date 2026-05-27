mock_bridge_error_message <- function(
  stage = "runtime",
  code = "bridge_error",
  message = "bridge error",
  diagnostic = NULL,
  details = list()
) {
  if (!is.list(details)) {
    details <- list()
  }

  payload <- jsonlite::toJSON(
    list(
      kind = "error",
      schema_version = 1,
      stage = stage,
      code = code,
      message = message,
      diagnostic = diagnostic,
      details = details
    ),
    auto_unbox = TRUE,
    null = "null"
  )

  rendered <- message
  if (!is.null(diagnostic)) {
    rendered <- paste(rendered, diagnostic, sep = "\n")
  }

  paste0(rendered, "\n\nPMETRICS_BRIDGE_ERROR=", payload)
}