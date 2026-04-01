normalize_engine_index <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(x)
  }

  if (min(x, na.rm = TRUE) == 0) {
    return(x + 1)
  }

  x
}

decode_error_model_rows <- function(models, observed_outeq) {
  if (length(models) == 0) {
    return(tibble::tibble(
      outeq = numeric(),
      type = character(),
      c0 = numeric(),
      c1 = numeric(),
      c2 = numeric(),
      c3 = numeric()
    ))
  }

  outeq_shift <- if (length(observed_outeq) == 0 || all(is.na(observed_outeq))) {
    0L
  } else if (min(observed_outeq, na.rm = TRUE) == 0) {
    1L
  } else {
    0L
  }

  purrr::imap_dfr(models, function(model, idx) {
    if (is.character(model) && length(model) == 1) {
      return(tibble::tibble(
        outeq = (idx - 1L) + outeq_shift,
        type = model,
        c0 = 0,
        c1 = 0,
        c2 = 0,
        c3 = 0
      ))
    }

    model_type <- names(model)[[1]]
    model_data <- model[[1]]
    poly <- purrr::pluck(model_data, "poly")

    tibble::tibble(
      outeq = (idx - 1L) + outeq_shift,
      type = model_type,
      c0 = purrr::pluck(poly, "c0", .default = 0),
      c1 = purrr::pluck(poly, "c1", .default = 0),
      c2 = purrr::pluck(poly, "c2", .default = 0),
      c3 = purrr::pluck(poly, "c3", .default = 0)
    )
  })
}