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

  decode_one_model <- function(model, outeq) {
    if (is.character(model) && length(model) == 1) {
      return(tibble::tibble(
        outeq = outeq,
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
      outeq = outeq,
      type = model_type,
      c0 = purrr::pluck(poly, "c0", .default = 0),
      c1 = purrr::pluck(poly, "c1", .default = 0),
      c2 = purrr::pluck(poly, "c2", .default = 0),
      c3 = purrr::pluck(poly, "c3", .default = 0)
    )
  }

  observed_outeq <- normalize_engine_index(observed_outeq)
  observed_outeq <- sort(unique(observed_outeq[!is.na(observed_outeq)]))

  if (length(observed_outeq) == 0) {
    return(purrr::imap_dfr(models, function(model, idx) {
      decode_one_model(model, idx)
    }))
  }

  shift <- length(models) - max(observed_outeq, na.rm = TRUE)
  if (!(shift %in% c(0L, 1L))) {
    shift <- 0L
  }

  model_indices <- observed_outeq + shift
  if (any(model_indices < 1L | model_indices > length(models))) {
    model_indices <- observed_outeq
  }

  purrr::map2_dfr(model_indices, observed_outeq, function(model_idx, out_idx) {
    decode_one_model(models[[model_idx]], out_idx)
  })
}
