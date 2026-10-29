normalize_engine_index <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(x)
  }

  if (min(x, na.rm = TRUE) == 0) {
    return(x + 1)
  }

  x
}

# Convert the output labels the backend writes (the `outeq` column of pred.csv,
# the `gamlam.<label>` columns of cycles.csv) to Pmetrics output numbers. A
# numeric label is the output number itself. Any other label is numbered by its
# position among the model's declared `outputs`, and a legacy `outeq_N` label is
# output `N`. Files written by older backends hold 0-based output indices, which
# `normalize_engine_index()` shifts.
engine_outeq_index <- function(x, outputs = NULL) {
  if (is.numeric(x)) {
    return(normalize_engine_index(x))
  }
  x <- trimws(as.character(x))
  idx <- rep(NA_real_, length(x))
  numeric_label <- !is.na(x) & grepl("^[0-9]+$", x)
  idx[numeric_label] <- as.numeric(x[numeric_label])
  if (!is.null(outputs)) {
    named <- !is.na(x) & !numeric_label
    idx[named] <- match(x[named], as.character(outputs))
  }
  # Models saved by earlier releases declare `outeq_N`, which is output `N`.
  legacy <- is.na(idx) & !is.na(x) & grepl("^outeq_[0-9]+$", x)
  idx[legacy] <- as.numeric(sub("^outeq_", "", x[legacy]))
  normalize_engine_index(idx)
}

# The output labels recorded in a run's settings.json, or `NULL` if absent.
engine_output_labels <- function(path) {
  settings_file <- file.path(path, "settings.json")
  if (!file.exists(settings_file)) {
    return(NULL)
  }
  outputs <- jsonlite::fromJSON(settings_file)$outputs
  if (is.null(outputs)) NULL else as.character(outputs)
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
