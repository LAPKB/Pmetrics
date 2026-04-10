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

    # Normalize observed_outeq to 1-based indexing so we emit consistent 1-based
    # `outeq` values. Some data sources use 0-based outeq (min == 0); convert
    # those to 1-based here. If observed_outeq is empty/NA leave as-is.
    if (!(length(observed_outeq) == 0 || all(is.na(observed_outeq)))) {
        if (min(observed_outeq, na.rm = TRUE) == 0) {
            observed_outeq <- observed_outeq + 1L
        }
    }

    purrr::imap_dfr(models, function(model, idx) {
        if (is.character(model) && length(model) == 1) {
            return(tibble::tibble(
                # emit 1-based outeq consistently; `idx` is 1-based from
                # purrr::imap_dfr, so use it directly after normalization above
                outeq = idx,
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
            # emit 1-based outeq consistently; `idx` is 1-based from
            # purrr::imap_dfr, so use it directly after normalization above
            outeq = idx,
            type = model_type,
            c0 = purrr::pluck(poly, "c0", .default = 0),
            c1 = purrr::pluck(poly, "c1", .default = 0),
            c2 = purrr::pluck(poly, "c2", .default = 0),
            c3 = purrr::pluck(poly, "c3", .default = 0)
        )
    })
}
