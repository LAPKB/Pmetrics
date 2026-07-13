# Static analysis helpers for Pmetrics model blocks.
#
# These functions inspect the R expressions written inside model blocks (e.g.
# `eqn`, `out`, `lag`, `fa`, `ini`) to infer structural information such as the
# number of compartments and outputs, the maximum indices used, and reserved
# name conflicts. They operate purely on the R abstract syntax tree; models are
# rendered to the pharmsol DSL by `model_to_dsl()` (see `model_dsl.R`) and
# compiled just-in-time by the Rust backend.

# Size of a dense vector needed to hold indices up to `max_index` (1-based
# indices, so a maximum index of `n` needs `n + 1` slots to allow slot 0).
index_vector_size <- function(max_index) {
  if (max_index <= 0L) {
    return(1L)
  }

  max_index + 1L
}

# Largest literal index used to *read* any of `targets` (e.g. `x[3]` -> 3).
get_max_index <- function(fn_or_expr, targets) {
  targets <- tolower(targets)

  walk_expr <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], as.name("["))) {
      target_name <- tolower(as.character(expr[[2]]))
      if (target_name %in% targets) {
        idx <- expr[[3]]
        if (is.numeric(idx) && length(idx) == 1) {
          return(as.integer(idx))
        }
      }
    }

    if (is.call(expr)) {
      return(max(vapply(as.list(expr), walk_expr, integer(1)), 0L))
    }

    0L
  }

  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
}

# Sorted, unique literal indices *assigned* to any of `targets`
# (e.g. `dx[2] <- ...` -> 2).
get_assignment_indices <- function(fn_or_expr, targets) {
  targets <- tolower(targets)
  found <- integer(0)

  walk_expr <- function(expr) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }

    if (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) {
      lhs <- expr[[2]]
      if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
        target_name <- tolower(as.character(lhs[[2]]))
        idx <- lhs[[3]]
        if (target_name %in% targets && is.numeric(idx) && length(idx) == 1) {
          found <<- c(found, as.integer(idx))
        }
      }
    }

    lapply(as.list(expr), walk_expr)
    invisible(NULL)
  }

  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
  sort(unique(found))
}

# Largest literal index assigned to any of `targets`.
get_max_assignment_index <- function(fn_or_expr, targets) {
  found <- get_assignment_indices(fn_or_expr, targets)
  if (length(found) == 0) {
    return(0L)
  }
  max(found)
}

# TRUE if any of `targets` is indexed with a non-literal (dynamic) index, e.g.
# `x[i]`. Such models cannot have their dimensions inferred statically.
has_nonliteral_index <- function(fn_or_expr, targets) {
  targets <- tolower(targets)

  walk_expr <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], as.name("["))) {
      target_name <- tolower(as.character(expr[[2]]))
      if (target_name %in% targets) {
        idx <- expr[[3]]
        return(!(is.numeric(idx) && length(idx) == 1))
      }
    }

    if (is.call(expr)) {
      return(any(vapply(as.list(expr), walk_expr, logical(1))))
    }

    FALSE
  }

  expr <- if (is.function(fn_or_expr)) body(fn_or_expr) else fn_or_expr
  walk_expr(expr)
}

# Count the number of indexed assignments to `assign` (e.g. how many `dx[...]`
# or `y[...]` equations a block contains).
get_assignments <- function(fn, assign) {
  count_assignments <- function(expr) {
    if (is.call(expr)) {
      if (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) {
        lhs <- expr[[2]]
        # Check if lhs is an indexing call (i.e., assign[...] <- ...)
        if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
          target_name <- as.character(lhs[[2]])
          if (tolower(target_name) == tolower(assign)) {
            return(1 + count_assignments(expr[[3]]))
          }
        }
        return(count_assignments(expr[[2]]) + count_assignments(expr[[3]]))
      } else {
        return(sum(sapply(expr, count_assignments)))
      }
    }
    return(0)
  }

  body_expr <- body(fn)
  count_assignments(body_expr)
}

# Detect use of reserved engine names (e.g. `x`, `dx`, `b`, `rateiv`) as
# user-defined variables in model blocks.
reserved_name_conflicts <- function(blocks) {
  reserved <- c(
    "t",
    "x",
    "dx",
    "p",
    "b",
    "bolus",
    "r",
    "rateiv",
    "cov",
    "y"
  )

  conflicts <- purrr::map(blocks, \(b) {
    purrr::map_chr(reserved, \(r) {
      if (is.function(b)) {
        b <- func_to_char(b)
      }
      if (is.list(b)) {
        b <- names(b)
        con_match <- stringr::str_detect(tolower(b), glue::glue("^\\b{r}\\b")) # stand alone
      } else {
        con_match <- stringr::str_detect(tolower(b), glue::glue("^{r}\\s+[=<]")) # assignment in function
      }

      if (any(con_match)) {
        return(r)
      } else {
        return(NA_character_)
      }
    })
  }) |>
    unlist() |>
    purrr::discard(\(d) is.na(d)) |>
    unique()

  return(conflicts)
}
