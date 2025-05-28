# R-to-Rust Transpiler for ODE Function
# Transforms an R ODE function into Rust closures with zero-based indexing,
# parameter/covariate fetching, and arithmetic operations.

# Convert an R expression to Rust code (recursive)
expr_to_rust <- function(expr, params = NULL, covs = NULL) {
  # Base cases: numeric literals as floats, symbols
  if (is.numeric(expr) && length(expr) == 1) {
    val <- expr
    if (val == floor(val)) {
      return(sprintf("%d.0", as.integer(val)))
    } else {
      return(as.character(val))
    }
  }
  if (is.symbol(expr)) {
    return(as.character(expr))
  }

  # Handle indexing: var[idx] with zero-based adjustment
  if (is.call(expr) && as.character(expr[[1]]) == "[") {
    var <- as.character(expr[[2]])
    idx_raw <- expr[[3]]
    if (is.numeric(idx_raw) && length(idx_raw) == 1) {
      idx_val <- as.integer(idx_raw) - 1
      return(sprintf("%s[%d]", var, idx_val))
    } else {
      idx_code <- expr_to_rust(idx_raw, params, covs)
      return(sprintf("%s[%s - 1]", var, idx_code))
    }
  }

  if (!is.call(expr)) stop("Unknown expression type")
  op <- as.character(expr[[1]])
  args <- as.list(expr[-1])
  rust_args <- lapply(args, expr_to_rust, params = params, covs = covs)

  switch(op,
    # Grouping
    "(" = sprintf("(%s)", rust_args[[1]]),
    "{" = {
      inner <- if (length(rust_args) > 0) rust_args[[1]] else ""
      sprintf("{ %s }", inner)
    },

    # Arithmetic
    "+" = if (length(rust_args) == 1) {
      sprintf("+(%s)", rust_args[[1]])
    } else {
      sprintf("%s + %s", rust_args[[1]], rust_args[[2]])
    },
    "-" = if (length(rust_args) == 1) {
      sprintf("-(%s)", rust_args[[1]])
    } else {
      sprintf("(%s) - (%s)", rust_args[[1]], rust_args[[2]])
    },
    "*" = sprintf("%s * %s", rust_args[[1]], rust_args[[2]]),
    "/" = sprintf("%s / %s", rust_args[[1]], rust_args[[2]]),
    "^" = sprintf("%s.powf(%s)", rust_args[[1]], rust_args[[2]]),

    # Comparison
    "==" = sprintf("%s == %s", rust_args[[1]], rust_args[[2]]),
    "!=" = sprintf("%s != %s", rust_args[[1]], rust_args[[2]]),
    ">=" = sprintf("%s >= %s", rust_args[[1]], rust_args[[2]]),
    "<=" = sprintf("%s <= %s", rust_args[[1]], rust_args[[2]]),
    ">" = sprintf("%s > %s", rust_args[[1]], rust_args[[2]]),
    "<" = sprintf("%s < %s", rust_args[[1]], rust_args[[2]]),

    # Logical
    "&" = sprintf("%s && %s", rust_args[[1]], rust_args[[2]]),
    "|" = sprintf("%s || %s", rust_args[[1]], rust_args[[2]]),

    # Function calls
    "abs" = sprintf("%s.abs()", rust_args[[1]]),
    "log" = sprintf("%s.ln()", rust_args[[1]]),
    "exp" = sprintf("%s.exp()", rust_args[[1]]),

    # Assignment
    "<-" = ,
    "=" = {
      lhs <- args[[1]]
      rhs_code <- rust_args[[2]]

      if (is.symbol(lhs)) {
        # It's a plain variable like `ka = ...`
        sprintf("let %s = %s;", as.character(lhs), rhs_code)
      } else {
        # It’s likely something like dx[1] = ...
        lhs_code <- expr_to_rust(lhs, params, covs)
        sprintf("%s = %s;", lhs_code, rhs_code)
      }
    },

    # If
    "if" = {
      cond <- rust_args[[1]]
      then_exprs <- if (is.call(args[[2]]) && as.character(args[[2]][[1]]) == "{") as.list(args[[2]][-1]) else list(args[[2]])
      then_block <- stmts_to_rust(then_exprs, params, covs)
      sprintf("if %s {\n%s}\n", cond, indent(then_block))
    },

    # For
    "for" = {
      var <- as.character(args[[1]])
      n_sym <- as.character(args[[2]][[3]])
      loop_exprs <- if (is.call(args[[3]]) && as.character(args[[3]][[1]]) == "{") as.list(args[[3]][-1]) else list(args[[3]])
      body <- stmts_to_rust(loop_exprs, params, covs)
      sprintf("for %s in 0..%s as usize {\n%s}\n", var, n_sym, indent(body))
    },
    stop(sprintf("Unsupported operation: %s", op))
  )
}

# Helpers: convert list of statements to Rust and indent blocks
stmts_to_rust <- function(exprs, params = NULL, covs = NULL) {
  lines <- vapply(exprs, expr_to_rust, character(1), params = params, covs = covs)
  paste(lines, collapse = "\n")
}

indent <- function(text, spaces = 4) {
  prefix <- strrep(" ", spaces)
  paste0(prefix, gsub("\n", paste0("\n", prefix), text))
}

# Transpile an R ODE function to Rust closure
transpile_ode <- function(fun, params, covs) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|x, p, t, dx, rateiv, cov| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s);",
    paste(covs, collapse = ", "), paste(params, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs)
  sprintf("%s\n%s\n}", header, indent(body_rust, spaces = 4))
}

transpile_sec <- function(fun, params, covs) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|p, t, cov| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s);",
    paste(covs, collapse = ", "), paste(params, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs)
  sprintf("%s\n%s\n}", header, indent(body_rust, spaces = 4))
}

transpile_fa <- function(fun, params, covs) {
  "|_p| fa! {}"
}

transpile_lag <- function(fun, params, covs) {
  "|_p| lag! {}"
}

transpile_ini <- function(fun, params, covs) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|p, t, cov, _x| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s);",
    paste(covs, collapse = ", "), paste(params, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs)
  sprintf("%s\n%s\n}", header, indent(body_rust, spaces = 4))
}

transpile_out <- function(fun, params, covs) {
  exprs <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") as.list(body(fun)[-1]) else list(body(fun))
  header <- sprintf(
    "|x, p, t, cov, y| {\n    fetch_cov!(cov, t, %s);\n    fetch_params!(p, %s);",
    paste(covs, collapse = ", "), paste(params, collapse = ", ")
  )
  body_rust <- stmts_to_rust(exprs, params, covs)
  sprintf("%s\n%s\n}", header, indent(body_rust, spaces = 4))
}

empty_sec <- function() {
  "|_p, _t, _cov| { }"
}
empty_fa <- function() {
  "|_p| fa! {}"
}
empty_lag <- function() {
  "|_p| lag! {}"
}
empty_ini <- function() {
  "|_p, _t, _cov, _x| { }"
}

empty_out <- function() {
  "|_x, _p, _t, _cov, _y| { }"
}

# ------------------ Error Transpilation Support ------------------
# Validate and transpile an R 'error' function into a real R list of evaluated calls

collect_error_entries <- function(fun) {
  body_expr <- if (is.call(body(fun)) && as.character(body(fun)[[1]]) == "{") {
    as.list(body(fun)[-1])
  } else {
    list(body(fun))
  }

  entries <- list()
  expected_idx <- 1L

  for (expr in body_expr) {
    if (!is.call(expr) || !(as.character(expr[[1]]) %in% c("<-", "="))) {
      cli::cli_abort(c(
        "x" = "Invalid syntax in {.fn err}",
        "i" = "Only assingments allowed, but found {.code {deparse(expr)}}"))
    }
    lhs <- expr[[2]]
    rhs <- expr[[3]]

    if (!is.call(lhs) || as.character(lhs[[1]]) != "[" || as.character(lhs[[2]]) != "e") {
      cli::cli_abort(c(
        "x" = "Invalid LHS in {.fn err}",
        "i" = "Expected {.code e[index]}, got {.code {deparse(lhs)}}"))
    }
    idx_raw <- lhs[[3]]
    if (!is.numeric(idx_raw) || length(idx_raw) != 1) {
      cli::cli_abort(c(
        "x" = "Invalid index in {.fn err}",
        "i" = "Must be a single numeric literal, got  {.code {deparse(idx_raw)}}"))
    }
    idx <- as.integer(idx_raw)
    if (idx != expected_idx) {
      cli::cli_abort(c(
        "x" = "Error indices must start at 1 and increment by 1.",
        "i" = "Expected index {.val {expected_idx}}, got  {.val {idx}}"))
      
    }
    expected_idx <- expected_idx + 1L

    if (!is.call(rhs) || !(as.character(rhs[[1]]) %in% c("proportional", "additive"))) {
      cli::cli_abort(c(
        "x" = "Invalid RHS in {.fn err}",
        "i" = "Only {.fn proportional} or {.fn additive} calls allowed, but found {.code {deparse(rhs)}}"))
    }

    n_args <- length(rhs) - 1
    if (!(n_args %in% c(2, 3))) {
      cli::cli_abort(c(
        "x" = "Invalid syntax in {.fn err}",
        "i" = "{.code {as.character(rhs[[1]])}} must have 2 or 3 arguments, but got {.code {n_args}}"))
    }

    if (n_args == 2) {
      rhs <- as.call(c(as.list(rhs), list(fixed = FALSE)))
    }

    entries[[as.character(idx)]] <- rhs
  }
  entries
}

# Transpile an R error function
transpile_error <- function(fun) {
  entries <- collect_error_entries(fun)
  result <- list()
  for (i in 1:length(entries)) {
    # evaluate each proportional/additive call
    result[[i]] <- eval(entries[[i]])
  }
  # ensure names are correct ("1","2",...)
  names(result) <- 1:length(result)
  result
}

get_assignments <- function(fn, assign) {
  count_dx_assignments <- function(expr) {
    if (is.call(expr)) {
      if (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) {
        lhs <- expr[[2]]
        # Check if lhs is an indexing call (i.e., assign[...] <- ...)
        if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
          target_name <- as.character(lhs[[2]])
          if (tolower(target_name) == tolower(assign)) {
            return(1 + count_dx_assignments(expr[[3]]))
          }
        }
        return(count_dx_assignments(expr[[2]]) + count_dx_assignments(expr[[3]]))
      } else {
        return(sum(sapply(expr, count_dx_assignments)))
      }
    }
    return(0)
  }
  
  body_expr <- body(fn)
  count_dx_assignments(body_expr)
}

