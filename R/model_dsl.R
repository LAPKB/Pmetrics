# R-to-DSL emitter for Pmetrics models (pharmsol DSL).
#
# ---------------------------------------------------------------------------
# Overview
# ---------------------------------------------------------------------------
# Historically, Pmetrics translated model blocks into Rust source code that was
# compiled with `cargo` into a shared library. As of the pharmsol DSL backend,
# models are instead emitted as a small text description in the pharmsol DSL and
# JIT-compiled at run time inside the `pm_rs` Rust library. This removes the need
# for a Rust toolchain on the user's machine.
#
# This file converts the R model definition (stored in `model$arg_list`) into the
# pharmsol DSL "authoring shorthand". The DSL expression grammar is very close to
# R: mathematical functions use call syntax (`exp(x)`, `ln(x)`, `sqrt(x)`), the
# `^` operator is supported for powers, and `&&`/`||` are used for logical AND/OR.
#
# Naming conventions used by the emitter:
#   * States   `x[i]`            -> `x{i}`          (declared in `states = ...`)
#   * Outputs  `Y[i]` / `y[i]`   -> `outeq_{i}`     (declared in `outputs = ...`)
#   * Inputs   `b[j]`/`bolus[j]` -> `bolus(input_{j}) -> x{k}`    (route)
#              `rateiv[j]`/`r[j]`-> `infusion(input_{j}) -> x{k}` (route)
#   * Params / covariates keep their (lower-cased) names.
#
# Infusions and boluses are declared as routes into the compartment in which they
# appear in the derivative equations; the corresponding `rateiv[j]`/`b[j]` terms
# are stripped from the derivative because the DSL runtime injects them
# automatically from the route declaration.
# ---------------------------------------------------------------------------

# The set of DSL intrinsic functions and how R names map to them. R names that
# are not present here (e.g. hyperbolic or inverse-trigonometric functions) are
# not currently expressible in the DSL and trigger an informative error.
dsl_function_map <- function() {
  c(
    abs = "abs", exp = "exp", sqrt = "sqrt",
    ln = "ln", log = "ln", log10 = "log10", log2 = "log2",
    sin = "sin", cos = "cos", tan = "tan",
    floor = "floor", ceiling = "ceil", round = "round",
    max = "max", min = "min"
  )
}

# Convert a pure R expression (no assignments or blocks) to a DSL expression.
expr_to_dsl <- function(expr, allow_if = TRUE) {
  # Numeric literals: emit integers with a trailing `.0` so the DSL treats them
  # as floating point, matching the Rust transpiler's behaviour.
  if (is.numeric(expr) && length(expr) == 1) {
    val <- expr
    if (is.finite(val) && val == floor(val)) {
      return(sprintf("%d.0", as.integer(val)))
    }
    return(as.character(val))
  }
  if (is.symbol(expr)) {
    return(tolower(as.character(expr)))
  }
  if (!is.call(expr)) {
    cli::cli_abort("Unable to convert model expression to DSL: unsupported token.")
  }

  op <- as.character(expr[[1]])

  # Indexing: x[i] -> x{i}. Only literal, positive integer indices are allowed.
  if (op == "[") {
    var <- tolower(as.character(expr[[2]]))
    idx_raw <- expr[[3]]
    if (!(is.numeric(idx_raw) && length(idx_raw) == 1)) {
      cli::cli_abort(c(
        "x" = "Dynamic (non-literal) indices are not supported in the DSL backend.",
        "i" = "Use literal indices such as {.code x[1]}."
      ))
    }
    idx <- as.integer(idx_raw)
    if (var %in% c("b", "bolus", "rateiv", "r")) {
      cli::cli_abort(c(
        "x" = "Bolus/infusion inputs may only be used as standalone additive terms in derivative equations.",
        "i" = "Write, for example, {.code dx[1] = -ke * x[1] + rateiv[1]}, not inside a product."
      ))
    }
    return(sprintf("%s%d", var, idx))
  }

  # Conditionals map to `if (cond) then else else`. The pharmsol DSL authoring
  # surface only accepts a conditional as an entire equation right-hand side or
  # as the `else` branch of another conditional (an `else if` chain). Any other
  # position (nested in an operator/function, or in the `then` branch) is
  # rejected by the DSL parser, so we catch it here to give an R-level error
  # that points at the model instead of a cryptic parse error on generated code.
  if (op == "if") {
    if (!allow_if) {
      cli::cli_abort(c(
        "x" = "A conditional {.code if (...) ... else ...} can only be a whole equation right-hand side.",
        "i" = "It cannot be nested inside another expression (e.g. {.code 2 * if (...)}) or in the {.code then} branch.",
        "i" = "Assign it to a secondary variable first, e.g. {.code tmp = if (cond) a else b}, then use {.code tmp}."
      ))
    }
    args <- as.list(expr[-1])
    if (length(args) != 3) {
      cli::cli_abort(c(
        "x" = "Conditional expressions in the DSL must include an `else` branch.",
        "i" = "Write {.code if (cond) a else b}."
      ))
    }
    # Only the `else` branch may itself be a conditional (right-associative
    # `else if` chains); the condition and `then` branch may not.
    cond <- expr_to_dsl(args[[1]], allow_if = FALSE)
    then_code <- expr_to_dsl(args[[2]], allow_if = FALSE)
    else_code <- expr_to_dsl(args[[3]], allow_if = TRUE)
    return(sprintf("if (%s) %s else %s", cond, then_code, else_code))
  }

  args <- as.list(expr[-1])
  a <- lapply(args, function(x) expr_to_dsl(x, allow_if = FALSE))

  fmap <- dsl_function_map()

  out <- switch(op,
    "(" = sprintf("(%s)", a[[1]]),
    "+" = if (length(a) == 1) sprintf("+(%s)", a[[1]]) else sprintf("%s + %s", a[[1]], a[[2]]),
    "-" = if (length(a) == 1) sprintf("-(%s)", a[[1]]) else sprintf("(%s) - (%s)", a[[1]], a[[2]]),
    "*" = sprintf("%s * %s", a[[1]], a[[2]]),
    "/" = sprintf("%s / %s", a[[1]], a[[2]]),
    "^" = sprintf("(%s)^(%s)", a[[1]], a[[2]]),
    "==" = sprintf("%s == %s", a[[1]], a[[2]]),
    "!=" = sprintf("%s != %s", a[[1]], a[[2]]),
    ">=" = sprintf("%s >= %s", a[[1]], a[[2]]),
    "<=" = sprintf("%s <= %s", a[[1]], a[[2]]),
    ">" = sprintf("%s > %s", a[[1]], a[[2]]),
    "<" = sprintf("%s < %s", a[[1]], a[[2]]),
    "&" = sprintf("%s && %s", a[[1]], a[[2]]),
    "&&" = sprintf("%s && %s", a[[1]], a[[2]]),
    "|" = sprintf("%s || %s", a[[1]], a[[2]]),
    "||" = sprintf("%s || %s", a[[1]], a[[2]]),
    "!" = sprintf("!(%s)", a[[1]]),
    {
      # Function call: look up in the DSL intrinsic map.
      if (!is.null(fmap[[op]])) {
        sprintf("%s(%s)", fmap[[op]], paste(unlist(a), collapse = ", "))
      } else {
        cli::cli_abort(c(
          "x" = "Unsupported function {.val {op}} for the DSL backend.",
          "i" = "Supported functions are: {.val {sort(unique(unname(dsl_function_map())))}}."
        ))
      }
    }
  )
  out
}

# Return the top-level statements of a model block function body.
dsl_body_stmts <- function(fun) {
  b <- body(fun)
  if (is.call(b) && as.character(b[[1]]) == "{") {
    as.list(b[-1])
  } else {
    list(b)
  }
}

# Is `expr` an assignment (`<-` or `=`)?
dsl_is_assign <- function(expr) {
  is.call(expr) && (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("=")))
}

# Flatten an expression into signed additive terms, i.e. a list of
# `list(sign = +/-1, expr = <expr>)`.
dsl_flatten_add <- function(expr, sign = 1) {
  if (is.call(expr) && identical(expr[[1]], as.name("+"))) {
    if (length(expr) == 3) {
      return(c(dsl_flatten_add(expr[[2]], sign), dsl_flatten_add(expr[[3]], sign)))
    }
    return(dsl_flatten_add(expr[[2]], sign)) # unary +
  }
  if (is.call(expr) && identical(expr[[1]], as.name("-"))) {
    if (length(expr) == 3) {
      return(c(dsl_flatten_add(expr[[2]], sign), dsl_flatten_add(expr[[3]], -sign)))
    }
    return(dsl_flatten_add(expr[[2]], -sign)) # unary -
  }
  list(list(sign = sign, expr = expr))
}

# If `expr` is a route reference (`b[j]`, `bolus[j]`, `rateiv[j]`, `r[j]`),
# return `list(kind = "bolus"|"infusion", input = j)`, else NULL.
dsl_route_of <- function(expr) {
  if (is.call(expr) && as.character(expr[[1]]) == "[") {
    v <- tolower(as.character(expr[[2]]))
    idx <- expr[[3]]
    if (v %in% c("b", "bolus", "rateiv", "r") && is.numeric(idx) && length(idx) == 1) {
      kind <- if (v %in% c("b", "bolus")) "bolus" else "infusion"
      return(list(kind = kind, input = as.integer(idx)))
    }
  }
  NULL
}

# Return the sorted, unique data input indices referenced by bolus (`b[]` /
# `bolus[]`) or infusion (`rateiv[]` / `r[]`) terms in a model equation function.
# Used to validate that the model represents every dose input present in the data.
eqn_route_inputs <- function(fun, kind = c("bolus", "infusion")) {
  kind <- match.arg(kind)
  targets <- if (kind == "bolus") c("b", "bolus") else c("rateiv", "r")
  found <- integer(0)
  walk <- function(expr) {
    if (is.call(expr)) {
      if (identical(expr[[1]], as.name("[")) &&
        tolower(as.character(expr[[2]])) %in% targets &&
        is.numeric(expr[[3]]) && length(expr[[3]]) == 1) {
        found <<- c(found, as.integer(expr[[3]]))
      }
      lapply(as.list(expr), walk)
    }
    invisible(NULL)
  }
  walk(if (is.function(fun)) body(fun) else fun)
  sort(unique(found))
}

# Build a DSL expression string from a list of signed terms.
dsl_join_terms <- function(terms) {
  if (length(terms) == 0) {
    return("0.0")
  }
  pieces <- character(0)
  for (i in seq_along(terms)) {
    t <- terms[[i]]
    es <- expr_to_dsl(t$expr)
    if (i == 1) {
      pieces <- if (t$sign < 0) sprintf("-(%s)", es) else es
    } else {
      pieces <- paste0(pieces, if (t$sign < 0) " - " else " + ", sprintf("(%s)", es))
    }
  }
  pieces
}

# Convert a derivative RHS into (routes, stripped DSL expression) for the given
# destination compartment index `comp`.
dsl_extract_routes <- function(rhs, comp) {
  terms <- dsl_flatten_add(rhs)
  routes <- list()
  kept <- list()
  for (t in terms) {
    route <- dsl_route_of(t$expr)
    if (!is.null(route)) {
      if (t$sign < 0) {
        cli::cli_abort(c(
          "x" = "Bolus/infusion inputs must be added (not subtracted) in derivative equations.",
          "i" = "Write {.code dx[{comp}] = ... + rateiv[{route$input}]}."
        ))
      }
      route$comp <- comp
      routes[[length(routes) + 1]] <- route
    } else {
      kept[[length(kept) + 1]] <- t
    }
  }
  list(routes = routes, expr = dsl_join_terms(kept))
}

# Emit the ODE equation block: returns routes, derived assignments, and dx lines.
dsl_eqn_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  routes <- list()
  derived <- character(0)
  dx_lines <- character(0)

  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort(c(
        "x" = "Only assignments are supported in the equation block for the DSL backend.",
        "i" = "Control-flow constructs such as {.code for} loops are not supported."
      ))
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "dx") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in equation block.")
      }
      res <- dsl_extract_routes(rhs, idx)
      routes <- c(routes, res$routes)
      dx_lines <- c(dx_lines, sprintf("dx(x%d) = %s", idx, res$expr))
    } else {
      # Scalar (secondary/derived) assignment.
      name <- tolower(as.character(lhs))
      derived <- c(derived, sprintf("%s = %s", name, expr_to_dsl(rhs)))
    }
  }

  list(routes = routes, derived = derived, dx = dx_lines)
}

# Emit the output block: returns derived assignments and out() lines.
dsl_out_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  out_lines <- character(0)

  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the output block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "y") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in output block.")
      }
      # Pmetrics data uses 1-based OUTEQ labels, so output `y[i]` maps to the
      # DSL output label `outeq_{i}` (numeric label resolves to index i).
      out_lines <- c(out_lines, sprintf("out(outeq_%d) = %s", idx, expr_to_dsl(rhs)))
    } else {
      name <- tolower(as.character(lhs))
      derived <- c(derived, sprintf("%s = %s", name, expr_to_dsl(rhs)))
    }
  }

  list(derived = derived, out = out_lines)
}

# Emit `derive` assignments from a secondary-equation block.
dsl_sec_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the secondary block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (!is.symbol(lhs)) {
      cli::cli_abort("Secondary equations must assign to scalar variables.")
    }
    derived <- c(derived, sprintf("%s = %s", tolower(as.character(lhs)), expr_to_dsl(rhs)))
  }
  derived
}

# Emit route-property modifiers (`lag(...)` / `fa(...)`) from a lag/fa block.
# `target` is the DSL property name ("lag" or "fa"); the R block assigns to
# `lag[j]` / `fa[j]` where `j` is the 1-based input index.
dsl_route_property_block <- function(fun, target) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  lines <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the {target} block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != target) {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in {target} block.")
      }
      lines <- c(lines, sprintf("%s(input_%d) = %s", target, idx, expr_to_dsl(rhs)))
    } else {
      derived <- c(derived, sprintf("%s = %s", tolower(as.character(lhs)), expr_to_dsl(rhs)))
    }
  }
  list(derived = derived, lines = lines)
}

# Emit `init(...)` statements from an initial-conditions block.
dsl_ini_block <- function(fun) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  lines <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the initial-conditions block for the DSL backend.")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "x") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in initial-conditions block.")
      }
      lines <- c(lines, sprintf("init(x%d) = %s", idx, expr_to_dsl(rhs)))
    } else {
      derived <- c(derived, sprintf("%s = %s", tolower(as.character(lhs)), expr_to_dsl(rhs)))
    }
  }
  list(derived = derived, lines = lines)
}

# Finalize route usages into concrete DSL routes plus a data-remap table.
#
# Each usage is `list(kind, input, comp)` where `input` is the 1-based Pmetrics
# data input number. The DSL requires a unique label per route, but the pharmsol
# runtime keeps *separate* index spaces for bolus and infusion routes, so a
# single data input can legitimately drive both a bolus and an infusion route.
# To express that in the DSL we keep the bolus on the original `input_{n}` label
# and give the infusion a fresh `input_{m}` label, recording a remap so the
# data's infusion events (DUR > 0) on input `n` are rewritten to input `m` at
# fit/simulation time.
#
# Returns `list(routes = <list of {kind, label, comp}>, remap = <list of
# {kind, from, to}>)`.
dsl_finalize_routes <- function(routes) {
  by_input <- list()
  seen_inputs <- integer(0)
  for (r in routes) {
    key <- as.character(r$input)
    if (is.null(by_input[[key]])) {
      by_input[[key]] <- list()
      seen_inputs <- c(seen_inputs, r$input)
    }
    by_input[[key]][[length(by_input[[key]]) + 1L]] <- r
  }

  inputs <- sort(unique(seen_inputs))
  next_label <- if (length(inputs) > 0) max(inputs) + 1L else 1L

  final_routes <- list()
  remap <- list()

  for (inp in inputs) {
    grp <- by_input[[as.character(inp)]]
    has_bolus <- any(vapply(grp, function(r) identical(r$kind, "bolus"), logical(1)))

    for (kd in c("bolus", "infusion")) {
      comps <- unique(vapply(
        Filter(function(r) identical(r$kind, kd), grp),
        function(r) as.integer(r$comp), integer(1)
      ))
      if (length(comps) == 0) next
      if (length(comps) > 1) {
        cli::cli_abort(c(
          "x" = "Input {inp} directs a {kd} into more than one compartment.",
          "i" = "Each input may direct a bolus (or an infusion) into a single compartment."
        ))
      }
      comp <- comps[[1]]

      # Bolus keeps the original input label. When the same input is also used as
      # an infusion, the infusion route receives a fresh label and the data is
      # remapped accordingly.
      label <- inp
      if (identical(kd, "infusion") && has_bolus) {
        label <- next_label
        next_label <- next_label + 1L
        remap[[length(remap) + 1L]] <- list(kind = "infusion", from = inp, to = label)
      }

      final_routes[[length(final_routes) + 1L]] <- list(kind = kd, label = label, comp = comp)
    }
  }

  list(routes = final_routes, remap = remap)
}

# Rewrite Pmetrics data labels to match DSL route and output names.
remap_input_csv <- function(path, remap) {
  if (length(remap) == 0 || !file.exists(path)) {
    return(invisible(path))
  }

  df <- utils::read.csv(
    path,
    check.names = FALSE, colClasses = "character",
    na.strings = character(0), stringsAsFactors = FALSE
  )
  cols <- toupper(names(df))
  dur_col <- match("DUR", cols)
  input_col <- match("INPUT", cols)
  outeq_col <- match("OUTEQ", cols)

  has_routes <- any(vapply(remap, function(x) x$kind %in% c("bolus", "infusion"), logical(1)))
  has_outputs <- any(vapply(remap, function(x) identical(x$kind, "output"), logical(1)))
  if (has_routes && (is.na(dur_col) || is.na(input_col))) {
    cli::cli_abort("Unable to apply route mapping: {.field DUR}/{.field INPUT} columns not found.")
  }
  if (has_outputs && is.na(outeq_col)) {
    cli::cli_abort("Unable to apply output mapping: {.field OUTEQ} column not found.")
  }

  if (has_routes) {
    dur <- suppressWarnings(as.numeric(df[[dur_col]]))
    input <- df[[input_col]]
  }

  for (m in remap) {
    if (identical(m$kind, "infusion")) {
      sel <- !is.na(dur) & dur > 0 & input == as.character(m$from)
      df[[input_col]][sel] <- as.character(m$to)
    } else if (identical(m$kind, "bolus")) {
      sel <- (is.na(dur) | dur <= 0) & input == as.character(m$from)
      df[[input_col]][sel] <- as.character(m$to)
    } else if (identical(m$kind, "output")) {
      sel <- df[[outeq_col]] == as.character(m$from)
      df[[outeq_col]][sel] <- as.character(m$to)
    }
  }

  utils::write.csv(df, path, row.names = FALSE, quote = FALSE, na = ".")
  invisible(path)
}

# Map Pmetrics analytical library template names to DSL analytical structures.
dsl_analytical_structure <- function(tem) {
  dplyr::case_when(
    tem == "one_comp_iv" ~ "one_compartment",
    tem == "one_comp_iv_cl" ~ "one_compartment_cl",
    tem == "one_comp_bolus" ~ "one_compartment_with_absorption",
    tem == "one_comp_bolus_cl" ~ "one_compartment_cl_with_absorption",
    tem == "two_comp_iv" ~ "two_compartments",
    tem == "two_comp_iv_cl" ~ "two_compartments_cl",
    tem == "two_comp_bolus" ~ "two_compartments_with_absorption",
    tem == "two_comp_bolus_cl" ~ "two_compartments_cl_with_absorption",
    tem == "three_comp_iv" ~ "three_compartments",
    tem == "three_comp_iv_cl" ~ "three_compartments_cl",
    tem == "three_comp_bolus" ~ "three_compartments_with_absorption",
    tem == "three_comp_bolus_cl" ~ "three_compartments_cl_with_absorption",
    .default = NA_character_
  )
}

# Derived-parameter aliases mapping Pmetrics library parameter names to the names
# required by each DSL analytical structure. Each entry is
# `<dsl_required_name> = <library_parameter_name>`.
dsl_analytical_param_map <- function(structure) {
  switch(structure,
    "two_compartments" = c(kcp = "k12", kpc = "k21"),
    "two_compartments_cl" = c(vc = "v1", vp = "v2"),
    "two_compartments_with_absorption" = c(kcp = "k23", kpc = "k32"),
    "two_compartments_cl_with_absorption" = c(vc = "v2", vp = "v3"),
    "three_compartments" = c(k10 = "ke"),
    "three_compartments_cl" = c(vc = "v1"),
    "three_compartments_with_absorption" = c(
      k10 = "ke", k12 = "k23", k13 = "k24", k21 = "k32", k31 = "k42"
    ),
    character(0)
  )
}

# Assemble the full pharmsol DSL text for a PM_model object.
model_to_dsl <- function(model) {
  arg_list <- model$arg_list
  model_list <- model$model_list
  if (is.null(arg_list) || is.null(model_list)) {
    cli::cli_abort("Model is not fully defined; cannot generate DSL source.")
  }

  type <- model_list$type
  parameters <- tolower(names(arg_list$pri))
  covariate_names <- tolower(names(arg_list$cov))

  header <- character(0)
  header <- c(header, sprintf("name = %s", if (is.null(model_list$name)) "user" else model_list$name))
  header <- c(header, sprintf("kind = %s", if (type == "Analytical") "analytical" else "ode"))
  header <- c(header, sprintf("params = %s", paste(parameters, collapse = ", ")))

  if (length(covariate_names) > 0) {
    cov_decls <- vapply(seq_along(covariate_names), function(i) {
      # arg_list$cov holds 1 for linear interpolation, 0 for carry-forward.
      if (isTRUE(unname(arg_list$cov[i]) == 0)) {
        paste0(covariate_names[i], "@carry_forward")
      } else {
        covariate_names[i]
      }
    }, character(1))
    header <- c(header, sprintf("covariates = %s", paste(cov_decls, collapse = ", ")))
  }

  # Derived (secondary) equations shared across blocks.
  derived <- character(0)
  if (!is.null(arg_list$sec)) {
    derived <- c(derived, dsl_sec_block(arg_list$sec))
  }

  if (type == "Analytical") {
    return(dsl_analytical(model, header, derived, parameters))
  }

  # ---- ODE model ----
  eqn <- dsl_eqn_block(arg_list$eqn)
  derived <- c(derived, eqn$derived)

  out <- dsl_out_block(arg_list$out)
  derived <- c(derived, out$derived)

  init_lines <- character(0)
  if (!is.null(arg_list$ini)) {
    ini <- dsl_ini_block(arg_list$ini)
    derived <- c(derived, ini$derived)
    init_lines <- ini$lines
  }

  lag_lines <- character(0)
  if (!is.null(arg_list$lag)) {
    lag <- dsl_route_property_block(arg_list$lag, "lag")
    derived <- c(derived, lag$derived)
    lag_lines <- lag$lines
  }

  fa_lines <- character(0)
  if (!is.null(arg_list$fa)) {
    fa <- dsl_route_property_block(arg_list$fa, "fa")
    derived <- c(derived, fa$derived)
    fa_lines <- fa$lines
  }

  # Number of states and outputs.
  n_states <- max(
    get_max_assignment_index(arg_list$eqn, "dx"),
    get_max_index(arg_list$eqn, "x"),
    if (!is.null(arg_list$ini)) get_max_index(arg_list$ini, "x") else 0L,
    get_max_index(arg_list$out, "x")
  )
  n_out <- get_max_assignment_index(arg_list$out, "y")

  states <- paste0("x", seq_len(n_states))
  outputs <- paste0("outeq_", seq_len(n_out))

  routes_result <- dsl_finalize_routes(eqn$routes)
  routes <- routes_result$routes
  # Pmetrics data uses 1-based INPUT labels. Bolus routes keep the data input
  # label; infusion routes that share an input with a bolus receive a fresh
  # label (see `dsl_finalize_routes`), captured in the remap table.
  route_lines <- vapply(routes, function(r) {
    sprintf("%s(input_%d) -> x%d", r$kind, r$label, r$comp)
  }, character(1))

  # Assemble the DSL text in an order that respects definite assignment:
  # declarations, routes, derived values, route properties, initial conditions,
  # derivatives, and finally outputs.
  lines <- c(
    header,
    sprintf("states = %s", paste(states, collapse = ", ")),
    sprintf("outputs = %s", paste(outputs, collapse = ", ")),
    "",
    route_lines,
    if (length(route_lines) > 0) "" else NULL,
    derived,
    if (length(derived) > 0) "" else NULL,
    lag_lines,
    fa_lines,
    if (length(lag_lines) > 0 || length(fa_lines) > 0) "" else NULL,
    init_lines,
    if (length(init_lines) > 0) "" else NULL,
    eqn$dx,
    "",
    out$out
  )

  list(dsl = paste(lines, collapse = "\n"), remap = routes_result$remap)
}

# Assemble DSL text for an analytical (library-structure) model.
dsl_analytical <- function(model, header, derived, parameters) {
  arg_list <- model$arg_list
  template <- model$arg_list$tem
  if (is.null(template)) {
    template <- attr(model, "model_template")
  }
  tem_name <- model$model_list$name
  structure <- dsl_analytical_structure(tem_name)
  if (is.na(structure)) {
    cli::cli_abort(c(
      "x" = "Analytical model template {.val {tem_name}} is not supported by the DSL backend.",
      "i" = "See {.fn model_lib} for supported templates."
    ))
  }

  out <- dsl_out_block(arg_list$out)
  derived <- c(derived, out$derived)

  # The DSL analytical structures require specific derived-parameter names (e.g.
  # `kcp`, `kpc`, `vc`). The Pmetrics model-library templates use their own
  # parameter names, so emit derived aliases mapping the library names to the
  # names the structure expects.
  param_aliases <- dsl_analytical_param_map(structure)
  if (length(param_aliases) > 0) {
    alias_lines <- paste0(names(param_aliases), " = ", unname(param_aliases))
    derived <- c(alias_lines, derived)
  }

  # Determine the number of compartments the structure requires.
  n_states <- dsl_analytical_state_count(structure)
  states <- paste0("x", seq_len(n_states))
  n_out <- get_max_assignment_index(arg_list$out, "y")
  outputs <- paste0("outeq_", seq_len(n_out))

  # Declare the dose route. Absorption ("bolus") templates receive a bolus into
  # the depot (x1); IV templates receive an infusion into the central
  # compartment (x1).
  route_line <- if (stringr::str_detect(structure, "absorption")) {
    "bolus(input_1) -> x1"
  } else {
    "infusion(input_1) -> x1"
  }

  lines <- c(
    header,
    sprintf("structure = %s", structure),
    sprintf("states = %s", paste(states, collapse = ", ")),
    sprintf("outputs = %s", paste(outputs, collapse = ", ")),
    "",
    route_line,
    "",
    derived,
    if (length(derived) > 0) "" else NULL,
    out$out
  )

  # Analytical (library-structure) models declare no explicit routes, so there
  # is nothing to remap.
  list(dsl = paste(lines, collapse = "\n"), remap = list())
}

# Number of states (compartments) for a DSL analytical structure.
dsl_analytical_state_count <- function(structure) {
  dplyr::case_when(
    stringr::str_starts(structure, "one_compartment") ~ 1L + as.integer(stringr::str_detect(structure, "absorption")),
    stringr::str_starts(structure, "two_compartments") ~ 2L + as.integer(stringr::str_detect(structure, "absorption")),
    stringr::str_starts(structure, "three_compartments") ~ 3L + as.integer(stringr::str_detect(structure, "absorption")),
    .default = NA_integer_
  )
}
