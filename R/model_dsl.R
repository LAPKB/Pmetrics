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
#   * ODE inputs `b[j]`/`bolus[j]` -> `bolus(input_{j})`       (RHS term)
#                `rateiv[j]`/`r[j]`-> `infusion(input_{j})`    (RHS term)
#   * Params / covariates keep their (lower-cased) names.
#
# pharmsol identifies routes and outputs by label and rejects bare numeric
# labels, so a numeric Pmetrics identifier `n` becomes `input_n` / `outeq_n`
# (see `pm_input_label()` / `pm_output_label()`). Identifiers are never
# renumbered: the data's `INPUT`/`OUTEQ` values are canonicalised to the same
# labels before the backend reads them.
#
# ODE inputs and scales remain on the derivative RHS. The DSL infers their routes
# and lowers them into the existing simulator machinery. Analytical models keep
# explicit route declarations and route properties.
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

# Runtime-only effect functions provided by pharmsol, with their required
# argument counts. Pmetrics validates the arity here so a wrong call is
# reported against the R expression rather than against generated DSL text.
dsl_effect_functions <- function() {
  c(estimate_effect_2 = 5L, estimate_effect_3 = 10L)
}

# The documented signature of each runtime effect function, used in messages.
dsl_effect_signature <- function() {
  c(
    estimate_effect_2 = "estimate_effect_2(u, v, alpha, h1, h2)",
    estimate_effect_3 = "estimate_effect_3(a, b, c, alpha12, alpha13, alpha23, alpha123, h1, h2, h3)"
  )
}

# Function names that used to exist with different arguments. They get a
# migration message instead of a generic "unsupported function" error, because
# the replacement changes the *meaning* of the call rather than just its name.
dsl_legacy_functions <- function() {
  c(
    get_e2 = paste(
      "Replace {.code get_e2(a, b, w, h1, h2, alpha_s)} with",
      "{.code estimate_effect_2(a, b, alpha_s, h1, h2)}."
    ),
    get_e3 = paste(
      "Replace the six-argument {.code get_e3} call with",
      "{.code estimate_effect_3(a, b, c, alpha12, alpha13, alpha23, alpha123, h1, h2, h3)}."
    )
  )
}

# One-line rendering of an R expression, used to quote the user's own code in
# diagnostics instead of generated DSL text.
dsl_expr_label <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

# Abort with a message that names the unsupported call and what to write
# instead. Every rejection in this file goes through this helper so the wording
# stays consistent and always carries a fix. `reason` and `fix` are plain text:
# braces in them are escaped so cli shows them literally.
dsl_literal <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

dsl_unsupported <- function(expr, reason, fix = NULL) {
  bullets <- c("x" = paste0(dsl_literal(reason), " {.code {dsl_expr_label(expr)}}"))
  if (!is.null(fix)) bullets <- c(bullets, "i" = dsl_literal(fix))
  cli::cli_abort(bullets, call = NULL)
}

# Turn a backend DSL diagnostic into one that points at the user's model. The
# backend reports positions in generated text; `rendered$src` records the R
# statement behind each generated line, so the message can quote that instead.
dsl_error_bullets <- function(message, rendered) {
  core <- sub("^.*?error\\[DSL[0-9]+\\]:\\s*", "", message)
  # Keep the first line only: the rest is the generated-source caret diagram.
  core <- trimws(sub("\\n.*$", "", core))
  if (!nzchar(core)) {
    core <- trimws(message)
  }

  bullets <- c("x" = dsl_literal(core))

  line <- NA_integer_
  found <- regmatches(message, regexpr("line ([0-9]+)", message))
  if (length(found) == 1L && nzchar(found)) {
    line <- as.integer(sub("^line ", "", found))
  }
  origin <- NA_character_
  if (!is.na(line) && line >= 1L && line <= length(rendered$src)) {
    origin <- rendered$src[[line]]
  }

  if (!is.na(origin) && nzchar(origin)) {
    bullets <- c(bullets, "i" = dsl_literal(sprintf("From your model, %s", origin)))
  }
  c(
    bullets,
    "i" = "Check the block this line came from for an undeclared name, a duplicate assignment, or a value that is not a number."
  )
}

# ---------------------------------------------------------------------------
# Render context
# ---------------------------------------------------------------------------
# Rendering carries a context so an expression can introduce helper derived
# values when the DSL cannot express an R construct in place. Helpers are
# emitted as ordinary derived assignments immediately before the statement that
# needs them, which keeps R's meaning without teaching the backend new syntax.

dsl_ctx <- function(used = character(0)) {
  ctx <- new.env(parent = emptyenv())
  ctx$used <- unique(c(used, dsl_language_words()))
  ctx$index <- 0L
  ctx$hoist <- list()
  ctx
}

# Words the DSL treats specially, so a generated helper can never shadow them.
dsl_language_words <- function() {
  c(
    "model", "kind", "ode", "analytical", "sde", "parameters", "constants",
    "covariates", "states", "routes", "derive", "dynamics", "outputs", "init",
    "drift", "diffusion", "particles", "if", "else", "for", "in", "let",
    "true", "false"
  )
}

# A helper name that cannot collide with anything the model already uses.
dsl_fresh_temp <- function(ctx) {
  repeat {
    ctx$index <- ctx$index + 1L
    name <- sprintf("_pmt%d", ctx$index)
    if (!name %in% ctx$used) {
      ctx$used <- c(ctx$used, name)
      return(name)
    }
  }
}

# Emit `code` as a derived helper and return the helper's name.
dsl_hoist_code <- function(ctx, code) {
  name <- dsl_fresh_temp(ctx)
  ctx$hoist[[length(ctx$hoist) + 1L]] <- list(name = name, code = code)
  name
}

# Render `expr` into a helper derived value and return its name.
dsl_hoist <- function(ctx, expr, allow_if = TRUE) {
  dsl_hoist_code(ctx, expr_to_dsl(expr, ctx, allow_if = allow_if))
}

# Consume the helpers accumulated while rendering one statement. Returns the
# helper lines, in the order they must be emitted.
dsl_take_hoists <- function(ctx) {
  hoists <- ctx$hoist
  ctx$hoist <- list()
  vapply(hoists, function(h) sprintf("%s = %s", h$name, h$code), character(1))
}

# ---------------------------------------------------------------------------
# Provenance
# ---------------------------------------------------------------------------
# Every emitted line records the R statement it came from, so a backend
# diagnostic that points at generated text can be reported against the user's
# own model instead.

dsl_with_src <- function(lines, src) {
  attr(lines, "src") <- src
  lines
}

dsl_src <- function(lines, default = NA_character_) {
  src <- attr(lines, "src")
  if (is.null(src)) rep(default, length(lines)) else src
}

# Human-readable origin of a statement, used in diagnostics.
dsl_statement_label <- function(block, expr) {
  sprintf("%s: %s", block, dsl_expr_label(expr))
}

# Does this expression reference a dose input anywhere?
dsl_expr_has_route <- function(expr) {
  length(eqn_route_inputs(expr, "bolus")) > 0L ||
    length(eqn_route_inputs(expr, "infusion")) > 0L
}

# A dose input inside a conditional has no meaning: the route is inferred from
# the derivative RHS, and conditional dosing is not a supported event model.
# Report it against the user's expression instead of letting the helper hoist
# the input out of the derivative where it would silently stop being a dose.
dsl_check_conditional_routes <- function(expr) {
  if (is.call(expr)) {
    head <- as.character(expr[[1]])
    if (head %in% c("if", "ifelse") && dsl_expr_has_route(expr)) {
      cli::cli_abort(
        c(
          "x" = "A dose input cannot be inside a conditional: {.code {dsl_expr_label(expr)}}.",
          "i" = "Put the conditional part in a derived value, then use one {.code bolus(input) * scale} term outside the conditional."
        ),
        call = NULL
      )
    }
    lapply(as.list(expr), dsl_check_conditional_routes)
  }
  invisible(NULL)
}

# Fold n-ary rendered arguments into the binary form the DSL provides.
dsl_fold_binary <- function(fn, codes) {
  acc <- codes[[1]]
  for (i in seq_along(codes)[-1]) {
    acc <- sprintf("%s(%s, %s)", fn, acc, codes[[i]])
  }
  acc
}

# R rounds halves to even; the DSL's `round` rounds halves away from zero. Emit
# R's rule explicitly rather than accept a different answer for exact halves.
dsl_round_helpers <- function(ctx, code, digits) {
  scaled <- if (digits == 0L) code else sprintf("(%s * 10.0^%d.0)", code, digits)
  value <- dsl_hoist_code(ctx, scaled)
  base <- dsl_hoist_code(ctx, sprintf("floor(%s)", value))
  rest <- dsl_hoist_code(ctx, sprintf("%s - %s", value, base))
  # The conditional has to be a statement of its own: the DSL rejects a
  # conditional wrapped in parentheses, so it cannot be scaled in place.
  rounded <- dsl_hoist_code(
    ctx,
    sprintf(
      "if (%s < 0.5) %s else if (%s > 0.5) %s + 1.0 else if (floor(%s / 2.0) * 2.0 == %s) %s else %s + 1.0",
      rest, base, rest, base, base, base, base, base
    )
  )
  if (digits == 0L) {
    return(rounded)
  }
  dsl_hoist_code(ctx, sprintf("(%s / 10.0^%d.0)", rounded, digits))
}

# Convert a pure R expression (no assignments or blocks) to a DSL expression.
expr_to_dsl <- function(expr, ctx = NULL, allow_if = TRUE) {
  # Numeric literals: emit integers with a trailing `.0` so the DSL treats them
  # as floating point, matching the Rust transpiler's behaviour.
  if (is.numeric(expr) && length(expr) == 1 && is.na(expr)) {
    dsl_unsupported(
      expr, "Model expressions must be finite numbers, but this one is missing:",
      "Remove the missing value or replace it with a number."
    )
  }
  if (is.numeric(expr) && length(expr) == 1 && !is.finite(expr)) {
    dsl_unsupported(expr, "Model expressions must be finite numbers, but this one is not:", "Use a large finite value such as 1e30 instead of Inf or NaN.")
  }
  if (is.numeric(expr) && length(expr) == 1) {
    val <- expr
    if (is.finite(val) && val == floor(val) && abs(val) <= .Machine$integer.max) {
      return(sprintf("%d.0", as.integer(val)))
    }
    return(as.character(val))
  }
  # R's logical constants map to the DSL's boolean literals, which are valid in
  # conditions. They are not numbers, so the DSL rejects them as values.
  if (is.logical(expr) && length(expr) == 1) {
    if (is.na(expr)) {
      dsl_unsupported(
        expr, "Missing values cannot be used in a model:",
        "Remove the missing value or replace it with a number."
      )
    }
    return(if (expr) "true" else "false")
  }
  if (is.symbol(expr)) {
    return(tolower(as.character(expr)))
  }
  if (!is.call(expr)) {
    dsl_unsupported(expr, "This token cannot be used in a model expression:")
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
    if (var %in% c("b", "bolus")) {
      cli::cli_abort(c(
        "x" = "Bolus inputs may only be standalone additive terms or one exact product with a scale.",
        "i" = "Use {.code B[1]}, {.code B[1] * scale}, or {.code scale * B[1]}."
      ))
    }
    if (var %in% c("rateiv", "r")) {
      cli::cli_abort(c(
        "x" = "Infusion inputs may only be standalone additive terms or one exact product with a scale.",
        "i" = "Use {.code R[1]}, {.code R[1] * scale}, or {.code scale * R[1]}."
      ))
    }
    return(sprintf("%s%d", var, idx))
  }

  # R conditionals are expressions, but the DSL can only place one as a whole
  # assignment right-hand side. Anywhere else the value is hoisted into a
  # derived helper, so nested conditionals work exactly as they do in R.
  if (op %in% c("if", "ifelse")) {
    args <- as.list(expr[-1])
    if (length(args) != 3) {
      dsl_unsupported(
        expr, "Conditional expressions must include an else branch:",
        "Write if (cond) a else b."
      )
    }
    dsl_check_conditional_routes(expr)
    if (!allow_if) {
      if (is.null(ctx)) {
        dsl_unsupported(
          expr, "A conditional cannot be nested inside another expression:",
          "Assign it to a secondary variable first, for example tmp = if (cond) a else b."
        )
      }
      return(dsl_hoist(ctx, expr, allow_if = TRUE))
    }
    # Only the else branch may itself be a conditional (right-associative
    # else-if chains); nested conditionals in the condition or the then branch
    # are hoisted into helpers.
    cond <- expr_to_dsl(args[[1]], ctx, allow_if = FALSE)
    then_code <- expr_to_dsl(args[[2]], ctx, allow_if = FALSE)
    else_code <- expr_to_dsl(args[[3]], ctx, allow_if = TRUE)
    return(sprintf("if (%s) %s else %s", cond, then_code, else_code))
  }

  args <- as.list(expr[-1])
  a <- lapply(args, function(x) expr_to_dsl(x, ctx, allow_if = FALSE))

  # Legacy spellings carry a migration message, because the replacement changes
  # what the call means and not only its name.
  legacy <- dsl_legacy_functions()
  if (op %in% names(legacy)) {
    cli::cli_abort(
      c(
        "x" = "Unsupported function {.val {op}} in {.code {dsl_expr_label(expr)}}.",
        "i" = legacy[[op]],
        "i" = "The runtime computes w = alpha * u * v, so check that the value you used to pass as w followed the same convention."
      ),
      call = NULL
    )
  }

  # Runtime-only effect functions. Arity is checked here so a wrong call is
  # reported against the R expression rather than generated DSL text.
  effects <- dsl_effect_functions()
  if (op %in% names(effects)) {
    expected <- unname(effects[[op]])
    if (length(a) != expected) {
      cli::cli_abort(
        c(
          "x" = "{.fn {op}} takes {expected} arguments, but {.code {dsl_expr_label(expr)}} has {length(a)}.",
          "i" = "Write {.code {dsl_effect_signature()[[op]]}}.",
          "i" = "u and v are normalised exposures, alpha is the interaction coefficient, and h1/h2 are the Hill exponents."
        ),
        call = NULL
      )
    }
    return(sprintf("%s(%s)", op, paste(unlist(a), collapse = ", ")))
  }

  # pmin/pmax are elementwise in R; for the scalar expressions a model uses,
  # that is min/max. The DSL's min/max are binary while R's are variadic, and
  # trunc/sign are not DSL intrinsics at all.
  if (op %in% c("pmin", "pmax")) {
    if (length(a) < 2) {
      dsl_unsupported(expr, stringr::str_glue("{op} needs at least two arguments:"), stringr::str_glue("Write {op}(a, b)."))
    }
    return(dsl_fold_binary(if (op == "pmin") "min" else "max", a))
  }
  if (op %in% c("max", "min") && length(a) != 2) {
    if (length(a) == 1) {
      return(a[[1]])
    }
    if (length(a) == 0) {
      dsl_unsupported(expr, stringr::str_glue("{op} needs at least one argument:"))
    }
    return(dsl_fold_binary(op, a))
  }
  if (op == "trunc") {
    if (length(a) != 1) dsl_unsupported(expr, "trunc takes one argument:")
    value <- dsl_hoist_code(ctx, a[[1]])
    return(dsl_hoist_code(ctx, sprintf("if (%s < 0.0) ceil(%s) else floor(%s)", value, value, value)))
  }
  if (op == "sign") {
    if (length(a) != 1) dsl_unsupported(expr, "sign takes one argument:")
    value <- dsl_hoist_code(ctx, a[[1]])
    # Multiplying by zero keeps NaN propagating, as sign() does in R.
    return(dsl_hoist_code(ctx, sprintf(
      "if (%s < 0.0) -1.0 else if (%s > 0.0) 1.0 else (%s * 0.0)", value, value, value
    )))
  }
  if (op == "round") {
    if (length(a) == 1) {
      return(dsl_round_helpers(ctx, a[[1]], 0L))
    }
    digits <- if (length(args) >= 2) args[[2]] else NULL
    if (!(is.numeric(digits) && length(digits) == 1 && is.finite(digits) &&
      digits == floor(digits) && abs(digits) <= 15)) {
      dsl_unsupported(
        expr, "round needs a whole number of digits between -15 and 15:",
        "Write the number of digits literally, for example round(x, 2)."
      )
    }
    if (length(a) != 2) dsl_unsupported(expr, "round takes a value and an optional number of digits:")
    return(dsl_round_helpers(ctx, a[[1]], as.integer(digits)))
  }
  if (op == "log" && length(a) == 2) {
    # R's log(x, base); the DSL has no two-argument logarithm.
    return(sprintf("(ln(%s) / ln(%s))", a[[1]], a[[2]]))
  }

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
      if (op %in% names(fmap)) {
        sprintf("%s(%s)", fmap[[op]], paste(unlist(a), collapse = ", "))
      } else {
        dsl_unsupported(
          expr, stringr::str_glue("Unsupported function '{op}':"),
          stringr::str_glue(
            "Supported functions are {paste(sort(unique(unname(dsl_function_map()))), collapse = ', ')}, "
          , "plus the runtime effect functions {paste(names(dsl_effect_functions()), collapse = ', ')}."
          )
        )
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

# Recognize one exact linear route product, with the route on either side.
dsl_scaled_route_of <- function(expr) {
  if (!is.call(expr) || !identical(expr[[1]], as.name("*")) || length(expr) != 3) {
    return(NULL)
  }

  left <- dsl_route_of(expr[[2]])
  right <- dsl_route_of(expr[[3]])
  if (is.null(left) == is.null(right)) {
    return(NULL)
  }

  route <- if (!is.null(left)) left else right
  route$scale <- if (!is.null(left)) expr[[3]] else expr[[2]]
  if (route$kind == "bolus") {
    uses_state_or_input <- function(value) {
      if (!is.call(value)) return(FALSE)
      if (identical(value[[1]], as.name("["))) {
        name <- tolower(as.character(value[[2]]))
        if (name %in% c("x", "dx", "b", "bolus", "r", "rateiv")) return(TRUE)
      }
      any(vapply(as.list(value)[-1], uses_state_or_input, logical(1)))
    }
    if (uses_state_or_input(route$scale)) {
      cli::cli_abort("A bolus scale cannot depend directly on state or dose inputs.")
    }
  }
  route
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

# Every identifier a block assigns or reads as a plain variable, so generated
# helper names can never collide with the model's own names.
dsl_assigned_names <- function(fun) {
  if (is.null(fun) || !is.function(fun)) {
    return(character(0))
  }
  found <- character(0)
  walk <- function(expr) {
    if (is.call(expr)) {
      if (dsl_is_assign(expr)) {
        lhs <- expr[[2]]
        if (is.symbol(lhs)) {
          found <<- c(found, tolower(as.character(lhs)))
        } else if (is.call(lhs) && identical(lhs[[1]], as.name("["))) {
          found <<- c(found, tolower(as.character(lhs[[2]])))
        }
      }
      lapply(as.list(expr), walk)
    }
    invisible(NULL)
  }
  walk(body(fun))
  unique(found)
}

# Build a DSL expression string from a list of signed terms.
dsl_join_terms <- function(terms, ctx) {
  if (length(terms) == 0) {
    return("0.0")
  }
  # A conditional may be emitted in place only when it is the whole right-hand
  # side. Anywhere else it is hoisted into a derived helper, so summing or
  # negating a conditional is fine.
  allow_if <- length(terms) == 1L && terms[[1]]$sign > 0
  pieces <- character(0)
  for (i in seq_along(terms)) {
    t <- terms[[i]]
    es <- if (!is.null(t$dsl)) t$dsl else expr_to_dsl(t$expr, ctx, allow_if = allow_if)
    if (i == 1) {
      pieces <- if (t$sign < 0) sprintf("-(%s)", es) else es
    } else {
      pieces <- paste0(pieces, if (t$sign < 0) " - " else " + ", sprintf("(%s)", es))
    }
  }
  pieces
}

# Convert a derivative RHS into (routes, explicit-input DSL expression) for the given
# destination compartment index `comp`.
dsl_extract_routes <- function(rhs, comp, ctx) {
  dsl_check_conditional_routes(rhs)
  terms <- dsl_flatten_add(rhs)
  routes <- list()
  kept <- list()
  for (t in terms) {
    route <- dsl_route_of(t$expr)
    if (is.null(route)) {
      route <- dsl_scaled_route_of(t$expr)
    }
    if (!is.null(route)) {
      if (t$sign < 0) {
        cli::cli_abort(c(
          "x" = "Bolus/infusion inputs must be added (not subtracted) in derivative equations.",
          "i" = "Write {.code dx[{comp}] = ... + rateiv[{route$input}]}."
        ))
      }
      route$comp <- comp
      routes[[length(routes) + 1]] <- route
      input <- sprintf("%s(%s)", route$kind, pm_input_label(route$input))
      if (!is.null(route$scale)) {
        input <- sprintf("%s * (%s)", input, expr_to_dsl(route$scale, ctx, allow_if = FALSE))
      }
      kept[[length(kept) + 1]] <- list(sign = t$sign, dsl = input)
    } else {
      kept[[length(kept) + 1]] <- t
    }
  }
  list(routes = routes, expr = dsl_join_terms(kept, ctx))
}

# Emit the ODE equation block: returns routes, derived assignments, and dx lines.
dsl_eqn_block <- function(fun, ctx) {
  exprs <- dsl_body_stmts(fun)
  routes <- list()
  derived <- character(0)
  derived_src <- character(0)
  dx_lines <- character(0)
  dx_src <- character(0)

  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      dsl_unsupported(
        e, "Only assignments are supported here:",
        "Control flow such as for loops is not available. Write the assignment for each case instead."
      )
    }
    label <- dsl_statement_label("equation block", e)
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "dx") {
        dsl_unsupported(e, sprintf("Unexpected assignment to %s[%s] in the equation block:", tgt, idx))
      }
      res <- dsl_extract_routes(rhs, idx, ctx)
      routes <- c(routes, res$routes)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted)
      derived_src <- c(derived_src, rep(label, length(hoisted)))
      dx_lines <- c(dx_lines, sprintf("dx(x%d) = %s", idx, res$expr))
      dx_src <- c(dx_src, label)
    } else {
      # Scalar (secondary/derived) assignment.
      name <- tolower(as.character(lhs))
      code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted, sprintf("%s = %s", name, code))
      derived_src <- c(derived_src, rep(label, length(hoisted) + 1L))
    }
  }

  list(
    routes = routes,
    derived = derived,
    derived_src = derived_src,
    dx = dx_lines,
    dx_src = dx_src
  )
}

# Emit the output block: returns derived assignments and out() lines.
dsl_out_block <- function(fun, ctx) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  derived_src <- character(0)
  out_lines <- character(0)
  out_src <- character(0)

  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      dsl_unsupported(e, "Only assignments are supported in the output block:")
    }
    label <- dsl_statement_label("output block", e)
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "y") {
        dsl_unsupported(e, sprintf("Unexpected assignment to %s[%s] in the output block:", tgt, idx))
      }
      # Pmetrics data uses numeric OUTEQ identifiers, so output `y[i]` maps to
      # the canonical pharmsol output label for `i`.
      code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted)
      derived_src <- c(derived_src, rep(label, length(hoisted)))
      out_lines <- c(out_lines, sprintf("out(%s) = %s", pm_output_label(idx), code))
      out_src <- c(out_src, label)
    } else {
      name <- tolower(as.character(lhs))
      code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted, sprintf("%s = %s", name, code))
      derived_src <- c(derived_src, rep(label, length(hoisted) + 1L))
    }
  }

  list(
    derived = derived,
    derived_src = derived_src,
    out = out_lines,
    out_src = out_src
  )
}

# Emit `derive` assignments from a secondary-equation block.
dsl_sec_block <- function(fun, ctx) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  derived_src <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      dsl_unsupported(e, "Only assignments are supported in the secondary block:")
    }
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (!is.symbol(lhs)) {
      dsl_unsupported(e, "Secondary equations must assign to a scalar variable:")
    }
    label <- dsl_statement_label("secondary block", e)
    code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
    hoisted <- dsl_take_hoists(ctx)
    derived <- c(derived, hoisted, sprintf("%s = %s", tolower(as.character(lhs)), code))
    derived_src <- c(derived_src, rep(label, length(hoisted) + 1L))
  }
  list(lines = derived, src = derived_src)
}

# Emit route-property modifiers (`lag(...)` / `fa(...)`) from a lag/fa block.
# `target` is the DSL property name ("lag" or "fa"); the R block assigns to
# `lag[j]` / `fa[j]` where `j` is the 1-based input index.
dsl_route_property_block <- function(fun, target, ctx) {
  exprs <- if (is.null(fun)) list() else dsl_body_stmts(fun)
  derived <- character(0)
  derived_src <- character(0)
  lines <- character(0)
  line_src <- character(0)
  values <- list()
  seen_inputs <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the {target} block for the DSL backend.")
    }
    label <- dsl_statement_label(paste(target, "block"), e)
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != target) {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in {target} block.")
      }
      key <- as.character(idx)
      if (key %in% seen_inputs) {
        cli::cli_abort("The {target} block assigns input {idx} more than once.")
      }
      seen_inputs <- c(seen_inputs, key)
      # A route property cannot take a conditional directly, so any conditional
      # is hoisted into an event-safe derived value first.
      values[[key]] <- expr_to_dsl(rhs, ctx, allow_if = FALSE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted)
      derived_src <- c(derived_src, rep(label, length(hoisted)))
      lines <- c(lines, sprintf("%s(%s) = %s", target, pm_input_label(idx), values[[key]]))
      line_src <- c(line_src, label)
    } else {
      name <- tolower(as.character(lhs))
      code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted, sprintf("%s = %s", name, code))
      derived_src <- c(derived_src, rep(label, length(hoisted) + 1L))
    }
  }

  list(
    derived = derived,
    derived_src = derived_src,
    lines = lines,
    line_src = line_src,
    values = values
  )
}

# Emit `init(...)` statements from an initial-conditions block.
dsl_ini_block <- function(fun, ctx) {
  exprs <- dsl_body_stmts(fun)
  derived <- character(0)
  derived_src <- character(0)
  lines <- character(0)
  line_src <- character(0)
  for (e in exprs) {
    if (!dsl_is_assign(e)) {
      cli::cli_abort("Only assignments are supported in the initial-conditions block for the DSL backend.")
    }
    label <- dsl_statement_label("initial-conditions block", e)
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      tgt <- tolower(as.character(lhs[[2]]))
      idx <- as.integer(lhs[[3]])
      if (tgt != "x") {
        cli::cli_abort("Unexpected indexed assignment to {.code {tgt}[{idx}]} in initial-conditions block.")
      }
      code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted)
      derived_src <- c(derived_src, rep(label, length(hoisted)))
      lines <- c(lines, sprintf("init(x%d) = %s", idx, code))
      line_src <- c(line_src, label)
    } else {
      name <- tolower(as.character(lhs))
      code <- expr_to_dsl(rhs, ctx, allow_if = TRUE)
      hoisted <- dsl_take_hoists(ctx)
      derived <- c(derived, hoisted, sprintf("%s = %s", name, code))
      derived_src <- c(derived_src, rep(label, length(hoisted) + 1L))
    }
  }
  list(
    derived = derived,
    derived_src = derived_src,
    lines = lines,
    line_src = line_src
  )
}

# Finalize route usages into concrete DSL routes.
#
# Each usage is `list(kind, input, comp, scale)` where `scale` is present for an
# exact route-input product. Route labels are unique *per kind*
# in the pharmsol DSL, so the same label may declare both a bolus and an
# infusion; no relabelling of the data is ever required.
#
# Returns a list of `{kind, input, label, comp, scale}` route declarations.
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

  final_routes <- list()

  for (inp in sort(unique(seen_inputs))) {
    grp <- by_input[[as.character(inp)]]

    for (kd in c("bolus", "infusion")) {
      kind_routes <- Filter(function(r) identical(r$kind, kd), grp)
      comps <- unique(vapply(kind_routes, function(r) as.integer(r$comp), integer(1)))
      if (length(comps) == 0) next
      if (length(comps) > 1) {
        cli::cli_abort(c(
          "x" = "Input {inp} directs a {kd} into more than one compartment.",
          "i" = "Each input may direct a bolus (or an infusion) into a single compartment."
        ))
      }

      scaled <- Filter(function(r) !is.null(r$scale), kind_routes)
      if ((kd == "bolus" || length(scaled) > 0) && length(kind_routes) > 1) {
        cli::cli_abort(c(
          "x" = "The {kd} input {inp} is used more than once.",
          "i" = "Use one input term with a combined scale."
        ))
      }
      scale <- if (length(scaled) == 1) scaled[[1]]$scale else NULL

      final_routes[[length(final_routes) + 1L]] <- list(
        kind = kd, input = inp, label = pm_input_label(inp), comp = comps[[1]], scale = scale
      )
    }
  }

  final_routes
}

# ---------------------------------------------------------------------------
# Input / output labels
# ---------------------------------------------------------------------------
# pharmsol identifies dose routes and output equations by *label*, and rejects
# bare numeric labels in DSL source: a numeric identifier `n` must be written
# `input_n` (route) or `outeq_n` (output). Pmetrics therefore canonicalises
# purely numeric `INPUT`/`OUTEQ` identifiers to that form and passes every other
# identifier through untouched. Identifiers are never renumbered.

pm_label <- function(x, prefix) {
  x <- as.character(x)
  numeric_label <- !is.na(x) & grepl("^[[:space:]]*[0-9]+[[:space:]]*$", x)
  x[numeric_label] <- paste0(prefix, trimws(x[numeric_label]))
  x
}

# Canonical pharmsol route label for a Pmetrics `INPUT` identifier.
pm_input_label <- function(x) pm_label(x, "input_")

# Canonical pharmsol output label for a Pmetrics `OUTEQ` identifier.
pm_output_label <- function(x) pm_label(x, "outeq_")

# The model's output labels, in declaration order. DSL models declare them
# explicitly; R-defined models declare outputs as `y[i]`.
model_output_labels <- function(model) {
  if (!is.null(model$model_list$outputs)) {
    as.character(model$model_list$outputs)
  } else {
    pm_output_label(seq_len(model$model_list$n_out))
  }
}

# Rewrite the `INPUT`/`OUTEQ` columns of a written Pmetrics data file to their
# canonical pharmsol labels, so the data agrees with the labels declared in the
# DSL. Only the file handed to the backend is touched; the user's data is not.
label_data_csv <- function(path) {
  if (!file.exists(path)) {
    return(invisible(path))
  }

  df <- utils::read.csv(
    path,
    check.names = FALSE, colClasses = "character",
    na.strings = character(0), stringsAsFactors = FALSE
  )
  cols <- toupper(names(df))
  input_col <- match("INPUT", cols)
  outeq_col <- match("OUTEQ", cols)

  if (!is.na(input_col)) df[[input_col]] <- pm_input_label(df[[input_col]])
  if (!is.na(outeq_col)) df[[outeq_col]] <- pm_output_label(df[[outeq_col]])

  utils::write.csv(df, path, row.names = FALSE, quote = FALSE, na = ".")
  invisible(path)
}
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
  dsl_render(model)$text
}

# The same rendering, plus the R statement behind each emitted line. Used to
# report backend diagnostics against the user's model instead of generated text.
model_to_dsl_traced <- function(model) {
  dsl_render(model)
}

# Render a PM_model to DSL text, keeping per-line provenance.
dsl_render <- function(model) {
  arg_list <- model$arg_list
  model_list <- model$model_list
  if (is.null(arg_list) || is.null(model_list)) {
    cli::cli_abort("Model is not fully defined; cannot generate DSL source.")
  }

  type <- model_list$type
  parameters <- tolower(names(arg_list$pri))
  covariate_names <- tolower(names(arg_list$cov))

  # Generated helper names must never collide with a name the model uses.
  ctx <- dsl_ctx(used = c(
    parameters,
    covariate_names,
    unlist(lapply(
      list(arg_list$sec, arg_list$eqn, arg_list$out, arg_list$ini, arg_list$lag, arg_list$fa),
      dsl_assigned_names
    ))
  ))

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
  derived_src <- character(0)
  if (!is.null(arg_list$sec)) {
    sec <- dsl_sec_block(arg_list$sec, ctx)
    derived <- c(derived, sec$lines)
    derived_src <- c(derived_src, sec$src)
  }

  if (type == "Analytical") {
    return(dsl_analytical(model, header, derived, derived_src, parameters, ctx))
  }

  # ---- ODE model ----
  eqn <- dsl_eqn_block(arg_list$eqn, ctx)
  derived <- c(derived, eqn$derived)
  derived_src <- c(derived_src, eqn$derived_src)
  routes <- dsl_finalize_routes(eqn$routes)

  out <- dsl_out_block(arg_list$out, ctx)
  derived <- c(derived, out$derived)
  derived_src <- c(derived_src, out$derived_src)

  init_lines <- character(0)
  init_src <- character(0)
  if (!is.null(arg_list$ini)) {
    ini <- dsl_ini_block(arg_list$ini, ctx)
    derived <- c(derived, ini$derived)
    derived_src <- c(derived_src, ini$derived_src)
    init_lines <- ini$lines
    init_src <- ini$line_src
  }

  lag_lines <- character(0)
  lag_src <- character(0)
  if (!is.null(arg_list$lag)) {
    lag <- dsl_route_property_block(arg_list$lag, "lag", ctx)
    derived <- c(derived, lag$derived)
    derived_src <- c(derived_src, lag$derived_src)
    lag_lines <- lag$lines
    lag_src <- lag$line_src
  }

  # Keep the established R fa block, but express its effect on the ODE RHS.
  if (!is.null(arg_list$fa)) {
    fa <- dsl_route_property_block(arg_list$fa, "fa", ctx)
    derived <- c(derived, fa$derived)
    derived_src <- c(derived_src, fa$derived_src)
    bolus_inputs <- vapply(Filter(function(r) r$kind == "bolus", routes),
      function(r) as.character(r$input), character(1))
    for (key in names(fa$values)) {
      if (!key %in% bolus_inputs) {
        cli::cli_abort("The fa block references input {key}, which has no bolus term.")
      }
      input <- sprintf("bolus(%s)", pm_input_label(key))
      eqn$dx <- gsub(input, sprintf("%s * (%s)", input, fa$values[[key]]), eqn$dx, fixed = TRUE)
    }
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
  outputs <- pm_output_label(seq_len(n_out))

  # Inputs remain explicit in the derivatives; the DSL infers their routes.
  # Derived values precede lag, initial conditions, and derivatives.
  lines <- character(0)
  srcs <- character(0)
  push <- function(part, src = NA_character_) {
    lines <<- c(lines, part)
    srcs <<- c(srcs, if (length(src) == 1L) rep(src, length(part)) else src)
  }
  push(header, "header")
  push(sprintf("states = %s", paste(states, collapse = ", ")))
  push(sprintf("outputs = %s", paste(outputs, collapse = ", ")))
  push("")
  push(derived, derived_src)
  if (length(derived) > 0) push("")
  push(lag_lines, lag_src)
  if (length(lag_lines) > 0) push("")
  push(init_lines, init_src)
  if (length(init_lines) > 0) push("")
  push(eqn$dx, eqn$dx_src)
  push("")
  push(out$out, out$out_src)

  list(text = paste(lines, collapse = "\n"), lines = lines, src = srcs)
}

# Assemble DSL text for an analytical (library-structure) model.
dsl_analytical <- function(model, header, derived, derived_src, parameters, ctx) {
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

  out <- dsl_out_block(arg_list$out, ctx)
  derived <- c(derived, out$derived)
  derived_src <- c(derived_src, out$derived_src)

  has_absorption <- stringr::str_detect(structure, "absorption")
  if (!has_absorption && (!is.null(arg_list$lag) || !is.null(arg_list$fa))) {
    cli::cli_abort("The `lag` and `fa` blocks can only be used with analytical bolus models.")
  }

  lag_lines <- character(0)
  lag_src <- character(0)
  fa_lines <- character(0)
  fa_src <- character(0)
  if (has_absorption && !is.null(arg_list$lag)) {
    lag <- dsl_route_property_block(arg_list$lag, "lag", ctx)
    derived <- c(derived, lag$derived)
    derived_src <- c(derived_src, lag$derived_src)
    lag_lines <- lag$lines
    lag_src <- lag$line_src
  }
  if (has_absorption && !is.null(arg_list$fa)) {
    fa <- dsl_route_property_block(arg_list$fa, "fa", ctx)
    derived <- c(derived, fa$derived)
    derived_src <- c(derived_src, fa$derived_src)
    fa_lines <- fa$lines
    fa_src <- fa$line_src
  }

  # The DSL analytical structures require specific derived-parameter names (e.g.
  # `kcp`, `kpc`, `vc`). The Pmetrics model-library templates use their own
  # parameter names, so emit derived aliases mapping the library names to the
  # names the structure expects.
  param_aliases <- dsl_analytical_param_map(structure)
  if (length(param_aliases) > 0) {
    alias_lines <- paste0(names(param_aliases), " = ", unname(param_aliases))
    derived <- c(alias_lines, derived)
    derived_src <- c(rep(NA_character_, length(alias_lines)), derived_src)
  }

  # Determine the number of compartments the structure requires.
  n_states <- dsl_analytical_state_count(structure)
  states <- paste0("x", seq_len(n_states))
  n_out <- get_max_assignment_index(arg_list$out, "y")
  outputs <- pm_output_label(seq_len(n_out))

  # Declare the dose route. Absorption ("bolus") templates receive a bolus into
  # the depot (x1); IV templates receive an infusion into the central
  # compartment (x1).
  route_line <- if (has_absorption) {
    sprintf("bolus(%s) -> x1", pm_input_label(1))
  } else {
    sprintf("infusion(%s) -> x1", pm_input_label(1))
  }

  lines <- character(0)
  srcs <- character(0)
  push <- function(part, src = NA_character_) {
    lines <<- c(lines, part)
    srcs <<- c(srcs, if (length(src) == 1L) rep(src, length(part)) else src)
  }
  push(header, "header")
  push(sprintf("structure = %s", structure))
  push(sprintf("states = %s", paste(states, collapse = ", ")))
  push(sprintf("outputs = %s", paste(outputs, collapse = ", ")))
  push("")
  push(route_line)
  push("")
  push(derived, derived_src)
  if (length(derived) > 0) push("")
  push(lag_lines, lag_src)
  push(fa_lines, fa_src)
  if (length(lag_lines) > 0 || length(fa_lines) > 0) push("")
  push(out$out, out$out_src)

  list(text = paste(lines, collapse = "\n"), lines = lines, src = srcs)
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
