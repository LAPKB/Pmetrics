testthat::skip_on_cran()

CL <- NULL
V <- NULL

build_passthrough_ode_model <- function(solver = NULL) {
  PM_model$new(
    list(
      pri = list(
        CL = ab(0.5, 1.5),
        V = ab(5, 15)
      ),
      eqn = function() {
        dx[1] <- -(CL / V) * x[1] + rateiv[1]
      },
      out = function() {
        y[1] <- x[1] / V
      },
      err = list(additive(1, c(0.1, 0, 0, 0))),
      solver = solver
    ),
    compile = FALSE
  )
}

test_that("ODE generation produces a DSL model with 1-based indices and an infusion route", {
  mod <- build_passthrough_ode_model("TSIT45")
  mod$compile(quiet = TRUE)
  dsl <- mod$dsl

  testthat::expect_match(dsl, "kind = ode")
  testthat::expect_match(dsl, "infusion\\(input_1\\) -> x1")
  testthat::expect_match(dsl, "dx\\(x1\\)")
  testthat::expect_match(dsl, "out\\(outeq_1\\)")
  # Outputs are 1-based to match the Pmetrics data OUTEQ column.
  testthat::expect_false(grepl("outeq_0", dsl, fixed = TRUE))

  # The DSL model compiles just-in-time and reports its parameters.
  testthat::expect_equal(
    model_parameters(dsl),
    tolower(names(mod$model_list$pri))
  )
})

test_that("Analytical migration produces a DSL structure model", {
  mod <- build_library_model("one_comp_iv", mode = "analytical")
  mod$compile(quiet = TRUE)
  dsl <- mod$dsl

  testthat::expect_match(dsl, "kind = analytical")
  testthat::expect_match(dsl, "structure = one_compartment")
  testthat::expect_no_error(model_parameters(dsl))
})

test_that("Inline if/else expressions emit parenthesized, unbraced DSL conditionals", {
  # Regression: the pharmsol DSL authoring surface requires
  # `if (cond) a else b` (parenthesized condition, bare branch expressions).
  # A prior emitter produced `if cond { a } else { b }`, which the backend
  # rejected with DSL1000 "expected `(` after `if` in conditional expression".
  expr_to_dsl <- getFromNamespace("expr_to_dsl", "Pmetrics")

  testthat::expect_equal(
    expr_to_dsl(quote(if (eff_time > t1) kehc else 0)),
    "if (eff_time > t1) kehc else 0.0"
  )
  # Right-associative else-if chains stay valid (nested if lands in `else`).
  testthat::expect_equal(
    expr_to_dsl(quote(if (c1) a else if (c2) b else 0)),
    "if (c1) a else if (c2) b else 0.0"
  )
  # The broken braced form must never be emitted.
  testthat::expect_false(
    grepl("{", expr_to_dsl(quote(if (a > b) x else y)), fixed = TRUE)
  )
})

test_that("Conditionals in unsupported positions raise a clear R-level error", {
  # The DSL accepts a conditional only as a whole equation right-hand side or
  # chained in the `else` branch. Other positions must fail with an actionable
  # R error rather than a cryptic DSL parse error on generated code. Patterns
  # match single tokens so they survive cli line-wrapping of the message.
  expr_to_dsl <- getFromNamespace("expr_to_dsl", "Pmetrics")

  # Nested inside an operator or function call.
  testthat::expect_error(
    expr_to_dsl(quote(2 * if (c) a else b)),
    "right-hand"
  )
  testthat::expect_error(
    expr_to_dsl(quote(exp(if (c) a else b))),
    "right-hand"
  )
  # In the `then` branch (only the `else` branch may chain).
  testthat::expect_error(
    expr_to_dsl(quote(if (a) (if (b) x else y) else z)),
    "right-hand"
  )
  # Missing `else` branch.
  testthat::expect_error(
    expr_to_dsl(quote(if (a) b)),
    "include"
  )
})

test_that("Conditionals embedded in derivative equations raise a clear error", {
  # Regression: the additive-term path for `dx` equations must not bypass the
  # conditional guard. An `if` summed with other terms is rejected, but a lone
  # whole-RHS conditional (which the backend accepts) still emits.
  dsl_eqn_block <- getFromNamespace("dsl_eqn_block", "Pmetrics")

  testthat::expect_error(
    dsl_eqn_block(function() {
      dx[1] <- a + if (c) b else d
    }),
    "right-hand"
  )
  testthat::expect_error(
    dsl_eqn_block(function() {
      dx[1] <- a - if (c) b else d
    }),
    "right-hand"
  )
  # A whole-RHS conditional remains valid and is emitted bare.
  whole_rhs <- dsl_eqn_block(function() {
    dx[1] <- if (c) a else b
  })
  testthat::expect_equal(whole_rhs$dx, "dx(x1) = if (c) a else b")
})

test_that("Secondary if/else conditionals generate a parseable DSL model", {
  mod <- PM_model$new(
    list(
      pri = list(
        CL = ab(0.5, 1.5),
        V = ab(5, 15)
      ),
      sec = function() {
        cl_eff <- if (CL > 1) CL * 1.5 else CL
      },
      eqn = function() {
        dx[1] <- -(cl_eff / V) * x[1] + rateiv[1]
      },
      out = function() {
        y[1] <- x[1] / V
      },
      err = list(additive(1, c(0.1, 0, 0, 0)))
    ),
    compile = FALSE
  )
  mod$compile(quiet = TRUE)
  dsl <- mod$dsl

  testthat::expect_match(dsl, "cl_eff = if (cl > 1.0)", fixed = TRUE)
  testthat::expect_false(grepl("} else {", dsl, fixed = TRUE))
  # The generated conditional must be accepted by the pharmsol backend.
  testthat::expect_no_error(model_parameters(dsl))
})
