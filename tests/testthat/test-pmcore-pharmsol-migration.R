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

test_that("ODE generation produces 1-based labels and an explicit infusion term", {
  mod <- build_passthrough_ode_model("TSIT45")
  mod$compile(quiet = TRUE)
  dsl <- mod$dsl

  testthat::expect_match(dsl, "kind = ode")
  testthat::expect_match(dsl, "infusion(input_1)", fixed = TRUE)
  testthat::expect_false(grepl("->", dsl, fixed = TRUE))
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

test_that("Conditionals nested in expressions are hoisted into helpers", {
  # A conditional may appear anywhere in R. The DSL can only take one as a whole
  # right-hand side, so the emitter hoists the value into a derived helper.
  expr_to_dsl <- getFromNamespace("expr_to_dsl", "Pmetrics")
  dsl_ctx <- getFromNamespace("dsl_ctx", "Pmetrics")
  dsl_take_hoists <- getFromNamespace("dsl_take_hoists", "Pmetrics")
  dsl_text <- getFromNamespace("dsl_text", "Pmetrics")

  ctx <- dsl_ctx()
  testthat::expect_equal(expr_to_dsl(quote(2 * if (c) a else b), ctx), "2.0 * _pmt1")
  testthat::expect_equal(dsl_text(dsl_take_hoists(ctx)), "_pmt1 = if (c) a else b")
  ctx <- dsl_ctx()
  testthat::expect_equal(expr_to_dsl(quote(exp(if (c) a else b)), ctx), "exp(_pmt1)")
  testthat::expect_equal(dsl_text(dsl_take_hoists(ctx)), "_pmt1 = if (c) a else b")

  # A nested conditional in the `then` branch is hoisted as well. The
  # parentheses are the ones the caller wrote around the inner conditional.
  ctx <- dsl_ctx()
  testthat::expect_equal(
    expr_to_dsl(quote(if (a) (if (b) x else y) else z), ctx),
    "if (a) (_pmt1) else z"
  )
  testthat::expect_equal(dsl_text(dsl_take_hoists(ctx)), "_pmt1 = if (b) x else y")

  # A missing `else` branch is still an error.
  testthat::expect_error(
    expr_to_dsl(quote(if (a) b), dsl_ctx()),
    "else branch"
  )
})

test_that("Conditionals summed into derivative equations are hoisted", {
  # The additive-term path for `dx` equations must hoist a conditional rather
  # than emit DSL the backend rejects.
  dsl_eqn_block <- getFromNamespace("dsl_eqn_block", "Pmetrics")
  dsl_ctx <- getFromNamespace("dsl_ctx", "Pmetrics")
  dsl_text <- getFromNamespace("dsl_text", "Pmetrics")

  summed <- dsl_eqn_block(
    function() {
      dx[1] <- a + if (c) b else d
    },
    dsl_ctx()
  )
  testthat::expect_equal(dsl_text(summed$dx), "dx(x1) = a + (_pmt1)")
  testthat::expect_equal(dsl_text(summed$derived), "_pmt1 = if (c) b else d")

  # A whole-RHS conditional remains valid and is emitted bare.
  whole_rhs <- dsl_eqn_block(
    function() {
      dx[1] <- if (c) a else b
    },
    dsl_ctx()
  )
  testthat::expect_equal(dsl_text(whole_rhs$dx), "dx(x1) = if (c) a else b")
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
