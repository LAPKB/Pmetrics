testthat::skip_on_cran()

# The R-to-DSL emitter should read like R: constructs R users expect are either
# translated exactly or rejected with a message that names the fix. These tests
# pin both halves of that contract, plus the numbers the translated code
# actually produces.

model_to_dsl <- getFromNamespace("model_to_dsl", "Pmetrics")
model_to_dsl_traced <- getFromNamespace("model_to_dsl_traced", "Pmetrics")
dsl_is_valid <- function(dsl) {
  is.null(tryCatch({ model_metadata(dsl, NULL); NULL }, error = function(e) e))
}

make_fn <- function(text) {
  eval(call("function", as.pairlist(alist()), parse(text = text)[[1]]))
}

# A model whose single output is the expression under test. State 1 is frozen so
# the output is a pure function of the parameters; state 2 exists so the dose
# events in the template data resolve to a declared route.
dose_model <- function(out_expr, x_init = 3.5, sec_lines = NULL, ini_lines = NULL, cov_names = NULL, compile = FALSE) {
  fn <- function(lines) make_fn(sprintf("{\n%s\n}", paste(lines, collapse = "\n")))
  PM_model$new(
    pri = list(p1 = ab(-10, 10), p2 = ab(-10, 10), p3 = ab(-10, 10)),
    cov = if (is.null(cov_names)) NULL else stats::setNames(lapply(cov_names, function(x) interp()), cov_names),
    sec = if (is.null(sec_lines)) NULL else fn(sec_lines),
    ini = fn(if (is.null(ini_lines)) c(sprintf("x[1] <- %s", format(x_init, digits = 15)), "x[2] <- 0.0") else ini_lines),
    eqn = fn(c("dx[1] <- 0.0", "dx[2] <- b[1]")),
    out = fn(sprintf("y[1] <- %s", out_expr)),
    err = list(additive(1, c(0, 0.1, 0, 0))),
    compile = compile
  )
}

# Run the expression through the compiled DSL and compare with R's own answer.
dsl_value <- function(out_expr, p1 = 1, p2 = 1, p3 = 1, x_init = 3.5, sec_lines = NULL, ini_lines = NULL, cov_names = NULL) {
  mod <- dose_model(out_expr, x_init = x_init, sec_lines = sec_lines, ini_lines = ini_lines, cov_names = cov_names)
  res <- mod$sim(data = dataEx, theta = matrix(c(p1, p2, p3), nrow = 1), quiet = TRUE)
  res$out[1]
}

r_value <- function(out_expr, p1 = 1, p2 = 1, p3 = 1, x_init = 3.5, cov_values = list(), sec = NULL) {
  x <- c(x_init)
  env <- list2env(c(list(p1 = p1, p2 = p2, p3 = p3, x = x), cov_values), parent = baseenv())
  if (!is.null(sec)) {
    # Evaluate the secondary block in R so the reference value is R's answer for
    # the whole model, not a hand-computed stand-in.
    eval(parse(text = paste(c("{", sec, "}"), collapse = "\n")), envir = env)
  }
  eval(parse(text = out_expr)[[1]], envir = env)
}

testthat::test_that("an unsupported function is reported against the R expression", {
  for (bad in c("pnorm(p1)", "qnorm(p1)", "foo(p1, p2)")) {
    message <- tryCatch(model_to_dsl(dose_model(bad)), error = function(e) conditionMessage(e))
    testthat::expect_match(message, "Unsupported function")
    testthat::expect_match(message, "Supported functions are")
    testthat::expect_false(grepl("subscript out of bounds", message, fixed = TRUE))
  }
})

testthat::test_that("values that are not finite numbers are rejected with a fix", {
  for (bad in c("Inf * p1", "NaN + p1", "NA * p1")) {
    message <- tryCatch(model_to_dsl(dose_model(bad)), error = function(e) conditionMessage(e))
    testthat::expect_match(message, "finite numbers|Missing values")
    testthat::expect_false(grepl("subscript out of bounds", message, fixed = TRUE))
  }
})

testthat::test_that("the retired effect name is reported as a migration", {
  message <- tryCatch(
    model_to_dsl(dose_model("get_e2(p1, p2, p3, 1, 1, 0.5)")),
    error = function(e) conditionMessage(e)
  )
  testthat::expect_match(message, "estimate_effect_2")
  testthat::expect_match(message, "w = alpha \\* u \\* v")
})

testthat::test_that("effect functions are available with a checked signature", {
  two <- model_to_dsl(dose_model("estimate_effect_2(p1, p2, p3, 1, 1)"))
  testthat::expect_match(two, "out(outeq_1) = estimate_effect_2(p1, p2, p3, 1.0, 1.0)", fixed = TRUE)
  testthat::expect_true(dsl_is_valid(two))

  three <- model_to_dsl(dose_model("estimate_effect_3(p1, p2, p3, 0.1, 0.1, 0.1, 0.1, 1, 1, 1)"))
  testthat::expect_match(three, "estimate_effect_3(", fixed = TRUE)
  testthat::expect_true(dsl_is_valid(three))

  message <- tryCatch(
    model_to_dsl(dose_model("estimate_effect_2(p1, p2)")),
    error = function(e) conditionMessage(e)
  )
  testthat::expect_match(message, "takes 5 arguments")
  testthat::expect_match(message, "estimate_effect_2\\(u, v, alpha, h1, h2\\)")
})

testthat::test_that("conditionals are accepted in any position", {
  # The DSL can only put a conditional in a whole right-hand side; everything
  # else is hoisted into a derived helper.
  cases <- c(
    "2.0 * if (p1 > 0) p2 else p3",
    "if (p1 > 0) (if (p2 > 0) 1 else 2) else 3",
    "if ((if (p1 > 0) 1 else 2) > 1.5) p2 else p3",
    "exp(if (p1 > 0) 1 else 2)",
    "p1 + 2.0 * if (p2 > 0) 1 else 3",
    "-(2.0 * if (p1 > 0) p2 else p3)"
  )
  for (case in cases) {
    dsl <- model_to_dsl(dose_model(case))
    testthat::expect_true(dsl_is_valid(dsl), info = case)
  }
})

testthat::test_that("conditional dose inputs are refused with the supported alternative", {
  mod <- PM_model$new(
    pri = list(p1 = ab(0, 1), p2 = ab(0, 1), p3 = ab(0, 1)),
    eqn = make_fn("{\ndx[1] <- if (p1 > 0.0) b[1] else 0.0\ndx[2] <- 0.0\n}"),
    out = make_fn("y[1] <- x[1]"),
    err = list(additive(1, c(0, 0.1, 0, 0))),
    compile = FALSE
  )
  message <- tryCatch({ mod$compile(quiet = TRUE); "no error" }, error = function(e) conditionMessage(e))
  testthat::expect_match(message, "dose input cannot be inside a conditional")
  testthat::expect_match(message, "bolus\\(input\\) \\* scale")
})

testthat::test_that("the function names R has that the DSL does not are translated", {
  translated <- list(
    "pmin(p1, p2)" = "min(p1, p2)",
    "pmax(p1, p2, p3)" = "max(max(p1, p2), p3)",
    "max(p1, p2, p3)" = "max(max(p1, p2), p3)",
    "min(p1)" = "p1",
    "log(p1, 2)" = "(ln(p1) / ln(2.0))"
  )
  for (case in names(translated)) {
    dsl <- model_to_dsl(dose_model(case))
    testthat::expect_true(grepl(translated[[case]], dsl, fixed = TRUE), info = case)
    testthat::expect_true(dsl_is_valid(dsl), info = case)
  }

  # trunc and sign need a helper, so check the pieces rather than one string.
  trunc_dsl <- model_to_dsl(dose_model("trunc(p1)"))
  testthat::expect_match(trunc_dsl, "ceil(", fixed = TRUE)
  testthat::expect_match(trunc_dsl, "floor(", fixed = TRUE)
  testthat::expect_true(dsl_is_valid(trunc_dsl))

  sign_dsl <- model_to_dsl(dose_model("sign(p1)"))
  testthat::expect_match(sign_dsl, "if (", fixed = TRUE)
  testthat::expect_true(dsl_is_valid(sign_dsl))

  # R rounds halves to even; the DSL's round does not, so it is emitted as R's
  # rule and not as the intrinsic.
  round_dsl <- model_to_dsl(dose_model("round(p1)"))
  testthat::expect_false(grepl("round(", round_dsl, fixed = TRUE))
  testthat::expect_true(dsl_is_valid(round_dsl))
})

testthat::test_that("translated expressions compute what R computes", {
  # One entry per construct. Anything the model must declare beyond the three
  # parameters (a covariate, a secondary value) is passed alongside, so a case
  # is a complete model rather than only an expression.
  case <- function(expr, p1 = 1, p2 = 1, p3 = 1, x = 3.5,
                   cov_names = NULL, cov_values = list(), sec = NULL) {
    list(expr = expr, p1 = p1, p2 = p2, p3 = p3, x = x,
         cov_names = cov_names, cov_values = cov_values, sec = sec)
  }

  cases <- list(
    # --- R functions the DSL does not have -------------------------------
    case("pmin(p1, p2)", p1 = 2, p2 = 5),
    case("pmin(p1, p2)", p1 = -3, p2 = -7),
    case("pmax(p1, p2)", p1 = -3, p2 = -7),
    case("pmax(p1, p2, p3)", p1 = 2, p2 = 5, p3 = 7.5),
    case("max(p1, p2, p3)", p1 = 2, p2 = 5, p3 = 7.5),
    case("min(p1, p2)", p1 = -2, p2 = 5),
    case("max(p1)", p1 = 7),
    case("min(p1, p2, p3, 0)", p1 = 1, p2 = 2, p3 = 3),
    case("log(p1, 2)", p1 = 8),
    case("log(p1, 10)", p1 = 1000),
    case("log(p1)", p1 = exp(3)),
    case("exp(log(p1, 2))", p1 = 8),
    case("trunc(p1 * x[1])", p1 = -1.7),
    case("trunc(p1 * x[1])", p1 = 1.7),
    case("trunc(p1)", p1 = -0.5),
    case("trunc(p1)", p1 = 0.5),
    case("sign(p1)", p1 = -2),
    case("sign(p1)", p1 = 0),
    case("sign(p1)", p1 = 2),
    case("sign(p1)", p1 = -0.0001),
    # --- round: R rounds halves to even, the DSL intrinsic does not ------
    case("round(x[1])", x = 2.5),
    case("round(x[1])", x = 3.5),
    case("round(x[1])", x = -2.5),
    case("round(x[1])", x = -3.5),
    case("round(x[1], 0)", x = 2.5),
    case("round(p1 * x[1], 2)", p1 = 1.234, x = 3),
    case("round(p1 * x[1], 1)", p1 = 0.25, x = 5),
    case("round(x[1], -1)", x = 25),
    # --- conditionals ----------------------------------------------------
    case("if (p1 > 0) 1 else 2", p1 = 1),
    case("if (p1 > 0) 1 else 2", p1 = -1),
    case("if (p1 > 0) 2 else 4", p1 = 1),
    case("2.0 * if (p1 > 0) p2 else p3", p1 = 1, p2 = 3, p3 = 9),
    case("2.0 * if (p1 > 0) p2 else p3", p1 = -1, p2 = 3, p3 = 9),
    case("2.0 * if (p1 > 0) 1 else 2", p1 = 1),
    case("2.0 * if (p1 > 0) 1 else 2", p1 = -1),
    case("p1 / if (p2 > 0) 2 else 4", p1 = 8, p2 = 1),
    case("ifelse(p1 > 0, p2, p3)", p1 = -1, p2 = 3, p3 = 9),
    case("if (p1 > 0) (if (p2 > 0) p2 else p3) else p1", p1 = 1, p2 = -2, p3 = 9),
    case("if (p1 > 0) p1 else if (p2 > 0) p2 else p3", p1 = 0, p2 = 5, p3 = 9),
    case("if (p1 > 0) p1 else if (p2 > 0) p2 else p3", p1 = 0, p2 = 0, p3 = 9),
    case("(if (p1 > 0) 1 else 2) * (if (p2 > 0) 3 else 4)", p1 = 1, p2 = 1),
    case("(if (p1 > 0) 1 else 2) * (if (p2 > 0) 3 else 4)", p1 = -1, p2 = 1),
    case("if (p1 > 0) 1 else 2 + if (p2 > 0) 3 else 4", p1 = 1, p2 = 1),
    case("if (p1 > 0) 1 else 2 + if (p2 > 0) 3 else 4", p1 = -1, p2 = 1),
    case("exp(if (p1 > 0) 1 else 2)", p1 = 1),
    case("exp(if (p1 > 0) 1 else 2)", p1 = -1),
    case("if ((if (p1 > 0) 1 else 2) > 1.5) p2 else p3", p1 = -1, p2 = 3, p3 = 9),
    case("p1 + 2.0 * if (p2 > 0) 1 else 3", p1 = 1, p2 = -1),
    case("-(2.0 * if (p1 > 0) p2 else p3)", p1 = 1, p2 = 3, p3 = 9),
    # --- conditionals on a covariate, and a helper-name clash ------------
    case("if (wt > 40) p1 else p2", p1 = 7, p2 = 9, cov_names = "wt", cov_values = list(wt = 46.7)),
    case("if (wt > 50) p1 else p2", p1 = 7, p2 = 9, cov_names = "wt", cov_values = list(wt = 46.7)),
    case("z1 + z1", sec = "z1 <- if (p1 > 0) 2 else 3", p1 = 1),
    # A helper name can only collide through a backticked assignment, which the
    # emitter must notice when it picks names for generated helpers.
    case("z1 + `_pmt1`", sec = c("`_pmt1` <- 100", "z1 <- 2 * if (p1 > 0) 1 else 2"), p1 = 1)
  )

  for (cs in cases) {
    expected <- r_value(cs$expr, cs$p1, cs$p2, cs$p3, cs$x, cs$cov_values, cs$sec)
    got <- dsl_value(cs$expr, cs$p1, cs$p2, cs$p3, cs$x,
      sec_lines = cs$sec, cov_names = cs$cov_names
    )
    testthat::expect_equal(got, expected, tolerance = 1e-9,
      info = sprintf(
        "%s with p=(%s, %s, %s), x=%s%s", cs$expr, cs$p1, cs$p2, cs$p3, cs$x,
        if (is.null(cs$sec)) "" else paste0(", sec=", paste(cs$sec, collapse = "; "))
      )
    )
  }
})

testthat::test_that("derived values keep their value through the DSL", {
  # A derived value used by an output must survive the DSL's f64 derived buffer,
  # whether it comes from a parameter, a constant or a conditional.
  testthat::expect_equal(dsl_value("z1", sec_lines = "z1 <- 1.0"), 1)
  testthat::expect_equal(dsl_value("z1", sec_lines = "z1 <- 1.5"), 1.5)
  testthat::expect_equal(dsl_value("z1", sec_lines = "z1 <- p1", p1 = 4.25), 4.25)
  testthat::expect_equal(dsl_value("z1", sec_lines = "z1 <- if (p1 > 0) 1.0 else 2.0", p1 = 1), 1)
})

testthat::test_that("a backend diagnostic names the R statement that produced it", {
  traced <- model_to_dsl_traced(dose_model("z1 * undeclared_value"))
  testthat::expect_equal(length(traced$lines), length(traced$src))
  testthat::expect_match(traced$src[[which(grepl("^out\\(", traced$lines))]], "output block: y\\[1\\]")

  mod <- dose_model("undeclared_value * x[1]")
  message <- tryCatch({ mod$compile(quiet = TRUE); "no error" }, error = function(e) conditionMessage(e))
  testthat::expect_match(message, "unknown identifier")
  testthat::expect_match(message, "output block: y\\[1\\]")
})

testthat::test_that("a model that cannot compile fails where it is defined", {
  # The constructor validates by default, so an undeclared name is reported when
  # the model is created rather than at the start of a fit.
  message <- tryCatch(
    {
      dose_model("undeclared_value * p1", compile = TRUE)
      "no error"
    },
    error = function(e) conditionMessage(e)
  )
  testthat::expect_match(message, "unknown identifier")

  # A model built with compile = FALSE is validated when it is compiled.
  mod <- dose_model("undeclared_value * p1")
  testthat::expect_null(mod$dsl)
  message <- tryCatch(
    {
      mod$compile(quiet = TRUE)
      "no error"
    },
    error = function(e) conditionMessage(e)
  )
  testthat::expect_match(message, "unknown identifier")
})

testthat::test_that("every rejection is a structured error that names the fix", {
  # A model that cannot be translated must fail as a cli condition with an
  # explanation. Raw R errors used to escape - every unknown function raised
  # `subscript out of bounds`, because the lookup that produced the helpful
  # message could never be reached.
  cases <- list(
    list("pnorm(p1)", "Unsupported function"),
    list("qnorm(p1)", "Unsupported function"),
    list("dnorm(p1)", "Unsupported function"),
    list("gamma(p1)", "Unsupported function"),
    list("lgamma(p1)", "Unsupported function"),
    list("log1p(p1)", "Unsupported function"),
    list("expm1(p1)", "Unsupported function"),
    list("c(1, 2)", "Unsupported function"),
    list("base::exp(p1)", "cannot be translated"),
    list("get_e2(p1, p2, p3, 1, 1, 0.5)", "estimate_effect_2"),
    list("get_e3(p1, p2, p3, 1, 1, 1, 1, 1, 1, 1, 1)", "estimate_effect_3"),
    list("estimate_effect_2(p1, p2)", "takes 5 arguments"),
    list("estimate_effect_2(p1, p2, p3, 1, 1, 1)", "takes 5 arguments"),
    list("Inf * p1", "finite numbers"),
    list("NaN + p1", "finite numbers"),
    list("NA * p1", "Missing values"),
    list("if (p1 > 0) 1", "else branch"),
    list("\"text\"", "token"),
    list("p1[1]", "cannot be used as a value"),
    list("y[1] * 2", "cannot be used as a value"),
    list("dx[1] * 2", "cannot be used as a value"),
    list("x[i]", "Dynamic")
  )

  for (cs in cases) {
    err <- tryCatch(
      {
        model_to_dsl(dose_model(cs[[1]]))
        NULL
      },
      error = function(e) e
    )
    testthat::expect_false(is.null(err), info = cs[[1]])
    testthat::expect_true(inherits(err, "rlang_error"), info = cs[[1]])
    testthat::expect_match(conditionMessage(err), cs[[2]], info = cs[[1]])
  }

  # Control flow is refused, and the message says what to write instead.
  err <- tryCatch(
    {
      model_to_dsl(dose_model("z1", sec_lines = "for (i in 1:3) z1 <- i"))
      NULL
    },
    error = function(e) e
  )
  testthat::expect_true(inherits(err, "rlang_error"))
  testthat::expect_match(conditionMessage(err), "Only assignments are supported")
})

testthat::test_that("a braced conditional block chooses between scalar values", {
  dsl <- model_to_dsl(
    dose_model(
      "x[1] + z2",
      sec_lines = c(
        "if (p1 > 0) {",
        "  z1 <- 1",
        "  z2 <- 2",
        "} else {",
        "  z1 <- 3",
        "  z2 <- 4",
        "}"
      )
    )
  )
  testthat::expect_match(dsl, "if (p1 > 0.0) {", fixed = TRUE)
  testthat::expect_match(dsl, "} else {", fixed = TRUE)
  testthat::expect_match(dsl, "z1 = 1.0", fixed = TRUE)
  testthat::expect_match(dsl, "z2 = 4.0", fixed = TRUE)
  testthat::expect_true(dsl_is_valid(dsl))

  # The chosen value reaches the arithmetic: p1 > 0 selects the first branch.
  testthat::expect_equal(
    dsl_value(
      "x[1] + z2",
      p1 = 1,
      sec_lines = c("if (p1 > 0) {", "  z1 <- 1", "  z2 <- 2", "} else {", "  z1 <- 3", "  z2 <- 4", "}")
    ),
    3.5 + 2
  )
  testthat::expect_equal(
    dsl_value(
      "x[1] + z2",
      p1 = -1,
      sec_lines = c("if (p1 > 0) {", "  z1 <- 1", "  z2 <- 2", "} else {", "  z1 <- 3", "  z2 <- 4", "}")
    ),
    3.5 + 4
  )
})

testthat::test_that("a conditional block without an else keeps the earlier value", {
  dsl <- model_to_dsl(
    dose_model("z1", sec_lines = c("z1 <- 0", "if (p1 > 0) {", "  z1 <- 1", "}"))
  )
  testthat::expect_match(dsl, "z1 = 0.0", fixed = TRUE)
  testthat::expect_match(dsl, "if (p1 > 0.0) {", fixed = TRUE)
  testthat::expect_true(dsl_is_valid(dsl))

  testthat::expect_equal(
    dsl_value("z1", p1 = 1, sec_lines = c("z1 <- 0", "if (p1 > 0) {", "  z1 <- 1", "}")), 1
  )
  testthat::expect_equal(
    dsl_value("z1", p1 = -1, sec_lines = c("z1 <- 0", "if (p1 > 0) {", "  z1 <- 1", "}")), 0
  )
})

testthat::test_that("a conditional block around an equation says what to write instead", {
  # The DSL accepts only plain assignments inside an `if` statement, so a
  # derivative, output or initial condition inside one is refused with the
  # conditional-equation form.
  for (case in list(
    list(c("if (p1 > 0) {", "  dx[1] <- -p1 * x[1]", "} else {", "  dx[1] <- -p2 * x[1]", "}"), "equation block", "dx\\[1\\] <- if \\(cond\\) a else b"),
    list(c("if (p1 > 0) {", "  y[1] <- 1", "} else {", "  y[1] <- 2", "}"), "output block", "conditional"),
    list(c("if (p1 > 0) {", "  x[1] <- 1", "} else {", "  x[1] <- 2", "}"), "initial-conditions block", "conditional")
  )) {
    lines <- case[[1]]
    where <- case[[2]]
    mod <- if (where == "equation block") {
      PM_model$new(
        pri = list(p1 = ab(0, 1), p2 = ab(0, 1), p3 = ab(0, 1)),
        eqn = make_fn(sprintf("{\n%s\ndx[2] <- 0.0\n}", paste(lines, collapse = "\n"))),
        out = make_fn("y[1] <- x[1]"),
        err = list(additive(1, c(0, 0.1, 0, 0))), compile = FALSE
      )
    } else if (where == "output block") {
      PM_model$new(
        pri = list(p1 = ab(0, 1), p2 = ab(0, 1), p3 = ab(0, 1)),
        eqn = make_fn("{\ndx[1] <- 0.0\ndx[2] <- 0.0\n}"),
        out = make_fn(sprintf("{\n%s\n}", paste(lines, collapse = "\n"))),
        err = list(additive(1, c(0, 0.1, 0, 0))), compile = FALSE
      )
    } else {
      PM_model$new(
        pri = list(p1 = ab(0, 1), p2 = ab(0, 1), p3 = ab(0, 1)),
        ini = make_fn(sprintf("{\n%s\n}", paste(lines, collapse = "\n"))),
        eqn = make_fn("{\ndx[1] <- 0.0\ndx[2] <- 0.0\n}"),
        out = make_fn("y[1] <- x[1]"),
        err = list(additive(1, c(0, 0.1, 0, 0))), compile = FALSE
      )
    }
    err <- tryCatch(
      {
        model_to_dsl(mod)
        NULL
      },
      error = function(e) e
    )
    testthat::expect_true(inherits(err, "rlang_error"), info = where)
    testthat::expect_match(conditionMessage(err), where, info = where)
    testthat::expect_match(conditionMessage(err), case[[3]], info = where)
  }
})

testthat::test_that("a diagnostic names the file and line when R kept them", {
  # The model's blocks are defined here, so R keeps their source references when
  # the file was sourced with keep.source = TRUE. A plain `Rscript` run does not,
  # and the diagnostic is still complete without them.
  mod <- PM_model$new(
    pri = list(p1 = ab(0, 1), p2 = ab(0, 1), p3 = ab(0, 1)),
    ini = function() {
      x[1] <- 3.5
      x[2] <- 0.0
    },
    eqn = function() {
      dx[1] <- 0.0
      dx[2] <- b[1]
    },
    out = function() {
      y[1] <- undeclared_name * p1
    },
    err = list(additive(1, c(0, 0.1, 0, 0))), compile = FALSE
  )

  if (is.null(attr(body(mod$arg_list$out), "srcref"))) {
    testthat::skip("source references are not kept in this environment")
  }

  err <- tryCatch(
    {
      mod$compile(quiet = TRUE)
      NULL
    },
    error = function(e) e
  )
  testthat::expect_true(inherits(err, "rlang_error"))
  testthat::expect_match(conditionMessage(err), "unknown identifier")
  testthat::expect_match(conditionMessage(err), "test-model-dsl-language\\.R:[0-9]+")
})
