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
dose_model <- function(out_expr, x_init = 3.5, sec_lines = NULL, ini_lines = NULL, compile = FALSE) {
  fn <- function(lines) make_fn(sprintf("{\n%s\n}", paste(lines, collapse = "\n")))
  PM_model$new(
    pri = list(p1 = ab(-10, 10), p2 = ab(-10, 10), p3 = ab(-10, 10)),
    sec = if (is.null(sec_lines)) NULL else fn(sec_lines),
    ini = fn(if (is.null(ini_lines)) c(sprintf("x[1] <- %s", format(x_init, digits = 15)), "x[2] <- 0.0") else ini_lines),
    eqn = fn(c("dx[1] <- 0.0", "dx[2] <- b[1]")),
    out = fn(sprintf("y[1] <- %s", out_expr)),
    err = list(additive(1, c(0, 0.1, 0, 0))),
    compile = compile
  )
}

# Run the expression through the compiled DSL and compare with R's own answer.
dsl_value <- function(out_expr, p1 = 1, p2 = 1, p3 = 1, x_init = 3.5, sec_lines = NULL, ini_lines = NULL) {
  mod <- dose_model(out_expr, x_init = x_init, sec_lines = sec_lines, ini_lines = ini_lines)
  res <- mod$sim(data = dataEx, theta = matrix(c(p1, p2, p3), nrow = 1), quiet = TRUE)
  res$out[1]
}

r_value <- function(out_expr, p1 = 1, p2 = 1, p3 = 1, x_init = 3.5) {
  x <- c(x_init)
  eval(parse(text = out_expr)[[1]], envir = list(p1 = p1, p2 = p2, p3 = p3, x = x))
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
  cases <- list(
    list("pmin(p1, p2)", 2, 5, 1, 3.5),
    list("pmax(p1, p2, p3)", 2, 5, 7.5, 3.5),
    list("max(p1, p2, p3)", 2, 5, 7.5, 3.5),
    list("min(p1, p2)", -2, 5, 1, 3.5),
    list("log(p1, 2)", 8, 1, 1, 3.5),
    list("trunc(p1 * x[1])", -1.7, 1, 1, 3.5),
    list("trunc(p1 * x[1])", 1.7, 1, 1, 3.5),
    list("sign(p1)", -2, 1, 1, 3.5),
    list("sign(p1)", 0, 1, 1, 3.5),
    list("sign(p1)", 2, 1, 1, 3.5),
    list("round(x[1])", 1, 1, 1, 2.5),
    list("round(x[1])", 1, 1, 1, 3.5),
    list("round(x[1])", 1, 1, 1, -2.5),
    list("round(p1 * x[1], 2)", 1.234, 1, 1, 3),
    list("2.0 * if (p1 > 0) p2 else p3", 1, 3, 9, 1),
    list("2.0 * if (p1 > 0) p2 else p3", -1, 3, 9, 1),
    list("ifelse(p1 > 0, p2, p3)", -1, 3, 9, 1),
    list("if (p1 > 0) (if (p2 > 0) p2 else p3) else p1", 1, -2, 9, 1),
    list("exp(if (p1 > 0) 1 else 2)", 1, 0, 0, 1),
    list("if ((if (p1 > 0) 1 else 2) > 1.5) p2 else p3", -1, 3, 9, 1),
    list("p1 + 2.0 * if (p2 > 0) 1 else 3", 1, -1, 0, 1),
    list("-(2.0 * if (p1 > 0) p2 else p3)", 1, 3, 9, 1),
    list("if (p1 > 0) 1 else 2", 1, 1, 1, 1),
    list("if (p1 > 0) 1 else 2", -1, 1, 1, 1)
  )
  for (case in cases) {
    expected <- r_value(case[[1]], case[[2]], case[[3]], case[[4]], case[[5]])
    got <- dsl_value(case[[1]], case[[2]], case[[3]], case[[4]], case[[5]])
    testthat::expect_equal(got, expected, tolerance = 1e-9,
      info = sprintf("%s with p=(%s, %s, %s), x=%s", case[[1]], case[[2]], case[[3]], case[[4]], case[[5]])
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
