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
