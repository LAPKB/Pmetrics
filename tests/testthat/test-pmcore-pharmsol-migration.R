testthat::skip_on_cran()

testthat::skip_if_not(
  is_cargo_installed(),
  message = "Cargo is required to compile migration test models."
)

CL <- NULL
V <- NULL

build_passthrough_ode_model <- function(solver = NULL) {
  PM_model$new(list(
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
  ))
}

build_shared_input_ode_model <- function() {
  PM_model$new(list(
    pri = list(
      ke = ab(0.1, 1.5),
      v = ab(5, 15)
    ),
    eqn = function() {
      dx[1] <- -(ke) * x[1] + rateiv[1] + b[1]
    },
    out = function() {
      y[1] <- x[1] / v
    },
    err = list(additive(1, c(0.1, 0, 0, 0)))
  ))
}

test_that("ODE DSL generation preserves 1-based indices", {
  mod <- build_passthrough_ode_model("TSIT45")
  dsl <- mod$dsl()

  testthat::expect_match(dsl, "kind = ode")
  testthat::expect_match(dsl, "dx\\(state_1\\)")
  testthat::expect_match(dsl, "infusion\\(input_1\\) -> state_1")
  testthat::expect_match(dsl, "out\\(outeq_1\\) = state_1 / v ~ continuous\\(\\)")
  testthat::expect_false(grepl("state_0", dsl, fixed = TRUE))
  testthat::expect_no_error(mod$compile(quiet = TRUE))
})

test_that("PM_model$dsl prints readable DSL and removes explicit route input terms", {
  mod <- build_shared_input_ode_model()

  dsl <- mod$dsl()
  printed <- capture.output(print(dsl))

  testthat::expect_match(dsl, "bolus\\(input_1_bolus\\) -> state_1")
  testthat::expect_match(dsl, "infusion\\(input_1_infusion\\) -> state_1")
  testthat::expect_match(dsl, "dx\\(state_1\\) = -ke \\* state_1")
  testthat::expect_match(dsl, "out\\(outeq_1\\) = state_1 / v ~ continuous\\(\\)")
  testthat::expect_false(grepl("+ 0", dsl, fixed = TRUE))
  testthat::expect_false(any(grepl("[1]", printed, fixed = TRUE)))
  testthat::expect_identical(printed[[1]], "name = user")
  testthat::expect_identical(printed[[2]], "kind = ode")
})

test_that("Analytical migration compiles and reports analytical parameters", {
  mod <- build_library_model("one_comp_iv", mode = "analytical")
  dsl <- mod$dsl()

  testthat::expect_match(dsl, "kind = analytical")
  testthat::expect_match(dsl, "structure = one_compartment")
  testthat::expect_match(dsl, "outputs = outeq_1")
  testthat::expect_match(dsl, "out\\(outeq_1\\)")

  testthat::expect_no_error(mod$compile(quiet = TRUE))
  testthat::expect_equal(
    model_parameters(mod$dsl(), "analytical"),
    tolower(names(mod$model_list$pri))
  )
})

test_that("PM_model$debug_dsl reports DSL text and runtime validation state", {
  mod <- build_passthrough_ode_model("TSIT45")

  debug <- mod$debug_dsl(quiet = TRUE)

  testthat::expect_true(debug$ok)
  testthat::expect_identical(debug$stage, "validated")
  testthat::expect_equal(trimws(debug$dsl), trimws(as.character(mod$dsl())))
  testthat::expect_true(grepl("^\\s*1 \\| ", debug$numbered_dsl))
  testthat::expect_null(debug$diagnostic)
})

test_that("PM_model$debug_dsl captures runtime validation diagnostics", {
  mod <- build_passthrough_ode_model("TSIT45")
  mod$arg_list$solver <- "bad"
  mod$model_list$solver <- "bad"

  debug <- mod$debug_dsl(quiet = TRUE)

  testthat::expect_false(debug$ok)
  testthat::expect_identical(debug$stage, "settings")
  testthat::expect_match(debug$diagnostic, "Unsupported ODE solver: BAD")
  testthat::expect_true(grepl("kind = ode", debug$dsl, fixed = TRUE))
})

test_that("PM_build is a no-op under the DSL runtime", {
  testthat::expect_null(PM_build())
})
