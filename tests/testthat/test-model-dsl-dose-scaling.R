testthat::skip_on_cran()

model_to_dsl <- getFromNamespace("model_to_dsl", "Pmetrics")

scaled_bolus_model <- function(eqn_block, fa_block = NULL) {
  PM_model$new(
    list(
      pri = list(
        kes = ab(0, 5),
        vs = ab(0, 5),
        ff = ab(0, 1)
      ),
      cov = list(wt = interp()),
      sec = function() {
        v <- vs * (wt / 70)
        ke <- kes * (wt / 70)^-0.25
      },
      fa = fa_block,
      eqn = eqn_block,
      out = function() {
        y[1] <- x[1] / v
        y[2] <- x[2]
      },
      err = list(
        additive(1, c(1, 0, 0, 0), outeq = 1),
        additive(1, c(1, 0, 0, 0), outeq = 2)
      )
    ),
    compile = FALSE
  )
}

testthat::test_that("ODE dose inputs and derived scales stay on the RHS", {
  mod <- scaled_bolus_model(function() {
    dx[1] <- B[1] * v + R[1] - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })

  dsl <- model_to_dsl(mod)
  testthat::expect_match(dsl, "bolus(input_1) * (v)", fixed = TRUE)
  testthat::expect_match(dsl, "infusion(input_1)", fixed = TRUE)
  testthat::expect_false(grepl("->", dsl, fixed = TRUE))
  testthat::expect_false(grepl("fa(input_1)", dsl, fixed = TRUE))
  testthat::expect_match(dsl, "v = vs * (wt / 70.0)", fixed = TRUE)
})

testthat::test_that("The bolus may be on either side of the scale", {
  mod <- scaled_bolus_model(function() {
    dx[1] <- v * B[1] + R[1] - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })

  testthat::expect_match(model_to_dsl(mod), "bolus(input_1) * (v)", fixed = TRUE)
})

testthat::test_that("Equation scaling and an explicit fa block are multiplied", {
  mod <- scaled_bolus_model(
    eqn_block = function() {
      dx[1] <- B[1] * v + R[1] - ke * x[1]
      dx[2] <- B[2] + x[1] / v * ff
    },
    fa_block = function() {
      fa[1] <- ff
    }
  )

  dsl <- model_to_dsl(mod)
  testthat::expect_match(dsl, "bolus(input_1) * (ff) * (v)", fixed = TRUE)
  testthat::expect_false(grepl("fa(input_1)", dsl, fixed = TRUE))
  matches <- gregexpr("bolus(input_1)", dsl, fixed = TRUE)[[1]]
  testthat::expect_equal(sum(matches > 0), 1)
})

testthat::test_that("A linear infusion scale stays explicit in the derivative", {
  right <- scaled_bolus_model(function() {
    dx[1] <- R[1] * v - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })
  left <- scaled_bolus_model(function() {
    dx[1] <- v * R[1] - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })

  for (mod in list(right, left)) {
    dsl <- model_to_dsl(mod)
    testthat::expect_false(grepl("->", dsl, fixed = TRUE))
    testthat::expect_match(dsl, "infusion(input_1) * (v)", fixed = TRUE)
    testthat::expect_false(grepl("fa(input_1)", dsl, fixed = TRUE))
  }
})

testthat::test_that("Complex route expressions stay unsupported", {
  nested_infusion <- scaled_bolus_model(function() {
    dx[1] <- R[1] * v * ff - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })
  testthat::expect_error(model_to_dsl(nested_infusion), "Infusion inputs may only")

  nested_bolus <- scaled_bolus_model(function() {
    dx[1] <- B[1] * v * ff + R[1] - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })
  testthat::expect_error(model_to_dsl(nested_bolus), "Bolus inputs may only")

  state_dependent <- scaled_bolus_model(function() {
    dx[1] <- B[1] * x[1] + R[1] - ke * x[1]
    dx[2] <- B[2] + x[1] / v * ff
  })
  testthat::expect_error(model_to_dsl(state_dependent), "cannot depend directly on state")
})

testthat::test_that("Analytical bolus models emit lag and fa", {
  mod <- PM_model$new(
    pri = list(Ka = ab(0.5, 6), Ke = ab(0.1, 1.5), V = ab(25, 120)),
    eqn = function() {
      one_comp_bolus
    },
    lag = function() {
      lag[1] <- 2
    },
    fa = function() {
      fa[1] <- 0.5
    },
    out = function() {
      y[1] <- x[2] / V
    },
    err = list(proportional(5, c(0.1, 0.15, 0, 0))),
    compile = FALSE
  )

  dsl <- model_to_dsl(mod)
  testthat::expect_match(dsl, "bolus(input_1) -> x1", fixed = TRUE)
  testthat::expect_match(dsl, "lag(input_1) = 2.0", fixed = TRUE)
  testthat::expect_match(dsl, "fa(input_1) = 0.5", fixed = TRUE)
  testthat::expect_no_error(model_parameters(dsl))
})

testthat::test_that("Analytical infusion models reject lag and fa", {
  mod <- PM_model$new(
    pri = list(Ke = ab(0.1, 1.5), V = ab(25, 120)),
    eqn = function() {
      one_comp_iv
    },
    fa = function() {
      fa[1] <- 0.5
    },
    out = function() {
      y[1] <- x[1] / V
    },
    err = list(proportional(5, c(0.1, 0.15, 0, 0))),
    compile = FALSE
  )

  testthat::expect_error(model_to_dsl(mod), "analytical bolus models")
})
