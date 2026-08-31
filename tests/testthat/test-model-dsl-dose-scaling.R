testthat::skip_on_cran()

model_to_dsl <- getFromNamespace("model_to_dsl", "Pmetrics")
model_metadata <- getFromNamespace("model_metadata", "Pmetrics")

scaled_bolus_model <- function(fa_block, eqn_block) {
    PM_model$new(
        list(
            pri = list(
                kes = ab(0, 5),
                vs = ab(0, 5),
                ff = ab(0, 1) # free fraction
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

testthat::test_that("Scaling a bolus inside a derivative is rejected and points at `fa`", {
    # A bolus is a discrete state jump in pharmsol, not a value in the
    # derivative, so `B[1] * v` cannot be expressed.
    mod <- scaled_bolus_model(
        fa_block = NULL,
        eqn_block = function() {
            dx[1] <- B[1] * v + R[1] - ke * x[1] # total drug
            dx[2] <- B[2] + x[1] / v * ff # free drug
        }
    )

    err <- testthat::expect_error(model_to_dsl(mod))
    msg <- gsub("\\s+", " ", conditionMessage(err))
    testthat::expect_match(
        msg, "standalone additive terms in derivative equations",
        fixed = TRUE
    )
    testthat::expect_match(msg, "use the `fa` block", fixed = TRUE)
})

testthat::test_that("Bolus dose scaling moves to `fa` and reaches the backend", {
    mod <- scaled_bolus_model(
        fa_block = function() {
            fa[1] <- v
        },
        eqn_block = function() {
            dx[1] <- B[1] + R[1] - ke * x[1] # total drug
            dx[2] <- B[2] + x[1] / v * ff # free drug
        }
    )
    mod$compile(quiet = TRUE)

    # Input 1 drives both a bolus and an infusion; `fa` binds to the bolus only.
    testthat::expect_match(mod$dsl, "bolus(input_1) -> x1", fixed = TRUE)
    testthat::expect_match(mod$dsl, "infusion(input_1) -> x1", fixed = TRUE)
    testthat::expect_match(mod$dsl, "bolus(input_2) -> x2", fixed = TRUE)
    # pharmsol does not put derived values in scope for route properties, so the
    # secondary equation for `v` is inlined.
    testthat::expect_match(mod$dsl, "fa(input_1) = (vs * (wt / 70.0))", fixed = TRUE)

    metadata <- model_metadata(mod$dsl, NULL)
    testthat::expect_identical(metadata$routes, c("input_1", "input_1", "input_2"))
    testthat::expect_identical(metadata$route_kinds, c("bolus", "infusion", "bolus"))
    testthat::expect_identical(metadata$covariates, "wt")
    testthat::expect_identical(metadata$outputs, c("outeq_1", "outeq_2"))
})
