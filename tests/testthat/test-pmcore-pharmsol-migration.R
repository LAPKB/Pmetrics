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

testthat::test_that("ODE generation uses ode! and preserves 1-based indices", {
    mod <- build_passthrough_ode_model("TSIT45")
    rust_file <- tempfile(fileext = ".rs")
    on.exit(unlink(rust_file), add = TRUE)

    mod$.__enclos_env__$private$write_model_to_rust(rust_file)
    rust <- paste(readLines(rust_file), collapse = "\n")

    testthat::expect_match(rust, "fn build_eqn\\(\\) -> impl Equation")
    testthat::expect_match(rust, "ode! \\{")
    testthat::expect_match(rust, "dx\\[1\\]")
    testthat::expect_match(rust, "rateiv\\[1\\]")
    testthat::expect_match(rust, "y\\[1\\]")
    testthat::expect_match(
        rust,
        "\\.with_solver\\(OdeSolver::ExplicitRk\\(ExplicitRkTableau::Tsit45\\)\\)"
    )
    testthat::expect_false(grepl("dx[0]", rust, fixed = TRUE))
})

testthat::test_that("Analytical migration compiles and reports analytical parameters", {
    mod <- build_library_model("one_comp_iv", mode = "analytical")
    rust_file <- tempfile(fileext = ".rs")
    on.exit(unlink(rust_file), add = TRUE)

    mod$.__enclos_env__$private$write_model_to_rust(rust_file)
    rust <- paste(readLines(rust_file), collapse = "\n")

    testthat::expect_match(rust, "equation::Analytical::new")
    testthat::expect_match(rust, "one_compartment")
    testthat::expect_match(rust, "\\.with_nstates\\(")
    testthat::expect_match(rust, "\\.with_ndrugs\\(")
    testthat::expect_match(rust, "\\.with_nout\\(")

    testthat::expect_no_error(mod$compile(quiet = TRUE))
    testthat::expect_true(file.exists(mod$binary_path))
    testthat::expect_equal(
        model_parameters(mod$binary_path, "analytical"),
        tolower(names(mod$model_list$pri))
    )
})
