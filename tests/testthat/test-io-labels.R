testthat::skip_on_cran()

pm_input_label <- getFromNamespace("pm_input_label", "Pmetrics")
pm_output_label <- getFromNamespace("pm_output_label", "Pmetrics")
label_data_csv <- getFromNamespace("label_data_csv", "Pmetrics")
pm_resolve_labels <- getFromNamespace("pm_resolve_labels", "Pmetrics")
engine_outeq_index <- getFromNamespace("engine_outeq_index", "Pmetrics")

testthat::test_that("Identifiers are used as labels verbatim", {
    # pharmsol matches labels exactly, so `1` stays `1`.
    testthat::expect_identical(pm_input_label(c(1, 2, 10)), c("1", "2", "10"))
    testthat::expect_identical(pm_output_label(1:2), c("1", "2"))
    testthat::expect_identical(pm_output_label(" 01 "), "1")

    # Named labels pass through untouched; identifiers are never renumbered.
    testthat::expect_identical(pm_input_label(c("iv", "oral")), c("iv", "oral"))
    testthat::expect_identical(pm_output_label("cp"), "cp")

    # Canonical labels are idempotent.
    testthat::expect_identical(pm_input_label(pm_input_label(1)), "1")

    # Missing values and non-numeric placeholders are preserved.
    testthat::expect_identical(pm_input_label(c(NA, ".")), c(NA, "."))
})

testthat::test_that("label_data_csv rewrites only INPUT and OUTEQ", {
    path <- withr::local_tempfile(fileext = ".csv")
    writeLines(
        c(
            "ID,EVID,TIME,DUR,DOSE,INPUT,OUT,OUTEQ,WT",
            "1,1,0,0,100,1,.,.,70",
            "1,1,1,2,100,iv,.,.,70",
            "1,0,2,.,.,.,10,2,70"
        ),
        path
    )

    label_data_csv(path)
    out <- utils::read.csv(path, check.names = FALSE, colClasses = "character")

    testthat::expect_identical(out$INPUT, c("1", "iv", "."))
    testthat::expect_identical(out$OUTEQ, c(".", ".", "2"))
    # Numeric covariates and other columns must not be relabelled.
    testthat::expect_identical(out$WT, rep("70", 3))
    testthat::expect_identical(out$ID, rep("1", 3))
})

testthat::test_that("label_data_csv maps numeric identifiers to legacy model labels", {
    path <- withr::local_tempfile(fileext = ".csv")
    writeLines(
        c(
            "ID,EVID,TIME,DUR,DOSE,INPUT,OUT,OUTEQ",
            "1,1,0,0,100,1,.,.",
            "1,0,2,.,.,.,10,1",
            "1,0,3,.,.,.,10,Plasma"
        ),
        path
    )

    label_data_csv(path, inputs = "input_1", outputs = c("outeq_1", "plasma"))
    out <- utils::read.csv(path, check.names = FALSE, colClasses = "character")

    testthat::expect_identical(out$INPUT, c("input_1", ".", "."))
    testthat::expect_identical(out$OUTEQ, c(".", "outeq_1", "plasma"))
})

testthat::test_that("Identifiers resolve exactly, then case-insensitively, then by legacy prefix", {
    testthat::expect_identical(
        pm_resolve_labels(c("1", "CP", "2", "x"), c("1", "cp", "outeq_2"), "outeq_"),
        c("1", "cp", "outeq_2", "x")
    )
    # Without declared labels, identifiers are only canonicalised.
    testthat::expect_identical(pm_resolve_labels(c("1", "cp"), NULL, "outeq_"), c("1", "cp"))
})

testthat::test_that("Backend output labels are converted to output numbers", {
    # Numeric labels are the output number.
    testthat::expect_identical(engine_outeq_index(c("1", "2", "1")), c(1, 2, 1))
    # Other labels are numbered by their position among the model's outputs.
    testthat::expect_identical(engine_outeq_index(c("cp", "effect"), c("cp", "effect")), c(1, 2))
    # Older backends wrote 0-based indices.
    testthat::expect_identical(engine_outeq_index(c(0L, 1L)), c(1, 2))
})

testthat::test_that("An input driving both a bolus and an infusion keeps one label", {
    # pharmsol route labels are unique per kind, so `1` may declare both a
    # bolus and an infusion. Nothing is renumbered and the data needs no remapping.
    mod <- PM_model$new(
        list(
            pri = list(ke = ab(0.01, 5), V = ab(1, 100)),
            eqn = function() {
                dx[1] <- b[1] + rateiv[1] - ke * x[1]
            },
            out = function() {
                y[1] <- x[1] / V
            },
            err = list(additive(1, c(0.1, 0, 0, 0)))
        ),
        compile = FALSE
    )
    mod$compile(quiet = TRUE)

    testthat::expect_match(mod$dsl, "bolus(1)", fixed = TRUE)
    testthat::expect_match(mod$dsl, "infusion(1)", fixed = TRUE)
    testthat::expect_false(grepl("->", mod$dsl, fixed = TRUE))
    testthat::expect_false(grepl("(2)", mod$dsl, fixed = TRUE))
    # The shared label must be accepted by the pharmsol backend.
    testthat::expect_no_error(model_parameters(mod$dsl))
})

testthat::test_that("Error models carry the canonical output label to the backend", {
    err <- proportional(2, c(0.1, 0.15, 0, 0), outeq = 2)
    testthat::expect_identical(err$flatten()$outeq, "2")

    named <- additive(1, c(0.2, 0, 0, 0), outeq = "cp")
    testthat::expect_identical(named$flatten()$outeq, "cp")
})

testthat::test_that("An error model for an unknown output is rejected", {
    build <- function() {
        PM_model$new(
            list(
                pri = list(ke = ab(0.01, 5), V = ab(1, 100)),
                eqn = function() {
                    dx[1] <- rateiv[1] - ke * x[1]
                },
                out = function() {
                    y[1] <- x[1] / V
                },
                err = list(additive(1, c(0.1, 0, 0, 0), outeq = 3))
            ),
            compile = FALSE
        )
    }

    testthat::expect_message(build(), "does not match any model output")
})

testthat::test_that("settings.json keys error models by output slot, not by declaration order", {
    write_settings_json <- getFromNamespace("write_settings_json", "Pmetrics")
    path <- withr::local_tempfile(fileext = ".json")

    write_settings_json(
        path = path,
        param_ranges = list(ke = c(0, 5)),
        # Declared out of order to prove the label, not the position, picks the slot.
        error_models = list(
            additive(1, c(0.2, 0, 0, 0), outeq = 2),
            proportional(2, c(0.1, 0, 0, 0), outeq = 1)
        ),
        outputs = c("1", "2"),
        algorithm = "NPAG", cycles = 1, idelta = 0.5, tad = 0,
        prior = "sobol", points = 10, seed = 22
    )

    settings <- jsonlite::read_json(path)
    models <- settings$errormodels$models
    testthat::expect_identical(models[[1]], "None")
    testthat::expect_true(!is.null(models[[2]]$Proportional))
    testthat::expect_true(!is.null(models[[3]]$Additive))
    # The output labels are recorded for the parsers.
    testthat::expect_identical(unlist(settings$outputs), c("1", "2"))
})
