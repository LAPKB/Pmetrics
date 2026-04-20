#!/usr/bin/env Rscript

pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

path <- "inst/Examples/Runs/bimodal_ke_analytical"

dir.create(path, recursive = TRUE, showWarnings = FALSE)

data <- PM_data$new(
    "inst/Examples/src/bimodal_ke.csv",
    quiet = TRUE
)

model <- PM_model$new(
    pri = list(
        ke = ab(0.001, 3.0),
        v = ab(25.0, 250.0)
    ),
    eqn = function() {
        one_comp_iv
    },
    out = function() {
        y[1] <- x[1] / v
    },
    err = list(
        additive(1, c(0.0, 0.5, 0.0, 0.0))
    )
)

fit <- model$fit(
    data = data,
    path = path,
    run = 1,
    algorithm = "NPAG",
    cycles = 1000L,
    prior = "sobol",
    points = 2028,
    seed = 22,
    idelta = 0.01,
    report = "none",
    overwrite = TRUE
)

cat("Status:", fit$cycle$data$status, "\n")
cat("Outputs:", file.path(path, "1", "outputs"), "\n")

ode_run <- "inst/Examples/Runs/bimodal_ke/1"
analytical_run <- file.path(path, "1")

if (dir.exists(file.path(ode_run, "outputs"))) {
    ode_fit <- PM_load(ode_run)
    analytical_fit <- PM_load(analytical_run)

    comparison <- tibble::tibble(
        parameter = colnames(analytical_fit$final$popMean),
        ode = as.numeric(ode_fit$final$popMean[1, ]),
        analytical = as.numeric(analytical_fit$final$popMean[1, ])
    ) |>
        dplyr::mutate(
            abs_diff = abs(analytical - ode),
            rel_diff_pct = 100 * abs_diff / pmax(abs(ode), .Machine$double.eps)
        )

    cat("\nPopulation mean comparison:\n")
    print(comparison, row.names = FALSE)
    cat(
        sprintf(
            "\nMax relative difference in population means: %.2f%%\n",
            max(comparison$rel_diff_pct)
        )
    )
} else {
    cat(
        "\nComparison skipped: run inst/Examples/Rscript/bimodal_ke.R first to compare",
        "the analytical and ODE examples.\n"
    )
}