#!/usr/bin/env Rscript

pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

path <- "inst/Examples/Runs/bimodal_ke"

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
        dx[1] <- -ke * x[1] + rateiv[1] + b[1]
    },
    out = function() {
        y[1] <- x[1] / v
    },
    err = list(
        additive(1, c(0.0, 0.5, 0.0, 0.0))
    ),
    solver = "TSIT45"
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
    overwrite = TRUE
)

cat("Status:", fit$cycle$data$status, "\n")
cat("Outputs:", file.path(path, "1", "outputs"), "\n")
