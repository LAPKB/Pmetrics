#!/usr/bin/env Rscript

devtools::load_all()

path <- "inst/Examples/Runs/sylvain_sim"

dir.create(path, recursive = TRUE, showWarnings = FALSE)

data <- PM_data$new(
    "inst/Examples/src/Dapto_Lyon.csv",
    quiet = TRUE
)

model <- source(
    "inst/Examples/Rscript/sylvain.R",
    local = TRUE
)$value

theta <- matrix(
    c(
        0.35, 1.58, 9.0, 7.0,
        0.65, 1.58, 13.0, 11.0
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("CL", "Q", "V1", "V2"))
)

simulation <- model$sim(
    data = data,
    theta = theta
)

utils::write.csv(theta, file.path(path, "theta.csv"), row.names = FALSE)
utils::write.csv(simulation, file.path(path, "simulation.csv"), row.names = FALSE)

cat("Rows:", nrow(simulation), "\n")
cat("Theta:", file.path(path, "theta.csv"), "\n")
cat("Simulation:", file.path(path, "simulation.csv"), "\n")

print(utils::head(simulation))
