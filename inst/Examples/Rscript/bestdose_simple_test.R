devtools::load_all()

mod_onecomp <- PM_model$new(
    pri = list(
        ke = ab(0.001, 3.0),
        v = ab(25.0, 250.0)
    ),
    eqn = function() {
        dx[1] <- -ke * X[1] + B[1]
    },
    out = function() {
        Y[1] <- X[1] / v
    },
    err = list(
        additive(0, c(0, 0.20, 0, 0))
    )
)

past_file <- "inst/Examples/src/bestdose_past.csv"
target_file <- "inst/Examples/src/bestdose_target.csv"
prior_file <- "inst/Examples/src/bestdose_prior.csv"

cat("Using PMcore prior file\n")

posterior <- bd_post$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500,
    quiet = TRUE
)

cat("\nPosterior theta (first 5 rows):\n")
print(head(posterior$theta, 5))
cat("\nPosterior weights (first 10):\n")
print(head(posterior$posterior_weights, 10))

prior_weights <- seq(0, 1, by = 0.1)
for (lambda in prior_weights) {
    r <- posterior$optimize(
        target = target_file,
        dose_range = list(min = 0, max = 300),
        prior_weight = lambda # ought to be called "prior_weight"
    )
    cat(sprintf(
        "Prior weight: %.2f\t\tOptimal dose: [%.4f, %.4f]\t\tCost: %.6f\t\tln Cost: %.4f\t\tMethod: %s\n",
        lambda,
        r$doses[1], r$doses[2],
        r$objf,
        log(r$objf),
        r$method
    ))
}

# Last result predictions
r <- posterior$optimize(
    target = target_file,
    dose_range = list(min = 0, max = 300),
    prior_weight = 0.0
)
cat("\nConcentration-time predictions for prior_weight=0.0:\n")
preds <- r$result$predictions
for (j in seq_len(nrow(preds))) {
    p <- preds[j, ]
    cat(sprintf(
        "Time: %.2f h, Observed: %.2f, (Pop Mean: %.4f, Pop Median: %.4f, Post Mean: %.4f, Post Median: %.4f)\n",
        p$time, p$obs, p$pop_mean, p$pop_median, p$post_mean, p$post_median
    ))
}


# ===== One-shot API =====
bd1 <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 500,
    future = PM_data$new(target_file, quiet = TRUE),
    dose_range = list(min = 0, max = 5000),
    prior_weight = 0,
    start = 0
)

bd1

bd1$plot()

bd2 <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    max_cycles = 500,
    future = target_file,
    dose_range = list(min = 0, max = 300),
    prior_weight = 0.0
)

bd2



bd2$plot()

plot(bd1)


## adding ability to specify future as argument to bd$new() instead of target file.
## This will allow for more flexible future specifications and avoid the need for a separate target file.

future_list <- list(dose = c(120,0), 
    frequency = 12, 
    route = 0, 
    number = 3, 
    target_time = 11.5, 
    target = 0.3,
    target_type = "concentration")

future_list <- list(dose = 320, 
    frequency = 12, 
    route = 0, 
    number = 3, 
    target_time = 0.6, 
    target = 0.3,
    target_type = "time")

future_list <- list(dose = 320, 
    frequency = 12, 
    route = 0, 
    number = 3, 
    target_time = 11.5, 
    target = 10,
    target_type = "auc")


bd_new <- bd$new(
    prior = prior_file,
    model = mod_onecomp,
    past_data = PM_data$new(past_file, quiet = TRUE),
    max_cycles = 50,
    future = future_list,
    dose_range = list(min = 0, max = 5000),
    prior_weight = 0,
    start = "02/02/26 00:00",
)

# bd_new$plot()
bd_new$report()
