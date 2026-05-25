PM_model$new(
    pri = list(
        CL = ab(0.05, 2),
        Q = ab(0, 2),
        V1 = ab(2, 25),
        V2 = ab(0.5, 20)
    ),
    cov = list(
        AGE = interp("none"),
        CREAT = interp("none"),
        WEIGHT = interp(),
        MALE = interp("none"),
        CLCR = interp("none"),
        OCC = interp("none")
    ),
    sec = function() {
        Q <- 1.58
    },
    eqn = function() {
        two_comp_iv_cl
    },
    out = function() {
        Y[1] <- X[1] / V1
    },
    err = list(
        proportional(5, c(0.1, 0.1, 0.0, 0.0))
    )
)
