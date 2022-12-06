#' @export
modelLibrary <- list(
    mod_1C_1 = list(
        pri = list(
            Ke = ab(0, 5),
            V = ab(0.01, 100)
        ),
        out = list(
            y1 = list(
                val = "X(1)/V",
                err = list(
                    model = additive(0.1),
                    assay = c(0.1, 0.1, 0, 0)
                )
            )
        )
    ),
    mod_2C_1_A = list(
        pri = list(
            Ke = ab(0, 5),
            V = ab(0.01, 100),
            Ka = ab(0, 5)
        ),
        out = list(
            y1 = list(
                val = "X(2)/V",
                err = list(
                    model = additive(0.1),
                    assay = c(0.1, 0.1, 0, 0)
                )
            )
        )
    ),
    mod_2C_1_D = list(
        pri = list(
            Ke = ab(0, 5),
            V = ab(0.01, 100),
            Ka = ab(0, 5)
        ),
        dif = list(
            xp1 = "-ka*X(1)",
            xp2 = "ka*X(1) - Ke*X(2)"
        ),
        out = list(
            y1 = list(
                val = "X(2)/V",
                err = list(
                    model = additive(0.1),
                    assay = c(0.1, 0.1, 0, 0)
                )
            )
        )
    )
)
