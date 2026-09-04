# The objects in data/ are serialised R6 instances, so they freeze both the
# member set and the method bodies present when data-raw/data-raw.R last ran.
# Stale copies silently exercise old code from every test that touches them.

pm_namespace <- asNamespace("Pmetrics")

r6_generator_for <- function(obj) {
    for (cl in class(obj)) {
        gen <- get0(cl, envir = pm_namespace, inherits = FALSE)
        if (inherits(gen, "R6ClassGenerator")) {
            return(gen)
        }
    }
    NULL
}

r6_expected_members <- function(gen) {
    sort(unique(c(
        ".__enclos_env__",
        names(gen$public_fields),
        names(gen$public_methods),
        names(gen$active)
    )))
}

# Every R6 instance reachable from the shipped datasets, keyed by access path.
collect_r6 <- function(obj, label, depth = 3L, acc = list()) {
    acc[[label]] <- obj
    if (depth <= 0L) {
        return(acc)
    }
    gen <- r6_generator_for(obj)
    fields <- if (is.null(gen)) names(obj) else names(gen$public_fields)
    for (nm in fields) {
        value <- tryCatch(obj[[nm]], error = function(e) NULL)
        if (inherits(value, "R6")) {
            acc <- collect_r6(value, paste0(label, "$", nm), depth - 1L, acc)
        }
    }
    acc
}

shipped_r6_objects <- function() {
    items <- utils::data(package = "Pmetrics")$results[, "Item"]
    acc <- list()
    for (item in items) {
        env <- new.env(parent = emptyenv())
        utils::data(list = item, package = "Pmetrics", envir = env)
        obj <- env[[item]]
        if (inherits(obj, "R6")) {
            acc <- collect_r6(obj, item, acc = acc)
        }
    }
    acc
}

regen_hint <- paste(
    "The example objects in data/ are out of date with the current R6 classes.",
    "Regenerate them from the repository root with:",
    "  R CMD INSTALL . && Rscript data-raw/data-raw.R",
    "then commit the updated data/*.rda. Note this runs a full NPAG fit.",
    sep = "\n  "
)

report_problems <- function(problems) {
    if (length(problems)) {
        fail(paste(c(regen_hint, problems), collapse = "\n  "))
    } else {
        succeed()
    }
}

test_that("shipped example objects expose the current R6 members", {
    objects <- shipped_r6_objects()
    expect_gt(length(objects), 0)

    problems <- character()
    for (label in names(objects)) {
        gen <- r6_generator_for(objects[[label]])
        if (is.null(gen)) next

        expected <- r6_expected_members(gen)
        actual <- sort(names(objects[[label]]))
        missing <- setdiff(expected, actual)
        extra <- setdiff(actual, expected)

        if (length(missing) || length(extra)) {
            problems <- c(problems, sprintf(
                "%s (%s): missing [%s], unexpected [%s]",
                label, class(objects[[label]])[1],
                paste(missing, collapse = ", "), paste(extra, collapse = ", ")
            ))
        }
    }

    report_problems(problems)
})

test_that("shipped example objects embed the current R6 method bodies", {
    objects <- shipped_r6_objects()

    problems <- character()
    for (label in names(objects)) {
        gen <- r6_generator_for(objects[[label]])
        if (is.null(gen)) next

        stale <- Filter(
            function(method) {
                stored <- tryCatch(objects[[label]][[method]], error = function(e) NULL)
                current <- gen$public_methods[[method]]
                is.function(stored) && is.function(current) &&
                    !identical(deparse(body(stored)), deparse(body(current)))
            },
            setdiff(names(gen$public_methods), "clone")
        )

        if (length(stale)) {
            problems <- c(problems, sprintf("%s: %s", label, paste(stale, collapse = ", ")))
        }
    }

    report_problems(problems)
})
