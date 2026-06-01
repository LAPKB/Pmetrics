build_example_fit_params <- function(model) {
    list(
        ranges = list(ke = c(0.1, 1.0), v = c(1.0, 10.0)),
        error_models = lapply(model$model_list$err, function(x) x$flatten()),
        prior = "sobol",
        points = 20
    )
}

build_example_fit_payload_contents <- function(include_cens = TRUE, missing_prediction_fields = character()) {
    prediction <- list(
        id = "1",
        time = 0,
        outeq = 0,
        block = 0,
        obs = 10,
        pop_mean = 11,
        pop_median = 12,
        post_mean = 13,
        post_median = 14
    )

    if (include_cens) {
        prediction$cens <- "none"
    }
    if (length(missing_prediction_fields)) {
        prediction[missing_prediction_fields] <- NULL
    }

    list(
        parameter_names = c("ke", "v"),
        covariate_names = c("cov1"),
        iterations = list(list(
            cycle = 1,
            converged = FALSE,
            status = "payload",
            neg2ll = 123,
            nspp = 2,
            `gamlam.0` = 0.5,
            `ke.mean` = 2,
            `ke.median` = 2,
            `ke.sd` = 0.25,
            `v.mean` = 4,
            `v.median` = 4,
            `v.sd` = 0.5
        )),
        theta = list(
            list(ke = 2, v = 4, prob = 0.6),
            list(ke = 3, v = 5, prob = 0.4)
        ),
        posterior = list(
            list(id = "1", point = 1, ke = 2, v = 4, prob = 0.7),
            list(id = "1", point = 2, ke = 3, v = 5, prob = 0.3)
        ),
        predictions = list(prediction),
        covariates = list(list(id = "1", time = 0, block = 0, cov1 = 5))
    )
}

build_example_fit_payload_json <- function(include_cens = TRUE, missing_prediction_fields = character()) {
    jsonlite::toJSON(
        build_example_fit_payload_contents(
            include_cens = include_cens,
            missing_prediction_fields = missing_prediction_fields
        ),
        auto_unbox = TRUE,
        null = "null"
    )
}

build_example_fit_result_envelope_json <- function(
    fit_params,
    transport = c("inline", "manifest"),
    include_cens = TRUE,
    missing_prediction_fields = character()
) {
    transport <- match.arg(transport)
    payload <- build_example_fit_payload_contents(
        include_cens = include_cens,
        missing_prediction_fields = missing_prediction_fields
    )
    build_config <- getFromNamespace("build_fit_payload_config", "Pmetrics")

    envelope <- list(
        kind = "ok",
        schema_version = 1,
        transport = transport,
        config = build_config(fit_params)
    )

    if (identical(transport, "inline")) {
        envelope$payload <- payload
    } else {
        envelope$manifest <- list(
            generator = list(name = "pm_rs", version = "0.1.0"),
            tables = list(
                iterations = list(path = "iterations.csv", row_count = length(payload$iterations), sha256 = "stub"),
                theta = list(path = "theta.csv", row_count = length(payload$theta), sha256 = "stub"),
                posterior = list(path = "posterior.csv", row_count = length(payload$posterior), sha256 = "stub"),
                predictions = list(path = "predictions.csv", row_count = length(payload$predictions), sha256 = "stub"),
                covariates = list(path = "covariates.csv", row_count = length(payload$covariates), sha256 = "stub")
            )
        )
    }

    jsonlite::toJSON(envelope, auto_unbox = TRUE, null = "null")
}

write_example_fit_outputs <- function(path, fit_params, include_cens = TRUE, missing_prediction_fields = character()) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    payload <- build_example_fit_payload_contents(
        include_cens = include_cens,
        missing_prediction_fields = missing_prediction_fields
    )
    build_config <- getFromNamespace("build_fit_payload_config", "Pmetrics")

    readr::write_csv(dplyr::bind_rows(payload$theta), file.path(path, "theta.csv"))
    readr::write_csv(dplyr::bind_rows(payload$posterior), file.path(path, "posterior.csv"))
    readr::write_csv(dplyr::bind_rows(payload$iterations), file.path(path, "iterations.csv"))
    readr::write_csv(dplyr::bind_rows(payload$predictions), file.path(path, "predictions.csv"))
    readr::write_csv(dplyr::bind_rows(payload$covariates), file.path(path, "covariates.csv"))
    writeLines(
        jsonlite::toJSON(build_config(fit_params), auto_unbox = TRUE),
        file.path(path, "settings.json")
    )
}

write_example_fit_manifest <- function(path, fit_params, include_cens = TRUE, missing_prediction_fields = character()) {
    write_example_fit_outputs(
        path,
        fit_params = fit_params,
        include_cens = include_cens,
        missing_prediction_fields = missing_prediction_fields
    )
    unlink(file.path(path, "settings.json"))
    writeLines(
        build_example_fit_result_envelope_json(
            fit_params = fit_params,
            transport = "manifest",
            include_cens = include_cens,
            missing_prediction_fields = missing_prediction_fields
        ),
        file.path(path, "fit_manifest.json")
    )
}

testthat::test_that("PM_model$fit builds PM_result without PM_parse or PM_load", {
    testthat::skip_on_cran()
    testthat::skip_if_not(
        is_cargo_installed(),
        message = "Cargo is required to compile and run PM_model$fit tests."
    )

    mod <- build_example_ode_model(compile = TRUE)
    dat <- PM_data$new(data = "ex.csv", quiet = TRUE)
    run_root <- withr::local_tempdir(pattern = "fit-payload-")

    original_parse <- get("PM_parse", envir = asNamespace("Pmetrics"))
    original_load <- get("PM_load", envir = asNamespace("Pmetrics"))
    withr::defer(assignInNamespace("PM_parse", original_parse, ns = "Pmetrics"))
    withr::defer(assignInNamespace("PM_load", original_load, ns = "Pmetrics"))

    assignInNamespace(
        "PM_parse",
        function(...) stop("PM_parse should not be called on the active fit return path"),
        ns = "Pmetrics"
    )
    assignInNamespace(
        "PM_load",
        function(...) stop("PM_load should not be called on the active fit return path"),
        ns = "Pmetrics"
    )

    res <- suppressMessages(
        mod$fit(
            data = dat,
            path = run_root,
            run = 1,
            overwrite = TRUE,
            cycles = 1,
            points = 20,
            report = "none",
            quiet = TRUE
        )
    )

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_s3_class(res$op, "PM_op")
    testthat::expect_s3_class(res$pop, "PM_pop")
    testthat::expect_s3_class(res$post, "PM_post")
    testthat::expect_s3_class(res$cycle, "PM_cycle")
    testthat::expect_s3_class(res$final, "PM_final")
    testthat::expect_true(file.exists(file.path(run_root, "1", "outputs", "PMout.Rdata")))
})

testthat::test_that("PM_result refit helpers stay stable on payload return path", {
    testthat::skip_on_cran()
    testthat::skip_if_not(
        is_cargo_installed(),
        message = "Cargo is required to compile and run PM_model$fit tests."
    )

    mod <- build_example_ode_model(compile = TRUE)
    dat <- PM_data$new(data = "ex.csv", quiet = TRUE)

    base_root <- withr::local_tempdir(pattern = "fit-payload-base-")
    base_result <- suppressMessages(
        mod$fit(
            data = dat,
            path = base_root,
            run = 1,
            overwrite = TRUE,
            cycles = 1,
            points = 20,
            report = "none",
            quiet = TRUE
        )
    )

    refit_root <- withr::local_tempdir(pattern = "fit-payload-refit-")
    refit_result <- suppressMessages(
        base_result$fit(
            cycles = 1,
            points = 20,
            path = refit_root,
            run = 1,
            overwrite = TRUE,
            report = "none",
            quiet = TRUE
        )
    )

    continue_root <- withr::local_tempdir(pattern = "fit-payload-continue-")
    continue_result <- suppressMessages(
        base_result$continue(
            cycles = 1,
            path = continue_root,
            run = 1,
            overwrite = TRUE,
            report = "none",
            quiet = TRUE
        )
    )

    testthat::expect_s3_class(refit_result, "PM_result")
    testthat::expect_s3_class(continue_result, "PM_result")
    testthat::expect_true(file.exists(file.path(refit_root, "1", "outputs", "PMout.Rdata")))
    testthat::expect_true(file.exists(file.path(continue_root, "1", "outputs", "PMout.Rdata")))
})

testthat::test_that("build_pm_result_from_fit_payload prefers payload data over output files", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    out_path <- withr::local_tempdir(pattern = "fit-payload-prefer-")

    writeLines(
        c(
            "ke,v,prob",
            "999,999,1"
        ),
        file.path(out_path, "theta.csv")
    )
    writeLines(
        c(
            "id,point,ke,v,prob",
            "1,1,999,999,1"
        ),
        file.path(out_path, "posterior.csv")
    )
    writeLines(
        c(
            "cycle,converged,status,neg2ll,nspp,gamlam.0,ke.mean,ke.median,ke.sd,v.mean,v.median,v.sd",
            "1,false,file,999,1,999,999,999,999,999,999,999"
        ),
        file.path(out_path, "iterations.csv")
    )
    writeLines(
        c(
            "id,time,outeq,block,obs,cens,pop_mean,pop_median,post_mean,post_median",
            "1,0,0,0,10,none,999,999,999,999"
        ),
        file.path(out_path, "predictions.csv")
    )
    writeLines(
        c(
            "id,time,block,cov1",
            "1,0,0,999"
        ),
        file.path(out_path, "covariates.csv")
    )
    writeLines(
        '{"parameters":[[{"name":"ke","lower":0.1,"upper":1},{"name":"v","lower":1,"upper":10}]],"errormodels":{"models":[{"Additive":{"poly":{"c0":999,"c1":0,"c2":0,"c3":0}}}]},"prior":{"Sobol":[999]}}',
        file.path(out_path, "settings.json")
    )

    fit_params <- list(
        ranges = list(ke = c(0.1, 1.0), v = c(1.0, 10.0)),
        error_models = lapply(model$model_list$err, function(x) x$flatten()),
        prior = "sobol",
        points = 20
    )

    payload_json <- build_example_fit_result_envelope_json(
        fit_params = fit_params,
        transport = "inline"
    )

    res <- build_pm_result_from_fit_payload(
        payload_json = payload_json,
        data = data,
        model = model,
        path = out_path
    )

    testthat::expect_equal(
        unique(res$op$data$pred[res$op$data$pred.type == "pop" & res$op$data$icen == "mean"]),
        11
    )
    testthat::expect_equal(
        unique(res$pop$data$pred[res$pop$data$icen == "mean"]),
        11
    )
    testthat::expect_equal(
        unique(res$post$data$pred[res$post$data$icen == "mean"]),
        13
    )
    testthat::expect_equal(res$cycle$data$objective$neg2ll[[1]], 123)
    testthat::expect_equal(res$final$data$popPoints$ke[[1]], 2)
    testthat::expect_equal(res$final$data$gridpts, 20)
})

testthat::test_that("build_pm_result_from_fit_payload creates a missing output directory", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    out_path <- file.path(withr::local_tempdir(pattern = "fit-payload-missing-out-"), "outputs")

    testthat::expect_false(dir.exists(out_path))

    fit_params <- list(
        ranges = list(ke = c(0.1, 1.0), v = c(1.0, 10.0)),
        error_models = lapply(model$model_list$err, function(x) x$flatten()),
        prior = "sobol",
        points = 20
    )

    payload_json <- build_example_fit_result_envelope_json(
        fit_params = fit_params,
        transport = "inline"
    )

    res <- build_pm_result_from_fit_payload(
        payload_json = payload_json,
        data = data,
        model = model,
        path = out_path
    )

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_true(dir.exists(out_path))
    testthat::expect_true(file.exists(file.path(out_path, "PMout.Rdata")))
})

testthat::test_that("build_pm_result_from_fit_payload supports manifest transport", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    out_path <- withr::local_tempdir(pattern = "fit-payload-manifest-")

    fit_params <- list(
        ranges = list(ke = c(0.1, 1.0), v = c(1.0, 10.0)),
        error_models = lapply(model$model_list$err, function(x) x$flatten()),
        prior = "sobol",
        points = 20
    )

    write_example_fit_manifest(out_path, fit_params = fit_params)

    res <- build_pm_result_from_fit_payload(
        payload_json = build_example_fit_result_envelope_json(
            fit_params = fit_params,
            transport = "manifest"
        ),
        data = data,
        model = model,
        path = out_path
    )

    testthat::expect_s3_class(res, "PM_result")
    testthat::expect_equal(
        unique(res$op$data$pred[res$op$data$pred.type == "pop" & res$op$data$icen == "mean"]),
        11
    )
    testthat::expect_equal(res$final$data$gridpts, 20)
    testthat::expect_true(file.exists(file.path(out_path, "fit_payload.rds")))
})

testthat::test_that("decode_fit_payload errors on missing required payload columns", {
    model <- build_example_ode_model(compile = FALSE)
    fit_params <- build_example_fit_params(model)

    err <- rlang::catch_cnd(
        decode_fit_payload(
            build_example_fit_payload_json(missing_prediction_fields = "obs"),
            fit_params = fit_params
        ),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_result_schema_missing_field_error")
    testthat::expect_s3_class(err, "pmetrics_result_schema_error")
    testthat::expect_equal(err$table, "predictions")
    testthat::expect_equal(err$column, "obs")
})

testthat::test_that("fit payload normalization applies cens defaults for payload and outputs", {
    model <- build_example_ode_model(compile = FALSE)
    fit_params <- build_example_fit_params(model)
    out_path <- withr::local_tempdir(pattern = "fit-payload-default-cens-")

    payload <- decode_fit_payload(
        build_example_fit_payload_json(include_cens = FALSE),
        fit_params = fit_params
    )
    write_example_fit_outputs(out_path, fit_params = fit_params, include_cens = FALSE)
    file_payload <- read_fit_payload_from_outputs(out_path)

    testthat::expect_equal(payload$predictions$cens, "none")
    testthat::expect_equal(file_payload$predictions$cens, "none")
})

testthat::test_that("read_fit_payload_from_outputs errors on missing required output columns", {
    model <- build_example_ode_model(compile = FALSE)
    fit_params <- build_example_fit_params(model)
    out_path <- withr::local_tempdir(pattern = "fit-payload-missing-output-col-")

    write_example_fit_outputs(
        out_path,
        fit_params = fit_params,
        missing_prediction_fields = "obs"
    )

    err <- rlang::catch_cnd(
        read_fit_payload_from_outputs(out_path),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_result_schema_missing_field_error")
    testthat::expect_s3_class(err, "pmetrics_result_schema_error")
    testthat::expect_equal(err$table, "predictions")
    testthat::expect_equal(err$column, "obs")
})

testthat::test_that("PM_model$compile rethrows structured bridge errors with compile metadata", {
    model <- build_example_ode_model(compile = FALSE)

    err <- rlang::catch_cnd(
        testthat::with_mocked_bindings(
            validate_model_source = function(...) {
                stop(
                    mock_bridge_error_message(
                        stage = "compile",
                        code = "runtime_compile_failed",
                        message = "compile failed",
                        diagnostic = "line 1: unexpected token"
                    ),
                    call. = FALSE
                )
            },
            {
                model$compile(quiet = TRUE)
            },
            .package = "Pmetrics"
        ),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_bridge_compile_error")
    testthat::expect_equal(err$stage, "compile")
    testthat::expect_equal(err$code, "runtime_compile_failed")
    testthat::expect_equal(err$diagnostic, "line 1: unexpected token")
    testthat::expect_match(conditionMessage(err), "compile failed")
    testthat::expect_match(conditionMessage(err), "line 1: unexpected token")
})

testthat::test_that("PM_model$fit rethrows runtime bridge failures instead of returning NULL", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    run_root <- withr::local_tempdir(pattern = "fit-bridge-runtime-")

    err <- rlang::catch_cnd(
        testthat::with_mocked_bindings(
            validate_model_source = function(...) invisible(NULL),
            fit = function(...) {
                stop(
                    mock_bridge_error_message(
                        stage = "runtime",
                        code = "fit_execution_failed",
                        message = "fit failed",
                        details = list(algorithm = "npag")
                    ),
                    call. = FALSE
                )
            },
            {
                suppressMessages(
                    model$fit(
                        data = data,
                        path = run_root,
                        run = 1,
                        overwrite = TRUE,
                        cycles = 1,
                        points = 20,
                        report = "none",
                        quiet = TRUE
                    )
                )
            },
            .package = "Pmetrics"
        ),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_bridge_runtime_error")
    testthat::expect_equal(err$stage, "runtime")
    testthat::expect_equal(err$code, "fit_execution_failed")
    testthat::expect_equal(err$details$algorithm, "npag")
    testthat::expect_false(inherits(err, "pmetrics_bridge_unstructured_error"))
})

testthat::test_that("PM_model$fit keeps compatibility with unstructured bridge failures", {
    model <- build_example_ode_model(compile = FALSE)
    data <- PM_data$new(data = "ex.csv", quiet = TRUE)
    run_root <- withr::local_tempdir(pattern = "fit-bridge-unstructured-")

    err <- rlang::catch_cnd(
        testthat::with_mocked_bindings(
            validate_model_source = function(...) invisible(NULL),
            fit = function(...) {
                stop("old flat string bridge failure", call. = FALSE)
            },
            {
                suppressMessages(
                    model$fit(
                        data = data,
                        path = run_root,
                        run = 1,
                        overwrite = TRUE,
                        cycles = 1,
                        points = 20,
                        report = "none",
                        quiet = TRUE
                    )
                )
            },
            .package = "Pmetrics"
        ),
        classes = "error"
    )

    testthat::expect_s3_class(err, "pmetrics_bridge_runtime_error")
    testthat::expect_s3_class(err, "pmetrics_bridge_unstructured_error")
    testthat::expect_equal(err$stage, "runtime")
    testthat::expect_equal(err$code, "fit_execution_failed")
    testthat::expect_match(conditionMessage(err), "old flat string bridge failure")
})
