pm_fit_payload_schema_version <- 1L
pm_fit_manifest_file_name <- "fit_manifest.json"

pm_fit_payload_column_spec <- function(type, required = TRUE, default = NULL) {
    list(type = type, required = required, default = default)
}

pm_fit_payload_named_specs <- function(columns, type, required = TRUE, default = NULL) {
    if (!length(columns)) {
        return(list())
    }

    stats::setNames(
        rep(list(pm_fit_payload_column_spec(type, required = required, default = default)), length(columns)),
        columns
    )
}

pm_fit_payload_empty_column <- function(type) {
    switch(type,
        numeric = numeric(),
        integer = integer(),
        character = character(),
        logical = logical(),
        cli::cli_abort(c("x" = "Unknown fit payload schema type {.val {type}}."))
    )
}

pm_fit_payload_empty_table <- function(column_specs) {
    tibble::new_tibble(
        stats::setNames(
            lapply(column_specs, function(spec) pm_fit_payload_empty_column(spec$type)),
            names(column_specs)
        ),
        nrow = 0L
    )
}

pm_fit_payload_schema_classes <- function(class = NULL) {
    unique(c(class, "pmetrics_result_schema_error"))
}

abort_pm_fit_payload_schema <- function(
    message,
    class = NULL,
    table = NULL,
    column = NULL,
    source = NULL,
    path = NULL,
    expected = NULL
) {
    rlang::abort(
        message = message,
        class = pm_fit_payload_schema_classes(class),
        table = table,
        column = column,
        source = source,
        path = path,
        expected = expected
    )
}

pm_fit_payload_parameter_stat_columns <- function(parameter_names) {
    unlist(lapply(parameter_names, function(name) {
        c(paste0(name, ".mean"), paste0(name, ".median"), paste0(name, ".sd"))
    }), use.names = FALSE)
}

pm_fit_payload_table_specs <- function(parameter_names, covariate_names, error_model_count) {
    iteration_specs <- c(
        list(
            cycle = pm_fit_payload_column_spec("integer"),
            converged = pm_fit_payload_column_spec("logical"),
            status = pm_fit_payload_column_spec("character"),
            neg2ll = pm_fit_payload_column_spec("numeric"),
            nspp = pm_fit_payload_column_spec("integer")
        ),
        pm_fit_payload_named_specs(
            paste0("gamlam.", seq_len(error_model_count) - 1L),
            "numeric"
        ),
        pm_fit_payload_named_specs(pm_fit_payload_parameter_stat_columns(parameter_names), "numeric")
    )

    list(
        iterations = iteration_specs,
        theta = c(
            pm_fit_payload_named_specs(parameter_names, "numeric"),
            list(prob = pm_fit_payload_column_spec("numeric"))
        ),
        posterior = c(
            list(
                id = pm_fit_payload_column_spec("character"),
                point = pm_fit_payload_column_spec("integer")
            ),
            pm_fit_payload_named_specs(parameter_names, "numeric"),
            list(prob = pm_fit_payload_column_spec("numeric"))
        ),
        predictions = list(
            id = pm_fit_payload_column_spec("character"),
            time = pm_fit_payload_column_spec("numeric"),
            outeq = pm_fit_payload_column_spec("integer"),
            block = pm_fit_payload_column_spec("integer"),
            obs = pm_fit_payload_column_spec("numeric"),
            cens = pm_fit_payload_column_spec("character", required = FALSE, default = "none"),
            pop_mean = pm_fit_payload_column_spec("numeric"),
            pop_median = pm_fit_payload_column_spec("numeric"),
            post_mean = pm_fit_payload_column_spec("numeric"),
            post_median = pm_fit_payload_column_spec("numeric")
        ),
        covariates = c(
            list(
                id = pm_fit_payload_column_spec("character"),
                time = pm_fit_payload_column_spec("numeric"),
                block = pm_fit_payload_column_spec("integer")
            ),
            pm_fit_payload_named_specs(covariate_names, "numeric")
        )
    )
}

pm_fit_payload_default_column <- function(spec, nrow) {
    default_value <- if (is.null(spec$default)) {
        rep(NA, nrow)
    } else {
        rep(spec$default, nrow)
    }

    switch(spec$type,
        numeric = as.numeric(default_value),
        integer = as.integer(default_value),
        character = as.character(default_value),
        logical = as.logical(default_value),
        cli::cli_abort(c("x" = "Unknown fit payload schema type {.val {spec$type}}."))
    )
}

pm_fit_payload_scalar_input <- function(value) {
    if (is.factor(value)) {
        return(as.character(value))
    }

    if (is.list(value)) {
        return(vapply(value, function(item) paste(as.character(item), collapse = ","), character(1)))
    }

    value
}

pm_fit_payload_as_numeric <- function(value, table, column, source, path) {
    raw_value <- pm_fit_payload_scalar_input(value)
    numeric_value <- suppressWarnings(as.numeric(raw_value))
    invalid <- !is.na(raw_value) & is.na(numeric_value)

    if (any(invalid)) {
        abort_pm_fit_payload_schema(
            message = paste0("Table `", table, "` column `", column, "` contains values that cannot be read as numeric."),
            class = "pmetrics_result_schema_coercion_error",
            table = table,
            column = column,
            source = source,
            path = path,
            expected = "numeric"
        )
    }

    numeric_value
}

pm_fit_payload_as_integer <- function(value, table, column, source, path) {
    numeric_value <- pm_fit_payload_as_numeric(value, table, column, source, path)
    invalid <- !is.na(numeric_value) & numeric_value != trunc(numeric_value)

    if (any(invalid)) {
        abort_pm_fit_payload_schema(
            message = paste0("Table `", table, "` column `", column, "` contains non-integer values."),
            class = "pmetrics_result_schema_coercion_error",
            table = table,
            column = column,
            source = source,
            path = path,
            expected = "integer"
        )
    }

    as.integer(numeric_value)
}

pm_fit_payload_as_logical <- function(value, table, column, source, path) {
    if (is.logical(value)) {
        return(value)
    }

    raw_value <- pm_fit_payload_scalar_input(value)
    text_value <- trimws(tolower(as.character(raw_value)))
    logical_value <- rep(NA, length(text_value))
    logical_value[text_value %in% c("true", "t", "1")] <- TRUE
    logical_value[text_value %in% c("false", "f", "0")] <- FALSE
    logical_value[is.na(raw_value)] <- NA

    invalid <- !is.na(raw_value) & is.na(logical_value)
    if (any(invalid)) {
        abort_pm_fit_payload_schema(
            message = paste0("Table `", table, "` column `", column, "` contains values that cannot be read as logical."),
            class = "pmetrics_result_schema_coercion_error",
            table = table,
            column = column,
            source = source,
            path = path,
            expected = "logical"
        )
    }

    logical_value
}

pm_fit_payload_coerce_column <- function(value, table, column, spec, source, path = NULL) {
    switch(spec$type,
        numeric = pm_fit_payload_as_numeric(value, table, column, source, path),
        integer = pm_fit_payload_as_integer(value, table, column, source, path),
        logical = pm_fit_payload_as_logical(value, table, column, source, path),
        character = as.character(pm_fit_payload_scalar_input(value)),
        cli::cli_abort(c("x" = "Unknown fit payload schema type {.val {spec$type}}."))
    )
}

pm_fit_payload_posterior_iterations_row <- function(column_specs) {
    values <- lapply(names(column_specs), function(column_name) {
        if (column_name == "cycle") {
            return(0L)
        }
        if (column_name == "converged") {
            return(NA)
        }
        if (column_name == "status") {
            return("Posterior")
        }

        pm_fit_payload_default_column(column_specs[[column_name]], 1L)
    })

    tibble::as_tibble(stats::setNames(values, names(column_specs)))
}

pm_fit_payload_normalize_table <- function(
    table_name,
    data,
    column_specs,
    source,
    path = NULL,
    empty_table = NULL
) {
    if (is.null(data)) {
        abort_pm_fit_payload_schema(
            message = paste0("Missing required table `", table_name, "` in ", source, "."),
            class = "pmetrics_result_schema_missing_table_error",
            table = table_name,
            source = source,
            path = path
        )
    }

    table <- if (length(data) == 0) {
        pm_fit_payload_empty_table(column_specs)
    } else {
        tibble::as_tibble(data)
    }

    required_columns <- names(column_specs)[vapply(column_specs, function(spec) isTRUE(spec$required), logical(1))]
    missing_columns <- setdiff(required_columns, names(table))
    if (length(missing_columns)) {
        abort_pm_fit_payload_schema(
            message = paste0(
                "Table `", table_name, "` is missing required column `", missing_columns[[1]], "`."
            ),
            class = "pmetrics_result_schema_missing_field_error",
            table = table_name,
            column = missing_columns[[1]],
            source = source,
            path = path
        )
    }

    optional_columns <- setdiff(names(column_specs), names(table))
    for (column_name in optional_columns) {
        table[[column_name]] <- pm_fit_payload_default_column(column_specs[[column_name]], nrow(table))
    }

    table <- table[, names(column_specs), drop = FALSE]

    for (column_name in names(column_specs)) {
        table[[column_name]] <- pm_fit_payload_coerce_column(
            value = table[[column_name]],
            table = table_name,
            column = column_name,
            spec = column_specs[[column_name]],
            source = source,
            path = path
        )
    }

    if (nrow(table) == 0 && is.function(empty_table)) {
        table <- empty_table(column_specs)
    }

    table
}

pm_fit_payload_table_names <- function(data, base_columns) {
    if (is.null(data) || length(data) == 0) {
        return(character())
    }

    setdiff(names(tibble::as_tibble(data)), base_columns)
}

pm_fit_payload_validate_config <- function(config, source, path = NULL) {
    if (!is.list(config)) {
        abort_pm_fit_payload_schema(
            message = paste0("Missing fit settings in ", source, "."),
            class = "pmetrics_result_schema_missing_table_error",
            table = "settings",
            source = source,
            path = path
        )
    }

    if (is.null(config$parameters) || length(config$parameters) == 0 || is.null(config$parameters[[1]][["name"]])) {
        abort_pm_fit_payload_schema(
            message = paste0("Settings are missing required field `parameters[[1]]$name` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "settings",
            column = "parameters[[1]]$name",
            source = source,
            path = path
        )
    }

    if (is.null(config$errormodels) || is.null(config$errormodels$models)) {
        abort_pm_fit_payload_schema(
            message = paste0("Settings are missing required field `errormodels$models` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "settings",
            column = "errormodels$models",
            source = source,
            path = path
        )
    }

    config
}

pm_fit_payload_parameter_names <- function(parsed, fit_params) {
    if (!is.null(parsed$parameter_names)) {
        return(as.character(parsed$parameter_names))
    }

    names(fit_params$ranges)
}

pm_fit_payload_covariate_names <- function(parsed) {
    if (!is.null(parsed$covariate_names)) {
        return(as.character(parsed$covariate_names))
    }

    pm_fit_payload_table_names(parsed$covariates, c("id", "time", "block"))
}

pm_fit_payload_build <- function(
    parameter_names,
    covariate_names,
    config,
    iterations,
    theta,
    posterior,
    predictions,
    covariates,
    transport,
    source,
    path = NULL
) {
    config <- pm_fit_payload_validate_config(config, source = source, path = path)
    specs <- pm_fit_payload_table_specs(
        parameter_names = parameter_names,
        covariate_names = covariate_names,
        error_model_count = length(config$errormodels$models)
    )

    structure(
        list(
            schema_version = pm_fit_payload_schema_version,
            transport = transport,
            parameter_names = parameter_names,
            covariate_names = covariate_names,
            iterations = pm_fit_payload_normalize_table(
                table_name = "iterations",
                data = iterations,
                column_specs = specs$iterations,
                source = source,
                path = path,
                empty_table = pm_fit_payload_posterior_iterations_row
            ),
            theta = pm_fit_payload_normalize_table(
                table_name = "theta",
                data = theta,
                column_specs = specs$theta,
                source = source,
                path = path
            ),
            posterior = pm_fit_payload_normalize_table(
                table_name = "posterior",
                data = posterior,
                column_specs = specs$posterior,
                source = source,
                path = path
            ),
            predictions = pm_fit_payload_normalize_table(
                table_name = "predictions",
                data = predictions,
                column_specs = specs$predictions,
                source = source,
                path = path
            ),
            covariates = pm_fit_payload_normalize_table(
                table_name = "covariates",
                data = covariates,
                column_specs = specs$covariates,
                source = source,
                path = path
            ),
            config = config
        ),
        class = "PM_fit_payload"
    )
}

build_fit_payload_config <- function(fit_params) {
    parameter_ranges <- purrr::imap_dfr(fit_params$ranges, function(bounds, name) {
        bounds <- as.numeric(bounds)
        tibble::tibble(name = name, lower = bounds[[1]], upper = bounds[[2]])
    })

    error_models <- purrr::map(fit_params$error_models, function(model) {
        coeff <- as.numeric(model$coeff)
        model_type <- as.character(model$type[[1]])

        stats::setNames(
            list(list(poly = list(c0 = coeff[[1]], c1 = coeff[[2]], c2 = coeff[[3]], c3 = coeff[[4]]))),
            model_type
        )
    })

    prior <- NULL
    if (is.character(fit_params$prior) && identical(tolower(fit_params$prior), "sobol")) {
        prior <- list(Sobol = c(as.numeric(fit_params$points)))
    }

    list(
        parameters = list(parameter_ranges),
        errormodels = list(models = error_models),
        prior = prior
    )
}

decode_fit_payload <- function(payload_json, fit_params) {
    parsed <- jsonlite::fromJSON(payload_json)
    parameter_names <- pm_fit_payload_parameter_names(parsed, fit_params)
    covariate_names <- pm_fit_payload_covariate_names(parsed)

    pm_fit_payload_build(
        parameter_names = parameter_names,
        covariate_names = covariate_names,
        config = build_fit_payload_config(fit_params),
        iterations = parsed$iterations,
        theta = parsed$theta,
        posterior = parsed$posterior,
        predictions = parsed$predictions,
        covariates = parsed$covariates,
        transport = "inline",
        source = "inline fit payload"
    )
}

pm_fit_payload_parameter_names_from_config <- function(config, source, path = NULL) {
    parameters <- config$parameters[[1]]
    parameter_names <- parameters[["name"]]

    if (is.null(parameter_names)) {
        abort_pm_fit_payload_schema(
            message = paste0("Settings are missing required field `parameters[[1]]$name` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "settings",
            column = "parameters[[1]]$name",
            source = source,
            path = path
        )
    }

    as.character(parameter_names)
}

read_fit_result_envelope <- function(payload_json, source, path = NULL) {
    parsed <- jsonlite::fromJSON(payload_json)

    if (!is.list(parsed) || is.null(parsed$kind) || !identical(as.character(parsed$kind[[1]]), "ok")) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit result envelope must include `kind = \"ok\"` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "fit_result",
            column = "kind",
            source = source,
            path = path
        )
    }

    schema_version <- suppressWarnings(as.integer(parsed$schema_version[[1]] %||% NA_integer_))
    if (length(schema_version) != 1L || is.na(schema_version) || !identical(schema_version, pm_fit_payload_schema_version)) {
        abort_pm_fit_payload_schema(
            message = paste0(
                "Unsupported fit result schema version `",
                parsed$schema_version[[1]] %||% "<missing>",
                "` in ",
                source,
                ". Expected `",
                pm_fit_payload_schema_version,
                "`."
            ),
            class = "pmetrics_result_schema_version_error",
            table = "fit_result",
            source = source,
            path = path
        )
    }

    transport <- as.character(parsed$transport[[1]] %||% NA_character_)
    if (length(transport) != 1L || is.na(transport) || !(transport %in% c("inline", "manifest"))) {
        abort_pm_fit_payload_schema(
            message = paste0("Unsupported fit result transport `", transport, "` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "fit_result",
            column = "transport",
            source = source,
            path = path
        )
    }

    if (is.null(parsed$config)) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit result envelope is missing required field `config` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "fit_result",
            column = "config",
            source = source,
            path = path
        )
    }

    if (identical(transport, "inline") && is.null(parsed$payload)) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit result envelope is missing required field `payload` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "fit_result",
            column = "payload",
            source = source,
            path = path
        )
    }

    if (identical(transport, "manifest") && is.null(parsed$manifest)) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit result envelope is missing required field `manifest` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = "fit_result",
            column = "manifest",
            source = source,
            path = path
        )
    }

    list(
        transport = transport,
        config = pm_fit_payload_validate_config(parsed$config, source = source, path = path),
        payload = parsed$payload,
        manifest = parsed$manifest
    )
}

read_fit_manifest_table <- function(path, manifest, table_name, source) {
    if (is.null(manifest$tables) || is.null(manifest$tables[[table_name]])) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit manifest is missing required table metadata `", table_name, "` in ", source, "."),
            class = "pmetrics_result_schema_missing_table_error",
            table = table_name,
            source = source,
            path = path
        )
    }

    table_manifest <- manifest$tables[[table_name]]
    file_name <- as.character(table_manifest$path[[1]] %||% NA_character_)
    row_count <- suppressWarnings(as.integer(table_manifest$row_count[[1]] %||% NA_integer_))
    sha256 <- as.character(table_manifest$sha256[[1]] %||% NA_character_)

    if (length(file_name) != 1L || is.na(file_name) || identical(file_name, "")) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit manifest is missing required field `path` for table `", table_name, "` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = table_name,
            column = "path",
            source = source,
            path = path
        )
    }

    if (length(row_count) != 1L || is.na(row_count)) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit manifest is missing required field `row_count` for table `", table_name, "` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = table_name,
            column = "row_count",
            source = source,
            path = path
        )
    }

    if (length(sha256) != 1L || is.na(sha256) || identical(sha256, "")) {
        abort_pm_fit_payload_schema(
            message = paste0("Fit manifest is missing required field `sha256` for table `", table_name, "` in ", source, "."),
            class = "pmetrics_result_schema_missing_field_error",
            table = table_name,
            column = "sha256",
            source = source,
            path = path
        )
    }

    table <- read_fit_payload_table(path, file_name, table_name)

    if (!identical(nrow(table), row_count)) {
        abort_pm_fit_payload_schema(
            message = paste0(
                "Fit manifest row count mismatch for table `",
                table_name,
                "` in ",
                source,
                ": expected ",
                row_count,
                ", got ",
                nrow(table),
                "."
            ),
            class = "pmetrics_result_schema_coercion_error",
            table = table_name,
            column = "row_count",
            source = source,
            path = path
        )
    }

    table
}

read_fit_payload_from_manifest <- function(manifest_payload, path, source = "fit manifest") {
    manifest <- manifest_payload$manifest
    config <- manifest_payload$config
    parameter_names <- pm_fit_payload_parameter_names_from_config(config, source = source, path = path)
    covariates <- read_fit_manifest_table(path, manifest, "covariates", source)
    covariate_names <- pm_fit_payload_table_names(covariates, c("id", "time", "block"))

    pm_fit_payload_build(
        parameter_names = parameter_names,
        covariate_names = covariate_names,
        config = config,
        iterations = read_fit_manifest_table(path, manifest, "iterations", source),
        theta = read_fit_manifest_table(path, manifest, "theta", source),
        posterior = read_fit_manifest_table(path, manifest, "posterior", source),
        predictions = read_fit_manifest_table(path, manifest, "predictions", source),
        covariates = covariates,
        transport = "manifest",
        source = source,
        path = path
    )
}

decode_fit_result_envelope <- function(payload_json, path = NULL) {
    parsed <- read_fit_result_envelope(
        payload_json = payload_json,
        source = "fit result envelope",
        path = path
    )

    if (identical(parsed$transport, "inline")) {
        payload <- parsed$payload
        parameter_names <- if (!is.null(payload$parameter_names)) {
            as.character(payload$parameter_names)
        } else {
            pm_fit_payload_parameter_names_from_config(parsed$config, source = "fit result envelope", path = path)
        }
        covariate_names <- pm_fit_payload_covariate_names(payload)

        return(pm_fit_payload_build(
            parameter_names = parameter_names,
            covariate_names = covariate_names,
            config = parsed$config,
            iterations = payload$iterations,
            theta = payload$theta,
            posterior = payload$posterior,
            predictions = payload$predictions,
            covariates = payload$covariates,
            transport = "inline",
            source = "fit result envelope",
            path = path
        ))
    }

    read_fit_payload_from_manifest(parsed, path = path, source = "fit result envelope")
}

read_fit_payload_from_outputs <- function(path) {
    settings_path <- file.path(path, "settings.json")
    if (!file.exists(settings_path)) {
        abort_pm_fit_payload_schema(
            message = paste0("Missing required settings file `", settings_path, "`."),
            class = "pmetrics_result_schema_missing_table_error",
            table = "settings",
            source = "output files",
            path = path
        )
    }

    config <- pm_fit_payload_validate_config(
        jsonlite::fromJSON(settings_path),
        source = "output files",
        path = path
    )
    parameter_names <- as.character(config$parameters[[1]][["name"]])
    covariates <- read_fit_payload_table(path, "covariates.csv", "covariates")
    covariate_names <- pm_fit_payload_table_names(covariates, c("id", "time", "block"))

    pm_fit_payload_build(
        parameter_names = parameter_names,
        covariate_names = covariate_names,
        config = config,
        iterations = read_fit_payload_table(path, "iterations.csv", "iterations"),
        theta = read_fit_payload_table(path, "theta.csv", "theta"),
        posterior = read_fit_payload_table(path, "posterior.csv", "posterior"),
        predictions = read_fit_payload_table(path, "predictions.csv", "predictions"),
        covariates = covariates,
        transport = "outputs",
        source = "output files",
        path = path
    )
}

load_fit_manifest_snapshot <- function(path) {
    manifest_path <- file.path(path, pm_fit_manifest_file_name)
    if (!file.exists(manifest_path)) {
        return(NULL)
    }

    decode_fit_result_envelope(
        payload_json = paste(readLines(manifest_path, warn = FALSE), collapse = "\n"),
        path = path
    )
}

read_fit_payload_table <- function(path, file_name, table_name) {
    file_path <- file.path(path, file_name)
    if (!file.exists(file_path)) {
        abort_pm_fit_payload_schema(
            message = paste0("Missing required output file `", file_path, "`."),
            class = "pmetrics_result_schema_missing_table_error",
            table = table_name,
            source = "output files",
            path = path
        )
    }

    readr::read_csv(file = file_path, show_col_types = FALSE)
}

fit_payload_from_source <- function(data, path = ".") {
    if (inherits(data, "PM_fit_payload")) {
        return(data)
    }

    manifest_payload <- load_fit_manifest_snapshot(path)
    if (!is.null(manifest_payload)) {
        return(manifest_payload)
    }

    read_fit_payload_from_outputs(path)
}

save_fit_payload_snapshot <- function(fit_payload, path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    suppressWarnings(saveRDS(fit_payload, file = file.path(path, "fit_payload.rds")))
    invisible(TRUE)
}

load_fit_payload_snapshot <- function(path) {
    snapshot_path <- file.path(path, "fit_payload.rds")
    if (!file.exists(snapshot_path)) {
        return(NULL)
    }

    fit_payload <- tryCatch(readRDS(snapshot_path), error = function(e) NULL)
    if (!inherits(fit_payload, "PM_fit_payload")) {
        return(NULL)
    }

    fit_payload
}

save_pm_result_snapshot <- function(out, path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    PMout <- out
    suppressWarnings(save(PMout, file = file.path(path, "PMout.Rdata")))
    invisible(TRUE)
}

build_pm_result_from_normalized_payload <- function(fit_payload, data, model, path) {
    out <- list(
        pop = PM_pop$new(fit_payload, path = path),
        post = PM_post$new(fit_payload, path = path),
        final = PM_final$new(fit_payload, path = path),
        cycle = PM_cycle$new(fit_payload, path = path),
        op = PM_op$new(fit_payload, path = path),
        cov = PM_cov$new(fit_payload, path = path),
        data = data,
        model = model,
        errfile = NULL,
        success = TRUE,
        valid = NULL
    )

    save_fit_payload_snapshot(fit_payload, path)
    save_pm_result_snapshot(out, path)
    PM_result$new(out, path = path, quiet = TRUE)
}

build_pm_result_from_fit_payload <- function(payload_json, data, model, path) {
    fit_payload <- decode_fit_result_envelope(payload_json, path = path)
    build_pm_result_from_normalized_payload(fit_payload, data = data, model = model, path = path)
}
