mod bridge_errors;
mod executor;
mod fit_payload;
mod live;
mod logs;
mod settings;
mod simulation;

use anyhow::{anyhow, Result as AnyResult};
use bridge_errors::{BridgeError, BridgeResult, BridgeStage};
use extendr_api::prelude::*;
use pmcore::api::LoggingLevel;
use pmcore::prelude::{
    data::{read_pmetrics, Data},
    pharmsol::dsl::{self, CompiledRuntimeModel, RuntimeCompilationTarget},
    EquationMetadataSource, ExplicitRkTableau, OdeSolver, Parameters, SdirkTableau,
};
use simulation::SimulationRow;
use std::path::Path;
use std::process::Command;
use tracing_subscriber::layer::SubscriberExt;

use crate::logs::{configure_logs, LogControls, RFormatLayer};

fn to_r_result<T>(result: BridgeResult<T>) -> extendr_api::Result<T> {
    result.map_err(Into::into)
}

fn validate_data_path(data_path: &str) -> BridgeResult<()> {
    if !Path::new(data_path).exists() {
        return Err(
            BridgeError::new(
                BridgeStage::Data,
                "data_path_missing",
                format!("Data path does not exist: {}", data_path),
            )
            .with_detail("data_path", data_path),
        );
    }
    Ok(())
}

fn read_pmetrics_data(data_path: &str) -> BridgeResult<Data> {
    read_pmetrics(data_path).map_err(|error| {
        BridgeError::new(
            BridgeStage::Data,
            "data_parse_failed",
            format!("Failed to parse data: {}", error),
        )
        .with_detail("data_path", data_path)
    })
}

fn compile_diagnostic(error: &dsl::RuntimeError, model_source: &str) -> Option<String> {
    error
        .render_diagnostic(model_source)
        .filter(|message| !message.trim().is_empty())
}

fn parse_ode_solver(solver: Option<String>) -> BridgeResult<Option<OdeSolver>> {
    match solver {
        None => Ok(None),
        Some(solver) if solver.trim().is_empty() => Ok(None),
        Some(solver) => match solver.trim().to_ascii_uppercase().as_str() {
            "BDF" => Ok(Some(OdeSolver::Bdf)),
            "TRBDF2" => Ok(Some(OdeSolver::Sdirk(SdirkTableau::TrBdf2))),
            "ESDIRK34" => Ok(Some(OdeSolver::Sdirk(SdirkTableau::Esdirk34))),
            "TSIT45" => Ok(Some(OdeSolver::ExplicitRk(ExplicitRkTableau::Tsit45))),
            other => Err(
                BridgeError::new(
                    BridgeStage::Settings,
                    "unsupported_ode_solver",
                    format!("Unsupported ODE solver: {}", other),
                )
                .with_detail("solver", other),
            ),
        },
    }
}

fn compiled_model_kind(model: &CompiledRuntimeModel) -> &'static str {
    match model {
        CompiledRuntimeModel::Ode(_) => "ode",
        CompiledRuntimeModel::Analytical(_) => "analytical",
        CompiledRuntimeModel::Sde(_) => "sde",
    }
}

fn apply_ode_solver(
    model: CompiledRuntimeModel,
    solver: Option<OdeSolver>,
) -> CompiledRuntimeModel {
    match (model, solver) {
        (CompiledRuntimeModel::Ode(model), Some(solver)) => {
            CompiledRuntimeModel::Ode(model.with_solver(solver))
        }
        (model, _) => model,
    }
}

fn compile_runtime_model(
    model_source: &str,
    kind: &str,
    solver: Option<String>,
) -> BridgeResult<CompiledRuntimeModel> {
    let model_source = model_source.trim();
    if model_source.is_empty() {
        return Err(BridgeError::new(
            BridgeStage::Compile,
            "model_source_empty",
            "Model source is empty",
        ));
    }

    match kind {
        "ode" | "analytical" => {}
        other => {
            return Err(
                BridgeError::new(
                    BridgeStage::Compile,
                    "unsupported_model_type",
                    format!("{} is not a supported model type", other),
                )
                .with_detail("model_kind", other),
            )
        }
    }

    let compiled = dsl::compile_module_source_to_runtime(
        model_source,
        None,
        RuntimeCompilationTarget::Jit,
        |_stage, _message| {},
    )
    .map_err(|error| {
        let message = error.to_string();
        let diagnostic = compile_diagnostic(&error, model_source);
        let mut bridge_error = BridgeError::new(BridgeStage::Compile, "runtime_compile_failed", message)
            .with_detail("model_kind", kind);
        if let Some(diagnostic) = diagnostic {
            bridge_error = bridge_error.with_diagnostic(diagnostic);
        }
        bridge_error
    })?;

    let actual_kind = compiled_model_kind(&compiled);
    if actual_kind != kind {
        return Err(
            BridgeError::new(
                BridgeStage::Compile,
                "model_kind_mismatch",
                format!(
                    "Model source kind mismatch: expected {}, got {}",
                    kind, actual_kind
                ),
            )
            .with_detail("expected_kind", kind)
            .with_detail("actual_kind", actual_kind),
        );
    }

    let solver = parse_ode_solver(solver)?;
    if solver.is_some() && actual_kind != "ode" {
        return Err(
            BridgeError::new(
                BridgeStage::Settings,
                "ode_solver_requires_ode_model",
                "ODE solver selection is only supported for ODE models",
            )
            .with_detail("actual_kind", actual_kind),
        );
    }

    Ok(apply_ode_solver(compiled, solver))
}

fn parse_theta(matrix: RMatrix<f64>) -> BridgeResult<Vec<Vec<f64>>> {
    let nspp = matrix.nrows();
    let ndim = matrix.ncols();
    let real_vector = matrix
        .as_real_vector()
        .ok_or_else(|| {
            BridgeError::new(
                BridgeStage::Data,
                "theta_matrix_not_real",
                "theta matrix must contain real values",
            )
        })?;
    let mut theta = vec![vec![0.0; ndim]; nspp];
    for i in 0..nspp {
        for j in 0..ndim {
            theta[i][j] = real_vector[i + j * nspp];
        }
    }
    Ok(theta)
}

#[extendr]
fn validate_model_source(model_source: &str, kind: &str, solver: Nullable<String>) {
    extendr_api::error::unwrap_or_throw_error(to_r_result(
        compile_runtime_model(model_source, kind, solver.into_option()).map(|_| ()),
    ))
}

/// Simulates the first subject in the data set using the model DSL source.
/// @param data_path Path to the data file.
/// @param model_source DSL source code for the model.
/// @param spp One support point as a numeric vector with probabiltity.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @param solver Optional ODE solver name.
/// @return Simulation results.
///@export
#[extendr]
fn simulate_one(
    data_path: &str,
    model_source: &str,
    spp: &[f64],
    kind: &str,
    solver: Nullable<String>,
) -> Dataframe<SimulationRow> {
    extendr_api::error::unwrap_or_throw_error(to_r_result(
        (|| -> BridgeResult<Dataframe<SimulationRow>> {
            validate_data_path(data_path)?;
            let data = read_pmetrics_data(data_path)?;
            let compiled_model = compile_runtime_model(model_source, kind, solver.into_option())?;
            let subjects = data.subjects();
            let first_subject = subjects
                .first()
                .ok_or_else(|| {
                    BridgeError::new(
                        BridgeStage::Data,
                        "data_subjects_missing",
                        "Data set contains no subjects",
                    )
                })?;

            let rows = match compiled_model {
                CompiledRuntimeModel::Ode(equation) => {
                    let metadata = equation
                        .equation_metadata()
                        .ok_or_else(|| {
                            BridgeError::new(
                                BridgeStage::Runtime,
                                "runtime_metadata_missing",
                                "runtime model metadata is required",
                            )
                        })?;
                    let parameters = Parameters::with_model(
                        &equation,
                        metadata
                            .parameters()
                            .iter()
                            .zip(spp.iter())
                            .map(|(parameter, value)| (parameter.name(), *value)),
                    )
                    .map_err(|error| {
                        BridgeError::from_anyhow(
                            BridgeStage::Runtime,
                            "parameter_binding_failed",
                            error,
                        )
                    })?;
                    executor::simulate(&equation, first_subject, &parameters, 0)?
                }
                CompiledRuntimeModel::Analytical(equation) => {
                    let metadata = equation
                        .equation_metadata()
                        .ok_or_else(|| {
                            BridgeError::new(
                                BridgeStage::Runtime,
                                "runtime_metadata_missing",
                                "runtime model metadata is required",
                            )
                        })?;
                    let parameters = Parameters::with_model(
                        &equation,
                        metadata
                            .parameters()
                            .iter()
                            .zip(spp.iter())
                            .map(|(parameter, value)| (parameter.name(), *value)),
                    )
                    .map_err(|error| {
                        BridgeError::from_anyhow(
                            BridgeStage::Runtime,
                            "parameter_binding_failed",
                            error,
                        )
                    })?;
                    executor::simulate(&equation, first_subject, &parameters, 0)?
                }
                CompiledRuntimeModel::Sde(_) => {
                    return Err(BridgeError::new(
                        BridgeStage::Runtime,
                        "simulation_model_kind_unsupported",
                        "SDE models are not supported for simulation in this Pmetrics runtime",
                    ));
                }
            };

            rows.into_dataframe()
                .map_err(|error| {
                    BridgeError::from_anyhow(
                        BridgeStage::Handoff,
                        "simulation_dataframe_build_failed",
                        anyhow!("Failed to build data frame: {}", error),
                    )
                })
        })(),
    ))
}

/// Simulates all subjects in the data set using the model DSL source.
/// @param data_path Path to the data file.
/// @param model_source DSL source code for the model.
/// @param theta Data frame of support points.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @param solver Optional ODE solver name.
/// @return Simulation results.
/// @export
#[extendr]
fn simulate_all(
    data_path: &str,
    model_source: &str,
    theta: RMatrix<f64>,
    kind: &str,
    solver: Nullable<String>,
) -> Dataframe<SimulationRow> {
    use rayon::prelude::*;

    extendr_api::error::unwrap_or_throw_error(to_r_result(
        (|| -> BridgeResult<Dataframe<SimulationRow>> {
            validate_data_path(data_path)?;
            let theta = parse_theta(theta)?;
            let data = read_pmetrics_data(data_path)?;
            let compiled_model = compile_runtime_model(model_source, kind, solver.into_option())?;
            let subjects = data.subjects();

            let rows: Vec<_> = match compiled_model {
                CompiledRuntimeModel::Ode(equation) => theta
                    .par_iter()
                    .enumerate()
                    .map(|(i, spp)| {
                        let equation = equation.clone();
                        let metadata = equation
                            .equation_metadata()
                            .ok_or_else(|| {
                                BridgeError::new(
                                    BridgeStage::Runtime,
                                    "runtime_metadata_missing",
                                    "runtime model metadata is required",
                                )
                            })?;
                        let parameters = Parameters::with_model(
                            &equation,
                            metadata
                                .parameters()
                                .iter()
                                .zip(spp.iter())
                                .map(|(parameter, value)| (parameter.name(), *value)),
                        )
                        .map_err(|error| {
                            BridgeError::from_anyhow(
                                BridgeStage::Runtime,
                                "parameter_binding_failed",
                                error,
                            )
                        })?;
                        subjects
                            .iter()
                            .map(|subject| executor::simulate(&equation, subject, &parameters, i))
                            .collect::<BridgeResult<Vec<_>>>()
                            .map(|rows| rows.into_iter().flatten().collect::<Vec<_>>())
                    })
                    .collect::<BridgeResult<Vec<_>>>()?
                    .into_iter()
                    .flatten()
                    .collect(),
                CompiledRuntimeModel::Analytical(equation) => theta
                    .par_iter()
                    .enumerate()
                    .map(|(i, spp)| {
                        let equation = equation.clone();
                        let metadata = equation
                            .equation_metadata()
                            .ok_or_else(|| {
                                BridgeError::new(
                                    BridgeStage::Runtime,
                                    "runtime_metadata_missing",
                                    "runtime model metadata is required",
                                )
                            })?;
                        let parameters = Parameters::with_model(
                            &equation,
                            metadata
                                .parameters()
                                .iter()
                                .zip(spp.iter())
                                .map(|(parameter, value)| (parameter.name(), *value)),
                        )
                        .map_err(|error| {
                            BridgeError::from_anyhow(
                                BridgeStage::Runtime,
                                "parameter_binding_failed",
                                error,
                            )
                        })?;
                        subjects
                            .iter()
                            .map(|subject| executor::simulate(&equation, subject, &parameters, i))
                            .collect::<BridgeResult<Vec<_>>>()
                            .map(|rows| rows.into_iter().flatten().collect::<Vec<_>>())
                    })
                    .collect::<BridgeResult<Vec<_>>>()?
                    .into_iter()
                    .flatten()
                    .collect(),
                CompiledRuntimeModel::Sde(_) => {
                    return Err(BridgeError::new(
                        BridgeStage::Runtime,
                        "simulation_model_kind_unsupported",
                        "SDE models are not supported for simulation in this Pmetrics runtime",
                    ));
                }
            };

            rows.into_dataframe()
                .map_err(|error| {
                    BridgeError::from_anyhow(
                        BridgeStage::Handoff,
                        "simulation_dataframe_build_failed",
                        anyhow!("Failed to build data frame: {}", error),
                    )
                })
        })(),
    ))
}

/// Fits the model DSL source to the data using the provided parameters.
/// @param model_source DSL source code for the model.
/// @param data Path to the data file.
/// @param params List of fitting parameters.
/// @param output_path Path to save the fitting results.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @param solver Optional ODE solver name.
/// @return Result of the fitting process.
/// @export
#[extendr]
pub fn fit(
    model_source: &str,
    data: &str,
    params: List,
    output_path: &str,
    kind: &str,
    solver: Nullable<String>,
) -> String {
    extendr_api::error::unwrap_or_throw_error(to_r_result((|| -> BridgeResult<String> {
        let log_controls = parse_log_controls(&params, output_path).map_err(|error| {
            BridgeError::from_anyhow(BridgeStage::Settings, "log_controls_invalid", error)
        })?;
        RFormatLayer::reset_global_timer();
        setup_logs_inner(&log_controls).map_err(|error| {
            BridgeError::from_anyhow(BridgeStage::Settings, "log_setup_failed", error)
        })?;
        println!("Initializing model fit...");

        validate_data_path(data)?;
        let data = read_pmetrics_data(data)?;
        let compiled_model = compile_runtime_model(model_source, kind, solver.into_option())?;

        let payload = match compiled_model {
            CompiledRuntimeModel::Ode(equation) => {
                executor::fit(equation, data, params, output_path)?
            }
            CompiledRuntimeModel::Analytical(equation) => {
                executor::fit(equation, data, params, output_path)?
            }
            CompiledRuntimeModel::Sde(_) => {
                return Err(BridgeError::new(
                    BridgeStage::Runtime,
                    "fit_model_kind_unsupported",
                    "SDE models are not supported for fitting in this Pmetrics runtime",
                ));
            }
        };

        serde_json::to_string(&payload).map_err(|error| {
            BridgeError::from_anyhow(
                BridgeStage::Handoff,
                "fit_payload_serialize_failed",
                anyhow!("Failed to serialize fit payload: {}", error),
            )
        })
    })()))
}

/// Checks if Cargo is installed on the system.
/// @return TRUE if Cargo is installed, FALSE otherwise.
/// @export
#[extendr]
fn is_cargo_installed() -> bool {
    Command::new("cargo").arg("--version").output().is_ok()
}

/// Retrieves the model parameters from compiled DSL metadata.
/// @param model_source DSL source code for the model.
/// @param kind Kind of model, which can either be "ODE" or "Analytical".
/// @return List of model parameters.
/// @export
#[extendr]
fn model_parameters(model_source: &str, kind: &str) -> Vec<String> {
    extendr_api::error::unwrap_or_throw_error(to_r_result((|| -> BridgeResult<Vec<String>> {
        match compile_runtime_model(model_source, kind, None)? {
            CompiledRuntimeModel::Ode(equation) => executor::model_parameters(&equation),
            CompiledRuntimeModel::Analytical(equation) => executor::model_parameters(&equation),
            CompiledRuntimeModel::Sde(_) => Err(BridgeError::new(
                BridgeStage::Runtime,
                "parameter_inspection_model_kind_unsupported",
                "SDE models are not supported for parameter inspection in this Pmetrics runtime",
            )),
        }
    })()))
}

/// Initialize the tracing subscriber with the custom R formatter
/// @keywords internal
/// @export
fn setup_logs_inner(controls: &LogControls) -> AnyResult<()> {
    use tracing_subscriber::filter::LevelFilter;

    let subscriber = tracing_subscriber::registry()
        .with(RFormatLayer::new())
        .with(LevelFilter::TRACE);

    let _ = tracing::subscriber::set_global_default(subscriber);
    configure_logs(controls)?;

    Ok(())
}

fn parse_log_level(value: &str) -> AnyResult<LoggingLevel> {
    match value.to_ascii_uppercase().as_str() {
        "ERROR" => Ok(LoggingLevel::Error),
        "WARN" => Ok(LoggingLevel::Warn),
        "INFO" => Ok(LoggingLevel::Info),
        "DEBUG" => Ok(LoggingLevel::Debug),
        "TRACE" => Ok(LoggingLevel::Trace),
        other => Err(anyhow!(
            "Unsupported log_level '{}'. Expected one of ERROR, WARN, INFO, DEBUG, TRACE",
            other
        )),
    }
}

fn parse_log_controls(params: &List, output_path: &str) -> AnyResult<LogControls> {
    let settings: std::collections::HashMap<&str, Robj> =
        std::collections::HashMap::try_from(params)
            .map_err(|e| anyhow!("Failed to convert settings list to map: {}", e))?;

    let stdout = match settings.get("stdout_logs") {
        Some(value) => value
            .as_logical()
            .ok_or_else(|| anyhow!("Setting 'stdout_logs' is not logical"))?
            .to_bool(),
        None => true,
    };

    let write = match settings.get("write_logs") {
        Some(value) => value
            .as_logical()
            .ok_or_else(|| anyhow!("Setting 'write_logs' is not logical"))?
            .to_bool(),
        None => false,
    };

    let level = match settings.get("log_level") {
        Some(value) => {
            let raw = value
                .as_str()
                .ok_or_else(|| anyhow!("Setting 'log_level' is not a string"))?;
            parse_log_level(raw)?
        }
        None => LoggingLevel::Info,
    };

    Ok(LogControls {
        level,
        stdout,
        write,
        output_path: Some(std::path::PathBuf::from(output_path)),
    })
}

#[extendr]
fn setup_logs() {
    let controls = LogControls::default();
    extendr_api::error::unwrap_or_throw_error(to_r_result(
        setup_logs_inner(&controls)
            .map_err(|error| BridgeError::from_anyhow(BridgeStage::Settings, "log_setup_failed", error)),
    ))
}

#[extendr]
fn start_live_session() -> List {
    extendr_api::error::unwrap_or_throw_error(to_r_result(
        live::start_live_session().map_err(|error| {
            BridgeError::from_anyhow(BridgeStage::Handoff, "live_session_start_failed", error)
        }),
    ))
}

#[extendr]
fn wait_live_session_connected(session_id: &str, timeout_ms: i32) -> bool {
    extendr_api::error::unwrap_or_throw_error(to_r_result(live::wait_live_session_connected(
        session_id, timeout_ms,
    )
    .map_err(|error| {
        BridgeError::from_anyhow(
            BridgeStage::Handoff,
            "live_session_wait_failed",
            error,
        )
    })))
}

#[extendr]
fn close_live_session(session_id: &str) {
    extendr_api::error::unwrap_or_throw_error(to_r_result(
        live::close_live_session(session_id).map_err(|error| {
            BridgeError::from_anyhow(BridgeStage::Handoff, "live_session_close_failed", error)
        }),
    ))
}

#[extendr]
fn publish_live_report_result(session_id: &str, result_payload: &str, report_generated_at: &str) {
    extendr_api::error::unwrap_or_throw_error(to_r_result(live::publish_live_report_result(
        session_id,
        result_payload,
        report_generated_at,
    )
    .map_err(|error| {
        BridgeError::from_anyhow(
            BridgeStage::Handoff,
            "live_report_publish_failed",
            error,
        )
    })))
}

#[extendr]
fn publish_live_report_failed(session_id: &str, message: &str) {
    extendr_api::error::unwrap_or_throw_error(to_r_result(live::publish_live_report_failed(
        session_id, message,
    )
    .map_err(|error| {
        BridgeError::from_anyhow(
            BridgeStage::Handoff,
            "live_report_failure_publish_failed",
            error,
        )
    })))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bridge_error_compile_stage_tags_empty_source() {
        let error = compile_runtime_model("", "ode", None).expect_err("empty source should fail");

        assert_eq!(error.stage(), BridgeStage::Compile);
        assert_eq!(error.code(), "model_source_empty");
        assert_eq!(error.message(), "Model source is empty");
    }

    #[test]
    fn bridge_error_data_stage_tags_missing_data_path() {
        let error = validate_data_path("/definitely/missing/path.csv")
            .expect_err("missing path should fail");

        assert_eq!(error.stage(), BridgeStage::Data);
        assert_eq!(error.code(), "data_path_missing");
    }

    #[test]
    fn bridge_error_settings_stage_tags_invalid_log_level() {
        let error = parse_log_level("nope").map_err(|error| {
            BridgeError::from_anyhow(BridgeStage::Settings, "log_controls_invalid", error)
        });
        let error = error.expect_err("invalid log level should fail");

        assert_eq!(error.stage(), BridgeStage::Settings);
        assert_eq!(error.code(), "log_controls_invalid");
    }
}

extendr_module! {
    mod Pmetrics;
    fn simulate_one;
    fn simulate_all;
    fn validate_model_source;
    fn is_cargo_installed;
    fn fit;
    fn model_parameters;
    fn setup_logs;
    fn start_live_session;
    fn wait_live_session_connected;
    fn close_live_session;
    fn publish_live_report_result;
    fn publish_live_report_failed;

}
