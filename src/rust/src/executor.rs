use crate::bridge_errors::{BridgeError, BridgeResult, BridgeStage};
use crate::fit_payload::{self, FitResultEnvelope};
use crate::live;
use crate::settings::{fit_payload_config, live_session_id, runtime_interval, settings};
use extendr_api::List;

use pmcore::prelude::{simulator::SubjectPredictions, Predictions, *};

use crate::simulation::SimulationRow;

pub(crate) fn model_parameters<E: EquationMetadataSource>(equation: &E) -> BridgeResult<Vec<String>> {
    let metadata = equation
        .equation_metadata()
        .ok_or_else(|| {
            BridgeError::new(
                BridgeStage::Runtime,
                "runtime_metadata_missing",
                "runtime model metadata is required",
            )
        })?;

    Ok(metadata
        .parameters()
        .iter()
        .map(|parameter| parameter.name().to_string())
        .collect())
}

pub(crate) fn simulate<E>(
    equation: &E,
    subject: &Subject,
    parameters: &Parameters,
    spp_index: usize,
) -> BridgeResult<Vec<SimulationRow>>
where
    E: Equation + EquationMetadataSource,
{
    let metadata = equation
        .equation_metadata()
        .ok_or_else(|| {
            BridgeError::new(
                BridgeStage::Runtime,
                "runtime_metadata_missing",
                "runtime model metadata is required",
            )
        })?;

    if metadata.parameters().len() != parameters.as_slice().len() {
        return Err(
            BridgeError::new(
                BridgeStage::Runtime,
                "support_point_parameter_count_mismatch",
                format!(
                    "Support point has {} values but model expects {} parameters",
                    parameters.as_slice().len(),
                    metadata.parameters().len()
                ),
            )
            .with_detail("provided_parameter_count", parameters.as_slice().len())
            .with_detail("expected_parameter_count", metadata.parameters().len()),
        );
    }

    let predictions: SubjectPredictions = equation
        .estimate_predictions(subject, parameters)
        .map_err(|error| {
            BridgeError::from_anyhow(
                BridgeStage::Runtime,
                "prediction_estimation_failed",
                error,
            )
        })?
        .get_predictions()
        .into();
    Ok(SimulationRow::from_subject_predictions(
        predictions,
        subject.id(),
        spp_index,
    ))
}

pub(crate) fn fit<E>(
    equation: E,
    data: Data,
    params: List,
    output_path: &str,
) -> BridgeResult<FitResultEnvelope>
where
    E: Equation + Clone + Send + 'static + EquationMetadataSource,
{
    let live_session_id = live_session_id(&params).map_err(|error| {
        BridgeError::from_anyhow(BridgeStage::Settings, "live_session_settings_invalid", error)
    })?;
    let (idelta, tad) = runtime_interval(&params).map_err(|error| {
        BridgeError::from_anyhow(BridgeStage::Settings, "runtime_interval_invalid", error)
    })?;
    let parameter_names = model_parameters(&equation)?;
    let transport_config = fit_payload_config(&params, &parameter_names).map_err(|error| {
        BridgeError::from_anyhow(BridgeStage::Settings, "fit_settings_invalid", error)
    })?;
    let builder = settings(params, equation, data, output_path)
        .map_err(|error| BridgeError::from_anyhow(BridgeStage::Settings, "fit_settings_invalid", error))?;

    if let Some(session_id) = live_session_id {
        let session = live::session(&session_id).map_err(|error| {
            BridgeError::from_anyhow(BridgeStage::Handoff, "live_session_not_available", error)
        })?;
        let live_result = builder.fit_with_progress_and_control_in_memory(
            |event| {
                if let Err(error) = session.publish_progress(event) {
                    tracing::warn!("live session progress publish failed: {}", error);
                }
            },
            || session.next_control(),
        );

        match live_result {
            Ok(mut result) => {
                let payload = fit_payload::from_fit_result(
                    &mut result,
                    transport_config,
                    output_path,
                    idelta,
                    tad,
                )
                .map_err(|error| {
                    BridgeError::from_anyhow(
                        BridgeStage::Handoff,
                        "fit_payload_build_failed",
                        error,
                    )
                })?;
                return Ok(payload);
            }
            Err(error) => {
                let _ = session.publish_fit_failed(&error.to_string());
                return Err(BridgeError::from_anyhow(
                    BridgeStage::Runtime,
                    "fit_execution_failed",
                    error,
                ));
            }
        }
    }

    let mut result = builder.fit_in_memory().map_err(|error| {
        BridgeError::from_anyhow(BridgeStage::Runtime, "fit_execution_failed", error)
    })?;
    let payload = fit_payload::from_fit_result(&mut result, transport_config, output_path, idelta, tad)
        .map_err(|error| {
        BridgeError::from_anyhow(BridgeStage::Handoff, "fit_payload_build_failed", error)
    })?;
    // write_outputs() removed: output writing is now explicit, not automatic
    Ok(payload)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fit_payload;
    use crate::live;
    use extendr_api::prelude::*;
    use std::fs;
    use std::io::{BufRead, BufReader};
    use std::net::TcpStream;
    use std::sync::Once;
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    fn ensure_r() {
        static START_R: Once = Once::new();
        START_R.call_once(|| {
            extendr_engine::start_r();
        });
    }

    fn equation() -> equation::ODE {
        equation::ODE::new(
            |x, p, _t, dx, b, _rateiv, _cov| {
                fetch_params!(p, ke);
                dx[0] = -ke * x[0] + b[0];
            },
            |_p, _t, _cov| lag! {},
            |_p, _t, _cov| fa! {},
            |_p, _t, _cov, _x| {},
            |x, p, _t, _cov, y| {
                fetch_params!(p, v);
                y[0] = x[0] / v;
            },
        )
        .with_nstates(1)
        .with_ndrugs(1)
        .with_nout(1)
        .with_metadata(
            equation::metadata::new("executor_live_fit")
                .parameters(["ke", "v"])
                .states(["central"])
                .outputs(["0"])
                .route(equation::Route::bolus("0").to_state("central")),
        )
        .expect("executor live-fit test metadata should validate")
    }

    fn data() -> Data {
        Data::new(vec![Subject::builder("1")
            .bolus(0.0, 100.0, 0)
            .observation(1.0, 10.0, 0)
            .observation(2.0, 7.0, 0)
            .build()])
    }

    fn params(live_session_id: Option<&str>) -> List {
        ensure_r();

        let error_model = List::from_pairs(vec![
            ("initial", r!(2.0)),
            ("type", r!(["additive"])),
            ("fixed", r!(false)),
            ("coeff", r!([0.0, 0.10, 0.0, 0.0])),
        ]);

        let mut pairs = vec![
            (
                "ranges",
                list!(ke = r!([0.05, 1.0]), v = r!([5.0, 50.0])).into(),
            ),
            ("algorithm", r!("NPAG")),
            ("error_models", List::from_values(vec![error_model]).into()),
            ("max_cycles", r!(2.0)),
            ("prior", r!("sobol")),
            ("points", r!(8.0)),
            ("seed", r!(7.0)),
            ("cache", r!(false)),
            ("progress", r!(false)),
        ];

        if let Some(session_id) = live_session_id {
            pairs.push(("live_session_id", r!(session_id)));
        }

        List::from_pairs(pairs)
    }

    fn output_dir(label: &str) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!(
            "pm-rs-executor-{label}-{}-{nanos}",
            std::process::id()
        ));
        fs::create_dir_all(&path).expect("test output directory should be created");
        path.to_string_lossy().into_owned()
    }

    #[test]
    fn fit_streams_cycle_events_to_live_session() -> Result<()> {
        let session = live::create_live_session()?;
        let stream = TcpStream::connect(("127.0.0.1", session.port()))?;
        stream.set_read_timeout(Some(Duration::from_secs(5)))?;
        let reader_handle = std::thread::spawn(move || -> Result<Vec<String>> {
            let mut reader = BufReader::new(stream);
            let mut lines = Vec::new();

            for _ in 0..32 {
                let mut line = String::new();
                match reader.read_line(&mut line) {
                    Ok(0) => break,
                    Ok(_) => {
                        let line = line.trim();
                        if !line.is_empty() {
                            lines.push(line.to_string());
                        }
                    }
                    Err(error)
                        if matches!(
                            error.kind(),
                            std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                        ) =>
                    {
                        break;
                    }
                    Err(error) => return Err(error.into()),
                }
            }

            Ok(lines)
        });

        let output_path = output_dir("live-fit");
        let payload = fit(equation(), data(), params(Some(session.id())), &output_path)?;

        assert_eq!(payload.transport, fit_payload::FitTransportMode::Inline);
        assert!(!payload
            .payload
            .as_ref()
            .expect("inline transport should keep the payload in memory")
            .predictions
            .is_empty());

        let backlog = session.backlog_snapshot();
        assert!(backlog.iter().any(|line| line.contains("\"fit_started\"")));

        live::close_live_session(session.id())?;
        let lines = reader_handle
            .join()
            .expect("live session reader thread should not panic")?;
        let saw_cycle = lines.iter().any(|line| {
            serde_json::from_str::<serde_json::Value>(line)
                .map(|message| {
                    message["kind"] == "progress"
                        && message["event"]["kind"] == "nonparametric_cycle"
                })
                .unwrap_or(false)
        });

        assert!(saw_cycle);
        assert!(std::path::Path::new(&output_path)
            .join("iterations.csv")
            .exists());
        Ok(())
    }

    #[test]
    fn fit_payload_builds_without_output_files() -> Result<()> {
        let output_path = output_dir("in-memory-payload");
        let builder = settings(params(None), equation(), data(), &output_path)?;
        let mut result = builder.fit_in_memory()?;
        let config = fit_payload_config(&params(None), &model_parameters(&equation())?)?;

        let payload = fit_payload::from_fit_result(&mut result, config, &output_path, 0.12, 0.0)?;

        assert_eq!(payload.transport, fit_payload::FitTransportMode::Inline);
        let payload = payload
            .payload
            .expect("inline transport should keep the payload in memory");

        assert!(!payload.iterations.is_empty());
        assert!(!payload.theta.is_empty());
        assert!(!payload.posterior.is_empty());
        assert!(!payload.predictions.is_empty());
        assert!(!std::path::Path::new(&output_path)
            .join("iterations.csv")
            .exists());
        assert!(!std::path::Path::new(&output_path)
            .join("settings.json")
            .exists());
        assert!(!std::path::Path::new(&output_path)
            .join("fit_manifest.json")
            .exists());

        Ok(())
    }
}
