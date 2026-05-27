use anyhow::{anyhow, Result};
use pmcore::estimation::nonparametric::{median, NonparametricWorkspace};
use pmcore::prelude::{AssayErrorModel, Equation, Event};
use pmcore::results::FitResult;
use serde::Serialize;
use serde_json::{Map, Value};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::Path;

const FIT_RESULT_SCHEMA_VERSION: u32 = 1;
const MANIFEST_FILE_NAME: &str = "fit_manifest.json";
const POSTERIOR_MANIFEST_ROW_LIMIT: usize = 100_000;
const PREDICTION_MANIFEST_ROW_LIMIT: usize = 500_000;
const INLINE_JSON_LIMIT_BYTES: usize = 50 * 1024 * 1024;

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitPayload {
    pub(crate) parameter_names: Vec<String>,
    pub(crate) covariate_names: Vec<String>,
    pub(crate) iterations: Vec<Map<String, Value>>,
    pub(crate) theta: Vec<Map<String, Value>>,
    pub(crate) posterior: Vec<Map<String, Value>>,
    pub(crate) predictions: Vec<PredictionRow>,
    pub(crate) covariates: Vec<Map<String, Value>>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitPayloadConfig {
    pub(crate) parameters: Vec<Vec<FitPayloadParameter>>,
    pub(crate) errormodels: FitPayloadErrorModels,
    pub(crate) prior: Option<HashMap<String, Vec<f64>>>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitPayloadParameter {
    pub(crate) name: String,
    pub(crate) lower: f64,
    pub(crate) upper: f64,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitPayloadErrorModels {
    pub(crate) models: Vec<HashMap<String, FitPayloadErrorModel>>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitPayloadErrorModel {
    pub(crate) poly: FitPayloadPoly,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitPayloadPoly {
    pub(crate) c0: f64,
    pub(crate) c1: f64,
    pub(crate) c2: f64,
    pub(crate) c3: f64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum FitTransportMode {
    Inline,
    Manifest,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitResultEnvelope {
    pub(crate) kind: &'static str,
    pub(crate) schema_version: u32,
    pub(crate) transport: FitTransportMode,
    pub(crate) config: FitPayloadConfig,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) payload: Option<FitPayload>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) manifest: Option<FitManifest>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitManifest {
    pub(crate) generator: FitManifestGenerator,
    pub(crate) tables: FitManifestTables,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitManifestGenerator {
    pub(crate) name: &'static str,
    pub(crate) version: &'static str,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitManifestTables {
    pub(crate) iterations: FitManifestTable,
    pub(crate) theta: FitManifestTable,
    pub(crate) posterior: FitManifestTable,
    pub(crate) predictions: FitManifestTable,
    pub(crate) covariates: FitManifestTable,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct FitManifestTable {
    pub(crate) path: String,
    pub(crate) row_count: usize,
    pub(crate) sha256: String,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct PredictionRow {
    id: String,
    time: f64,
    outeq: usize,
    block: usize,
    obs: Option<f64>,
    cens: String,
    pop_mean: f64,
    pop_median: f64,
    post_mean: f64,
    post_median: f64,
}

impl FitResultEnvelope {
    fn inline(config: FitPayloadConfig, payload: FitPayload) -> Self {
        Self {
            kind: "ok",
            schema_version: FIT_RESULT_SCHEMA_VERSION,
            transport: FitTransportMode::Inline,
            config,
            payload: Some(payload),
            manifest: None,
        }
    }

    fn manifest(config: FitPayloadConfig, manifest: FitManifest) -> Self {
        Self {
            kind: "ok",
            schema_version: FIT_RESULT_SCHEMA_VERSION,
            transport: FitTransportMode::Manifest,
            config,
            payload: None,
            manifest: Some(manifest),
        }
    }
}

pub(crate) fn from_fit_result<E>(
    result: &mut FitResult<E>,
    config: FitPayloadConfig,
    output_path: &str,
    idelta: f64,
    tad: f64,
) -> Result<FitResultEnvelope>
where
    E: Equation,
{
    match result {
        FitResult::Nonparametric(workspace) => {
            from_nonparametric_workspace(workspace, config, output_path, idelta, tad)
        }
    }
}

fn from_nonparametric_workspace<E>(
    workspace: &mut NonparametricWorkspace<E>,
    config: FitPayloadConfig,
    output_path: &str,
    idelta: f64,
    tad: f64,
) -> Result<FitResultEnvelope>
where
    E: Equation,
{
    let parameter_names = workspace.get_theta().param_names();
    let covariate_names = collect_covariate_names(workspace);

    if workspace.predictions().is_none() {
        workspace.calculate_predictions(idelta, tad)?;
    }

    let predictions = workspace
        .predictions()
        .ok_or_else(|| anyhow!("fit predictions were not materialized"))?;

    build_transport(
        FitPayload {
        parameter_names: parameter_names.clone(),
        covariate_names: covariate_names.clone(),
        iterations: iteration_rows(workspace, &parameter_names)?,
        theta: theta_rows(workspace, &parameter_names),
        posterior: posterior_rows(workspace, &parameter_names),
        predictions: prediction_rows(predictions),
        covariates: covariate_rows(workspace, &covariate_names),
        },
        config,
        output_path,
    )
}

fn build_transport(
    payload: FitPayload,
    config: FitPayloadConfig,
    output_path: &str,
) -> Result<FitResultEnvelope> {
    match select_transport(&payload, &config)? {
        FitTransportMode::Inline => {
            let _ = fs::remove_file(Path::new(output_path).join(MANIFEST_FILE_NAME));
            Ok(FitResultEnvelope::inline(config, payload))
        }
        FitTransportMode::Manifest => build_manifest_transport(payload, config, output_path),
    }
}

fn select_transport(payload: &FitPayload, config: &FitPayloadConfig) -> Result<FitTransportMode> {
    if payload.posterior.len() >= POSTERIOR_MANIFEST_ROW_LIMIT
        || payload.predictions.len() >= PREDICTION_MANIFEST_ROW_LIMIT
    {
        return Ok(FitTransportMode::Manifest);
    }

    if count_inline_envelope_bytes(payload, config)? >= INLINE_JSON_LIMIT_BYTES {
        return Ok(FitTransportMode::Manifest);
    }

    Ok(FitTransportMode::Inline)
}

fn count_inline_envelope_bytes(payload: &FitPayload, config: &FitPayloadConfig) -> Result<usize> {
    #[derive(Serialize)]
    struct InlineEnvelopeRef<'a> {
        kind: &'static str,
        schema_version: u32,
        transport: &'static str,
        config: &'a FitPayloadConfig,
        payload: &'a FitPayload,
    }

    let mut writer = CountingWriter::default();
    serde_json::to_writer(
        &mut writer,
        &InlineEnvelopeRef {
            kind: "ok",
            schema_version: FIT_RESULT_SCHEMA_VERSION,
            transport: "inline",
            config,
            payload,
        },
    )
    .map_err(|error| anyhow!("Failed to estimate inline fit payload size: {}", error))?;
    Ok(writer.bytes)
}

fn build_manifest_transport(
    payload: FitPayload,
    config: FitPayloadConfig,
    output_path: &str,
) -> Result<FitResultEnvelope> {
    fs::create_dir_all(output_path)?;

    let tables = write_manifest_tables(&payload, &config, output_path)?;
    let envelope = FitResultEnvelope::manifest(
        config,
        FitManifest {
            generator: FitManifestGenerator {
                name: env!("CARGO_PKG_NAME"),
                version: env!("CARGO_PKG_VERSION"),
            },
            tables,
        },
    );

    let manifest_path = Path::new(output_path).join(MANIFEST_FILE_NAME);
    fs::write(
        manifest_path,
        serde_json::to_vec_pretty(&envelope)
            .map_err(|error| anyhow!("Failed to serialize fit manifest: {}", error))?,
    )?;

    Ok(envelope)
}

fn write_manifest_tables(
    payload: &FitPayload,
    config: &FitPayloadConfig,
    output_path: &str,
) -> Result<FitManifestTables> {
    Ok(FitManifestTables {
        iterations: write_map_table(
            output_path,
            "iterations.csv",
            &iteration_columns(payload, config),
            &payload.iterations,
        )?,
        theta: write_map_table(output_path, "theta.csv", &theta_columns(payload), &payload.theta)?,
        posterior: write_map_table(
            output_path,
            "posterior.csv",
            &posterior_columns(payload),
            &payload.posterior,
        )?,
        predictions: write_predictions_table(output_path, "predictions.csv", &payload.predictions)?,
        covariates: write_map_table(
            output_path,
            "covariates.csv",
            &covariate_columns(payload),
            &payload.covariates,
        )?,
    })
}

fn iteration_columns(payload: &FitPayload, config: &FitPayloadConfig) -> Vec<String> {
    let mut columns = vec![
        "cycle".to_string(),
        "converged".to_string(),
        "status".to_string(),
        "neg2ll".to_string(),
        "nspp".to_string(),
    ];

    for index in 0..config.errormodels.models.len() {
        columns.push(format!("gamlam.{index}"));
    }

    for parameter_name in &payload.parameter_names {
        columns.push(format!("{parameter_name}.mean"));
        columns.push(format!("{parameter_name}.median"));
        columns.push(format!("{parameter_name}.sd"));
    }

    columns
}

fn theta_columns(payload: &FitPayload) -> Vec<String> {
    let mut columns = payload.parameter_names.clone();
    columns.push("prob".to_string());
    columns
}

fn posterior_columns(payload: &FitPayload) -> Vec<String> {
    let mut columns = vec!["id".to_string(), "point".to_string()];
    columns.extend(payload.parameter_names.clone());
    columns.push("prob".to_string());
    columns
}

fn covariate_columns(payload: &FitPayload) -> Vec<String> {
    let mut columns = vec!["id".to_string(), "time".to_string(), "block".to_string()];
    columns.extend(payload.covariate_names.clone());
    columns
}

fn write_map_table(
    output_path: &str,
    file_name: &str,
    columns: &[String],
    rows: &[Map<String, Value>],
) -> Result<FitManifestTable> {
    let path = Path::new(output_path).join(file_name);
    let hashing_writer = HashingWriter::new(BufWriter::new(File::create(&path)?));
    let mut writer = csv::Writer::from_writer(hashing_writer);

    writer.write_record(columns)?;
    for row in rows {
        let record = columns
            .iter()
            .map(|column| json_value_to_csv_cell(row.get(column).unwrap_or(&Value::Null)))
            .collect::<Vec<_>>();
        writer.write_record(record)?;
    }

    let hashing_writer = writer
        .into_inner()
        .map_err(|error| anyhow!("Failed to finalize manifest CSV {}: {}", file_name, error.error()))?;

    Ok(FitManifestTable {
        path: file_name.to_string(),
        row_count: rows.len(),
        sha256: hashing_writer.finish_hex()?,
    })
}

fn write_predictions_table(
    output_path: &str,
    file_name: &str,
    rows: &[PredictionRow],
) -> Result<FitManifestTable> {
    let columns = [
        "id",
        "time",
        "outeq",
        "block",
        "obs",
        "cens",
        "pop_mean",
        "pop_median",
        "post_mean",
        "post_median",
    ];
    let path = Path::new(output_path).join(file_name);
    let hashing_writer = HashingWriter::new(BufWriter::new(File::create(&path)?));
    let mut writer = csv::Writer::from_writer(hashing_writer);

    writer.write_record(columns)?;
    for row in rows {
        writer.write_record([
            row.id.clone(),
            row.time.to_string(),
            row.outeq.to_string(),
            row.block.to_string(),
            row.obs.map(|value| value.to_string()).unwrap_or_default(),
            row.cens.clone(),
            row.pop_mean.to_string(),
            row.pop_median.to_string(),
            row.post_mean.to_string(),
            row.post_median.to_string(),
        ])?;
    }

    let hashing_writer = writer
        .into_inner()
        .map_err(|error| anyhow!("Failed to finalize manifest CSV {}: {}", file_name, error.error()))?;

    Ok(FitManifestTable {
        path: file_name.to_string(),
        row_count: rows.len(),
        sha256: hashing_writer.finish_hex()?,
    })
}

fn json_value_to_csv_cell(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(value) => value.to_string(),
        Value::Number(value) => value.to_string(),
        Value::String(value) => value.clone(),
        other => serde_json::to_string(other).unwrap_or_default(),
    }
}

#[derive(Default)]
struct CountingWriter {
    bytes: usize,
}

impl Write for CountingWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.bytes += buf.len();
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

struct HashingWriter<W> {
    inner: W,
    hasher: Sha256,
}

impl<W> HashingWriter<W> {
    fn new(inner: W) -> Self {
        Self {
            inner,
            hasher: Sha256::new(),
        }
    }
}

impl<W: Write> HashingWriter<W> {
    fn finish_hex(mut self) -> Result<String> {
        self.inner.flush()?;
        Ok(format!("{:x}", self.hasher.finalize()))
    }
}

impl<W: Write> Write for HashingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.hasher.update(buf);
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

fn collect_covariate_names<E>(workspace: &NonparametricWorkspace<E>) -> Vec<String>
where
    E: Equation,
{
    let mut names = workspace
        .data()
        .subjects()
        .iter()
        .flat_map(|subject| subject.occasions().iter())
        .flat_map(|occasion| {
            occasion
                .covariates()
                .covariates()
                .keys()
                .cloned()
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    names.sort();
    names.dedup();
    names
}

fn iteration_rows<E>(
    workspace: &NonparametricWorkspace<E>,
    parameter_names: &[String],
) -> Result<Vec<Map<String, Value>>>
where
    E: Equation,
{
    workspace
        .cycle_log()
        .cycles()
        .iter()
        .map(|cycle| {
            let mut row = Map::new();
            row.insert("cycle".to_string(), Value::from(cycle.cycle() as u64));
            row.insert(
                "converged".to_string(),
                Value::from(cycle.status().to_string() == "Converged"),
            );
            row.insert("status".to_string(), Value::from(cycle.status().to_string()));
            row.insert("neg2ll".to_string(), Value::from(cycle.objf()));
            row.insert("nspp".to_string(), Value::from(cycle.theta().nspp() as u64));

            for (outeq, error_model) in cycle.error_models().iter() {
                if matches!(error_model, AssayErrorModel::None) {
                    continue;
                }

                let factor = (error_model.factor()? * 100000.0).round() / 100000.0;
                row.insert(format!("gamlam.{outeq}"), Value::from(factor));
            }

            for (column_index, parameter_name) in parameter_names.iter().enumerate() {
                let values = cycle
                    .theta()
                    .matrix()
                    .col(column_index)
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();
                let mean = values.iter().sum::<f64>() / values.len() as f64;
                let variance = values.iter().map(|value| (value - mean).powi(2)).sum::<f64>()
                    / (values.len() as f64 - 1.0);

                row.insert(format!("{parameter_name}.mean"), Value::from(mean));
                row.insert(
                    format!("{parameter_name}.median"),
                    Value::from(median(&values)),
                );
                row.insert(format!("{parameter_name}.sd"), Value::from(variance));
            }

            Ok(row)
        })
        .collect()
}

fn theta_rows<E>(workspace: &NonparametricWorkspace<E>, parameter_names: &[String]) -> Vec<Map<String, Value>>
where
    E: Equation,
{
    workspace
        .get_theta()
        .matrix()
        .row_iter()
        .zip(workspace.weights().iter())
        .map(|(theta_row, weight)| {
            let mut row = Map::new();
            for (parameter_name, value) in parameter_names.iter().zip(theta_row.iter()) {
                row.insert(parameter_name.clone(), Value::from(*value));
            }
            row.insert("prob".to_string(), Value::from(weight));
            row
        })
        .collect()
}

fn posterior_rows<E>(
    workspace: &NonparametricWorkspace<E>,
    parameter_names: &[String],
) -> Vec<Map<String, Value>>
where
    E: Equation,
{
    let subjects = workspace.data().subjects();

    workspace
        .posterior()
        .matrix()
        .row_iter()
        .enumerate()
        .flat_map(|(subject_index, posterior_row)| {
            let id = subjects[subject_index].id().clone();
            posterior_row.iter().enumerate().map(move |(point_index, probability)| {
                let mut row = Map::new();
                row.insert("id".to_string(), Value::from(id.clone()));
                row.insert("point".to_string(), Value::from(point_index as u64));

                for (parameter_name, value) in parameter_names.iter().zip(
                    workspace.get_theta().matrix().row(point_index).iter(),
                ) {
                    row.insert(parameter_name.clone(), Value::from(*value));
                }

                row.insert("prob".to_string(), Value::from(*probability));
                row
            })
        })
        .collect()
}

fn prediction_rows(
    predictions: &pmcore::estimation::nonparametric::NPPredictions,
) -> Vec<PredictionRow> {
    predictions
        .predictions()
        .iter()
        .map(|row| PredictionRow {
            id: row.id().to_string(),
            time: row.time(),
            outeq: row.outeq(),
            block: row.block(),
            obs: row.obs(),
            cens: match row.censoring() {
                pmcore::prelude::Censor::None => "none".to_string(),
                pmcore::prelude::Censor::BLOQ => "bloq".to_string(),
                pmcore::prelude::Censor::ALOQ => "aloq".to_string(),
            },
            pop_mean: row.pop_mean(),
            pop_median: row.pop_median(),
            post_mean: row.post_mean(),
            post_median: row.post_median(),
        })
        .collect()
}

fn covariate_rows<E>(
    workspace: &NonparametricWorkspace<E>,
    covariate_names: &[String],
) -> Vec<Map<String, Value>>
where
    E: Equation,
{
    workspace
        .data()
        .subjects()
        .iter()
        .flat_map(|subject| {
            subject.occasions().iter().flat_map(move |occasion| {
                let covariates = occasion.covariates().covariates();

                occasion.iter().map(move |event| {
                    let time = match event {
                        Event::Bolus(bolus) => bolus.time(),
                        Event::Infusion(infusion) => infusion.time(),
                        Event::Observation(observation) => observation.time(),
                    };

                    let mut row = Map::new();
                    row.insert("id".to_string(), Value::from(subject.id().clone()));
                    row.insert("time".to_string(), Value::from(time));
                    row.insert("block".to_string(), Value::from(occasion.index() as u64));

                    for covariate_name in covariate_names {
                        let value = covariates
                            .get(covariate_name)
                            .and_then(|covariate| covariate.interpolate(time).ok());

                        row.insert(
                            covariate_name.clone(),
                            value.map(Value::from).unwrap_or(Value::Null),
                        );
                    }

                    row
                })
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn sample_config() -> FitPayloadConfig {
        let mut error_model = HashMap::new();
        error_model.insert(
            "Additive".to_string(),
            FitPayloadErrorModel {
                poly: FitPayloadPoly {
                    c0: 0.0,
                    c1: 0.1,
                    c2: 0.0,
                    c3: 0.0,
                },
            },
        );

        let mut prior = HashMap::new();
        prior.insert("Sobol".to_string(), vec![20.0]);

        FitPayloadConfig {
            parameters: vec![vec![
                FitPayloadParameter {
                    name: "ke".to_string(),
                    lower: 0.1,
                    upper: 1.0,
                },
                FitPayloadParameter {
                    name: "v".to_string(),
                    lower: 1.0,
                    upper: 10.0,
                },
            ]],
            errormodels: FitPayloadErrorModels {
                models: vec![error_model],
            },
            prior: Some(prior),
        }
    }

    fn sample_payload(posterior_rows: usize) -> FitPayload {
        FitPayload {
            parameter_names: vec!["ke".to_string(), "v".to_string()],
            covariate_names: vec!["cov1".to_string()],
            iterations: vec![Map::from_iter([
                ("cycle".to_string(), Value::from(1_u64)),
                ("converged".to_string(), Value::from(false)),
                ("status".to_string(), Value::from("payload")),
                ("neg2ll".to_string(), Value::from(123.0)),
                ("nspp".to_string(), Value::from(2_u64)),
                ("gamlam.0".to_string(), Value::from(0.5)),
                ("ke.mean".to_string(), Value::from(2.0)),
                ("ke.median".to_string(), Value::from(2.0)),
                ("ke.sd".to_string(), Value::from(0.25)),
                ("v.mean".to_string(), Value::from(4.0)),
                ("v.median".to_string(), Value::from(4.0)),
                ("v.sd".to_string(), Value::from(0.5)),
            ])],
            theta: vec![
                Map::from_iter([
                    ("ke".to_string(), Value::from(2.0)),
                    ("v".to_string(), Value::from(4.0)),
                    ("prob".to_string(), Value::from(0.6)),
                ]),
                Map::from_iter([
                    ("ke".to_string(), Value::from(3.0)),
                    ("v".to_string(), Value::from(5.0)),
                    ("prob".to_string(), Value::from(0.4)),
                ]),
            ],
            posterior: (0..posterior_rows)
                .map(|index| {
                    Map::from_iter([
                        ("id".to_string(), Value::from("1")),
                        ("point".to_string(), Value::from(index as u64)),
                        ("ke".to_string(), Value::from(2.0)),
                        ("v".to_string(), Value::from(4.0)),
                        ("prob".to_string(), Value::from(0.7)),
                    ])
                })
                .collect(),
            predictions: vec![PredictionRow {
                id: "1".to_string(),
                time: 0.0,
                outeq: 0,
                block: 0,
                obs: Some(10.0),
                cens: "none".to_string(),
                pop_mean: 11.0,
                pop_median: 12.0,
                post_mean: 13.0,
                post_median: 14.0,
            }],
            covariates: vec![Map::from_iter([
                ("id".to_string(), Value::from("1")),
                ("time".to_string(), Value::from(0.0)),
                ("block".to_string(), Value::from(0_u64)),
                ("cov1".to_string(), Value::from(5.0)),
            ])],
        }
    }

    fn output_dir(label: &str) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!(
            "pm-rs-fit-payload-{label}-{}-{nanos}",
            std::process::id()
        ));
        fs::create_dir_all(&path).expect("test output directory should be created");
        path.to_string_lossy().into_owned()
    }

    #[test]
    fn payload_switches_to_manifest_at_posterior_threshold() -> Result<()> {
        let output_path = output_dir("manifest-threshold");
        let envelope = build_transport(sample_payload(POSTERIOR_MANIFEST_ROW_LIMIT), sample_config(), &output_path)?;

        assert_eq!(envelope.transport, FitTransportMode::Manifest);
        assert!(envelope.payload.is_none());
        let manifest = envelope.manifest.expect("manifest transport should include a manifest");
        assert_eq!(manifest.tables.posterior.row_count, POSTERIOR_MANIFEST_ROW_LIMIT);
        assert!(Path::new(&output_path).join(MANIFEST_FILE_NAME).exists());
        assert!(Path::new(&output_path).join("posterior.csv").exists());
        Ok(())
    }
}