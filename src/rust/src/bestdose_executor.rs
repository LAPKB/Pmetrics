use crate::{logs::RFormatLayer, settings::settings};
use extendr_api::prelude::*;
use pmcore::bestdose::{BestDoseProblem, BestDoseResult, DoseRange, Target};
use pmcore::prelude::{data, ODE};
use pmcore::routines::initialization::parse_prior;
use std::path::PathBuf;

/// Helper to parse target type from string
pub(crate) fn parse_target_type(target_str: &str) -> std::result::Result<Target, String> {
    match target_str.to_lowercase().as_str() {
        "concentration" => Ok(Target::Concentration),
        "auc_from_zero" | "auc" => Ok(Target::AUCFromZero),
        "auc_from_last_dose" | "auc_interval" => Ok(Target::AUCFromLastDose),
        _ => Err(format!(
            "Invalid target type: {}. Must be 'concentration', 'auc_from_zero', or 'auc_from_last_dose'",
            target_str
        )),
    }
}

/// R-compatible prediction row for BestDose output
#[derive(Debug, IntoDataFrameRow)]
pub struct BestDosePredictionRow {
    id: String,
    time: f64,
    observed: f64,
    pop_mean: f64,
    pop_median: f64,
    post_mean: f64,
    post_median: f64,
    outeq: usize,
}

impl BestDosePredictionRow {
    pub fn from_np_prediction(
        pred: &pmcore::routines::output::predictions::NPPredictionRow,
        id: &str,
    ) -> Self {
        Self {
            id: id.to_string(),
            time: pred.time(),
            observed: pred.obs().unwrap_or(0.0),
            pop_mean: pred.pop_mean(),
            pop_median: pred.pop_median(),
            post_mean: pred.post_mean(),
            post_median: pred.post_median(),
            outeq: pred.outeq(),
        }
    }
}

/// R-compatible AUC prediction row
#[derive(Debug, IntoDataFrameRow)]
pub struct BestDoseAucRow {
    time: f64,
    auc: f64,
}

/// Convert BestDoseResult to R-compatible list structure
pub(crate) fn convert_bestdose_result_to_r(
    result: BestDoseResult,
) -> std::result::Result<Robj, String> {
    // Extract doses
    let doses: Vec<f64> = result.doses();

    // Objective function
    let objf = result.objf();

    // Status
    let status_str = format!("{:?}", result.status());

    // Predictions as data frame
    let pred_rows: Vec<BestDosePredictionRow> = result
        .predictions()
        .predictions()
        .iter()
        .map(|p| BestDosePredictionRow::from_np_prediction(p, "subject_1"))
        .collect();
    let pred_df = pred_rows
        .into_dataframe()
        .map_err(|e| format!("Failed to create predictions dataframe: {:?}", e))?;

    // AUC predictions (if available)
    let auc_val = if let Some(auc_preds) = result.auc_predictions() {
        let auc_rows: Vec<BestDoseAucRow> = auc_preds
            .iter()
            .map(|(time, auc)| BestDoseAucRow {
                time: *time,
                auc: *auc,
            })
            .collect();
        let auc_df = auc_rows
            .into_dataframe()
            .map_err(|e| format!("Failed to create AUC dataframe: {:?}", e))?;
        Robj::from(auc_df)
    } else {
        Robj::from(()) // NULL for no AUC
    };

    // Optimization method
    let method_str = format!("{}", result.optimization_method());

    // Build the list using list! macro
    let output = list!(
        doses = doses,
        objf = objf,
        status = status_str,
        predictions = pred_df,
        auc_predictions = auc_val,
        method = method_str
    );

    Ok(output.into())
}
/// Opaque handle that keeps the dynamic model library alive while reusing the
/// prepared `BestDoseProblem` for multiple optimization runs.
pub struct BestDoseProblemHandle {
    problem: BestDoseProblem,
    #[allow(dead_code)]
    library: libloading::Library,
}

impl BestDoseProblemHandle {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        model_path: PathBuf,
        prior_path: PathBuf,
        past_data_path: Option<PathBuf>,
        target_data_path: PathBuf,
        time_offset: Option<f64>,
        dose_min: f64,
        dose_max: f64,
        bias_weight: f64,
        target_type: &str,
        params: List,
    ) -> std::result::Result<Self, String> {
        let (library, (eq, meta)) =
            unsafe { pmcore::prelude::pharmsol::exa::load::load::<ODE>(model_path) };

        let settings = settings(params, meta.get_params(), "/tmp/bestdose")
            .map_err(|e| format!("Failed to parse settings: {}", e))?;

        let (population_theta, prior_weights) =
            parse_prior(&prior_path.to_str().unwrap().to_string(), &settings)
                .map_err(|e| format!("Failed to parse prior: {}", e))?;

        let population_weights = prior_weights
            .ok_or_else(|| "Prior file must contain a 'prob' column with weights".to_string())?;

        let past_data = if let Some(path) = past_data_path {
            let data = data::read_pmetrics(path.to_str().unwrap())
                .map_err(|e| format!("Failed to read past data: {}", e))?;
            let subjects = data.subjects();
            if subjects.is_empty() {
                return Err("Past data file contains no subjects".to_string());
            }
            Some(subjects[0].clone())
        } else {
            None
        };

        let target_data = {
            let data = data::read_pmetrics(target_data_path.to_str().unwrap())
                .map_err(|e| format!("Failed to read target data: {}", e))?;
            let subjects = data.subjects();
            if subjects.is_empty() {
                return Err("Target data file contains no subjects".to_string());
            }
            subjects[0].clone()
        };

        let target_enum = parse_target_type(target_type)?;
        let doserange = DoseRange::new(dose_min, dose_max);

        let problem = BestDoseProblem::new(
            &population_theta,
            &population_weights,
            past_data,
            target_data,
            time_offset,
            eq,
            doserange,
            bias_weight,
            settings,
            target_enum,
        )
        .map_err(|e| format!("Failed to create BestDose problem: {}", e))?;

        Ok(Self { problem, library })
    }

    pub fn optimize(
        &self,
        bias_weight: Option<f64>,
    ) -> std::result::Result<BestDoseResult, String> {
        let configured_problem = match bias_weight {
            Some(weight) => self.problem.clone().with_bias_weight(weight),
            None => self.problem.clone(),
        };

        configured_problem
            .optimize()
            .map_err(|e| format!("Optimization failed: {}", e))
    }

    pub fn problem(&self) -> &BestDoseProblem {
        &self.problem
    }
}

pub(crate) fn bestdose_ode(
    model_path: PathBuf,
    prior_path: PathBuf,
    past_data_path: Option<PathBuf>,
    target_data_path: PathBuf,
    time_offset: Option<f64>,
    dose_min: f64,
    dose_max: f64,
    bias_weight: f64,
    target_type: &str,
    params: List,
) -> std::result::Result<BestDoseResult, String> {
    let handle = BestDoseProblemHandle::new(
        model_path,
        prior_path,
        past_data_path,
        target_data_path,
        time_offset,
        dose_min,
        dose_max,
        bias_weight,
        target_type,
        params,
    )?;

    handle.optimize(None)
}

/// Execute bestdose optimization for analytical models (placeholder - not yet supported)
pub(crate) fn bestdose_analytical(
    _model_path: PathBuf,
    _prior_path: PathBuf,
    _past_data_path: Option<PathBuf>,
    _target_data_path: PathBuf,
    _time_offset: Option<f64>,
    _dose_min: f64,
    _dose_max: f64,
    _bias_weight: f64,
    _target_type: &str,
    _params: List,
) -> std::result::Result<BestDoseResult, String> {
    Err("BestDose for analytical models is not yet supported".to_string())
}

pub(crate) struct PosteriorSummary {
    theta_values: Vec<f64>,
    theta_dim: (i32, i32),
    param_names: Vec<String>,
    posterior_weights: Vec<f64>,
    population_weights: Vec<f64>,
    bias_weight: f64,
    target_type: Target,
}

fn summarize_problem(problem: &BestDoseProblem) -> PosteriorSummary {
    let theta = problem.posterior_theta();
    let matrix = theta.matrix();
    let nrows = matrix.nrows() as i32;
    let ncols = matrix.ncols() as i32;
    let mut theta_values = vec![0.0; (nrows * ncols) as usize];

    for col in 0..ncols as usize {
        for row in 0..nrows as usize {
            theta_values[row + col * nrows as usize] = *matrix.get(row, col);
        }
    }

    PosteriorSummary {
        theta_values,
        theta_dim: (nrows, ncols),
        param_names: theta.param_names(),
        posterior_weights: problem.posterior_weights().to_vec(),
        population_weights: problem.population_weights().to_vec(),
        bias_weight: problem.bias_weight(),
        target_type: problem.target_type(),
    }
}

fn vec_to_doubles(values: Vec<f64>, label: &str) -> std::result::Result<Doubles, String> {
    Doubles::try_from(values)
        .map_err(|e| format!("Failed to convert {} to doubles: {:?}", label, e))
}

fn dims_to_integers(dim: (i32, i32)) -> std::result::Result<Integers, String> {
    Integers::try_from(vec![dim.0, dim.1])
        .map_err(|e| format!("Failed to convert dims to integers: {:?}", e))
}

fn names_to_strings(names: &[String]) -> Strings {
    Strings::from_values(names.iter().map(|s| s.as_str()))
}

pub(crate) fn prepare_bestdose_problem(
    model_path: PathBuf,
    prior_path: PathBuf,
    past_data_path: Option<PathBuf>,
    target_data_path: PathBuf,
    time_offset: Option<f64>,
    dose_min: f64,
    dose_max: f64,
    bias_weight: f64,
    target_type: &str,
    params: List,
) -> std::result::Result<(BestDoseProblemHandle, PosteriorSummary), String> {
    let handle = BestDoseProblemHandle::new(
        model_path,
        prior_path,
        past_data_path,
        target_data_path,
        time_offset,
        dose_min,
        dose_max,
        bias_weight,
        target_type,
        params,
    )?;

    let summary = summarize_problem(handle.problem());
    Ok((handle, summary))
}

pub(crate) fn bestdose_prepare_internal(
    model_path: &str,
    prior_path: &str,
    past_data_path: Nullable<String>,
    target_data_path: &str,
    time_offset: Nullable<f64>,
    dose_min: f64,
    dose_max: f64,
    bias_weight: f64,
    target_type: &str,
    params: List,
    kind: &str,
) -> Robj {
    RFormatLayer::reset_global_timer();
    let _ = crate::setup_logs();

    let past_path = past_data_path.into_option().map(PathBuf::from);
    let time_offset = time_offset.into_option();

    let preparation = match kind {
        "ode" => prepare_bestdose_problem(
            PathBuf::from(model_path),
            PathBuf::from(prior_path),
            past_path,
            PathBuf::from(target_data_path),
            time_offset,
            dose_min,
            dose_max,
            bias_weight,
            target_type,
            params.clone(),
        ),
        "analytical" => Err("BestDose for analytical models is not yet supported".to_string()),
        other => Err(format!("{} is not a supported model type", other)),
    };

    match preparation {
        Ok((handle, summary)) => {
            let theta_values = match vec_to_doubles(summary.theta_values, "theta_values") {
                Ok(values) => values,
                Err(e) => return Robj::from(e),
            };
            let theta_dim = match dims_to_integers(summary.theta_dim) {
                Ok(dim) => dim,
                Err(e) => return Robj::from(e),
            };
            let posterior_weights =
                match vec_to_doubles(summary.posterior_weights, "posterior_weights") {
                    Ok(values) => values,
                    Err(e) => return Robj::from(e),
                };
            let population_weights =
                match vec_to_doubles(summary.population_weights, "population_weights") {
                    Ok(values) => values,
                    Err(e) => return Robj::from(e),
                };
            let param_names = names_to_strings(&summary.param_names);
            let handle_ptr = ExternalPtr::new(handle);

            let output = list!(
                handle = handle_ptr,
                theta_values = theta_values,
                theta_dim = theta_dim,
                param_names = param_names,
                posterior_weights = posterior_weights,
                population_weights = population_weights,
                bias_weight = summary.bias_weight,
                target_type = format!("{:?}", summary.target_type),
                nspp = summary.theta_dim.0,
                n_parameters = summary.theta_dim.1
            );

            output.into()
        }
        Err(e) => Robj::from(format!("BestDose prepare failed: {}", e)),
    }
}

pub(crate) fn bestdose_optimize_internal(
    handle: ExternalPtr<BestDoseProblemHandle>,
    bias_weight: Nullable<f64>,
) -> Robj {
    let weight = bias_weight.into_option();

    match handle.try_addr() {
        Ok(inner) => match inner.optimize(weight) {
            Ok(result) => match convert_bestdose_result_to_r(result) {
                Ok(robj) => robj,
                Err(e) => Robj::from(format!("Failed to convert result: {}", e)),
            },
            Err(e) => Robj::from(format!("BestDose optimization failed: {}", e)),
        },
        Err(e) => Robj::from(format!("Invalid BestDose handle: {}", e)),
    }
}
