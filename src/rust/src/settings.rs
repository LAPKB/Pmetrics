use anyhow::{anyhow, bail, Context, Result as AnyResult};
use extendr_api::{Conversions, List, Robj};
use pmcore::prelude::*;
use std::collections::HashMap;

/// The parsed configuration required to run a non-parametric fit.
///
/// This replaces the old `pmcore::Settings` type. The new PMcore API composes a
/// fit from independent pieces (prior, error models, algorithm) instead of a
/// single settings object, and output-related options (`idelta`, `tad`, output
/// directory) are supplied directly to [`NonParametricResult::write_outputs`].
pub(crate) struct RunConfig {
    /// The prior distribution (support points) that seeds the algorithm. It also
    /// carries the parameter space, so no separate parameter declaration is needed.
    pub prior: Theta,
    /// The assay error models, one per output equation.
    pub error_models: AssayErrorModels,
    /// The configured algorithm to run.
    pub algorithm: NonParametricAlgorithm,
    /// Interval used to densify the prediction grid when writing outputs.
    pub idelta: f64,
    /// Additional time after the last event to simulate when writing outputs.
    pub tad: f64,
    /// Directory to which output artifacts are written.
    pub output_path: String,
}

/// Helper: get a field from the settings hashmap or return an error.
fn get_field<'a>(map: &'a HashMap<&str, Robj>, key: &str) -> AnyResult<&'a Robj> {
    map.get(key)
        .ok_or_else(|| anyhow!("Missing required setting '{}'", key))
}

/// Helper: get a field as a List or error.
fn get_list(map: &HashMap<&str, Robj>, key: &str) -> AnyResult<List> {
    get_field(map, key)?
        .as_list()
        .ok_or_else(|| anyhow!("Setting '{}' is not a list", key))
}

/// Helper: get a field as a string or error.
fn get_str(map: &HashMap<&str, Robj>, key: &str) -> AnyResult<String> {
    get_field(map, key)?
        .as_str()
        .ok_or_else(|| anyhow!("Setting '{}' is not a string", key))
        .map(|s| s.to_string())
}

/// Helper: get a field as a real (f64), with an optional default if it cannot be coerced.
fn get_real_or(map: &HashMap<&str, Robj>, key: &str, default: f64) -> AnyResult<f64> {
    Ok(get_field(map, key)?.as_real().unwrap_or(default))
}

pub(crate) fn settings(settings: List, params: &[String], output_path: &str) -> Result<RunConfig> {
    let settings: HashMap<&str, Robj> = HashMap::try_from(&settings)
        .map_err(|e| anyhow!("Failed to convert settings list to map: {}", e))?;

    let ranges = get_list(&settings, "ranges")?;
    let ranges = robj_to_hashmap(ranges)?;
    let parameters = parse_parameters(ranges, params)?;

    let max_cycles = get_real_or(&settings, "max_cycles", 100.0)? as usize;
    let ind_points = get_real_or(&settings, "points", 2028.0)? as usize;
    let seed = get_real_or(&settings, "seed", 22.0)? as usize;

    let algorithm = get_str(&settings, "algorithm")?;
    let algorithm: NonParametricAlgorithm = match algorithm.to_lowercase().as_str() {
        "npag" => NpagConfig::new().max_cycles(max_cycles).into(),
        "npod" => NpodConfig::new().max_cycles(max_cycles).into(),
        // Posterior probabilities are computed by a single-pass Bayesian
        // reweighting of the (fixed) prior support points, which is exactly what
        // the NCNPAG algorithm does.
        "postprob" => NcnpagConfig::new().into(),
        _ => return Err(anyhow!("Algorithm {} not supported", algorithm)),
    };

    let error_models_raw = get_list(&settings, "error_models")?;
    // Each error model declares the 1-based output equation (`outeq`) it applies
    // to. Error models are stored by output *slot* (declaration order, 0-based),
    // so the declared `outeq` maps to slot `outeq - 1`. The model DSL declares
    // outputs with 1-based numeric labels (`outeq_1`, ...) to match the Pmetrics
    // data `OUTEQ` column, and the first declared output occupies slot 0.
    let mut ems = AssayErrorModels::new();

    for (i, (_, em)) in error_models_raw.iter().enumerate() {
        let em_list = em
            .as_list()
            .ok_or_else(|| anyhow!("error_models[{}] is not a list", i + 1))?;
        let em: HashMap<&str, Robj> = HashMap::try_from(&em_list)
            .map_err(|e| anyhow!("Failed to parse error_models[{}]: {}", i + 1, e))?;

        // The output equation this error model applies to (1-based). Fall back to
        // positional order if the field is absent, preserving old behaviour.
        let outeq_1based = get_field(&em, "outeq")
            .ok()
            .and_then(|v| v.as_real())
            .map(|v| v as usize)
            .unwrap_or(i + 1);
        if outeq_1based < 1 {
            bail!("error_models[{}].outeq must be 1 or greater", i + 1);
        }
        let outeq = outeq_1based - 1;

        let gamlam = get_field(&em, "initial")?.as_real().ok_or_else(|| {
            anyhow!(
                "error_models for outeq {} initial is not a real number",
                outeq_1based
            )
        })?;
        let type_vec = get_field(&em, "type")?.as_string_vector().ok_or_else(|| {
            anyhow!(
                "error_models for outeq {} type is not a character vector",
                outeq_1based
            )
        })?;
        let err_type = type_vec
            .first()
            .ok_or_else(|| anyhow!("error_models for outeq {} type is empty", outeq_1based))?;
        let fixed = get_field(&em, "fixed")?.as_logical().ok_or_else(|| {
            anyhow!(
                "error_models for outeq {} fixed is not logical",
                outeq_1based
            )
        })?;
        let coeff = get_field(&em, "coeff")?.as_real_vector().ok_or_else(|| {
            anyhow!(
                "error_models for outeq {} coeff is not a numeric vector",
                outeq_1based
            )
        })?;
        if coeff.len() < 4 {
            bail!(
                "error_models for outeq {} coeff must have at least 4 values, got {}",
                outeq_1based,
                coeff.len()
            );
        }

        let poly = ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]);
        let model = match err_type.as_str() {
            "additive" => {
                if fixed.to_bool() {
                    AssayErrorModel::additive_fixed(poly, gamlam)
                } else {
                    AssayErrorModel::additive(poly, gamlam)
                }
            }
            "proportional" => {
                if fixed.to_bool() {
                    AssayErrorModel::proportional_fixed(poly, gamlam)
                } else {
                    AssayErrorModel::proportional(poly, gamlam)
                }
            }
            err => bail!("Invalid Error type: {}", err),
        };
        ems = ems.add(outeq, model)?;
    }

    let prior = get_str(&settings, "prior")?;
    let prior = match prior.as_str() {
        "sobol" => Theta::sobol_with_seed(&parameters, ind_points, seed)
            .context("Failed to build Sobol prior")?,
        "prior.csv" => {
            let (theta, _weights) = Theta::from_file("prior.csv", &parameters)
                .context("Failed to read prior from prior.csv")?;
            theta
        }
        _ => return Err(anyhow!("Prior {} not supported", prior)),
    };

    let idelta = get_real_or(&settings, "idelta", 0.01)?;
    let tad = get_real_or(&settings, "tad", 0.0)?;

    Ok(RunConfig {
        prior,
        error_models: ems,
        algorithm,
        idelta,
        tad,
        output_path: output_path.to_string(),
    })
}

fn robj_to_hashmap(list: List) -> AnyResult<HashMap<String, (f64, f64)>> {
    let mut map: HashMap<String, (f64, f64)> = HashMap::new();
    for (name, value) in list.iter() {
        let ranges = value
            .as_real_slice()
            .ok_or_else(|| anyhow!("Range for parameter '{}' is not numeric", name))?;
        if ranges.len() < 2 {
            bail!(
                "Range for parameter '{}' must have at least 2 values, got {}",
                name,
                ranges.len()
            );
        }
        map.insert(name.to_owned(), (ranges[0], ranges[1]));
    }
    Ok(map)
}

fn parse_parameters(
    ranges: HashMap<String, (f64, f64)>,
    params: &[String],
) -> Result<ParameterSpace<BoundedParameter>> {
    let mut parameters = ParameterSpace::bounded();
    for param in params.iter() {
        let (min, max) = match ranges.get(param) {
            Some(range) => range,
            None => {
                return Err(anyhow::anyhow!(
                    "Parameter {} not found in ranges {:?}",
                    param,
                    &ranges
                ));
            }
        };
        parameters = parameters.add(param.to_string(), *min, *max);
    }

    Ok(parameters)
}
