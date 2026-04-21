use anyhow::{anyhow, bail, Context, Result as AnyResult};
use extendr_api::{Conversions, List, Robj};
use pmcore::prelude::*;
use std::collections::HashMap;

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

pub(crate) fn settings(
    settings: List,
    params: &Vec<String>,
    output_path: &str,
) -> Result<Settings> {
    let settings: HashMap<&str, Robj> = HashMap::try_from(&settings)
        .map_err(|e| anyhow!("Failed to convert settings list to map: {}", e))?;

    let ranges = get_list(&settings, "ranges")?;
    let ranges = robj_to_hashmap(ranges)?;
    let parameters = parse_parameters(ranges, params)?;

    let algorithm = get_str(&settings, "algorithm")?;
    let algorithm = match algorithm.to_lowercase().as_str() {
        "npag" => pmcore::prelude::Algorithm::NPAG,
        "npod" => pmcore::prelude::Algorithm::NPOD,
        "postprob" => pmcore::prelude::Algorithm::POSTPROB,
        _ => return Err(anyhow!("Algorithm {} not supported", algorithm)),
    };

    let error_models_raw = get_list(&settings, "error_models")?;

    let mut ems = AssayErrorModels::new().add(0, AssayErrorModel::None)?;

    for (i, (_, em)) in error_models_raw.iter().enumerate() {
        let outeq = i + 1;
        let em_list = em
            .as_list()
            .ok_or_else(|| anyhow!("error_models[{}] is not a list", outeq))?;
        let em: HashMap<&str, Robj> = HashMap::try_from(&em_list)
            .map_err(|e| anyhow!("Failed to parse error_models[{}]: {}", outeq, e))?;

        let gamlam = get_field(&em, "initial")?
            .as_real()
            .ok_or_else(|| anyhow!("error_models[{}].initial is not a real number", outeq))?;
        let type_vec = get_field(&em, "type")?
            .as_string_vector()
            .ok_or_else(|| anyhow!("error_models[{}].type is not a character vector", outeq))?;
        let err_type = type_vec
            .first()
            .ok_or_else(|| anyhow!("error_models[{}].type is empty", outeq))?;
        let fixed = get_field(&em, "fixed")?
            .as_logical()
            .ok_or_else(|| anyhow!("error_models[{}].fixed is not logical", outeq))?;
        let coeff = get_field(&em, "coeff")?
            .as_real_vector()
            .ok_or_else(|| anyhow!("error_models[{}].coeff is not a numeric vector", outeq))?;
        if coeff.len() < 4 {
            bail!(
                "error_models[{}].coeff must have at least 4 values, got {}",
                outeq,
                coeff.len()
            );
        }

        match err_type.as_str() {
            "additive" => {
                ems = ems.add(
                    outeq,
                    if fixed.to_bool() {
                        AssayErrorModel::additive_fixed(
                            ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                            gamlam,
                        )
                    } else {
                        AssayErrorModel::additive(
                            ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                            gamlam,
                        )
                    },
                )?;
            }
            "proportional" => {
                ems = ems.add(
                    outeq,
                    if fixed.to_bool() {
                        AssayErrorModel::proportional_fixed(
                            ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                            gamlam,
                        )
                    } else {
                        AssayErrorModel::proportional(
                            ErrorPoly::new(coeff[0], coeff[1], coeff[2], coeff[3]),
                            gamlam,
                        )
                    },
                )?;
            }
            err => {
                bail!("Invalid Error type: {}", err);
            }
        }
    }

    let max_cycles = get_real_or(&settings, "max_cycles", 100.0)? as usize;
    let ind_points = get_real_or(&settings, "points", 2028.0)? as usize;
    let seed = get_real_or(&settings, "seed", 22.0)? as usize;

    let prior = get_str(&settings, "prior")?;
    let prior = match prior.as_str() {
        "sobol" => pmcore::prelude::Prior::sobol(ind_points, seed),
        "prior.csv" => pmcore::prelude::Prior::File("prior.csv".to_string()),
        _ => return Err(anyhow!("Prior {} not supported", prior)),
    };

    let idelta = get_real_or(&settings, "idelta", 0.01)?;
    let tad = get_real_or(&settings, "tad", 0.0)?;

    let mut settings = Settings::builder()
        .set_algorithm(algorithm)
        .set_parameters(parameters)
        .set_error_models(ems)
        .build();
    settings.set_idelta(idelta);
    settings.set_tad(tad);
    settings.set_cycles(max_cycles);
    settings.set_prior(prior);
    settings.set_output_path(output_path.to_string());
    settings.set_write_logs(true);
    settings.set_log_level(LogLevel::INFO);
    settings.write().context("Failed to write settings")?;
    Ok(settings)
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
    params: &Vec<String>,
) -> Result<Parameters> {
    let mut parameters = Parameters::new();
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
